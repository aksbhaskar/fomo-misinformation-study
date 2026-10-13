# 01_clean.R
# Reads the raw survey export, checks validity, applies the exclusion rules in
# docs/analysis-plan.md, scores the scales per materials/scales.md, and writes
# the analysis datasets to data/processed/ and an exclusion report to outputs/.
#
# Blinding: this script never prints or summarizes the share-likelihood or
# truth-belief ratings. They are checked for validity (range, missingness) only.
#
# Run from the project root: Rscript analysis/01_clean.R

raw_path      <- "data/raw/responses_raw.csv"
processed_dir <- "data/processed"
report_path   <- "outputs/exclusions.md"

# SHA-256 of the raw file as received on 12 October 2026. The script stops if
# the file has changed.
expected_sha256 <- "7bdffdd8802b169ce9d0d86e6c8a2b2333b1610e38b7bb61b44e6473529a5cca"


# ---- 1. Read and verify the raw file -----------------------------------------

raw_sha256 <- digest::digest(raw_path, algo = "sha256", file = TRUE)
if (raw_sha256 != expected_sha256) {
  stop("Raw file checksum does not match the recorded value. Has the file changed?")
}

raw <- read.csv(raw_path, check.names = FALSE, colClasses = "character",
                na.strings = character(0), encoding = "UTF-8")


# ---- 2. Rename columns by position -------------------------------------------

bfi_items     <- sprintf("bfi_%02d", 1:15)
fomo_items    <- sprintf("fomo_%02d", 1:10)
trait_items   <- c(bfi_items, fomo_items)
rating_items  <- as.vector(rbind(sprintf("share_v%d", 1:5), sprintf("truth_v%d", 1:5)))
share_items   <- sprintf("share_v%d", 1:5)
truth_items   <- sprintf("truth_v%d", 1:5)

new_names <- c("consent", "age", "gender", "state", "invested",
               trait_items, rating_items)

stopifnot(ncol(raw) == length(new_names))
# Spot-check that positions line up with the expected question text.
stopifnot(
  grepl("agree to participate", names(raw)[1]),
  grepl("age", names(raw)[2]),
  grepl("Tends to be quiet", names(raw)[6]),
  grepl("original, comes up with new ideas", names(raw)[20]),
  grepl("more rewarding experiences than me", names(raw)[21]),
  grepl("keep tabs on what my friends", names(raw)[30]),
  all(grepl("likely would you be to share", names(raw)[seq(31, 39, 2)])),
  all(grepl("How true do you think", names(raw)[seq(32, 40, 2)]))
)

d <- raw
names(d) <- new_names
d[] <- lapply(d, trimws)
n_start <- nrow(d)


# ---- 3. Validity checks (counts only) -----------------------------------------

count_invalid <- function(x, lo, hi) {
  v <- suppressWarnings(as.numeric(x))
  c(missing      = sum(is.na(x) | !nzchar(x)),
    out_of_range = sum(nzchar(x) & (is.na(v) | v != round(v) | v < lo | v > hi)))
}

validity <- rbind(
  trait_items  = count_invalid(unlist(d[trait_items]), 1, 5),
  rating_items = count_invalid(unlist(d[rating_items]), 1, 7)
)
cat("Validity checks (cell counts):\n")
print(validity)
cat("Responses without consent recorded:",
    sum(d$consent != "I agree to participate."), "\n\n")

d[c(trait_items, rating_items)] <- lapply(d[c(trait_items, rating_items)], as.integer)


# ---- 4. Age ------------------------------------------------------------------

age_raw <- d$age
d$age <- ifelse(grepl("^[0-9]+$", age_raw), as.integer(age_raw), NA_integer_)
unparsed_age <- sort(unique(age_raw[is.na(d$age)]))
cat("Age strings that could not be parsed:",
    if (length(unparsed_age)) paste(shQuote(unparsed_age), collapse = ", ") else "none",
    "\n\n")


# ---- 5. State names ----------------------------------------------------------

states_and_uts <- c(
  "Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh", "Goa",
  "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka", "Kerala",
  "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland",
  "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura",
  "Uttar Pradesh", "Uttarakhand", "West Bengal",
  "Andaman and Nicobar Islands", "Chandigarh",
  "Dadra and Nagar Haveli and Daman and Diu", "Delhi", "Jammu and Kashmir",
  "Ladakh", "Lakshadweep", "Puducherry"
)

# Variant spellings, city names, and short forms, keyed in lower case.
state_variants <- c(
  "new delhi"   = "Delhi",
  "nct"         = "Delhi",
  "nct of delhi"= "Delhi",
  "delhi ncr"   = "Delhi",
  "kashmir"     = "Jammu and Kashmir",
  "j&k"         = "Jammu and Kashmir",
  "bengaluru"   = "Karnataka",
  "bangalore"   = "Karnataka",
  "gurugram"    = "Haryana",
  "gurgaon"     = "Haryana"
)

standardize_state <- function(x) {
  key <- tolower(x)
  out <- states_and_uts[match(key, tolower(states_and_uts))]
  out[is.na(out)] <- unname(state_variants[key[is.na(out)]])
  out
}

state_raw <- d$state
d$state <- standardize_state(state_raw)
if (anyNA(d$state)) {
  stop("Unmapped state values: ",
       paste(unique(state_raw[is.na(d$state)]), collapse = ", "))
}
state_changes <- unique(data.frame(raw = state_raw, standardized = d$state)[state_raw != d$state, ])
cat("State names changed during standardization:\n")
print(state_changes[order(state_changes$standardized, state_changes$raw), ], row.names = FALSE)
cat("\n")


# ---- 6. Exclusions, in the order given in the analysis plan -------------------

steps <- data.frame(step = character(), rule = character(),
                    removed = integer(), remaining = integer())
log_step <- function(steps, step, rule, keep, data) {
  rbind(steps, data.frame(step = step, rule = rule,
                          removed = sum(!keep), remaining = sum(keep)))
}

# (a) Age under 18, missing, non-numeric, or over 100.
keep <- !is.na(d$age) & d$age >= 18 & d$age <= 100
steps <- log_step(steps, "(a)", "Age under 18, missing, non-numeric, or over 100", keep)
d <- d[keep, ]

# (b) Exact duplicate submissions on all items; keep the first by row order.
keep <- !duplicated(d[new_names])
steps <- log_step(steps, "(b)", "Exact duplicate of an earlier submission on all items", keep)
d <- d[keep, ]

# (c) Identical response on all 25 trait items.
keep <- apply(d[trait_items], 1, function(r) length(unique(r)) > 1)
steps <- log_step(steps, "(c)", "Identical response on all 25 trait items", keep)
d <- d[keep, ]

n_final <- nrow(d)
cat("Exclusions:\n")
print(steps, row.names = FALSE)
cat("Final N:", n_final, "\n\n")


# ---- 7. Robustness flag (not an exclusion) ------------------------------------

d$longest_run <- apply(d[trait_items], 1, function(r) max(rle(as.integer(r))$lengths))
d$flag_long_run <- d$longest_run >= 10
cat("Flagged for robustness check 6 (longest run >= 10):", sum(d$flag_long_run), "\n\n")


# ---- 8. Score the scales -------------------------------------------------------

reverse_items <- c("bfi_01", "bfi_03", "bfi_07", "bfi_08", "bfi_10", "bfi_14")
scored <- d
scored[reverse_items] <- lapply(scored[reverse_items], function(x) 6L - x)

domains <- list(
  extraversion          = c("bfi_01", "bfi_06", "bfi_11"),
  agreeableness         = c("bfi_02", "bfi_07", "bfi_12"),
  conscientiousness     = c("bfi_03", "bfi_08", "bfi_13"),
  negative_emotionality = c("bfi_04", "bfi_09", "bfi_14"),
  open_mindedness       = c("bfi_05", "bfi_10", "bfi_15")
)
for (dom in names(domains)) d[[dom]] <- rowMeans(scored[domains[[dom]]])
d$fomo <- rowMeans(scored[fomo_items])

# Acquiescence-corrected domain scores (robustness check 7): each person's 15
# raw BFI-2-XS items are centered on their own mean, reverse-keyed items are
# multiplied by -1, and each domain is the mean of its three centered items.
centered <- d[bfi_items] - rowMeans(d[bfi_items])
centered[reverse_items] <- -centered[reverse_items]
for (dom in names(domains)) d[[paste0(dom, "_acq")]] <- rowMeans(centered[domains[[dom]]])

d$participant_id <- seq_len(n_final)


# ---- 9. Save the analysis datasets --------------------------------------------

trait_scores <- c(names(domains), "fomo", paste0(names(domains), "_acq"))
covariates   <- c("age", "gender", "invested")

# Item columns are saved as answered (before reverse-scoring).
participants <- d[c("participant_id", covariates, "state", trait_scores,
                    trait_items, "longest_run", "flag_long_run")]

ratings_long <- do.call(rbind, lapply(1:5, function(v) {
  data.frame(participant_id = d$participant_id, vignette = v,
             share = d[[share_items[v]]], truth = d[[truth_items[v]]],
             d[c(trait_scores, covariates)])
}))
ratings_long <- ratings_long[order(ratings_long$participant_id, ratings_long$vignette), ]

dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)
write.csv(participants, file.path(processed_dir, "participants.csv"), row.names = FALSE)
write.csv(ratings_long, file.path(processed_dir, "ratings_long.csv"), row.names = FALSE)
cat("Wrote", nrow(participants), "participants and", nrow(ratings_long),
    "ratings rows to", processed_dir, "\n")


# ---- 10. Exclusion report ------------------------------------------------------

table_rows <- c(
  sprintf("| Start | Submissions in the exported file | | %d |", n_start),
  sprintf("| %s | %s | %d | %d |", steps$step, steps$rule, steps$removed, steps$remaining),
  sprintf("| Final | Analytic sample | | **%d** |", n_final)
)

report <- c(
  "# Exclusions",
  "",
  "Generated by `analysis/01_clean.R`. Exclusion rules are specified in `docs/analysis-plan.md`.",
  "",
  sprintf("Raw data file SHA-256: `%s`", raw_sha256),
  "",
  "Rows from respondents under 18 were removed from the response sheet before export; the number was not recorded.",
  "",
  "| Step | Rule | Removed | Remaining |",
  "|---|---|---|---|",
  table_rows,
  "",
  sprintf("Participants flagged for robustness check 6 (longest run of identical consecutive answers across the 25 trait items is 10 or more): %d. These participants are not excluded from the primary analysis.",
          sum(d$flag_long_run))
)

dir.create(dirname(report_path), showWarnings = FALSE, recursive = TRUE)
writeLines(report, report_path)
cat("Wrote", report_path, "\n")
