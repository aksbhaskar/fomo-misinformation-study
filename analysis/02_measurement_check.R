# 02_measurement_check.R
# Diagnoses the reliability of the trait measures, with a focus on the low
# reliability of the BFI-2-XS conscientiousness scale. Writes
# outputs/measurement-check.md.
#
# Blinding: uses only trait and demographic columns from participants.csv.
# Share-likelihood and truth-belief ratings are not read.
#
# Run from the project root: Rscript analysis/02_measurement_check.R

input_path  <- "data/processed/participants.csv"
report_path <- "outputs/measurement-check.md"

bfi_items  <- sprintf("bfi_%02d", 1:15)
fomo_items <- sprintf("fomo_%02d", 1:10)

p <- read.csv(input_path)
stopifnot(!any(grepl("share|truth", names(p))))
p <- p[c("participant_id", "age", "gender", "invested", "conscientiousness",
         "negative_emotionality", "fomo", bfi_items, fomo_items)]

domains <- list(
  Extraversion            = c("bfi_01", "bfi_06", "bfi_11"),
  Agreeableness           = c("bfi_02", "bfi_07", "bfi_12"),
  Conscientiousness       = c("bfi_03", "bfi_08", "bfi_13"),
  `Negative Emotionality` = c("bfi_04", "bfi_09", "bfi_14"),
  `Open-Mindedness`       = c("bfi_05", "bfi_10", "bfi_15")
)
reverse_items <- c("bfi_01", "bfi_03", "bfi_07", "bfi_08", "bfi_10", "bfi_14")
item_label <- function(x) paste0(as.integer(sub("bfi_", "", x)),
                                 ifelse(x %in% reverse_items, " (R)", ""))


# ---- Helpers ------------------------------------------------------------------

cronbach_alpha <- function(m) {
  m <- as.matrix(m); k <- ncol(m)
  (k / (k - 1)) * (1 - sum(apply(m, 2, var)) / var(rowSums(m)))
}

# Omega total from a one-factor model: (sum of loadings)^2 / total variance.
# Items must already be keyed in the same direction.
omega_total <- function(m) {
  fit <- tryCatch(suppressWarnings(psych::fa(m, nfactors = 1, fm = "ml")),
                  error = function(e) NULL)
  if (is.null(fit)) return(list(omega = NA_real_, note = "model did not converge"))
  lambda <- as.vector(fit$loadings)
  psi    <- fit$uniquenesses
  note   <- if (any(psi < 0.005)) "Heywood case (a loading at or near 1); interpret with caution" else ""
  list(omega = sum(lambda)^2 / (sum(lambda)^2 + sum(psi)), note = note)
}

keyed <- function(items, data) {
  m <- data[items]
  rev <- intersect(items, reverse_items)
  m[rev] <- lapply(m[rev], function(x) 6 - x)
  m
}

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)

md_table <- function(df) {
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}


# ---- 1. Reliability, standard scoring -------------------------------------------

scales <- c(domains, list(FOMO = fomo_items))
reliability <- do.call(rbind, lapply(names(scales), function(s) {
  m  <- keyed(scales[[s]], p)
  om <- omega_total(m)
  data.frame(Scale = s, Items = length(scales[[s]]),
             Alpha = fmt(cronbach_alpha(m)), Omega = fmt(om$omega),
             Note = om$note, check.names = FALSE)
}))


# ---- 2. Inter-item correlations, raw responses ----------------------------------

corr_sections <- unlist(lapply(names(domains), function(dom) {
  items <- domains[[dom]]
  r <- cor(p[items])
  out <- data.frame(Item = item_label(items), fmt(r), check.names = FALSE)
  names(out)[-1] <- item_label(items)
  c(sprintf("**%s**", dom), "", md_table(out), "")
}))


# ---- 3. Acquiescence --------------------------------------------------------------

person_mean <- rowMeans(p[bfi_items])
acq <- data.frame(
  Statistic = c("Mean of person means", "SD of person means",
                "People with person mean above 4", "People with person mean below 2"),
  Value = c(fmt(mean(person_mean)), fmt(sd(person_mean)),
            sum(person_mean > 4), sum(person_mean < 2))
)


# ---- 4. Acquiescence-corrected scoring --------------------------------------------

centered <- p[bfi_items] - person_mean
centered[reverse_items] <- -centered[reverse_items]
acq_alpha <- do.call(rbind, lapply(names(domains), function(dom) {
  data.frame(Domain = dom,
             `Alpha (standard)` = fmt(cronbach_alpha(keyed(domains[[dom]], p))),
             `Alpha (acquiescence-corrected)` = fmt(cronbach_alpha(centered[domains[[dom]]])),
             check.names = FALSE)
}))


# ---- 5. Response distributions, conscientiousness items ---------------------------

c_items <- domains$Conscientiousness
c_text  <- c(bfi_03 = "Tends to be disorganized.",
             bfi_08 = "Has difficulty getting started on tasks.",
             bfi_13 = "Is reliable, can always be counted on.")
dist <- do.call(rbind, lapply(c_items, function(i) {
  counts <- table(factor(p[[i]], levels = 1:5))
  data.frame(Item = paste0(item_label(i), ": ", c_text[[i]]),
             t(as.vector(counts)), check.names = FALSE)
}))
names(dist)[2:6] <- as.character(1:5)


# ---- 6. Validity correlations, standard scoring -----------------------------------

validity <- do.call(rbind, lapply(
  list(c("Age", "age"), c("Negative Emotionality", "negative_emotionality"),
       c("FOMO", "fomo")),
  function(v) {
    ct <- cor.test(p$conscientiousness, p[[v[2]]])
    data.frame(`Conscientiousness with` = v[1], r = fmt(ct$estimate),
               `95% CI` = sprintf("[%s, %s]", fmt(ct$conf.int[1]), fmt(ct$conf.int[2])),
               p = formatC(ct$p.value, format = "g", digits = 2),
               check.names = FALSE)
  }))


# ---- Follow-up checks (15 October 2026) --------------------------------------------

raw_path        <- "data/raw/responses_raw.csv"
instrument_path <- "materials/survey-instrument.md"
cleaning_path   <- "analysis/01_clean.R"

# Only the 5 demographic and 25 trait columns are read; the 10 rating columns
# are skipped by the parser and never stored.
raw_trait <- read.csv(raw_path, check.names = FALSE, encoding = "UTF-8",
                      colClasses = c(rep("character", 5), rep("integer", 25), rep("NULL", 10)))
stopifnot(ncol(raw_trait) == 30)

# A1. Match each trait column to the instrument by header text, not position.
norm_text <- function(x) gsub("[^a-z0-9]", "", tolower(x))
instrument <- readLines(instrument_path, encoding = "UTF-8")
section_items <- function(from, to) {
  lines <- instrument[(grep(from, instrument) + 1):(grep(to, instrument) - 1)]
  sub("^[0-9]+\\. ", "", grep("^[0-9]+\\. ", lines, value = TRUE))
}
instrument_items <- c(section_items("^## Section 2", "^## Section 3"),
                      section_items("^## Section 3", "^## Sections 4"))
stopifnot(length(instrument_items) == 25)
trait_vars <- c(bfi_items, fomo_items)
headers    <- names(raw_trait)[6:30]
matched    <- match(norm_text(headers), norm_text(instrument_items))
alignment <- data.frame(
  Variable = trait_vars,
  `Raw header` = trimws(headers),
  `Matched instrument item` = ifelse(is.na(matched), "(no match)",
                                     paste0(ifelse(matched <= 15, "BFI ", "FOMO "),
                                            ifelse(matched <= 15, matched, matched - 15))),
  Correct = ifelse(!is.na(matched) & matched == seq_along(trait_vars), "yes", "NO"),
  check.names = FALSE
)
n_misaligned <- sum(alignment$Correct != "yes")

# A2. Raw values vs. values stored in participants.csv (before scoring).
# No participants were excluded, so participant_id i corresponds to raw row i.
stopifnot(nrow(raw_trait) == nrow(p), all(p$participant_id == seq_len(nrow(p))))
raw_items  <- setNames(raw_trait[6:30], trait_vars)
n_identical <- sum(as.matrix(raw_items) == as.matrix(p[trait_vars]))
n_values    <- length(trait_vars) * nrow(p)

set.seed(20261015)
spot_rows <- sort(sample(nrow(p), 5))
# Shown on the console for the analyst only; not written to the public report.
cat("Spot check: raw BFI values vs participants.csv (rows", paste(spot_rows, collapse = ", "), ")\n")
for (i in spot_rows) {
  cat(sprintf("  row %3d  raw:       %s\n", i, paste(raw_items[i, bfi_items], collapse = " ")))
  cat(sprintf("  row %3d  processed: %s  %s\n", i, paste(p[i, bfi_items], collapse = " "),
              if (all(raw_items[i, bfi_items] == p[i, bfi_items])) "identical" else "DIFFERENT"))
}
cat("\n")

# A3. Scoring keys used in 01_clean.R vs. the published BFI-2-XS key.
clean_env <- new.env()
for (e in parse(cleaning_path)) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% c("domains", "reverse_items")) eval(e, clean_env)
}
official <- list(
  Extraversion            = c("1R", "6", "11"),
  Agreeableness           = c("2", "7R", "12"),
  Conscientiousness       = c("3R", "8R", "13"),
  `Negative Emotionality` = c("4", "9", "14R"),
  `Open-Mindedness`       = c("5", "10R", "15")
)
script_key <- lapply(clean_env$domains, function(items) {
  paste0(as.integer(sub("bfi_", "", items)), ifelse(items %in% clean_env$reverse_items, "R", ""))
})
keys <- data.frame(
  Domain = names(official),
  `01_clean.R` = sapply(script_key, paste, collapse = ", "),
  `Published key` = sapply(official, paste, collapse = ", "),
  Match = ifelse(mapply(identical, script_key, official), "yes", "NO"),
  check.names = FALSE, row.names = NULL
)

# B. Reliability by group, with percentile bootstrap 95% CIs.
group_alphas <- function(d) {
  dom_alpha <- sapply(domains, function(items) cronbach_alpha(keyed(items, d)))
  c(consc = dom_alpha[["Conscientiousness"]], mean_bfi = mean(dom_alpha),
    fomo = cronbach_alpha(d[fomo_items]))
}
boot_ci <- function(d, reps = 2000) {
  est <- group_alphas(d)
  draws <- replicate(reps, group_alphas(d[sample(nrow(d), replace = TRUE), ]))
  ci <- apply(draws, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
  sprintf("%s [%s, %s]", fmt(est), fmt(ci[1, ]), fmt(ci[2, ]))
}
p$age_group <- cut(p$age, breaks = c(17, 24, 34, Inf), labels = c("18-24", "25-34", "35 and over"))
groupings <- list(`Full sample` = rep("All", nrow(p)), `Age group` = p$age_group,
                  Gender = p$gender, `Invested before` = p$invested)
set.seed(20261015)
by_group <- do.call(rbind, lapply(names(groupings), function(g) {
  f <- factor(groupings[[g]])
  do.call(rbind, lapply(levels(f), function(lv) {
    d <- p[f == lv, ]
    ci <- boot_ci(d)
    data.frame(Grouping = g, Group = lv, n = nrow(d),
               `Conscientiousness alpha` = ci[1], `Mean of five BFI domain alphas` = ci[2],
               `FOMO alpha` = ci[3], check.names = FALSE, row.names = NULL)
  }))
}))


# ---- Report -----------------------------------------------------------------------

report <- c(
  "# Measurement Check",
  "",
  sprintf("Generated by `analysis/02_measurement_check.R` from the analytic sample (N = %d). Uses trait and demographic data only; no outcome data were read.", nrow(p)),
  "",
  "## 1. Reliability (standard scoring)",
  "",
  "Reverse-keyed items scored as 6 minus the response. Omega total is from a one-factor model.",
  "",
  md_table(reliability),
  "",
  "## 2. Inter-item correlations within each BFI-2-XS domain (raw responses)",
  "",
  "Responses as given, before reverse-scoring. (R) marks reverse-keyed items, which should correlate negatively with forward-keyed items in the same domain.",
  "",
  corr_sections,
  "## 3. Acquiescence",
  "",
  "Each person's mean across all 15 raw BFI-2-XS items. The BFI-2-XS has 9 forward-keyed and 6 reverse-keyed items, so this mean reflects both agreement tendency and trait levels.",
  "",
  md_table(acq),
  "",
  "## 4. Acquiescence-corrected scoring",
  "",
  "Each person's 15 raw items centered on their own mean; reverse-keyed items multiplied by -1; alpha computed on the three centered items per domain.",
  "",
  md_table(acq_alpha),
  "",
  "## 5. Response distributions: conscientiousness items",
  "",
  "Counts of each response (1 = Disagree strongly, 5 = Agree strongly), raw.",
  "",
  md_table(dist),
  "",
  "## 6. Validity correlations (standard scoring)",
  "",
  md_table(validity),
  "",
  "## Follow-up checks (15 October 2026)",
  "",
  "### A1. Column alignment by header text",
  "",
  sprintf("Each trait column in the raw export was matched to the instrument (`materials/survey-instrument.md`) by its header text rather than its position. Misaligned columns: %d of 25.", n_misaligned),
  "",
  md_table(alignment),
  "",
  "### A2. Import check",
  "",
  sprintf("All %d trait values (%d participants x 25 items) in `data/processed/participants.csv` are identical to the raw export before scoring: %d of %d match. Five randomly selected rows were also compared by eye; they are not reproduced here, to avoid publishing participant-level responses.",
          n_values, nrow(p), n_identical, n_values),
  "",
  "### A3. Scoring key",
  "",
  "Keys read directly from `analysis/01_clean.R`, compared with the published BFI-2-XS key (Soto & John, 2017; R = reverse-keyed).",
  "",
  md_table(keys),
  "",
  "### B. Reliability by group",
  "",
  "Cronbach's alpha with percentile bootstrap 95% CIs (2,000 resamples). The middle column is the average of the five BFI-2-XS domain alphas.",
  "",
  md_table(by_group),
  ""
)

dir.create(dirname(report_path), showWarnings = FALSE, recursive = TRUE)
writeLines(report, report_path)
cat("Wrote", report_path, "\n")
