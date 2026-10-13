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
p <- p[c("participant_id", "age", "conscientiousness", "negative_emotionality",
         "fomo", bfi_items, fomo_items)]

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
  ""
)

dir.create(dirname(report_path), showWarnings = FALSE, recursive = TRUE)
writeLines(report, report_path)
cat("Wrote", report_path, "\n")
