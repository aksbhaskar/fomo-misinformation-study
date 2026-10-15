# 03_models.R
# Implements docs/analysis-plan.md: descriptives, primary and secondary models,
# interaction probing, robustness checks 1-7, sensitivity analysis, and
# exploratory analyses.
#
# BLINDING SWITCH
#   UNBLIND <- FALSE  Share and truth ratings are SIMULATED from known effects.
#                     The real ratings file is never opened. Outputs go to a
#                     temporary folder outside the repository.
#   UNBLIND <- TRUE   Real ratings are used. Outputs go to outputs/results/.
#
# Run from the project root: Rscript analysis/03_models.R
# In simulated mode, `Rscript analysis/03_models.R floor` simulates share
# ratings concentrated at 1, to test the outcome-suitability rule.

UNBLIND <- FALSE

args <- commandArgs(trailingOnly = TRUE)
SIM_SCENARIO <- if (length(args) && args[1] == "floor") "floor" else "default"   # simulated mode only


# ---- Settings -------------------------------------------------------------------

participants_path <- "data/processed/participants.csv"
ratings_path      <- "data/processed/ratings_long.csv"   # read only when UNBLIND is TRUE
diagnostics_path  <- "outputs/measurement-check.md"

# Sensitivity analysis: interaction sizes tested (per 1 SD of each predictor,
# in rating-scale points) and simulations per size.
sens_grid  <- if (UNBLIND) seq(0.05, 0.40, by = 0.05) else c(0.10, 0.30)
sens_nsim  <- if (UNBLIND) 500 else 5

if (UNBLIND) {
  out_dir <- "outputs/results"
} else {
  out_dir <- file.path(dirname(tempdir()), paste0("fomo-misinformation-simulated-outputs",
                                                  if (SIM_SCENARIO == "floor") "-floor" else ""))
  if (startsWith(normalizePath(out_dir, mustWork = FALSE), normalizePath("."))) {
    stop("Simulated outputs must be written outside the repository.")
  }
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

mode_label <- if (UNBLIND) "REAL DATA" else
  sprintf("SIMULATED DATA (outcomes are not real; scenario: %s)", SIM_SCENARIO)
expl <- "EXPLORATORY"
cat("Mode:", mode_label, "\nOutputs:", normalizePath(out_dir), "\n\n")


# ---- Helpers ----------------------------------------------------------------------

warning_log <- character()

# Evaluates a model fit, recording (not hiding) any warnings and convergence or
# singularity messages.
fit_logged <- function(label, expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      warning_log <<- c(warning_log, paste0(label, ": ", conditionMessage(w)))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      if (grepl("singular|converge|boundary", conditionMessage(m), ignore.case = TRUE)) {
        warning_log <<- c(warning_log, paste0(label, ": ", trimws(conditionMessage(m))))
      }
      invokeRestart("muffleMessage")
    }
  )
}

z <- function(x) as.numeric(scale(x))

md_table <- function(df) {
  c(paste0("| ", paste(names(df), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|"),
    apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}

format_for_md <- function(df) {
  df[] <- lapply(names(df), function(n) {
    x <- df[[n]]
    if (!is.numeric(x)) return(as.character(x))
    if (n == "p") return(ifelse(is.na(x), "", ifelse(x < 0.001, "< .001", formatC(x, format = "f", digits = 3))))
    if (all(is.na(x) | x == round(x))) return(ifelse(is.na(x), "", format(x, big.mark = "")))
    ifelse(is.na(x), "", formatC(x, format = "f", digits = 3))
  })
  df
}

save_table <- function(df, name, title, notes = NULL) {
  write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE)
  writeLines(c(paste("#", title), "", paste0("*", mode_label, "*"), "",
               if (length(notes)) c(notes, "") else NULL,
               md_table(format_for_md(df))),
             file.path(out_dir, paste0(name, ".md")))
}

# Coefficient table for lmerTest, lm, and clmm fits, with 95% CIs.
tidy_fit <- function(fit) {
  s <- coef(summary(fit))
  if (inherits(fit, "clmm")) {
    df <- Inf; stat <- s[, "z value"]; p <- s[, "Pr(>|z|)"]
  } else if (inherits(fit, "lm")) {
    df <- fit$df.residual; stat <- s[, "t value"]; p <- s[, "Pr(>|t|)"]
  } else {
    df <- s[, "df"]; stat <- s[, "t value"]; p <- s[, "Pr(>|t|)"]
  }
  crit <- qt(0.975, df)
  data.frame(term = rownames(s), estimate = s[, "Estimate"], se = s[, "Std. Error"],
             df = df, statistic = stat, p = p,
             ci_low  = s[, "Estimate"] - crit * s[, "Std. Error"],
             ci_high = s[, "Estimate"] + crit * s[, "Std. Error"],
             row.names = NULL)
}


# ---- Data -------------------------------------------------------------------------

p <- read.csv(participants_path, stringsAsFactors = FALSE)
stopifnot(!any(grepl("share|truth", names(p))))

# Standardize at the person level, so each participant counts once.
p$consc_z     <- z(p$conscientiousness)
p$fomo_z      <- z(p$fomo)
p$consc_acq_z <- z(p$conscientiousness_acq)
p$extraversion_z          <- z(p$extraversion)
p$agreeableness_z         <- z(p$agreeableness)
p$negative_emotionality_z <- z(p$negative_emotionality)
p$open_mindedness_z       <- z(p$open_mindedness)
p$age_z    <- z(p$age)
p$gender   <- factor(p$gender)
p$invested <- factor(p$invested, levels = c("No", "Yes"))

# One row per participant x vignette.
long <- p[rep(seq_len(nrow(p)), each = 5), ]
long$vignette <- rep(1:5, times = nrow(p))
rownames(long) <- NULL

# Known effects used to simulate outcomes when blinded.
sim_truth <- list(
  share = c(intercept = 3.5, consc_z = -0.25, fomo_z = 0.30, `consc_z:fomo_z` = -0.20),
  truth = c(intercept = 3.0, consc_z = -0.15, fomo_z = 0.20, `consc_z:fomo_z` = -0.10)
)
# The "floor" scenario lowers the share intercept so most ratings are 1.
if (SIM_SCENARIO == "floor") sim_truth$share[["intercept"]] <- 0.5
sim_vignette_effects <- c(0.4, 0.1, -0.2, 0.0, -0.3)   # sum to zero
sim_sd_person <- 0.8
sim_sd_resid  <- 1.0

simulate_outcome <- function(eff, data) {
  ids <- unique(data$participant_id)
  u <- rnorm(length(ids), 0, sim_sd_person)[match(data$participant_id, ids)]
  mu <- eff[["intercept"]] + eff[["consc_z"]] * data$consc_z + eff[["fomo_z"]] * data$fomo_z +
    eff[["consc_z:fomo_z"]] * data$consc_z * data$fomo_z + sim_vignette_effects[data$vignette]
  pmin(pmax(round(mu + u + rnorm(nrow(data), 0, sim_sd_resid)), 1), 7)
}

if (UNBLIND) {
  ratings <- read.csv(ratings_path)[c("participant_id", "vignette", "share", "truth")]
  long <- merge(long, ratings, by = c("participant_id", "vignette"))
  long <- long[order(long$participant_id, long$vignette), ]
  stopifnot(nrow(long) == 5 * nrow(p), !anyNA(long$share), !anyNA(long$truth))
} else {
  set.seed(20261013)
  long$share <- simulate_outcome(sim_truth$share, long)
  long$truth <- simulate_outcome(sim_truth$truth, long)
}

long$vignette <- factor(long$vignette, levels = 1:5)
contrasts(long$vignette) <- contr.sum(5)
long$participant_id <- factor(long$participant_id)


# ---- Outcome suitability (runs before any model) ---------------------------------
# Rule set before unblinding (Deviations, 15 October 2026): if more than 50% of
# all share-likelihood ratings equal 1, the cumulative link mixed model
# (robustness check 1) is the primary estimate for H1.

cronbach_alpha <- function(m) {
  m <- as.matrix(m); k <- ncol(m)
  (k / (k - 1)) * (1 - sum(apply(m, 2, var)) / var(rowSums(m)))
}

share_wide <- sapply(levels(long$vignette), function(v) {
  rows <- long$vignette == v
  long$share[rows][order(long$participant_id[rows])]
})
prop_ones <- mean(long$share == 1)
h1_primary <- if (prop_ones > 0.5) "clmm" else "lmer"
h1_primary_label <- if (h1_primary == "clmm") {
  "Cumulative link mixed model (robustness check 1)"
} else {
  "Linear mixed model (primary model)"
}

suitability <- do.call(rbind, lapply(c("All", levels(long$vignette)), function(v) {
  x <- if (v == "All") long$share else long$share[long$vignette == v]
  counts <- as.vector(table(factor(x, levels = 1:7)))
  setNames(data.frame(v, length(x), 100 * mean(x == 1), t(counts)),
           c("vignette", "n_ratings", "percent_equal_1", as.character(1:7)))
}))
alpha_share <- cronbach_alpha(share_wide)
save_table(suitability, "table_00_outcome_suitability",
           "Outcome suitability: share-likelihood ratings",
           notes = c(sprintf("%.1f%% of all share-likelihood ratings equal 1 (rule threshold: more than 50%%).", 100 * prop_ones),
                     sprintf("Cronbach's alpha across each participant's five share ratings: %.2f.", alpha_share),
                     sprintf("Primary estimate for H1: %s.", h1_primary_label)))
cat(sprintf("Outcome suitability: %.1f%% of share ratings equal 1 -> primary model for H1: %s\n\n",
            100 * prop_ones, h1_primary_label))

# Person-level outcome means (used in descriptives and robustness check 4).
person_means <- aggregate(cbind(mean_share = share, mean_truth = truth) ~ participant_id,
                          data = long, FUN = mean)
p <- merge(p, person_means, by = "participant_id")


# ---- Descriptives -----------------------------------------------------------------

sample_table <- data.frame(
  characteristic = c("N", "Age, mean (SD)", "Age, range",
                     paste0("Gender: ", levels(p$gender)),
                     "Invested before: Yes"),
  value = c(nrow(p),
            sprintf("%.1f (%.1f)", mean(p$age), sd(p$age)),
            sprintf("%d-%d", min(p$age), max(p$age)),
            sprintf("%d (%.1f%%)", as.vector(table(p$gender)), 100 * as.vector(prop.table(table(p$gender)))),
            sprintf("%d (%.1f%%)", sum(p$invested == "Yes"), 100 * mean(p$invested == "Yes")))
)
save_table(sample_table, "table_01_sample", "Sample characteristics")

outcome_summary <- do.call(rbind, lapply(c("share", "truth"), function(o) {
  rbind(data.frame(outcome = o, vignette = "All", mean = mean(long[[o]]), sd = sd(long[[o]])),
        do.call(rbind, lapply(levels(long$vignette), function(v) {
          x <- long[[o]][long$vignette == v]
          data.frame(outcome = o, vignette = v, mean = mean(x), sd = sd(x))
        })))
}))
save_table(outcome_summary, "table_02_outcome_means", "Outcome means and SDs, overall and by vignette")

outcome_dist <- do.call(rbind, lapply(c("share", "truth"), function(o) {
  rows <- c(list(All = long[[o]]), split(long[[o]], long$vignette))
  do.call(rbind, lapply(names(rows), function(v) {
    counts <- as.vector(table(factor(rows[[v]], levels = 1:7)))
    setNames(data.frame(o, v, t(counts)), c("outcome", "vignette", as.character(1:7)))
  }))
}))
save_table(outcome_dist, "table_03_outcome_distributions",
           "Outcome distributions (counts of each rating), overall and by vignette")

# Reliabilities, read from the existing diagnostics report (section 1).
diag_lines <- readLines(diagnostics_path)
sec <- diag_lines[(grep("^## 1\\.", diag_lines) + 1):(grep("^## 2\\.", diag_lines) - 1)]
rel_rows <- grep("^\\| ", sec, value = TRUE)[-1]
rel_rows <- rel_rows[!grepl("^\\|-", rel_rows)]
reliability <- do.call(rbind, lapply(strsplit(rel_rows, "\\|"), function(cells) {
  cells <- trimws(cells[-1])
  data.frame(scale = cells[1], items = as.integer(cells[2]),
             alpha = as.numeric(cells[3]), omega = as.numeric(cells[4]), note = cells[5])
}))
save_table(reliability, "table_04_reliability", "Scale reliabilities (conscientiousness: EXPLORATORY)",
           notes = "Read from outputs/measurement-check.md.")

cor_vars <- c("conscientiousness", "conscientiousness_acq", "fomo", "age", "mean_share", "mean_truth")
cor_mat <- cor(p[cor_vars])
cor_table <- data.frame(variable = cor_vars, round(cor_mat, 3), row.names = NULL, check.names = FALSE)
save_table(cor_table, "table_05_correlations", "Person-level correlations (Pearson r); correlations involving conscientiousness are EXPLORATORY")

# Outcome distribution figures.
for (o in c("share", "truth")) {
  png(file.path(out_dir, sprintf("fig_distribution_%s.png", o)), width = 1600, height = 1000, res = 200)
  barplot(table(factor(long[[o]], levels = 1:7)), xlab = paste(o, "rating"), ylab = "Count",
          main = sprintf("Distribution of %s ratings (all vignettes)", o),
          sub = if (!UNBLIND) "SIMULATED DATA" else "", col = "grey70", border = NA)
  invisible(dev.off())
}


# ---- Primary model ----------------------------------------------------------------

f_primary <- share ~ consc_z * fomo_z + vignette + (1 | participant_id)
m_primary <- fit_logged("primary", lmerTest::lmer(f_primary, data = long, REML = TRUE))

primary_tab <- tidy_fit(m_primary)
primary_tab$estimate_per_outcome_sd <- ifelse(primary_tab$term == "(Intercept)", NA,
                                              primary_tab$estimate / sd(long$share))
r2_primary <- fit_logged("primary R2", performance::r2_nakagawa(m_primary))
save_table(primary_tab, "table_06_primary_model",
           paste0("Primary model: share ~ conscientiousness x FOMO + vignette + (1 | participant). ",
                  if (h1_primary == "lmer") "FOMO coefficient = PRIMARY H1 TEST" else "FOMO coefficient = H1 robustness estimate (outcome rule selected the ordinal model)",
                  "; conscientiousness terms EXPLORATORY"),
           notes = c("REML; Satterthwaite degrees of freedom; 95% CIs. Predictors are z-scored at the person level; vignette is sum-coded.",
                     sprintf("Marginal R2 = %.3f; conditional R2 = %.3f (Nakagawa and Schielzeth).",
                             r2_primary$R2_marginal, r2_primary$R2_conditional)))


# ---- Interaction probing ----------------------------------------------------------

int_term <- "consc_z:fomo_z"
b <- lme4::fixef(m_primary)
V <- as.matrix(vcov(m_primary))

# Simple slopes of FOMO at conscientiousness = -1 SD, mean, +1 SD.
levels_c <- c(-1, 0, 1)
L <- t(sapply(levels_c, function(m) {
  l <- setNames(numeric(length(b)), names(b)); l["fomo_z"] <- 1; l[int_term] <- m; l
}))
ss <- lmerTest::contest(m_primary, L, joint = FALSE)
simple_slopes <- data.frame(conscientiousness = c("-1 SD", "Mean", "+1 SD"),
                            fomo_slope = ss$Estimate, se = ss$`Std. Error`, df = ss$df,
                            statistic = ss$`t value`, p = ss$`Pr(>|t|)`,
                            ci_low = ss$lower, ci_high = ss$upper)
save_table(simple_slopes, "table_07_simple_slopes",
           "EXPLORATORY: Simple slopes of FOMO on share-likelihood at three levels of conscientiousness",
           notes = "Probed regardless of significance; interpreted only if the interaction is significant.")

# Johnson-Neyman interval: values of conscientiousness (z) at which the FOMO
# slope is significant at alpha = .05.
b1 <- b[["fomo_z"]]; b3 <- b[[int_term]]
v11 <- V["fomo_z", "fomo_z"]; v33 <- V[int_term, int_term]; v13 <- V["fomo_z", int_term]
df_jn <- coef(summary(m_primary))["fomo_z", "df"]
tc <- qt(0.975, df_jn)
qa <- b3^2 - tc^2 * v33
qb <- 2 * (b1 * b3 - tc^2 * v13)
qc <- b1^2 - tc^2 * v11
disc <- qb^2 - 4 * qa * qc
roots <- if (disc >= 0) sort((-qb + c(-1, 1) * sqrt(disc)) / (2 * qa)) else numeric(0)
obs_range <- range(p$consc_z)
grid_c <- seq(obs_range[1], obs_range[2], length.out = 2001)
sig_grid <- (qa * grid_c^2 + qb * grid_c + qc) > 0
# Describe the significant region(s) within the observed range only.
runs <- rle(sig_grid)
ends <- cumsum(runs$lengths); starts <- ends - runs$lengths + 1
segments <- sprintf("%.2f to %.2f", grid_c[starts[runs$values]], grid_c[ends[runs$values]])
jn_description <- if (all(sig_grid)) {
  "FOMO slope is significant across the full observed range of conscientiousness."
} else if (!any(sig_grid)) {
  "FOMO slope is not significant anywhere in the observed range of conscientiousness."
} else {
  sprintf("Within the observed range of conscientiousness (z = %.2f to %.2f), the FOMO slope is significant for z from %s. Boundaries outside the observed range are not interpretable.",
          obs_range[1], obs_range[2], paste(segments, collapse = " and "))
}
jn_table <- data.frame(
  item = c("Boundary 1 (z)", "Boundary 2 (z)", "Observed min (z)", "Observed max (z)",
           "Share of sample in significant region", "df used for critical t"),
  value = c(if (length(roots)) roots else c(NA, NA), obs_range,
            mean((qa * p$consc_z^2 + qb * p$consc_z + qc) > 0), df_jn)
)
save_table(jn_table, "table_08_johnson_neyman", "EXPLORATORY: Johnson-Neyman interval for the FOMO slope",
           notes = jn_description)

# Predicted share across FOMO at the three conscientiousness levels (vignette
# averaged via sum coding). No median splits.
predict_line <- function(fit, consc, fomo_grid) {
  bb <- lme4::fixef(fit); VV <- as.matrix(vcov(fit))
  X <- matrix(0, length(fomo_grid), length(bb), dimnames = list(NULL, names(bb)))
  X[, "(Intercept)"] <- 1; X[, "consc_z"] <- consc; X[, "fomo_z"] <- fomo_grid
  X[, int_term] <- consc * fomo_grid
  fit_v <- drop(X %*% bb); se <- sqrt(rowSums((X %*% VV) * X))
  data.frame(consc_level = consc, fomo_z = fomo_grid, predicted = fit_v,
             ci_low = fit_v - 1.96 * se, ci_high = fit_v + 1.96 * se)
}
fomo_grid <- seq(min(p$fomo_z), max(p$fomo_z), length.out = 100)
pred <- do.call(rbind, lapply(levels_c, function(m) predict_line(m_primary, m, fomo_grid)))
write.csv(pred, file.path(out_dir, "fig_interaction_share_data.csv"), row.names = FALSE)

png(file.path(out_dir, "fig_interaction_share.png"), width = 1800, height = 1300, res = 200)
cols <- c("#B2182B", "grey35", "#2166AC")
plot(NA, xlim = range(fomo_grid), ylim = range(pred$ci_low, pred$ci_high),
     xlab = "FOMO (z)", ylab = "Predicted share-likelihood (1-7)",
     main = "EXPLORATORY: Predicted share-likelihood by FOMO and conscientiousness",
     sub = if (!UNBLIND) "SIMULATED DATA" else "")
for (i in seq_along(levels_c)) {
  d_i <- pred[pred$consc_level == levels_c[i], ]
  polygon(c(d_i$fomo_z, rev(d_i$fomo_z)), c(d_i$ci_low, rev(d_i$ci_high)),
          col = adjustcolor(cols[i], 0.15), border = NA)
  lines(d_i$fomo_z, d_i$predicted, col = cols[i], lwd = 2.5)
}
legend("topleft", bty = "n", lwd = 2.5, col = cols,
       legend = c("Conscientiousness -1 SD", "Mean", "+1 SD"))
invisible(dev.off())


# ---- Secondary model --------------------------------------------------------------

m_secondary <- fit_logged("secondary", lmerTest::lmer(
  truth ~ consc_z * fomo_z + vignette + (1 | participant_id), data = long, REML = TRUE))
r2_secondary <- fit_logged("secondary R2", performance::r2_nakagawa(m_secondary))
save_table(tidy_fit(m_secondary), "table_09_secondary_model",
           "Secondary model (RQ2): truth ~ conscientiousness x FOMO + vignette + (1 | participant). FOMO coefficient = secondary test; conscientiousness terms EXPLORATORY",
           notes = sprintf("Marginal R2 = %.3f; conditional R2 = %.3f.",
                           r2_secondary$R2_marginal, r2_secondary$R2_conditional))


# ---- Robustness checks ------------------------------------------------------------

long$share_ord <- factor(long$share, ordered = TRUE)
n_flagged <- sum(p$flag_long_run)
long_strict <- long[!long$participant_id %in% p$participant_id[p$flag_long_run], ]
p_fomo <- p  # person-level data for check 4

robust <- list(
  r1 = list(label = "1. Ordinal (clmm, logit scale)",
            fit = fit_logged("robustness 1", ordinal::clmm(
              share_ord ~ consc_z * fomo_z + vignette + (1 | participant_id), data = long))),
  r2 = list(label = "2. + age, gender, invested",
            fit = fit_logged("robustness 2", lmerTest::lmer(
              share ~ consc_z * fomo_z + vignette + age_z + gender + invested + (1 | participant_id),
              data = long))),
  r3 = list(label = "3. + truth-belief",
            fit = fit_logged("robustness 3", lmerTest::lmer(
              share ~ consc_z * fomo_z + vignette + truth + (1 | participant_id), data = long))),
  r4 = list(label = "4. Person-level OLS (mean share)",
            fit = fit_logged("robustness 4", lm(mean_share ~ consc_z * fomo_z, data = p_fomo))),
  r5 = list(label = "5. Vignette as random effect",
            fit = fit_logged("robustness 5", lmerTest::lmer(
              share ~ consc_z * fomo_z + (1 | participant_id) + (1 | vignette), data = long))),
  r6 = list(label = sprintf("6. Stricter screening (%d flagged%s)", n_flagged,
                            if (n_flagged == 0) "; identical to primary" else ""),
            fit = fit_logged("robustness 6", lmerTest::lmer(f_primary, data = long_strict))),
  r7 = list(label = "7. Acquiescence-corrected conscientiousness",
            fit = fit_logged("robustness 7", lmerTest::lmer(
              share ~ consc_acq_z * fomo_z + vignette + (1 | participant_id), data = long))),
  r8 = list(label = "8. FOMO-only model",
            fit = fit_logged("robustness 8", lmerTest::lmer(
              share ~ fomo_z + vignette + (1 | participant_id), data = long)))
)

for (k in names(robust)) {
  save_table(tidy_fit(robust[[k]]$fit), sprintf("table_10_robustness_%s", sub("r", "", k)),
             paste0("Robustness check ", robust[[k]]$label,
                    if (k == "r7") " (EXPLORATORY)" else if (k != "r8") " (conscientiousness terms EXPLORATORY)" else "",
                    if (k == "r1" && h1_primary == "clmm") ". FOMO coefficient = PRIMARY H1 TEST (outcome rule)" else ""))
}

# Key coefficients across the primary model and all robustness checks. The
# status column marks the primary H1 test (chosen by the outcome-suitability
# rule); all conscientiousness terms are exploratory.
key_rows <- function(label, fit, is_h1_primary) {
  t <- tidy_fit(fit)
  c_term <- if ("consc_acq_z" %in% t$term) "consc_acq_z" else "consc_z"
  terms <- c(Conscientiousness = c_term, FOMO = "fomo_z", Interaction = paste0(c_term, ":fomo_z"))
  terms <- terms[terms %in% t$term]
  do.call(rbind, lapply(names(terms), function(nm) {
    r <- t[t$term == terms[[nm]], ]
    status <- if (nm != "FOMO") expl else if (is_h1_primary) "PRIMARY H1 TEST" else "H1 robustness"
    data.frame(model = label, coefficient = nm, status = status, estimate = r$estimate,
               ci_low = r$ci_low, ci_high = r$ci_high, p = r$p)
  }))
}
key_summary <- rbind(
  key_rows("Primary", m_primary, h1_primary == "lmer"),
  do.call(rbind, lapply(names(robust), function(k)
    key_rows(robust[[k]]$label, robust[[k]]$fit, k == "r1" && h1_primary == "clmm")))
)
save_table(key_summary, "table_11_key_coefficients",
           "Key coefficients: primary model and robustness checks 1-8",
           notes = c(sprintf("PRIMARY H1 TEST: the FOMO coefficient in the %s, selected by the outcome-suitability rule (%.1f%% of share ratings equal 1; threshold 50%%).",
                             tolower(h1_primary_label), 100 * prop_ones),
                     "All conscientiousness and interaction terms are EXPLORATORY (Deviations, 15 October 2026).",
                     "Check 1 is on the logit scale and is not directly comparable in size to the linear models.",
                     "Check 4 is at the person level (one row per participant)."))


# ---- Sensitivity analysis ---------------------------------------------------------

m_lme4 <- fit_logged("sensitivity refit", lme4::lmer(f_primary, data = long, REML = TRUE))
sens <- do.call(rbind, lapply(sens_grid, function(eff) {
  mm <- m_lme4
  bb <- lme4::fixef(mm); bb[[int_term]] <- eff
  mm <- simr::`fixef<-`(mm, bb)
  ps <- fit_logged(sprintf("sensitivity (interaction = %.2f)", eff),
                   simr::powerSim(mm, test = simr::fixed(int_term, "t"),
                                  nsim = sens_nsim, progress = FALSE, seed = 20261013))
  ci <- binom.test(ps$x, ps$n)$conf.int
  data.frame(interaction = eff, power = ps$x / ps$n, ci_low = ci[1], ci_high = ci[2],
             nsim = ps$n, sim_errors = nrow(ps$errors))
}))
detectable <- sens$interaction[sens$power >= 0.80]
sens_note <- if (length(detectable)) {
  sprintf("Smallest interaction in the grid detectable with at least 80%% power: %.2f.", min(detectable))
} else {
  sprintf("No interaction in the grid reached 80%% power (maximum %.2f at %.2f).",
          max(sens$power), sens$interaction[which.max(sens$power)])
}
save_table(sens, "table_12_sensitivity",
           "EXPLORATORY: Sensitivity analysis: power to detect the conscientiousness x FOMO interaction",
           notes = c(sprintf("Simulation-based (simr), N = %d, %d simulations per effect size, two-sided alpha = .05.",
                             nrow(p), sens_nsim),
                     "Interaction sizes are in rating-scale points per 1 SD of each predictor.",
                     if (!UNBLIND) "TEST RUN ONLY: too few simulations to interpret." else NULL,
                     sens_note))

png(file.path(out_dir, "fig_sensitivity_power.png"), width = 1600, height = 1100, res = 200)
plot(sens$interaction, sens$power, type = "b", pch = 19, ylim = c(0, 1),
     xlab = "Interaction effect (rating points per SD x SD)", ylab = "Power",
     main = "EXPLORATORY: Sensitivity analysis (interaction)", sub = if (!UNBLIND) "SIMULATED DATA - TEST RUN" else "")
arrows(sens$interaction, sens$ci_low, sens$interaction, sens$ci_high,
       angle = 90, code = 3, length = 0.04)
abline(h = 0.8, lty = 2)
invisible(dev.off())


# ---- Exploratory analyses ---------------------------------------------------------

m_expl_vig <- fit_logged("exploratory FOMO x vignette", lmerTest::lmer(
  share ~ consc_z * fomo_z + fomo_z * vignette + (1 | participant_id), data = long))
expl_vig_anova <- as.data.frame(anova(m_expl_vig))
expl_vig_anova <- data.frame(term = rownames(expl_vig_anova), expl_vig_anova,
                             row.names = NULL, check.names = FALSE)
names(expl_vig_anova)[names(expl_vig_anova) == "Pr(>F)"] <- "p"
save_table(expl_vig_anova, "exploratory_table_01_fomo_by_vignette",
           "EXPLORATORY: Does the FOMO effect differ by vignette?",
           notes = "Type III F tests, Satterthwaite df. The fomo_z:vignette row tests the FOMO x vignette interaction.")

m_expl_b5 <- fit_logged("exploratory Big Five", lmerTest::lmer(
  share ~ extraversion_z + agreeableness_z + consc_z + negative_emotionality_z +
    open_mindedness_z + fomo_z + vignette + (1 | participant_id), data = long))
save_table(tidy_fit(m_expl_b5), "exploratory_table_02_big_five",
           "EXPLORATORY: All Big Five domains and FOMO as predictors of share-likelihood")


# ---- Run log and simulation check ------------------------------------------------

singular <- sapply(c(list(primary = m_primary, secondary = m_secondary),
                     lapply(robust[c("r2", "r3", "r5", "r6", "r7", "r8")], `[[`, "fit")),
                   lme4::isSingular)

run_log <- c(
  "# Run log", "",
  sprintf("Mode: %s", mode_label),
  sprintf("Run: %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  sprintf("R %s; lme4 %s; lmerTest %s; ordinal %s; simr %s; performance %s",
          getRversion(), packageVersion("lme4"), packageVersion("lmerTest"),
          packageVersion("ordinal"), packageVersion("simr"), packageVersion("performance")),
  sprintf("N participants = %d; N ratings = %d", nrow(p), nrow(long)),
  sprintf("Outcome suitability: %.1f%% of share ratings equal 1; primary model for H1: %s",
          100 * prop_ones, h1_primary_label), "",
  "## Warnings and convergence messages", "",
  if (length(warning_log)) paste("-", unique(warning_log)) else "None.", "",
  "## Singular fits", "",
  if (any(singular)) paste("-", names(singular)[singular]) else "None."
)

if (!UNBLIND) {
  recover <- do.call(rbind, lapply(c("share", "truth"), function(o) {
    fit <- if (o == "share") m_primary else m_secondary
    t <- tidy_fit(fit)
    terms <- c("(Intercept)", "consc_z", "fomo_z", int_term)
    truth_vals <- sim_truth[[o]][c("intercept", "consc_z", "fomo_z", int_term)]
    est <- t[match(terms, t$term), ]
    data.frame(outcome = o, term = terms, true = unname(truth_vals),
               estimate = est$estimate, ci_low = est$ci_low, ci_high = est$ci_high,
               true_in_ci = truth_vals >= est$ci_low & truth_vals <= est$ci_high,
               row.names = NULL)
  }))
  save_table(recover, "simulation_check_parameter_recovery",
             "Simulation check: true vs. estimated effects",
             notes = "Outcomes were simulated, then rounded and clipped to 1-7, which slightly attenuates estimates.")
  run_log <- c(run_log, "", "## Parameter recovery (simulated data)", "",
               md_table(format_for_md(recover)))
}

writeLines(run_log, file.path(out_dir, "run_log.md"))
cat(run_log, sep = "\n")
cat("\nOutputs written to", normalizePath(out_dir), "\n")
