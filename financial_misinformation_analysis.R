# ============================================================
# Financial Misinformation Study: FOMO x Conscientiousness
# ============================================================
# Tests whether FOMO and Conscientiousness interact to predict
# financial misinformation sharing, the same kind of interaction
# Kakkar found using political ideology instead of FOMO.
#
# BEFORE RUNNING: update the COLUMN MAPPING section below so the
# names match your actual Google Sheet headers. Everything else
# should run as-is once that's done.

# ---- 1. Setup ----
# install.packages(c("tidyverse", "lme4", "lmerTest"))  # run once if you don't have these
library(tidyverse)
library(lme4)
library(lmerTest)   # adds p-values to lmer() output

# ---- 2. Load your data ----
# Export your Google Sheet as CSV (File > Download > .csv) and point to it here
df <- read_csv("your_data.csv")

# ---- 3. COLUMN MAPPING (edit this part) ----
# Your BFI-2-XS conscientiousness items
consc_items <- c("consc_1", "consc_2", "consc_3")

# Your FOMO scale items
fomo_items <- c("fomo_1", "fomo_2", "fomo_3", "fomo_4", "fomo_5",
                 "fomo_6", "fomo_7", "fomo_8", "fomo_9", "fomo_10")

# Any items from either scale that are reverse-keyed (check your scale's scoring guide)
reverse_items <- c("consc_2")

# One share-likelihood and one truth-belief column per vignette
vignette_share <- c("v1_share", "v2_share", "v3_share", "v4_share", "v5_share")
vignette_truth <- c("v1_truth", "v2_truth", "v3_truth", "v4_truth", "v5_truth")

# ---- 4. Reverse-score and build composite scores ----
# Assumes a 1-5 rating scale. If yours is 1-7, change the 6 to 8, and so on.
df <- df %>%
  mutate(across(all_of(reverse_items), ~ 6 - .x))

df <- df %>%
  mutate(
    respondent_id = row_number(),
    conscientiousness = rowMeans(select(., all_of(consc_items)), na.rm = TRUE),
    fomo = rowMeans(select(., all_of(fomo_items)), na.rm = TRUE)
  )

# ---- Quick check before running anything ----
cat("Respondents:", nrow(df), "\n")
cat("Missing conscientiousness scores:", sum(is.na(df$conscientiousness)), "\n")
cat("Missing FOMO scores:", sum(is.na(df$fomo)), "\n")

# ============================================================
# APPROACH A: Person-level aggregation
# One row per respondent, averaged across the 5 vignettes.
# N = number of respondents, which matches the 200-300 target directly.
# ============================================================

df_person <- df %>%
  mutate(
    avg_share = rowMeans(select(., all_of(vignette_share)), na.rm = TRUE),
    avg_truth = rowMeans(select(., all_of(vignette_truth)), na.rm = TRUE)
  )

model_person <- lm(avg_share ~ fomo * conscientiousness, data = df_person)
summary(model_person)

# Same test with truth-belief as the outcome instead of sharing
model_person_truth <- lm(avg_truth ~ fomo * conscientiousness, data = df_person)
summary(model_person_truth)

# ============================================================
# APPROACH B: Mixed-effects model
# One row per respondent per vignette (5 rows per person). Keeps
# vignette-level detail and handles the fact that one person's 5
# ratings aren't independent, using a random intercept per respondent.
# ============================================================

df_long <- df %>%
  pivot_longer(
    cols = all_of(vignette_share),
    names_to = "vignette",
    values_to = "share_rating"
  ) %>%
  select(respondent_id, fomo, conscientiousness, vignette, share_rating)

model_mixed <- lmer(
  share_rating ~ fomo * conscientiousness + vignette + (1 | respondent_id),
  data = df_long
)
summary(model_mixed)

# Want the same mixed model for truth-belief? Repeat the pivot_longer
# and lmer() calls above with vignette_truth / truth_rating instead.

# ============================================================
# BONUS: Visualize the interaction
# Median split here is only for the plot, so it's easy to look at.
# The actual test above uses conscientiousness as continuous, not split.
# ============================================================

df_person <- df_person %>%
  mutate(consc_group = ifelse(conscientiousness >= median(conscientiousness, na.rm = TRUE),
                               "High Conscientiousness", "Low Conscientiousness"))

ggplot(df_person, aes(x = fomo, y = avg_share, color = consc_group)) +
  geom_smooth(method = "lm", se = TRUE) +
  geom_point(alpha = 0.3) +
  labs(
    title = "FOMO x Conscientiousness on Sharing Likelihood",
    x = "FOMO Score",
    y = "Average Share-Likelihood",
    color = NULL
  ) +
  theme_minimal()
