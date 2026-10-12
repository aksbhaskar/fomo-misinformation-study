# Analysis Plan

Written 12 October 2026, after data collection closed (n = 300) and before any outcome data (share-likelihood or truth-belief ratings) were examined. Any change made after the data are opened will be recorded in the Deviations section at the end, with its date and reason.

## Background and provenance

This study extends Lawson and Kakkar's work on conscientiousness and fake-news sharing to financial misinformation, replacing political ideology with Fear of Missing Out (FOMO) as the domain-specific motivational trait. An earlier version of the analysis approach (measures and a mixed-effects model with ratings nested within participants) was drafted on 1 August 2026, shortly after recruitment began, and is preserved in docs/archive/; the use of a mixed-effects model was also recommended by the faculty advisor on 30 July 2026. That earlier version did not specify hypotheses or exclusion rules; both are specified here for the first time.

## Hypotheses and research questions

- H1: Higher FOMO will be associated with greater willingness to share financial misinformation.
- H2: Higher conscientiousness will be associated with lower willingness to share financial misinformation.
- RQ1 (primary, two-sided): Does conscientiousness moderate the relationship between FOMO and willingness to share?
- RQ2 (secondary): Do the same patterns hold for belief that the messages are true?

## Variables

Outcomes (each rated once per vignette, 5 vignettes per participant):
- Share-likelihood: 1 (Very unlikely) to 7 (Very likely). Primary outcome.
- Truth-belief: 1 (Definitely false) to 7 (Definitely true). Secondary outcome.

Predictors (person-level):
- Conscientiousness: mean of BFI-2-XS items 3 (reversed), 8 (reversed), and 13. Reversed items scored as 6 minus the response.
- FOMO: mean of the 10 Przybylski et al. (2013) items.
- Both predictors are standardized (z-scored) across the analytic sample before modelling, so coefficients are per 1 SD and the interaction is interpretable.

Covariates (robustness models only): age, gender, prior investing experience (yes/no).

Vignette: a 5-level factor identifying each message, sum-coded.

## Exclusion rules

Applied in this order, with the number removed at each step reported:

1. Age: exclude anyone under 18, and any age entry that is missing, non-numeric, or implausible (over 100). Participants under 18 did not provide parental consent and are excluded entirely.
2. Duplicates: if two or more submissions are identical on every item, keep only the first.
3. Straightlining on trait scales: exclude anyone who gave the same response to all 25 personality and FOMO items. Uniform responses on the vignette ratings are NOT grounds for exclusion, because consistently rating every scam as very unlikely to be shared is a plausible genuine response.

No other exclusions will be made in the primary analysis. The survey had no attention checks and did not record completion time; this is noted as a limitation.

## Descriptive analyses

- Sample characteristics (age, gender, state, investing experience).
- Means, SDs, and distributions of share-likelihood and truth-belief, overall and by vignette.
- Internal consistency (Cronbach's alpha and McDonald's omega) for each scale. The 3-item conscientiousness scale is expected to show modest reliability, consistent with its design for brevity.
- Correlations among conscientiousness, FOMO, mean share-likelihood, and mean truth-belief.

## Primary analysis

Linear mixed-effects model (R, lme4 with lmerTest for Satterthwaite degrees of freedom):

share ~ conscientiousness * FOMO + vignette + (1 | participant)

- H1 and H2 are tested by the FOMO and conscientiousness coefficients; RQ1 by the interaction term.
- Significance threshold: alpha = .05, two-sided. Coefficients reported with 95% confidence intervals.
- Effect sizes: standardized coefficients and marginal/conditional R-squared (Nakagawa and Schielzeth).

Probing the interaction (performed regardless of significance, and interpreted only if the interaction is significant):
- Simple slopes of FOMO at conscientiousness = -1 SD, mean, and +1 SD.
- Johnson-Neyman interval showing the range of conscientiousness over which the FOMO effect is significant.
- Plot of model-predicted share-likelihood across FOMO at the three conscientiousness levels. Median splits will not be used for inference.

## Secondary analysis

The primary model repeated with truth-belief as the outcome (RQ2).

## Robustness checks

Each is reported alongside the primary result. Conclusions are based on the primary model; robustness checks indicate whether the result depends on specific analytic choices.

1. Ordinal model: cumulative link mixed model (ordinal::clmm) with the same structure, since share-likelihood ratings are expected to cluster at the low end of the scale.
2. Covariates: primary model plus age, gender, and investing experience.
3. Truth-belief as covariate: primary model plus truth-belief. This is not the primary specification because belief in a message is plausibly part of the pathway from traits to sharing.
4. Person-level model: ordinary least squares regression of each participant's mean share-likelihood on conscientiousness * FOMO (the approach in the 1 August 2026 draft, docs/archive/).
5. Vignette as random effect: (1 | participant) + (1 | vignette) instead of a fixed vignette factor.
6. Stricter screening: excluding participants whose longest run of identical consecutive answers across the 25 trait items is 10 or more.

## Sensitivity analysis

A simulation-based sensitivity analysis will report the smallest interaction effect this design could detect with 80% power given the final sample size. This is used to interpret a null result, not to justify the sample size.

## Exploratory analyses

Clearly labelled as exploratory in any write-up:
- Whether the FOMO effect differs by vignette type (FOMO x vignette interaction).
- Whether other Big Five domains (especially negative emotionality and agreeableness) predict sharing.

## Interpretation rules

- A significant interaction will be described by its simple slopes, not by its coefficient alone.
- A non-significant interaction will be reported as no evidence of moderation at this sample size, with the sensitivity analysis indicating what size of effect can be ruled out. It will not be reported as evidence that no moderation exists.
- Results from robustness checks that disagree with the primary model will be reported, not omitted.

## Limitations known in advance

- Convenience sample recruited through social media; not representative of India.
- All five vignettes are false; the design cannot measure discernment between true and false messages.
- Vignettes were shown in a fixed order, so order effects cannot be separated from vignette effects.
- No attention checks or completion-time data.
- Self-reported sharing intention, not observed sharing behaviour.

## Deviations

None yet. Any departure from this plan after the data are opened will be listed here with its date and reason.
