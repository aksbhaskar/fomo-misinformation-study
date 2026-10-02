# Financial Misinformation and the FOMO × Conscientiousness Interaction

A study of *who* spreads financial misinformation, and why. It tests whether **fear of missing out (FOMO)** and **conscientiousness** interact to predict the sharing of false financial claims — extending an established personality-and-misinformation framework from the political domain into the financial one.

---

## Research question

Does the effect of FOMO on sharing financial misinformation depend on a person's conscientiousness?

This extends Kakkar's work on political ideology, conscientiousness, and fake-news sharing by swapping the **political trigger (ideology)** for a **financial trigger (FOMO)** — asking whether the same personality dynamics that drive political misinformation also operate in markets, investing, and personal finance.

## Working hypothesis

FOMO increases the likelihood of sharing false financial claims, and **conscientiousness moderates this effect**: the FOMO → sharing link is expected to be weaker among highly conscientious individuals, who are more likely to pause, verify, and withhold unverified content.

## Design and measures

| Component | Instrument |
|---|---|
| Conscientiousness | BFI-2-XS (Big Five Inventory, extra-short form) |
| FOMO | Przybylski et al. Fear of Missing Out scale |
| Outcome | Five financial-misinformation vignettes, each rated for **share-likelihood** and **truth-belief** |

Each respondent rates all five vignettes, so the five ratings are nested within respondent — handled with a mixed-effects model (random intercept per respondent).

## Analysis approach

Two complementary models, implemented in [`financial_misinformation_analysis.R`](financial_misinformation_analysis.R):

1. **Person-level regression** — one row per respondent, ratings averaged across vignettes. Directly interpretable at the person level.
2. **Mixed-effects model** — one row per respondent × vignette, preserving item-level variation and the non-independence of within-person ratings via a random intercept.

Both test the `fomo × conscientiousness` interaction, for share-likelihood and for truth-belief. The interaction is also visualized to make the moderation pattern easy to read.

## NLP component

Beyond the closed-form scales, the study includes open-ended responses analyzed with natural-language-processing methods, to surface the themes and reasoning people give when they decide to share — or not share — a financial claim. This phase is in progress.

## Project status

- ✅ Instrument design and vignette development
- ✅ Data collection complete — **300 respondents**
- 🔄 Quantitative analysis (interaction models)
- 🔄 NLP analysis of open-ended responses

## Repository contents

| File | Purpose |
|---|---|
| [`README.md`](README.md) | Project overview (this file) |
| [`financial_misinformation_analysis.R`](financial_misinformation_analysis.R) | Full analysis pipeline: scoring, interaction models, visualization |
| `.gitignore` | Standard R ignores; keeps data and local artifacts out of version control |

## Reproducing the analysis

```r
install.packages(c("tidyverse", "lme4", "lmerTest"))
```

Place the response export alongside the script, update the column-mapping section at the top of the script to match the survey's headers, and run it top to bottom. The script reports respondent counts and missingness before fitting any models.

## Data availability and ethics

Raw responses are **not** included in this repository. The data involves human subjects and requires appropriate handling and advisor sign-off before any release. Aggregate results and analysis code are shared here; participant-level data is held separately.

## Advisor

Prof. Hemant Kakkar

## Author

Akshat Bhaskar
