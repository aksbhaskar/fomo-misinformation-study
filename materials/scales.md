# Scales and Scoring

Item numbers refer to the order in which items appeared in the survey (see [`survey-instrument.md`](survey-instrument.md)). Reverse-keyed items are marked **(R)**.

---

## BFI-2-XS (Big Five Inventory-2, Extra-Short Form)

**Citation:** Soto, C. J., & John, O. P. (2017). Short and extra-short forms of the Big Five Inventory–2: The BFI-2-S and BFI-2-XS. *Journal of Research in Personality, 68*, 69–81. https://doi.org/10.1016/j.jrp.2017.02.004

**Format:** 15 items, stem "I am someone who...", rated 1 (Disagree strongly) to 5 (Agree strongly). Three items per domain.

### Items

| # | Item | Domain | Keying |
|---|---|---|---|
| 1 | Tends to be quiet. | Extraversion | **R** |
| 2 | Is compassionate, has a soft heart. | Agreeableness | |
| 3 | Tends to be disorganized. | Conscientiousness | **R** |
| 4 | Worries a lot. | Negative Emotionality | |
| 5 | Is fascinated by art, music, or literature. | Open-Mindedness | |
| 6 | Is dominant, acts as a leader. | Extraversion | |
| 7 | Is sometimes rude to others. | Agreeableness | **R** |
| 8 | Has difficulty getting started on tasks. | Conscientiousness | **R** |
| 9 | Tends to feel depressed, blue. | Negative Emotionality | |
| 10 | Has little interest in abstract ideas. | Open-Mindedness | **R** |
| 11 | Is full of energy. | Extraversion | |
| 12 | Assumes the best about people. | Agreeableness | |
| 13 | Is reliable, can always be counted on. | Conscientiousness | |
| 14 | Is emotionally stable, not easily upset. | Negative Emotionality | **R** |
| 15 | Is original, comes up with new ideas. | Open-Mindedness | |

### Scoring

1. Reverse-score the six reverse-keyed items (1, 3, 7, 8, 10, 14): reversed score = 6 − raw score.
2. Each domain score is the mean of its three items after reverse-scoring. Possible range: 1–5.

| Domain | Items | Reverse-keyed |
|---|---|---|
| Extraversion | 1, 6, 11 | 1 |
| Agreeableness | 2, 7, 12 | 7 |
| **Conscientiousness** (focal predictor) | **3, 8, 13** | **3, 8** |
| Negative Emotionality | 4, 9, 14 | 14 |
| Open-Mindedness | 5, 10, 15 | 10 |

Higher scores indicate more of the trait. For Negative Emotionality, a higher score means more negative emotionality; item 14 ("emotionally stable") is reversed for that reason.

### Note on reliability in non-WEIRD samples

Big Five inventories have shown low reliability and validity in non-WEIRD (non-Western, educated, industrialized, rich, democratic) populations, with acquiescent responding identified as a contributing factor:

Laajaj, R., Macours, K., Pinzon Hernandez, D. A., Arias, O., Gosling, S. D., Potter, J., Rubio-Codina, M., & Vakis, R. (2019). Challenges to capture the big five personality traits in non-WEIRD populations. *Science Advances, 5*(7), eaaw5226. https://doi.org/10.1126/sciadv.aaw5226

Diagnostics for this study's sample are in [`outputs/measurement-check.md`](../outputs/measurement-check.md).

---

## Fear of Missing Out Scale (FoMOs)

**Citation:** Przybylski, A. K., Murayama, K., DeHaan, C. R., & Gladwell, V. (2013). Motivational, emotional, and behavioral correlates of fear of missing out. *Computers in Human Behavior, 29*(4), 1841–1848. https://doi.org/10.1016/j.chb.2013.02.014

**Format:** 10 items, rated 1 (Not at all true of me) to 5 (Extremely true of me). Single factor.

### Items

| # | Item | Keying |
|---|---|---|
| 1 | I fear others have more rewarding experiences than me. | |
| 2 | I fear my friends have more rewarding experiences than me. | |
| 3 | I get worried when I find out my friends are having fun without me. | |
| 4 | I get anxious when I don't know what my friends are up to. | |
| 5 | It is important that I understand my friends' "in jokes." | |
| 6 | Sometimes, I wonder if I spend too much time keeping up with what is going on. | |
| 7 | It bothers me when I miss an opportunity to meet up with friends. | |
| 8 | When I have a good time it is important for me to share the details online (e.g. updating status). | |
| 9 | When I miss out on a planned get-together it bothers me. | |
| 10 | When I go on vacation, I continue to keep tabs on what my friends are doing. | |

### Scoring

No items are reverse-keyed. The FOMO score is the mean of all 10 items. Possible range: 1–5. Higher scores indicate greater fear of missing out.

---

## Vignette outcome ratings

Not a published scale. Each of the five vignettes was followed by two single-item ratings:

| Measure | Item | Scale |
|---|---|---|
| Share-likelihood | How likely would you be to share this with friends or family? | 1 (Very unlikely) to 7 (Very likely) |
| Truth-belief | How true do you think this message is? | 1 (Definitely false) to 7 (Definitely true) |

Neither item is reverse-keyed. This gives 10 ratings per participant (5 vignettes × 2 measures). How the ratings are modeled is specified in [`docs/analysis-plan.md`](../docs/analysis-plan.md).
