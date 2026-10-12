# Conscientiousness, FOMO, and the Sharing of Financial Misinformation

This study asks whether two personality traits, conscientiousness and fear of missing out (FOMO), work together to shape how willing people are to pass along false financial information. Adults in India (n = 300) completed a short personality measure, a FOMO scale, and then read five messages modeled on common financial scams, such as fake insider stock tips and "guaranteed return" schemes. For each message, they rated how likely they would be to share it with friends or family and how true they believed it to be. The study extends Lawson and Kakkar's work on conscientiousness and fake-news sharing from political news to personal finance.

## Status

- Data collection: **complete** (July–September 2026, n = 300)
- Analysis: **in progress**

## Folder guide

| Folder / file | Contents |
|---|---|
| `materials/` | The survey instrument as participants saw it, and scoring details for each scale |
| `docs/` | Study documentation, including the analysis plan |
| `analysis/` | R scripts, numbered in the order they are run |
| `data/raw/` | Raw survey export (not public; contents are gitignored) |
| `data/processed/` | Cleaned analysis datasets (not public; contents are gitignored) |
| `outputs/` | Tables and figures produced by the analysis scripts |

To recreate the R environment, open R in the project folder and run `renv::restore()`.

## Data availability

Raw data are not publicly available, to protect participant privacy.

## License

Code is released under the [MIT License](LICENSE). Original survey materials (the five vignettes, demographic items, and study instructions) are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). The BFI-2-XS and FOMO scale items are reproduced in `materials/` for documentation and remain subject to their original authors' terms.
