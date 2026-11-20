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

Numerical fits, grouped bootstrap intervals and sensitivity simulations are cached
in the gitignored `outputs/cache/`; reporting is regenerated each run. Set
`FOMO_CACHE=false` to bypass caching. Keys include input file hashes, analysis
code, blinding/scenario, model and simulation settings, RNG state and installed
package versions; simulated and real-data results never share a key. Cached
warnings/errors are retained and replayed. Treat the cache as private study data.
Set `FOMO_POWER_WORKERS=1` for serial sensitivity simulations, or a positive
integer to bound the PSOCK process pool (default: the smaller of the effect
count and available cores minus one). Each effect retains the same simulation
seed as the serial analysis; worker BLAS/OpenMP thread counts are limited to one.

## Data availability

Raw data are not publicly available, to protect participant privacy.

## License

Code is released under the [MIT License](LICENSE). Original survey materials (the five vignettes, demographic items, and study instructions) are licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). The BFI-2-XS and FOMO scale items are reproduced in `materials/` for documentation and remain subject to their original authors' terms.

## Acknowledgments

Thanks to Professor Hemant Kakkar (Indian School of Business), advisor on this project, for guidance on the study design and analysis.
