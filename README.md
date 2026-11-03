# A Simulation Study on the Stability and Recovery Behaviour of a Relative Belief Ratio Filter for Binary Feature Selection

Maher Emarly, Ayman Alzaatreh, Luai Al-Labadi, Firuz Kamalov

Preprint · [doi:10.21203/rs.3.rs-10883318/v1](https://doi.org/10.21203/rs.3.rs-10883318/v1) · under review

The relative belief ratio filter had been shown to predict well on real gene-expression data, but
prediction is not recovery: nobody had checked whether it finds the features that actually matter, or
whether it finds the same ones twice. This repository answers both on synthetic data where the
informative, redundant and noise features are fixed by construction. Five data-generating scenarios
run across four design cells that vary sample size and dimensionality one factor at a time, 300
independent datasets each, scored for informative-feature recovery, noise false discovery rate, and
selection stability by the Jaccard and Kuncheva indices — against Information Gain, Symmetrical
Uncertainty, mRMR, ReliefF and the Boruta wrapper.

## Repository structure

```text
.
├── configs
│   └── simulation.yml              # Feature structure, the four design cells, the scenario list
├── docs
│   └── reproducing.md              # Commands for the full study and for one cell at a time
├── R
│   ├── generators
│   │   ├── helpers.R               # Labels, truth table, correlated normals, standardise and shuffle
│   │   └── scenarios.R             # The five data-generating scenarios
│   ├── filters
│   │   ├── prior.R                 # Prior elicitation and the shared prior Monte Carlo cache
│   │   ├── rbr.R                   # Relative belief ratio filter
│   │   └── baselines.R             # IG, SU, mRMR, ReliefF, Boruta, and one call to run all six
│   └── utils
│       ├── setup.R                 # Packages, module loading, seeding
│       ├── args.R                  # Command line arguments for scripts/
│       ├── config.R                # configs/simulation.yml to one cell's derived configuration
│       ├── metrics.R               # Recovery, mismatch and native-threshold selection metrics
│       ├── stability.R             # Jaccard, Kuncheva, Spearman over all dataset pairs
│       ├── simulation.R            # One dataset, one scenario, one cell
│       └── outputs.R               # The per-scenario workbook and its two plots
├── scripts
│   ├── run_simulation.R            # Run the study, resuming anything already finished
│   └── summarise_results.R         # Collect the saved reports into one cross-cell workbook
├── .gitignore
├── DESCRIPTION
├── LICENSE
└── README.md
```

Running the study adds `output/rbr_binary_cell<X>_n<N>_p<P>_sims<S>/<scenario>/`, holding a
thirteen-sheet workbook and two plots per scenario, plus `output/cross_cell_summary.xlsx`.

## Installation

Tested with R 4.6.1 on macOS and R 4.3 on Windows 10.

```r
install.packages(c(
  "yaml", "dplyr", "tidyr", "purrr", "ggplot2", "FSelector",
  "praznik", "CORElearn", "Boruta", "openxlsx"
))
```

One external requirement: **Java 8 or newer**, which `FSelector` needs through `RWeka` for
Information Gain and Symmetrical Uncertainty. On macOS, `brew install --cask temurin`; on Linux, your
distribution's `default-jdk`; on Windows, a JDK with `JAVA_HOME` pointing at it. Run
`R CMD javareconf` afterwards if `rJava` still fails to load.

## Data

Nothing to download: every dataset is generated. `configs/simulation.yml` fixes the structure —
25 informative features, 5 redundant-but-relevant ones correlated at 0.90 with an informative
feature, and the rest noise — and the five scenarios in `R/generators/scenarios.R` decide the
distributions:

| Scenario | What it tests | Source of the design |
|---|---|---|
| `mean_shift_independent` | Gaussian location baseline | Fan & Fan (2008) |
| `correlated_gaussian_blocks` | Location signal under block correlation | Fan & Lv (2008) |
| `heavy_tailed_meanshift` | Location signal with t₃ tails | Fan & Fan (2008), modified |
| `skewed_meanshift` | Right-skewed log-normal marginals | microarray simulation practice |
| `mixture_meanshift` | Bimodal within-class, harder location problem | constructed stress variant |

These are the five the paper reports, under the short names *mean shift*, *correlated*,
*heavy-tailed*, *skewed* and *mixture*.

Counts, not fractions, are held fixed: at *p* = 1000 the 30 relevant features are 3% of the total, a
deliberate high-dimensional screening stress test.

## Usage

Run from the repository root.

```bash
Rscript scripts/run_simulation.R
Rscript scripts/summarise_results.R
```

That runs all four cells and all five scenarios. The study is the expensive part: 4 cells × 5
scenarios × 300 datasets is 6,000 datasets, each scored by six methods, two of which (ReliefF and
Boruta) are themselves costly at *p* = 1000. Expect days on one machine, which is why the run resumes:
a scenario whose report is already on disk is skipped, so it can be stopped and restarted freely, and
cells can be split across machines.

```bash
Rscript scripts/run_simulation.R --cells D                        # one cell
Rscript scripts/run_simulation.R --cells A,B --scenarios mixture_meanshift
```

Seeds are derived from each cell's and each scenario's position in `configs/simulation.yml`, so
running a subset produces exactly the datasets a full run would have produced — but **reordering that
file changes every random number**. See [docs/reproducing.md](docs/reproducing.md) for the full
commands.

## Acknowledgements

This work was carried out at the Department of Mathematics and Statistics, American University of
Sharjah, with the Department of Mathematical and Computational Sciences, University of Toronto
Mississauga. It evaluates the filter introduced in Alzaatreh, Emarly, Mitra and Al-Labadi (Physica
Scripta 100(8), 086010, 2025), whose code is in [rbr-binary-fs](../rbr-binary-fs). The relative
belief ratio machinery follows Evans' work on Bayesian measures of evidence; the synthetic
feature-selection framework follows Bolón-Canedo et al. (2013).

## License

MIT; see [LICENSE](LICENSE).

## Citation

```bibtex
@misc{Emarly_RBR_Simulation,
  author       = {Emarly, Maher and Alzaatreh, Ayman and Al-Labadi, Luai
                  and Kamalov, Firuz},
  title        = {A simulation study on the stability and recovery behaviour of a
                  relative belief ratio filter for binary feature selection},
  howpublished = {Preprint, under review},
  doi          = {10.21203/rs.3.rs-10883318/v1}
}
```
