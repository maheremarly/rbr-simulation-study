# Reproducing the study

All commands are run from the repository root.

The study is one command, but a long one: 4 cells × 5 scenarios × 300 independent datasets is 6,000
datasets, each scored by six methods. It is built to be stopped and restarted — a scenario whose
report is already on disk is skipped — so the practical approach is to run it a cell at a time, or in
parallel across machines.

## 1. The whole study

```bash
Rscript scripts/run_simulation.R
Rscript scripts/summarise_results.R
```

`run_simulation.R` writes, per cell and scenario:

```text
output/rbr_binary_cellA_n100_p500_sims300/
└── mean_shift_independent/
    ├── mean_shift_independent_simulation_report.xlsx    # thirteen sheets
    ├── mean_shift_independent_plot_top25_recovery.png
    └── mean_shift_independent_plot_top25_stability.png
```

`summarise_results.R` then reads every report found and writes
`output/cross_cell_summary.xlsx`, with the recovery, classification, stability and RBR threshold
summaries stacked across all cells and scenarios, each row tagged with its cell, `n` and `p`.

## 2. One cell at a time

The four cells are independent and can run on different machines. Each has its own seed stream and
its own output directory, so nothing collides.

```bash
Rscript scripts/run_simulation.R --cells A      # n = 100, p =  500  baseline
Rscript scripts/run_simulation.R --cells B      # n =  50, p =  500  sample size down
Rscript scripts/run_simulation.R --cells C      # n = 200, p =  500  sample size up
Rscript scripts/run_simulation.R --cells D      # n = 100, p = 1000  dimensionality up
```

Cells A to C at fixed `p = 500` give a clean sample-size effect; A against D at fixed `n = 100` gives
a clean dimensionality effect.

## 3. One scenario, or a few

```bash
Rscript scripts/run_simulation.R --cells A --scenarios mixture_meanshift
Rscript scripts/run_simulation.R --scenarios mean_shift_independent,skewed_meanshift
```

A dataset's seed is `cell_seed + 100000 * scenario_position + dataset_number`, and
`scenario_position` is the scenario's position in the full list in `configs/simulation.yml`, never its
position in the subset being run. So running one scenario gives exactly the 300 datasets a full run
would have given it.

**Reordering `configs/simulation.yml` changes every random number.** Cell seeds come from the cell's
position under `cells:`, and dataset seeds from the scenario's position under `scenarios:`. Add new
scenarios at the end and new cells at the end; never insert or reorder.

## 4. Resuming and re-running

A scenario is skipped when its `*_simulation_report.xlsx` already exists. To force one to run again,
delete its folder:

```bash
rm -rf output/rbr_binary_cellA_n100_p500_sims300/mixture_meanshift
Rscript scripts/run_simulation.R --cells A --scenarios mixture_meanshift
```

A scenario that errors writes `<scenario>_ERROR.txt` into its folder and the run continues to the
next one. Because no report is written, the next run retries it. To stop at the first failure
instead:

```bash
Rscript scripts/run_simulation.R --stop_on_error
```

## 5. A quick check before committing to the full run

Copy the configuration, shrink it, and point the output somewhere separate. Nothing else changes.

```bash
cp configs/simulation.yml configs/smoke.yml
```

Then in `configs/smoke.yml` set `n_sims: 3`, `B_rbr: 300`, `p_inf: 6`, `p_red: 2`, `top_k_core: 6`,
`top_k_all: 8`, `save_top_n: 12`, and a cell such as `A: {n_obs: 40, p: 30, role: "smoke"}`:

```bash
Rscript scripts/run_simulation.R --config configs/smoke.yml --output_dir output-smoke
```

`validate_config()` will reject an inconsistent set — `top_k_core` must equal `p_inf`, `top_k_all`
must equal `p_inf + p_red`, the feature counts must sum to `p`, and the classes must split `n` into
two equal whole numbers. The numbers from a run this small are meaningless; it only checks that
everything runs.

Note the cell key is `n_obs`, not `n`. A bare `n:` is read by R's YAML parser as the YAML 1.1 boolean
`no`, which would leave the cell with no sample size; `make_cell_config()` detects that and says so.

## 6. Other options

```bash
Rscript scripts/run_simulation.R   --config configs/other.yml --output_dir /scratch/sim
Rscript scripts/summarise_results.R --cells A,D --output_dir /scratch/sim
```

`summarise_results.R` reads whatever reports it finds and names the cell/scenario combinations that
have none, so it is safe to run on a partial study.

## 7. Which command produces which result in the paper

| In the paper | Produced by |
|---|---|
| Table of the four simulation configurations | `configs/simulation.yml` (`cells:`) |
| Appendix table of software implementations and settings | `R/filters/baselines.R` and `configs/simulation.yml` |
| Recovery results, per scenario and configuration | `run_simulation.R` → `recovery_summary` in each scenario report |
| Selected-set composition bar figures | `run_simulation.R` → `<scenario>_plot_top25_recovery.png` |
| Kuncheva and Jaccard stability results | `run_simulation.R` → `stability_top25` and `<scenario>_plot_top25_stability.png` |
| Selection-frequency figures | the `selection_frequency` and `selection_freq_summary` sheets |
| Replicate distributions of recovery (appendix boxplots) | the `recovery_detail` sheet, one row per dataset |
| Sample-size and dimensionality comparisons across cells | `summarise_results.R` → `cross_cell_summary.xlsx` |
| Monte Carlo standard errors beside each mean | `sd_* / sqrt(300)` from the reports; not written out |
| Bootstrap intervals on the stability indices | not in this repository |
| The paper's numbered figures and `paper_tables.xlsx` | not in this repository |

The scenario reports and `cross_cell_summary.xlsx` hold every number the paper's figures and tables
are drawn from. What is missing is the layout and the interval estimation.
