Reproducing Data and Graphs Presented in `rubias` Publication
================
18 September, 2017

-   [Timing benchmarks](#timing-benchmarks)
-   [Validation scripts](#validation-scripts)
-   [Data summary and plotting scripts](#data-summary-and-plotting-scripts)

<!-- README.md is generated from README.Rmd. Please edit that file -->
Directory `R-main/` in this repository includes a few R scripts taht allow the reproduction of all data, statistics, and graphs presented in the publication \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_. They also include numerous graphs for further visualization of our model validation data, which were excluded from the publication for brevity. The main scripts here are described below.

Timing benchmarks
-----------------

This involves a single script:

1.  `rubias_time_benchmarks.R` analyzes a few data sets (some are distributed with the `rubias` package and others are in this repostory in `data`) and records the execution times and puts it all into a table.

Validation scripts
------------------

Two scripts run a long series of cross-validation experiments:

1.  `cross-validation.R` includes the first two methods of validation listed in the publication: reproduction of the Leave-One-Out Cross-Validation in Hasselman *et al.* (2015) with and without parametric bootstrap correction, and the application of Monte Carlo Cross-Validation to the same dataset.
2.  `coalescent_sim.R` documents the third validation method, the creation of simulated genotypes from the coalescent, followed by Leave-One-Out Cross-Validation.

Sourcing the source files `cross-validation.R` and `coalescent_sim.R` will create (among other files from intermediate steps) two data.frames for each validation method. One, `XXX_rho_data`, contains true and estimated reporting unit proportions (rho) created during the corresponding cross-validation. The second, `XXX_rho_dev` contains summary statistics describing the deviation of each estimation method from its corresponding true rho value. The validation is identified by the prefix, `XXX`, with `coal`, `Hass`, and `mc` representing coalescent, Leave-One-Out, and Monte Carlo cross-validations, respectively. For quick access, these output data have been committed in this repository as `data/cjfas_simulation_results.Rdata`.

Data summary and plotting scripts
---------------------------------

The remaining scripts summarize and plot results from the simulations. They use the data created by the "validation scripts"; however, they begin by loading `data/cjfas_simulation_results.Rdata`, and so can be run independently of the original simulation scripts.

1.  `cjfas_graphs.R` contains the scripts used to generate the [ggplot2](http://ggplot2.tidyverse.org/) figures found in the paper. It creates the directory `plots_for_paper` and creates `figure_2.pdf` and `figure_3.pdf` there.
2.  `proportion_sig_test.R` includes some of the exploratory analyses that were used to identify and document the relationship between the residual for any given simulation, $\\tilde{\\rho}\_r - \\rho^\\mathrm{sim}\_r$, and the statistic $\\frac{K\_r}{K} - \\rho^\\mathrm{sim}\_r$, where *K* is the total number of populations and *K*<sub>*r*</sub> is the number of populations included in reporting unit *r*. Sourcing the file `proportion_sig_test.R` will create a multitude of rough graphs which visualize this data. `.lg` graphs plot the true vs. estimated rho value *a la* Figure 3, while `.mse`, `.m.bias`, and `.prop.bias` show mean squared error, mean bias, and mean proportional bias for each reporting unit, with and without parametric bootstrapping. Prefixes `c`, `h`, and `mc` represent coalescent, Leave-One-Out, and Monte Carlo cross-validations.
