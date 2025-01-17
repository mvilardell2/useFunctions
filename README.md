
# useFunctions

<!-- badges: start -->
<!-- badges: end -->

This package contain some useful functions for data analysis

## Installation

You can install the package useFunctions with:

``` r
# install.packages("devtools")
devtools::install_github("mvilardell2/useFunctions")
```

## Functions: 

- enhancedvolcanoplot: Generates a customizable volcano plot to visualize statistical significance (p-values) and fold changes of gene expression data. 

- create_heatmap: Creates a heatmap to visualize gene expression data.

- mean_exprs: Computes the mean expression values for a specified set of genes.

- compare_groups: Compares gene expression values between two groups using a statistical test (t-test or Wilcoxon).


### Example

``` r
library(useFunctions)
data(top.table)
enhancedvolcanoplot(top.table)
```

