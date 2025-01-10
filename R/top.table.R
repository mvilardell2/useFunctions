#' Example Data to test enhancedvolcanoplot
#'
#' A dataset containing simulated differential expression results, including
#' log fold changes (logFC) and p-values (P.Value) for a set of genes. This
#' dataset is used to demonstrate the functionality of the `enhancedvolcanoplot` function.
#'
#' @format A data frame with rows corresponding to genes and columns:
#' \describe{
#'   \item{logFC}{Log fold change values (numeric)}
#'   \item{P.Value}{P-values for each gene (numeric)}
#' }
#' @examples
#' # Load the example dataset
#' data(top.table)
#'
#' # Use the enhancedvolcanoplot function with the example data
#' enhancedvolcanoplot(top.table, title = "Example Volcano Plot")
"top.table"
