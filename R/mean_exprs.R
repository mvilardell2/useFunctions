#' Compute Mean expression for same genes
#'
#' This function computes the mean expression levels for genes with the same name,
#' typically in gene expression datasets where multiple probes may correspond to the same gene.
#'
#' @param data A dataframe containing gene expression levels, with a 'SYMBOL' column
#' that specifies the names of the genes and additional columns containing expression values for each sample.
#' @param symbols A character vector specifying the unique gene names for which the mean expression
#' levels should be computed.
#'
#' @return A dataframe where rows represent the specified genes (`symbols`) and columns contain
#' the computed mean expression values across samples.
#'
#'
#' @import dplyr
#'
#' @export
#' @examples
#' # Example dataset with duplicate gene symbols
#' data_mean_exprs <- data.frame(
#'   SYMBOL = c("GeneA", "GeneA", "GeneB", "GeneC", "GeneC"),
#'   Sample1 = c(2.3, 2.5, 1.1, 0.8, 0.9),
#'   Sample2 = c(2.1, 2.4, 1.0, 0.7, 1.0)
#' )
#'
#' # Compute mean expression for unique genes
#' mean_exprs(data_mean_exprs, unique(data_mean_exprs$SYMBOL))
#'

mean_exprs <- function(data, symbols) {

  if (!"SYMBOL" %in% colnames(data)) {
    stop("The input data must have a 'SYMBOL' column.")
  }

  # Initialize an empty dataframe to store results
  col_names <- setdiff(colnames(data), "SYMBOL")
  mean_exprs_df <- data.frame(matrix(ncol = length(col_names), nrow = length(symbols)))
  colnames(mean_exprs_df) <- col_names #colnames(expression_data)[1:20]
  rownames(mean_exprs_df) <- symbols

  for (symbol in symbols) {
    # Filter data for the specific symbol
    eset_mat <- data %>% filter(SYMBOL == symbol)
    eset_mat_no_symbol <- eset_mat %>% dplyr::select(-SYMBOL)

    # If there is more than one row, compute the mean across probes (rows)
    if (nrow(eset_mat) > 1) {
      eset_mean <- apply(eset_mat_no_symbol, MARGIN = 2, FUN = mean)  # Handle NA in mean calculation
    } else {
      eset_mean <-eset_mat_no_symbol  #as.vector(eset_mat_no_symbol)
    }

    # Add the result to the dataframe with the symbol as the row name
    mean_exprs_df[symbol, ] <- eset_mean
  }
  return(mean_exprs_df)
}
