#' Compare group expression data (statistical test)
#'
#' @description
#' This function compares the expression values of two groups using statistical tests (t-test or Wilcoxon rank-sum test).
#' It calculates the mean expression values for each group, performs the specified statistical test,
#' and adjusts the resulting p-values for multiple testing.

#' @param data A dataframe where rows represent genes, and columns represent samples.
#' @param group1_cols A character vector specifying the column names for samples belonging to the first group.
#' @param group2_cols A character vector specifying the column names for samples belonging to the second group.
#' @param group1_name A string representing the name of the first group. This will be used in the result as the mean column name.
#'                     Default is `"Group1"`.
#' @param group2_name A string representing the name of the second group. This will be used in the result as the mean column name.
#'                     Default is `"Group2"`.
#' @param test_type A string indicating the statistical test to use. Options are `"t-test"` for a t-test or `"wilcox"` for
#'   a Wilcoxon rank-sum test. Defaults to `"t-test"`.
#' @param adjust_method A string specifying the p-value adjustment method to account for multiple testing. Defaults to `"fdr"`.
#'   See `p.adjust` function for available methods.
#'
#' @export
#'
#' @return A data.frame with p.value, p.adjusted, and the mean value of each group.
#'
#' @examples
#' # example code
#' mean_expression_df <- data.frame(
#'   AITL_1 = c(1.2, 3.4, 5.6),
#'   AITL_2 = c(1.5, 3.7, 5.9),
#'   TFH_1 = c(2.1, 3.5, 4.6),
#'   TFH_2 = c(2.4, 3.8, 4.9),
#'   row.names = c("Gene1", "Gene2", "Gene3")
#' )
#'
#' # Define group columns
#' AITL_samples <- grep("^AITL", colnames(mean_expression_df), value = TRUE)
#' TFH_samples <- grep("^TFH", colnames(mean_expression_df), value = TRUE)
#'
#' # Perform comparison
#' compare_groups(mean_expression_df, AITL_samples, TFH_samples,group1_name='AITL',group2_name='Healthy', test_type = "t-test", adjust_method = "fdr")
#'
#'
compare_groups <- function(data, group1_cols, group2_cols, group1_name = "Group1", group2_name = "Group2", test_type = "t-test", adjust_method = "fdr") {

  # Dynamically create column names for means
  group1_mean_col <- paste0(group1_name, "_mean")
  group2_mean_col <- paste0(group2_name, "_mean")

  # Create a result data frame to store p-values and means
  results <- data.frame(
    Gene = rownames(data),
    P_Value = NA,
    Adjusted_P_Value = NA
  )

  # Add dynamic columns for means
  results[[group1_mean_col]] <- NA
  results[[group2_mean_col]] <- NA

  for (i in 1:nrow(data)) {
    group1_values <- as.numeric(data[i, group1_cols])
    group2_values <- as.numeric(data[i, group2_cols])

    results[[group1_mean_col]][i] <- mean(group1_values, na.rm = TRUE)
    results[[group2_mean_col]][i] <- mean(group2_values, na.rm = TRUE)

    if (test_type == "t-test") {
      test_result <- t.test(group1_values, group2_values)
      results$P_Value[i] <- test_result$p.value
    } else if (test_type == "wilcox") {
      test_result <- wilcox.test(group1_values, group2_values, exact = FALSE)
      results$P_Value[i] <- test_result$p.value
    } else {
      stop("Invalid test type. Choose 't-test' or 'wilcox'.")
    }
  }

  # Adjust p-values
  results$Adjusted_P_Value <- p.adjust(results$P_Value, method = adjust_method)

  return(results)
}
