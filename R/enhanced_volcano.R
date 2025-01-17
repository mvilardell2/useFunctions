#######################################################################
### FUNCTIONS FOR VISUALIZATION of DIFFERENTIAL EXPRESION ANALYSIS ####


#####################################################################

#' Volcano Plot with EnhancedVolcano
#'
#' This function creates an enhanced volcano plot for visualizing
#' differential expression results, such as log-fold changes and p-values.
#'
#' @param top.table Results from the DE analysis (logFC,P.value,), with genes in rownames.
#' @param filename Filename to save the plot (Default volcano_plot.png).
#' @param save Whether to save the plot or not (Default TRUE).
#' @param title Title of the plot.
#' @param top_genes Vector containing names of genes to highlight in the plot.
#'
#' @import EnhancedVolcano
#' @import ggplot2
#' @export
#'
#'
#' @examples
#' # Example data
#' data(top.table)
#' enhancedvolcanoplot(top.table, title = "Example Volcano Plot",save = FALSE)
#'


enhancedvolcanoplot <- function(top.table, filename = 'volcano_plot.png', save = TRUE,
                               title = 'Group1 vs Group2', top_genes = 'NULL') {

  # Create the volcano plot
  volcano_plot <- EnhancedVolcano(
    top.table, x = 'logFC',y = 'P.Value',
    lab = rownames(top.table),title = title,
    selectLab = top_genes, drawConnectors = TRUE,
    max.overlaps = 50, gridlines.major = TRUE,
    gridlines.minor = FALSE, colConnectors = 'black',
    cutoffLineWidth = 0.8, labSize = 4)

  # Save the plot if the user opts to do so
  if (save) {
    ggsave(filename = filename,plot = volcano_plot,width = 12,height = 8,
      units = "in",dpi = 300)
    message("Plot saved as: ", filename)
  }

  # Return the plot object for further use
  return(volcano_plot)
}




