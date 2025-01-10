
#' Heatmap with Pheatmap
#'
#' @param data Data frame with normalized values and genes in rownames
#' @param filename Name of the file to save the plot
#' @param save Whether to save the plot or not (Default TRUE).
#' @param scale Scale data (Default rows).
#' @param cluster_cols Default True
#' @param cluster_rows Default True
#' @param show_rownames Default False
#' @param show_colnames Default True
#' @param title Title of the plot.
#' @param top Select top rows (and do the heatmap with them).
#' @param annotation Data frame with annotaion
#'
#' @import pheatmap
#' @import ggplot2
#'
#' @export


pheatmap <- function(data, filename = "heatmap_plot.png", save = TRUE, scale = "row", fontsize = 7, cluster_rows = TRUE,
                     cluster_cols = TRUE, show_rownames = FALSE, show_colnames = TRUE,
                     annotation = NULL, title = "Heatmap",top = NULL) {


  if (!is.null(top)) {
    data <- data[1:top, , drop = FALSE] # Select top rows
  }

  # Generate the heatmap
  heatmap_plot <- pheatmap(
    data, scale = scale,          # Scale rows or columns
    fontsize = fontsize,          # Font size for text
    cluster_rows = cluster_rows,  # Cluster rows
    cluster_cols = cluster_cols,  # Cluster columns
    show_rownames = show_rownames,# Display row names
    show_colnames = show_colnames,# Display column names
    annotation_col = annotation, # Column annotations
    main = title                  # Title for the heatmap
  )

  # Save the heatmap if requested
  if (save) {
    ggsave(filename = filename,plot = as.ggplot(heatmap_plot),width = 12,
           height = 10,units = "in",dpi = 300)
    message("Heatmap saved as: ", filename)
  }

  # Return the heatmap object for further use
  return(heatmap_plot)
}


#create_heatmap(results_normalization,top=100,save = F,annotation = pD_annotation,show_rownames = T)
