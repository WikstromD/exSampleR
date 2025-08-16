#' Create Plot from Data
#'
#' Generates a single plot of a specified type.
#'
#' @param df A data.frame containing a sample_data column
#' @param var The variable to plot (typically "sample_data")
#' @param plot_type One of "qq_normal", "qq_detrended", "pp_plot", "histogram"
#' @param bands Optional confidence band types (character vector)
#'
#' @param xlim Optional numeric length-2 vector for x-axis limits
#' @param ylim Optional numeric length-2 vector for y-axis limits
#' @return A ggplot object
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_histogram geom_density stat_function theme_light theme scale_fill_identity element_blank element_text coord_cartesian
#' @importFrom qqplotr stat_qq_line stat_qq_band stat_qq_point stat_pp_line stat_pp_band stat_pp_point
#' @importFrom stats dnorm
create_single_plot <- function(df, var, plot_type, bands = NULL, xlim = NULL, ylim = NULL) {
  p <- ggplot(df, aes(sample = .data[[var]]))
  
  band_colors <- c(
    pointwise = "#E69F00",
    boot      = "#56B4E9",
    ts        = "#009E73",
    ks        = "#CC79A7"
  )
  
  # Only keep valid bands
  if (is.character(bands) && length(bands) > 0) {
    bands <- intersect(c("ks","ts","boot","pointwise"), bands)
  } else {
    bands <- NULL
  }
  
  if (plot_type == "qq_normal") {
    p <- p + stat_qq_line(linewidth= 0.25)
    if (!is.null(bands)) {
      for (b in bands) {
        p <- p + stat_qq_band(
          bandType    = b,
          conf        = 0.95,
          fill        = band_colors[[b]],
          alpha       = 0.5,
          show.legend = FALSE
        )
      }
    }
    p <- p + stat_qq_point(size = 0.7, stroke = 0.2, color = "darkblue", shape = 1)
    
  } else if (plot_type == "qq_detrended") {
    p <- p + stat_qq_line(detrend = TRUE, linewidth= 0.25)
    if (!is.null(bands)) {
      for (b in bands) {
        p <- p + stat_qq_band(
          detrend     = TRUE,
          bandType    = b,
          conf        = 0.95,
          fill        = band_colors[[b]],
          alpha       = 0.5,
          show.legend = FALSE
        )
      }
    }
    p <- p + stat_qq_point(detrend = TRUE, size = 0.7, stroke = 0.2, color = "darkblue", shape = 1)
    
  } else if (plot_type == "pp_plot") {
    p <- p + stat_pp_line(linewidth= 0.25)
    if (!is.null(bands) && "pointwise" %in% bands) {
      p <- p + stat_pp_band(
        conf        = 0.95,
        fill        = band_colors[["pointwise"]],
        alpha       = 0.5,
        show.legend = FALSE
      )
    }
    p <- p + stat_pp_point(size = 0.7, stroke = 0.2, color = "darkblue", shape = 1)
    
  } else if (plot_type == "histogram") {
    mu <- mean(df[[var]], na.rm = TRUE)
    sd <- sd(df[[var]], na.rm = TRUE)
    p <- ggplot(df, aes(x = .data[[var]])) +
      geom_histogram(aes(y = ggplot2::after_stat(density)), bins = 20,
                     fill = "skyblue1", color = "black", linewidth = 0.2) +
      geom_density(linewidth = 0.25, linetype = 2) +
      stat_function(fun  = dnorm,
                    args = list(mean = mu, sd = sd),
                    linewidth = 0.25,
                    color = "red")
  }
  
  p +
    theme_light(base_size = 4) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_text(),
      plot.title = element_text()
    ) +
    scale_fill_identity() +
    ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
}