# make_hex_logo.R
# Hex logo for the exsampler package with six QQ panels.
# This version removes panel grid lines (both major & minor) so no
# vertical/horizontal background lines appear in the QQ plots.

suppressPackageStartupMessages({
  library(ggplot2)
  library(qqplotr)
  library(patchwork)
  library(hexSticker)
  library(svglite)
})

# ---- Aesthetics to mirror your package plots ----
.ex_band_colors <- c(
  pointwise = "#E69F00",
  boot      = "#56B4E9",
  ts        = "#009E73",
  ks        = "#CC79A7"
)

# Minimal local version if your package's create_single_plot() isn't loaded
if (!exists("create_single_plot")) {
  create_single_plot <- function(df, var, plot_type, bands = NULL,
                                 xlim = NULL, ylim = NULL) {
    p <- ggplot(df, aes(sample = .data[[var]]))
    
    band_colors <- .ex_band_colors
    if (is.character(bands) && length(bands) > 0) {
      bands <- intersect(c("ks","ts","boot","pointwise"), bands)
    } else {
      bands <- NULL
    }
    
    if (plot_type == "qq_normal") {
      p <- p + stat_qq_line(linewidth = 0.25)
      
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
      
      # Half of your original 0.7 point size
      p <- p + stat_qq_point(size = 0.35, stroke = 0.2,
                             color = "darkblue", shape = 1)
      
    } else if (plot_type == "qq_detrended") {
      p <- p + stat_qq_line(detrend = TRUE, linewidth = 0.25)
      
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
      
      p <- p + stat_qq_point(detrend = TRUE, size = 0.35, stroke = 0.2,
                             color = "darkblue", shape = 1)
    } else {
      stop("Only QQ plots are supported by this logo script.")
    }
    
    # NOTE: theme_light() + panel.grid = element_blank() removes the background lines
    p +
      theme_light(base_size = 4) +
      theme(
        panel.grid.major = element_blank(),   # remove major grid lines
        panel.grid.minor = element_blank(),   # remove minor grid lines
        axis.title       = element_blank(),
        axis.text        = element_text(),
        plot.title       = element_text(),
        plot.background  = element_rect(fill = "transparent", colour = NA)
      ) +
      scale_fill_identity() +
      coord_cartesian(xlim = xlim, ylim = ylim)
  }
}

# ---- Data for six panels (2x3 grid) ----
set.seed(123)
n <- 50
d1 <- data.frame(sample_data = rnorm(n, 0, 1))
d2 <- data.frame(sample_data = rnorm(n, 0, 1))
d3 <- data.frame(sample_data = rnorm(n, 0, 1))
d4 <- data.frame(sample_data = rnorm(n, 0, 1))
d5 <- data.frame(sample_data = -rlnorm(n, meanlog = 0, sdlog = 0.6))
d6 <- data.frame(sample_data = rnorm(n, 0, 1))

p1 <- create_single_plot(d1, "sample_data", "qq_normal", bands = "pointwise")
p2 <- create_single_plot(d2, "sample_data", "qq_normal", bands = "boot")
p3 <- create_single_plot(d3, "sample_data", "qq_normal", bands = "ts")
p4 <- create_single_plot(d4, "sample_data", "qq_normal", bands = "ks")
p5 <- create_single_plot(d5, "sample_data", "qq_normal", bands = NULL)
p6 <- create_single_plot(d6, "sample_data", "qq_normal", bands = NULL)

# ---- Make panels wider *and* taller with tight per-plot margins ----
# aspect.ratio is height/width. 0.68 ≈ 1.47x as wide as tall (landscape).
make_wide_tall <- function(p, ar = 0.68) {
  p + theme(
    aspect.ratio = ar,
    plot.margin  = margin(1, 2, 1, 2) # trim panel whitespace (top, right, bottom, left)
  )
}
p1 <- make_wide_tall(p1); p2 <- make_wide_tall(p2); p3 <- make_wide_tall(p3)
p4 <- make_wide_tall(p4); p5 <- make_wide_tall(p5); p6 <- make_wide_tall(p6)

# ---- Assemble the grid with a small, fixed vertical gap ----
qq_top    <- p1 | p2 | p3
qq_bottom <- p4 | p5 | p6
gap       <- plot_spacer()

qq_grid <- (qq_top / gap / qq_bottom) +
  plot_layout(heights = c(1, 0.01, 1)) &   # adjust the middle value to control row gap
  theme(plot.margin = margin(0, 0, 0, 0))  # remove outer patchwork margins

qq_grid <- qq_grid + plot_annotation(
  theme = theme(plot.background = element_rect(fill = "transparent", colour = NA))
)

# ---- Build hex with hexSticker ----
out_png <- "man/figures/logo.png"
out_svg <- "man/figures/logo.svg"
dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)

st <- sticker(
  qq_grid,
  package  = "exsampler",
  p_color  = "#0f172a",
  p_size   = 36,     # big centered title
  p_x      = 1.00,
  p_y      = 1,   # “shoulders” of the hex
  h_fill   = "white",
  h_color  = "#0f172a",
  s_width  = 1.58,   # wide grid
  s_height = 1.22,   # slightly shorter to avoid crowding top/bottom
  s_x      = 1.00,
  s_y      = 0.95,   # nudge grid up
  filename = out_png,
  dpi      = 600,
  white_around_sticker = FALSE
)

# Save SVG with transparent background
ggplot2::ggsave(
  out_svg, st,
  width = 10, height = 11.5, units = "cm", dpi = 600,
  device = svglite::svglite,
  bg = "transparent"
)

# Optional: trim extra transparent pixels around the hex (especially bottom)
if (requireNamespace("magick", quietly = TRUE)) {
  img <- magick::image_read(out_png)
  img <- magick::image_trim(img, fuzz = 5)
  magick::image_write(img, path = out_png)
}

message("Wrote: ", normalizePath(out_png))
message("Wrote: ", normalizePath(out_svg))
