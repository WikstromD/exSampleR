#' Scale numeric vector to 1–10 and round
#'
#' @param x A numeric vector
#' @return A numeric vector scaled to 1–10
#' @export
scale_to_1_10 <- function(x) {
  scaled <- 1 + 9 * (x - min(x)) / (max(x) - min(x))
  round(scaled, 2)
}

#' Scale to 0–10
#' Rescales a numeric vector to `0–10`. NAs are ignored for the range and preserved.
#' Zero/non-finite range returns all zeros.
#' @param x Numeric vector.
#' @return Numeric vector of same length, scaled to `0–10`.
#' @export
scale_to_0_10 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  10 * (x - rng[1]) / diff(rng)
}

scale_to_0_10 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  10 * (x - rng[1]) / diff(rng)
}

#' Estimate data increment from a numeric vector
#'
#' @param x A numeric vector
#' @return A common increment value
#' @export
estimate_data_increment <- function(x) {
  x <- as.numeric(na.omit(x))
  if (length(x) < 2) return(1)
  unique_x <- sort(unique(x))
  diffs <- diff(unique_x)
  diffs <- diffs[diffs > .Machine$double.eps^0.5]
  if (length(diffs) == 0) return(1)
  min_diff <- min(diffs)
  common_increments <- c(
    0.0001, 0.0005, 0.001, 0.005, 0.01, 0.02, 0.025, 0.05, 0.1, 0.125, 0.2,
    0.25, 0.5, 1, 2, 5, 10, 25, 50, 100, 250, 500, 1000
  )
  diff_to_common <- abs(min_diff - common_increments)
  closest_increment <- common_increments[which.min(diff_to_common)]
  if (abs(min_diff - closest_increment) / min_diff > 0.1 && min_diff < 0.1) {
    if (closest_increment / min_diff > 2 && closest_increment > 0.01) {
      return(min_diff)
    }
  }
  return(closest_increment)
}

#' Generate a grid of simulated datasets matching a real variable
#'
#' Simulate `n - 1` additional samples from a normal distribution with the
#' same mean, standard deviation, and (optionally) increment spacing as the
#' real data, then shuffle them together for a “find the real data” grid.
#'
#' Ensures each simulated sample passes a Shapiro–Wilk normality test at
#' p >= `p_thresh` (defaults to 0.10), up to `max_attempts` retries.
#'
#' @param data A data.frame containing the real observations.
#' @param variable_name Character; the name of the numeric column in `data` to simulate from.
#' @param seed_modifier Integer; seed for `set.seed()`, so each click gives a new shuffle.
#' @param n Integer; total number of grids (1 real + n - 1 simulated).
#' @param match_increment Logical; if `TRUE`, rounds simulated values to the same increment as the real data (via `estimate_data_increment`).
#' @param p_thresh Numeric; required Shapiro–Wilk p-value threshold (default 0.10 means “looks normal enough”).
#' @param max_attempts Integer; max resimulation attempts per simulated sample (default 50).
#'
#' @return A list with elements:
#' * `shuffled_data_frames`: a list of data.frames, each with columns
#'   `sample_data` (the values) and `.type` (`"real"` or `"sim1"`, `"sim2"`, …), in random order.
#' * `variable_name`: always `"sample_data"` (for downstream plotting).
#' * `real_position`: the 1-based index in `shuffled_data_frames` where the real data landed.
#' * `grid_cols`: integer number of columns for layout (fixed at 3).
#'
#' @importFrom purrr map imap set_names
#' @importFrom stats rnorm shapiro.test
#' @export
generate_random_data_for_plot_grid <- function(
    data, variable_name,
    seed_modifier = 1,
    n = 9,
    match_increment = TRUE,
    p_thresh = 0.10,
    max_attempts = 50
) {
  x  <- stats::na.omit(data[[variable_name]])
  mu <- mean(x); sd <- stats::sd(x); N <- length(x)
  
  # Shapiro–Wilk requires 3 <= n <= 5000 and non-constant data
  shapiro_ok_to_run <- N >= 3 && N <= 5000
  
  increment <- if (match_increment) estimate_data_increment(x) else NULL
  
  # Keep simulations reproducible per "click" but different across sims/attempts
  set.seed(seed_modifier)
  
  simulate_until_normal <- function(mu, sd, N, increment, p_thresh, max_attempts, seed_offset = 0L) {
    # Degenerate case: no variability -> just return near-constant values; skip SW test.
    if (!is.finite(sd) || sd <= 0) {
      return(rep(mu, N))
    }
    
    best_sim <- NULL
    best_p   <- -Inf
    last_sim <- NULL
    
    for (i in seq_len(max_attempts)) {
      # diversify randomness for each sim/attempt
      set.seed(seed_modifier + seed_offset + i)
      
      sim <- stats::rnorm(N, mu, sd)
      if (!is.null(increment) && is.finite(increment) && increment > 0) {
        sim <- round(sim / increment) * increment
      }
      last_sim <- sim
      
      pval <- NA_real_
      if (shapiro_ok_to_run && length(unique(sim)) >= 3) {
        pval <- suppressWarnings(stats::shapiro.test(sim)$p.value)
      }
      
      if (!is.na(pval) && pval > best_p) {
        best_p  <- pval
        best_sim <- sim
      }
      
      if (!is.na(pval) && pval >= p_thresh) {
        return(sim)  # accept immediately
      }
      # Otherwise loop again
    }
    
    # Fallback: best passing attempt if we found one, else last try
    if (!is.null(best_sim)) return(best_sim)
    return(last_sim)
  }
  
  sims <- purrr::imap(1:(n - 1), function(idx, .x) {
    seed_offset <- idx * 1000L
    simulate_until_normal(mu, sd, N, increment, p_thresh, max_attempts, seed_offset)
  })
  
  df_list       <- c(list(real = x), purrr::set_names(sims, paste0("sim", 1:(n - 1))))
  shuffled      <- sample(names(df_list))
  dfs           <- purrr::map(shuffled, ~data.frame(sample_data = df_list[[.x]], .type = .x))
  real_position <- which(shuffled == "real")
  
  list(
    shuffled_data_frames = dfs,
    variable_name        = "sample_data",
    real_position        = real_position,
    grid_cols            = 3
  )
}

