#' Scale numeric vector to 1–10
#' 
#' @param x A numeric vector
#' @return A numeric vector scaled to 1–10
#' @export
scale_to_1_10 <- function(x) {
  scaled <- 1 + 9 * (x - min(x)) / (max(x) - min(x))
  round(scaled, 2)
}

#' Scale to 0–10
#' Rescales a numeric vector to 0–10
#' 
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

#' Estimate largest common increment
#'
#' Finds the largest grid step shared by gaps between unique values in `x`.
#'
#' @param x Numeric vector.
#' @param max_decimals Integer rounding for gaps.
#' @param tol Numeric tolerance for zero gaps.
#' @return Numeric scalar: estimated increment.
#' @export
estimate_data_increment <- estimate_data_increment <- function(x, max_decimals = 6L, tol = 1e-8) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2) return(1)
  
  u <- sort(unique(x))
  d <- diff(u)
  d <- d[abs(d) > tol]
  if (!length(d)) return(1)
  
  # tame floating noise and prepare integer scaling
  d <- round(d, max_decimals)
  maxd <- max(abs(d))
  if (!is.finite(maxd) || maxd == 0) return(1)
  
  # choose a scale to avoid 32-bit integer overflow
  p <- max(0L, min(max_decimals,
                   floor(log10((.Machine$integer.max - 1) / maxd))))
  scale <- 10^p
  
  ints <- as.integer(round(d * scale))
  ints <- abs(ints[ints != 0L])
  if (!length(ints)) return(min(d))
  
  # integer GCD of all gaps
  gcd2 <- function(a, b) if (b == 0L) abs(a) else Recall(b, a %% b)
  g <- Reduce(gcd2, ints)
  
  step <- g / scale
  
  # Guardrail: if GCD is implausibly tiny vs the smallest observed gap, fall back
  if (min(d) / step > 1e3) step <- min(d)
  
  step
}


#' Generate a grid of simulated datasets matching a real variable
#'
#' Simulate 7 additional samples from a normal distribution with the
#' same mean, standard deviation, and (optionally) increment spacing as the
#' real data, then shuffle them together for a “find the real data” grid.
#'
#' Ensures each simulated sample passes a Shapiro–Wilk normality test at
#' p >= `p_thresh` (defaults to 0.05), up to `max_attempts` retries (default = 50).
#'
#' @param data A data.frame containing the real observations.
#' @param variable_name Character; the name of the numeric column in `data` to simulate from.
#' @param seed_modifier Integer; seed for `set.seed()`, so each click gives a new shuffle.
#' @param n Integer; total number of grids (1 real +7).
#' @param match_increment Logical; if `TRUE`, rounds simulated values to the same increment as the real data (via `estimate_data_increment`).
#' @param p_thresh Numeric; required Shapiro–Wilk p-value threshold (default 0.01).
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
generate_random_data_for_plot_grid <- function(
    data, variable_name,
    seed_modifier = 1,
    n = 9,
    match_increment = TRUE,
    p_thresh = 0.01,
    max_attempts = 50
) {
  x  <- stats::na.omit(data[[variable_name]])
  mu <- mean(x); sd <- stats::sd(x); N <- length(x)
  min_x <- min(x); max_x <- max(x)
  
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
      sim <- pmax(min_x, pmin(max_x, sim))
      
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

