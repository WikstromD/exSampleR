#' Generates a dataset of example distributions
#'
#' Creates a data frame with various shaped distributions scaled to 1–10.
#'
#' @param n Number of samples to generate
#' @param seed Random seed
#'
#' @return A data.frame of simulated distributions
#' @export
#'
#' @importFrom sn rsn
#' @importFrom stats rnorm rt runif rexp
generate_exsample_distributions <- function(n = 1000, seed = 123) {
  set.seed(seed)

  light_right_skew  <- scale_to_1_10(sn::rsn(n, xi = 0, omega = 1, alpha =  2))
  heavy_right_skew  <- scale_to_1_10(sn::rsn(n, xi = 0, omega = 1, alpha =  8))
  light_left_skew   <- scale_to_1_10(sn::rsn(n, xi = 0, omega = 1, alpha = -2))
  heavy_left_skew   <- scale_to_1_10(sn::rsn(n, xi = 0, omega = 1, alpha = -8))

  positive_kurtosis <- scale_to_1_10(stats::rt(n, df = 10)) 
  negative_kurtosis <- scale_to_1_10(stats::rbeta(n, 2.5, 2.5))

  k                 <- floor(n / 2)
  bimodal           <- scale_to_1_10(c(
    stats::rnorm(k, -2, 0.5),
    stats::rnorm(n - k, 2,  0.5)
  ))

  uniform           <- scale_to_1_10(stats::runif(n, -3, 3))

  raw0              <- ifelse(stats::runif(n) < 0.3, 0, stats::rnorm(n))
  zero_inflated     <- numeric(n)
  nz                <- raw0 != 0
  if (any(nz)) zero_inflated[nz] <- scale_to_0_10(raw0[nz]) 
  zero_inflated[!nz] <- 0

  exponential       <- scale_to_1_10(stats::rexp(n, rate = 1))

  normal            <- scale_to_1_10(stats::rnorm(n, 0, 1))

  data.frame(
    Normal            = normal,
    Light_Right_Skew  = light_right_skew,
    Heavy_Right_Skew  = heavy_right_skew,
    Light_Left_Skew   = light_left_skew,
    Heavy_Left_Skew   = heavy_left_skew,
    Positive_Kurtosis  = positive_kurtosis,
    Negative_Kurtosis  = negative_kurtosis,
    Bimodal           = bimodal,
    Uniform           = uniform,
    Zero_Inflated     = zero_inflated,
    Exponential       = exponential,
    stringsAsFactors  = FALSE
  )
}
