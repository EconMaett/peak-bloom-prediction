gaussian_lowpass <- function(timeseries, sigma = 20 / 6, window_size = 20) {
  # Input validation
  if (!is.numeric(timeseries)) {
    stop("Input timeseries must be numeric")
  }

  # Create Gaussian weights
  x <- seq(-window_size / 2, window_size / 2, 1)
  weights <- dnorm(x, mean = 0, sd = sigma)
  weights <- weights / sum(weights) # Normalize weights to sum to 1

  # Pad the time series to handle edges
  n <- length(timeseries)
  pad_size <- floor(window_size / 2)
  padded_series <- c(
    rep(timeseries[1], pad_size),
    timeseries,
    rep(timeseries[n], pad_size)
  )

  # Apply the filter
  filtered <- numeric(n)
  for (i in 1:n) {
    window_start <- i
    window_end <- i + window_size - 1
    filtered[i] <- sum(padded_series[window_start:window_end] * weights)
  }

  return(filtered)
}

# Example usage:
# Generate sample data
set.seed(123)
years <- 1900:2000
temperatures <- cumsum(rnorm(101, 0, 0.1)) + sin(seq(0, 10, length.out = 101))

# Apply the filter
smoothed <- gaussian_lowpass(temperatures)

# Plot original and smoothed data
plot(years, temperatures,
  type = "l", col = "gray",
  main = "20-Year Gaussian Low-Pass Filter Example",
  xlab = "Year", ylab = "Temperature"
)
lines(years, smoothed, col = "blue", lwd = 2)
legend("topleft",
  legend = c("Original", "Filtered"),
  col = c("gray", "blue"), lwd = c(1, 2)
)
