estimate_coefficients <- function(dstats) {
  beta1 <- dstats$Sxy / dstats$Sxx
  beta0 <- dstats$ybar - beta1 * dstats$xbar
  return(c(beta0, beta1))  
}

get_dstats <- function(y, x) {
  xbar <- mean(x)
  ybar <- mean(y)
  Sxx <- sum((x - xbar)^2)
  Sxy <- sum((x - xbar) * (y - ybar))
  return(list(xbar = xbar, ybar = ybar, Sxx = Sxx, Sxy = Sxy))
}

make_prediction <- function(x, beta) {
  return(beta[1] + beta[2] * x)
}

estimate_sigma <- function(residuals) {
  n <- length(residuals)
  return(sqrt(sum(residuals^2) / (n - 2)))
}

make_regression_table <- function(x, beta, sigma, dstats) {
  return(data.frame(
    Term = c("Intercept", "Slope"),
    Estimate = beta,
    StdError = c(NA, NA),  # Placeholder
    tValue = c(NA, NA),
    pValue = c(NA, NA)
  ))
}

get_metrics <- function(y, sigma, dstats) {
  SST <- sum((y - dstats$ybar)^2)
  SSR <- sum((y - (dstats$ybar))^2) - sum((y - dstats$ybar)^2)
  R2 <- 1 - sum((y - (dstats$ybar))^2) / SST
  adjR2 <- R2 - (1 - R2) * (length(y) - 1) / (length(y) - 2)
  F <- NA  # placeholder
  return(list(R2 = R2, adjR2 = adjR2, F = F))
}
simple_regression <- function(x, y) {
  dstats <- get_dstats(y, x)
  beta_raw <- estimate_coefficients(dstats) 
  print("Raw beta:")
  print(beta_raw)                            
  
  beta <- as.numeric(beta_raw)               
  print("Numeric beta:")
  print(beta)
  
  fittedVals <- make_prediction(x, beta)
  residuals <- y - fittedVals
  sigma <- estimate_sigma(residuals)
  regtable <- make_regression_table(x, beta, sigma, dstats)
  df <- get_metrics(y, sigma, dstats)
  return(list(
    model = data.frame(y = y, x = x),
    coefficients = beta,
    residuals = residuals,
    fitted.values = fittedVals,
    sigma = sigma,
    R2 = df["R2"],
    adjR2 = df["adjR2"],
    Fstat = df["F"],
    regtable = regtable
  
  ))
}




display_report <- function(model) {
  cat("Fitted coeff:\n")
  cat("Intercept: ", model$coefficients[1], "\n")
  cat("Slope: ", model$coefficients[2],"\n'")
  cat("regression table: ")
  print(model$regtable)
  cat ("\n")
  cat("adjusted RA2:")
  cat (model$adj_r2)
  cat ("\n")
  cat("F statistic:")
  cat (model$fstat)
  cat ("\n")
  if (!is.null(model$fstat_pval) && !is.na(model$fstat_pval)) {
    cat("F-statistic p-value:", model$fstat_pval)
  }  else {
cat("null")

}
plot(model$model$x, model$model$y,
     main = "Regression Plot",
     xlab = "x", ylab = "y",
     pch = 16, col = "orange")
  abline(a = model$coefficients[1], b = model$coefficients[2], col = "blue", lwd = 2)
}


