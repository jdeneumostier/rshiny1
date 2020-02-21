# Calcul des stats descriptives
computeStats <- function(values, alpha) {
  n <- length(values)
  m <- mean(values)
  md <- median(values)
  std <- sd(values)
  stderr <- std/sqrt(length(values))
  
  ci = c();
  ci[1] <- (m - qt(1-alpha/2, n-1) * std/sqrt(n))
  ci[2] <- (m + qt(1-alpha/2, n-1) * std/sqrt(n))
  
  stats <- c(n, m, md, std, stderr, ci[1], ci[2])
  names(stats) <- c("N", "Mean", "Median", "Standard deviation", "Standard error", 
                    paste("Lower ",1-alpha,"% CL for Mean", sep=""), 
                    paste("Upper ",1-alpha,"% CL for Mean", sep=""))
  return(stats)  
}
