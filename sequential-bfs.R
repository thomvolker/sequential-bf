
# is a sequential bayes factor design really such a good idea?

library(magrittr)
library(bain)
library(BFpack)

# create an empty vector of length 500
BF_bain <- BF_BFpack <- vector("numeric", length = 500)

d <- 0.2 # small effect, according to cohen

# n stands for the sample size (2:1000 in steps of 2)
for(n in 1:length(BF_bain) * 2) {
  # create two groups of equal size n
  n1 <- n2 <- n
  # sample two observations for the first group
  x1 <- rnorm(n1)
  # standardize, such that cohen's d is not dependent on sampling variability
  x1 <- (x1 - mean(x1)) / sd(x1)
  # sample two observations for the second group
  x2 <- rnorm(n2)
  # standardize again, and add cohen's d, to fix the effect size throughout
  x2 <- (x2 - mean(x2)) / sd(x2) + d
  # create a data.frame with all observations, and add grouping grouping factor.
  df <- data.frame(X = c(x1, x2),
                   G = rep(c("A", "B"), 
                           times = c(length(x1), length(x2))))
  # Calculate Bayes factors using bain
  BF_bain[n/2] <- lm(X ~ G, df) %>% bain(hypothesis = "GB=0") %$% fit$BF.u[1]
  # Calculate Bayes factors using BFpack
  BF_BFpack[n/2] <- lm(X ~ G, df) %>% BF("GB = 0") %$% BFtable_confirmatory[1,7]
}

# Plot the results.
{
  plot(x = 1:length(BF_BFpack) * 2,
       y = BF_BFpack,
       type = "l",
       lty = 1,
       xlab = "Sample size",
       ylab = "BF")
  lines(x = 1:length(BF_bain) * 2,
        y = BF_bain,
        type = "l",
        lty = 2)
  legend("topright",
         c("BFpack", "bain"),
         lty = c(1,2))
}

max(BF_BFpack)
which.max(BF_BFpack)

# The maximum Bayes factor equals 7.49, which exceeds the threshold
# of BF = 6 as compelling evidence as used in Schonbrodth & Wagenmakers
# and occurs at quite a substantial sample size, namely at n = 26*2 = 52
# observations per group (i.e., a total sample size of n = 104)
