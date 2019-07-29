library(tidyverse)
expit <- plogis
get_sample <- function(n, beta) {
  X <- matrix(rnorm(3 * n), ncol = 3)
  prob <- expit(X[, 1] + beta * X[, 2])
  Y <- rbinom(n, size = 1, prob = prob)
  out <- cbind(X, Y)
  colnames(out) <- c("X1", "X2", "X3", "Y")
  return(out)
}
dat <- get_sample(1e3, beta = 5)

algo <- function(dat) {
  dat <- as.data.frame(dat)
  fit <- glm(Y ~ ., data = dat, family = "binomial")
  Gbar <- function(newdata) {
    newdata <- as.data.frame(newdata)
    predict(fit, newdata, type = "response")
  }
  return(Gbar)
}

Gbar1 <- algo(dat) ## output of algorithm on full data
Gbar2 <- algo(dat[, c("X1", "Y")]) ## output of algorithm deprived of X2 and X3
Gbar3 <- algo(dat[, "Y", drop = FALSE]) ## output of reference algorithm

compute_influence <- function(J, dat, algo, V = 5) {
  if ("Y" %in% J) {
    stop("Cannot include 'Y' in 'J'.\n")
  }
  if (length(setdiff(J, colnames(dat))) > 0) {
    stop("Argument 'J' is not valid.\n")
  }
  B_n <- rep(1:V, length.out = nrow(dat))
  R_n1 <- 0
  R_n2 <- 0
  R_n3 <- 0
  for (v in 1:V) {
    ## learning
    training <- dat[B_n != v, ]
    Gbar1 <- algo(training) ## output of algorithm on full data
    Gbar2 <- algo(training[, setdiff(colnames(dat), J)]) ## output of algorithm deprived of X^J
    Gbar3 <- algo(training[, "Y", drop = FALSE]) ## output of reference algorithm
    ## testing
    testing <- dat[B_n == v, ]
    R_n1 <- R_n1 + mean((testing[, "Y"] - Gbar1(testing))^2) / 3
    R_n2 <- R_n2 + mean((testing[, "Y"] - Gbar2(testing))^2) / 3 
    R_n3 <- R_n3 + mean((testing[, "Y"] - Gbar3(testing))^2) / 3 
  }
  S_n1 <- 1 - R_n1 / R_n3
  S_n2 <- 1 - R_n2 / R_n3
  Delta_n <- S_n1 - S_n2 ## quantifies the influence of X^J
  return(c(Delta_n = Delta_n, S_n1 = S_n1, S_n2 = S_n2))
}

influence_X3 <- compute_influence("X3", dat, algo)
influence_X2 <- compute_influence("X2", dat, algo)

beta <- seq(-2, 2, length.out = 100)
nn <- c(1e2, 5e2, 1e3)

influences <- matrix(NA, ncol = 4, nrow = 0,
                     dimnames = list(c(), c("beta", "n", "X2", "X3")))
for (n in nn) {
  for (bb in beta) {
    dat <- get_sample(n, beta = bb)
    influences <- rbind(influences,
                        c(bb, n,
                          compute_influence("X2", dat, algo)[1],
                          compute_influence("X3", dat, algo)[1]))
  }
}

influences %>% as.tibble %>%
  gather(`X2`, `X3`, key = "J", value = "influence") %>%
  mutate(n = as.factor(n)) %>% 
  ggplot() +
  geom_point(aes(x = beta, y = influence, color = n)) +
  facet_wrap(~ J, nrow = 2)
