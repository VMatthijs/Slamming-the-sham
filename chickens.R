# Reading in chicken data
chicks <- read.table("chickens.dat", header = TRUE)
J <- nrow(chicks)
x <- chicks$freq
y0 <- chicks$sham_est - 1
se0 <- chicks$sham_se
n0 <- chicks$sham_n
y1 <- chicks$exposed_est - 1
se1 <- chicks$exposed_se
n1 <- chicks$exposed_n
diff <- y1 - y0
diff_se <- sqrt(se1 ^ 2 + se0 ^ 2)

# Plotting Blackman exposed-sham treatment effects
pdf("blackman1a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(diff),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  yaxt = "n",
  bty = "l",
  type = "n",
  main = "Exposed - Sham",
  cex.main = .9
)
axis(2,
     seq(-.1, .4, .1),
     c("", "0.0", "", "0.2", "", "0.4"),
     mgp = c(1.5, .5, 0))
w <- 2.5
x_plot <- x
x_plot[x == 165] <- 165 + c(-w, w)
x_plot[x == 180] <- 180 + c(-w, w)
x_plot[x == 405] <- 405 + c(-w, w)
pvalue <- 2 * pt(-diff / diff_se, n1 + n0 - 2)
for (j in 1:J)
  polygon(
    x_plot[j] + c(-w, -w, w, w),
    c(diff[j], 0, 0, diff[j]),
    col = ifelse(pvalue[j] < .05, "black", 0),
    lwd = .5
  )
dev.off()

# Plotting Blackman exposed-sham p-values
pdf("blackman1b.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  c(0, max(x) + 10),
  c(0, 0.21),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "P-value",
  yaxt = "n",
  xaxs = "i",
  yaxs = "i",
  bty = "l",
  type = "n",
  main = "Exposed - Sham p-values",
  cex.main = .9
)
abline(0.01, 0, lty = 2, col = "gray40")
abline(0.05, 0, lty = 2, col = "gray40")
axis(
  2,
  c(0, 0.01, 0.05, 0.10, 0.20),
  c("0", ".01", ".05", ".10", "> .20"),
  cex.axis = 0.9,
  mgp = c(1.5, .3, 0),
  las = 1
)
pvalue_adj <- ifelse(pvalue < 0.0015, 0.0015, pvalue)
points(x_plot, ifelse(pvalue_adj > 0.2, 0.2, pvalue_adj), pch = 20)
dev.off()

# Plotting Blackman exposed only treatment effects
pdf("blackman2a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(diff),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  yaxt = "n",
  bty = "l",
  type = "n",
  main = "Exposed data only",
  cex.main = .9
)
axis(2,
     seq(-.1, .4, .1),
     c("", "0.0", "", "0.2", "", "0.4"),
     mgp = c(1.5, .5, 0))
w <- 2.5
x_plot <- x
x_plot[x == 165] <- 165 + c(-w, w)
x_plot[x == 180] <- 180 + c(-w, w)
x_plot[x == 405] <- 405 + c(-w, w)
pvalue <- 2 * pt(-y1 / se1, n1 - 1)
for (j in 1:J)
  polygon(
    x_plot[j] + c(-w, -w, w, w),
    c(y1[j], 0, 0, y1[j]),
    col = ifelse(pvalue[j] < .05, "black", 0),
    lwd = .5
  )
dev.off()

# Plotting Blackman exposed only p-values
pdf("blackman2b.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  c(0, max(x) + 10),
  c(0, 0.21),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "P-value",
  yaxt = "n",
  xaxs = "i",
  yaxs = "i",
  bty = "l",
  type = "n",
  main = "Exposed data only p-values",
  cex.main = .9
)
abline(0.01, 0, lty = 2, col = "gray40")
abline(0.05, 0, lty = 2, col = "gray40")
axis(
  2,
  c(0, 0.01, 0.05, 0.10, 0.20),
  c("0", ".01", ".05", ".10", "> .20"),
  cex.axis = 0.9,
  mgp = c(1.5, .3, 0),
  las = 1
)
pvalue_adj <- ifelse(pvalue < 0.0015, 0.0015, pvalue)
points(x_plot, ifelse(pvalue_adj > 0.2, 0.2, pvalue_adj), pch = 20)
dev.off()

## Look at sham data
x2 <- sum((y0 / se0) ^ 2)

pdf("blackman3a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y0 - se0, y0 + se0),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimate from sham data",
  bty = "l",
  type = "n"
)
abline(0, 0, col = "gray")
points(x, y0, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), y0[j] + c(-1, 1) * se0[j])
}
dev.off()

pdf("blackman3b.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(y1 - se1, y1 + se1),
  range(y0 - se0, y0 + se0),
  xlab = "Estimate from exposed data",
  ylab = "Estimate from sham data",
  bty = "l",
  type = "n"
)
abline(0, 0, col = "gray")
abline(v = 0, col = "gray")
points(y1, y0, pch = 20)
for (j in 1:J) {
  lines(rep(y1[j], 2),
        y0[j] + c(-1, 1) * se0[j],
        col = "gray40",
        lwd = .5)
  lines(y1[j] + c(-1, 1) * se1[j],
        rep(y0[j], 2),
        col = "gray40",
        lwd = .5)
}
dev.off()

## Get ready to fit Bayesian models
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

y <- c(y0, y1)
se <- c(se0, se1)
expt_id <- rep(1:J, 2)
z <- c(rep(0, J), rep(1, J))  # treatment:  0 = sham, 1 = exposed
df <- c(n0 - 1, n1 - 1)
data <- list(
  N = 2 * J,
  J = J,
  y = y,
  x = x,
  expt_id = expt_id,
  z = z,
  df = df
)

## Fit a hierarchical model
fit_hier <-
  stan(
    "chickens-no-corr-hier.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_hier)

theta <- extract(fit_hier)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from hierarchical model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

## Plot raw estimates from exposed data
pdf("blackman4b.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Raw estimates from exposed data",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, y1, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), y1[j] + c(-1, 1) * se1[j])
}
dev.off()

## Fake-data simulation, estimation, and comparison
chickens_hier <- stan_model("chickens-no-corr-hier.stan")
estimate_names <- c("Exposed only", "Difference", "Bayes")
summ_names <-
  c(
    "Number of stat. signif. estimates",
    "Type S error rate",
    "Root mean squared error",
    "Rank correlation of estimates with truth"
  )
summ_abbs <-
  c("Number stat. signif.",
    "Type S error rate",
    "RMS error",
    "Rank corr. with truth")
fake_sim <- function(J, sigma_b, theta_true, sigma_y) {
  b <- rnorm(J, 0, sigma_b)
  y0 <- rt(J, n0 - 1) * sigma_y + b
  y1 <- rt(J, n1 - 1) * sigma_y + b + theta_true
  y <- c(y0, y1)
  se <- rep(sigma_y, 2 * J)
  z <- c(rep(0, J), rep(1, J))  # treatment:  0 = sham, 1 = exposed
  data <- list(
    N = 2 * J,
    J = J,
    y = y,
    x = x,
    expt_id = expt_id,
    z = z
  )
  fit_sim <- sampling(chickens_hier,
                      data = data,
                      refresh = 0,
                      iter = 1000)
  theta <- extract(fit_sim)$theta
  ## inferential summaries
  estimates <- cbind(y1, y1 - y0, apply(theta, 2, mean))
  estimates_025 <-
    cbind(y1 - qt(0.975, df = n1 - 1) * sigma_y,
          (y1 - y0) - qt(0.975, df = n0 + n1 - 2) * sqrt(2) * sigma_y,
          apply(theta, 2, quantile, 0.025))
  estimates_975 <-
    cbind(y1 + qt(0.975, df = n1 - 1) * sigma_y,
          (y1 - y0) + qt(0.975, df = n0 + n1 - 2) * sqrt(2) * sigma_y,
          apply(theta, 2, quantile, 0.975))
  ## statistical properties
  significant <- sign(estimates_025) == sign(estimates_975)
  correct_sign <- sign(estimates) == sign(theta_true)
  proportion_significant <- apply(significant, 2, mean)
  type_s_rate <-
    1 - apply(significant &
                correct_sign, 2, mean) / proportion_significant
  mse <- apply((estimates - theta_true) ^ 2, 2, mean)
  rank_corr <- rep(NA, 3)
  for (k in 1:3) {
    rank_corr[k] <- cor(rank(estimates[, k]), rank(theta_true))
  }
  summ <-
    cbind(J * proportion_significant, type_s_rate, mse, rank_corr)
  return(summ)
}

fake_sim_dist <- function(J, sigma_b, theta_mat, sigma_y) {
  n_sims <- nrow(theta_mat)
  summ_all <- array(NA, c(n_sims, 3, 4))
  summ <- array(NA, c(3, 4))
  for (s in 1:n_sims) {
    if (s %% 10 == 0)
      cat(s, "")
    summ_all[s, , ] <- fake_sim(J, sigma_b, theta_mat[s, ], sigma_y)
  }
  cat("\n")
  for (k1 in 1:3) {
    for (k2 in 1:4) {
      summ[k1, k2] <- mean(summ_all[, k1, k2])
    }
  }
  summ[, 2] <- sqrt(summ[, 2])
  return(summ)
}

sigma_y <- 0.04
n_sims <- 200
theta_mat <- theta[sample(nrow(theta), n_sims), ]
sigma_b_grid <- seq(0, 0.10, 0.01)
n_grid <- length(sigma_b_grid)
fake_summ <-
  array(NA,
        c(n_grid, 3, 4),
        dimnames = list(sigma_b_grid, estimate_names, summ_names))
for (i in 1:n_grid) {
  sigma_b <- sigma_b_grid[i]
  cat("sigma_b =", sigma_b, ", simulations: ")
  fake_summ[i, , ] <- fake_sim_dist(J, sigma_b, theta_mat, sigma_y)
}

y_shift <-
  rbind(
    c(+0.8,+0.7,-0.5),
    c(+0.01,+0.0345,-0.0301),
    c(+0.0002,+0.0005,-0.0005),
    c(-0.04,-0.04,+0.04)
  )
pdf("blackman5a.pdf", height = 5.5, width = 7)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
par(mfrow = c(2, 2))
for (k in 1:4) {
  summ <- fake_summ[, , k]
  y_min <- min(summ)
  y_max <- max(summ)
  y_range <-
    if (k == 1)
      0.5 * (y_min + y_max) + c(-1, 1) * 0.54 * (y_max - y_min)
  else if (k == 4)
    c(0, 1)
  else
    c(0, 1.1 * max(summ))
  plot(
    range(sigma_b_grid),
    y_range,
    xlab = "Scale of sham effects",
    ylab = summ_abbs[k],
    xaxs = "i",
    yaxs = "i",
    bty = "l",
    type = "n",
    main = summ_names[k],
    cex.main = .9
  )
  for (l in 1:3) {
    lines(sigma_b_grid, summ[, l], col = l)
    text(
      sigma_b_grid[n_grid] - 0.001,
      summ[n_grid, l] + y_shift[k, l],
      estimate_names[l],
      adj = 1,
      cex = .9,
      col = l
    )
  }
}
dev.off()

# Appendix 1: hierarchical model without pooling thetas
fit_no_pooling_theta <-
  stan(
    "chickens-flat-theta-hier-b.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_no_pooling_theta)

theta <- extract(fit_no_pooling_theta)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4c.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from partially hierarchical model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

# Appendix 2: non-hierarchical linear model
fit_no_pooling <-
  stan(
    "chickens-flat.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_no_pooling)

theta <- extract(fit_no_pooling)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4d.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from non-hierarchical model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

# Appendix 3: locally smooth Gaussian process model
fit_gauss_proc <-
  stan(
    "chickens-no-corr-gp1.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_gauss_proc)

theta <- extract(fit_gauss_proc)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4e.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from SE Gaussian process model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

# Appendix 4: periodic Gaussian process model
fit_periodic_gp <-  stan(
  "chickens-no-corr-gp2.stan",
  data = data,
  control = list(adapt_delta = 0.9)
)
print(fit_periodic_gp, "theta")

theta <- extract(fit_periodic_gp)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4f.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from periodic Gaussian process model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

# Appendix 5: Hierarchical model with bias-experiment correlation
fit_hier_corr <-
  stan("chickens-corr-hier.stan",
       data = data,
       control = list(adapt_delta = 0.9))
print(fit_hier_corr)


theta <- extract(fit_hier_corr)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("blackman4g.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from bias-correlated hierarchical model",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

## Appendix 6: raw difference estimates from exposed and sham data
pdf("blackman4h.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Frequency of magnetic field (Hz)",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Raw estimates of exposed minus sham data",
  cex.main = .9
)
abline(0, 0, col = "gray")
points(x, diff, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), diff[j] + c(-1, 1) * diff_se[j])
}
dev.off()

## Appendix 7: Simulation study with raw thetas
sigma_y <- 0.04
n_sims <- 200
theta_mat <- t(replicate(n_sims, y1))
sigma_b_grid <- seq(0, 0.10, 0.01)
n_grid <- length(sigma_b_grid)
fake_summ <-
  array(NA,
        c(n_grid, 3, 4),
        dimnames = list(sigma_b_grid, estimate_names, summ_names))
for (i in 1:n_grid) {
  sigma_b <- sigma_b_grid[i]
  cat("sigma_b =", sigma_b, ", simulations: ")
  fake_summ[i, , ] <- fake_sim_dist(J, sigma_b, theta_mat, sigma_y)
}

y_shift <-
  rbind(
    c(-2.1,+0.5,-0.3),
    c(+0.01,+0.025,+0.0151),
    c(+0.0002,+0.0005,-0.0005),
    c(-0.04,-0.04,+0.04)
  )
pdf("blackman5b.pdf", height = 5.5, width = 7)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
par(mfrow = c(2, 2))
for (k in 1:4) {
  summ <- fake_summ[, , k]
  y_min <- min(summ)
  y_max <- max(summ)
  y_range <-
    if (k == 1)
      0.5 * (y_min + y_max) + c(-1, 1) * 0.54 * (y_max - y_min)
  else if (k == 4)
    c(0, 1)
  else
    c(0, 1.1 * max(summ))
  plot(
    range(sigma_b_grid),
    y_range,
    xlab = "Scale of sham effects",
    ylab = summ_abbs[k],
    xaxs = "i",
    yaxs = "i",
    bty = "l",
    type = "n",
    main = summ_names[k],
    cex.main = .9
  )
  for (l in 1:3) {
    lines(sigma_b_grid, summ[, l], col = l)
    text(
      sigma_b_grid[n_grid] - 0.001,
      summ[n_grid, l] + y_shift[k, l],
      estimate_names[l],
      adj = 1,
      cex = .9,
      col = l
    )
  }
}
dev.off()

## Run Shinystan for diagnostics	
library("shinystan")	
my_sso <- launch_shinystan(fit_gauss_proc) 
