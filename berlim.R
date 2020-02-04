# Reading in berlim data
berlim <- read.table("berlim.dat", header = TRUE)
J <- nrow(berlim)
x <- c(1:15)
y0 <- berlim$sham_est
se0 <- berlim$sham_se
y1 <- berlim$exposed_est
se1 <- berlim$exposed_se
n0 <- berlim$sham_n
n1 <- berlim$exposed_n
n0rem <- berlim$sham_n_rem
n1rem <- berlim$exposed_n_rem
diff <- y1 - y0
diff_se <- sqrt(se1 ^ 2 + se0 ^ 2)
xname <- berlim$Study
logoddsratios <- berlim$log_odds_ratio
lo_ses <- berlim$lo_se

# Plotting berlim exposed-sham treatment effects
pdf("berlim1a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(diff),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  yaxt = "n",
  bty = "l",
  type = "n",
  main = "Exposed - Sham",
  cex.main = .9,
  xaxt='n'
)
axis(2)
axis(1, at = c(1,4,8,12,16,20,24))
w <- 0.2
x_plot <- x
pvalue <- 2 * pt(-diff / diff_se, n1 + n0 - 2)
for (j in 1:J)
  polygon(
    x_plot[j] + c(-w, -w, w, w),
    c(diff[j], 0, 0, diff[j]),
    col = ifelse(pvalue[j] < .05, "black", 0),
    lwd = .5
  )
dev.off()

# Plotting berlim exposed-sham p-values
pdf("berlim1b.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  c(0, max(x) + 1),
  c(0, 0.21),
  xlab = "Study",
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


## Look at sham data
x2 <- sum((y0 / se0) ^ 2)

pdf("berlim3a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y0 - se0, y0 + se0),
  xlab = "Study",
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

pdf("berlim3b.pdf", height = 3, width = 4.5)
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
ntot <- c(n0, n1)
nrem <- c(n0rem, n1rem)
data <- list(
  N = 2 * J,
  J = J,
  ntot = ntot,
  nrem = nrem,
  y = y,
  se = se,
  x = x,
  expt_id = expt_id,
  z = z
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

pdf("berlim4a.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from hierarchical model",
  cex.main = .9,
  xaxt='n',
    ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

## raw difference estimates from exposed and sham data
pdf("berlim4h.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Raw estimates of exposed minus sham data",
  cex.main = .9,
  xaxt='n',
    ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, diff, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), diff[j] + c(-1, 1) * diff_se[j])
}
dev.off()

# hierarchical model without pooling thetas
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

pdf("berlim4c.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from partially hierarchical model",
  cex.main = .9,
  xaxt='n',
    ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()


# non-hierarchical linear model
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

pdf("berlim4d.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from non-hierarchical model",
  cex.main = .9,
  xaxt='n',
   ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()


## Fit a binomial hierarchical model
fit_hier <-
  stan(
    "binomial-no-corr-hier.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_hier)

theta <- extract(fit_hier)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("berlim4i.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from binomial hierarchical model",
  cex.main = .9,
  xaxt='n',
  ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()


## Fit a (normal) non-measurement error model
fit_hier <-
  stan(
    "chickens-no-corr-hier-non-meas-err.stan",
    data = data,
    control = list(adapt_delta = 0.9),
    refresh = 0
  )
print(fit_hier)

theta <- extract(fit_hier)$theta
theta_hat <- apply(theta, 2, mean)
theta_se <- apply(theta, 2, sd)

pdf("berlim4j.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates from plain hierarchical model",
  cex.main = .9,
  xaxt='n',
  ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()

## Plot raw data
theta_hat <- logoddsratios
theta_se <- lo_ses

pdf("berlim4raw.pdf", height = 3, width = 4.5)
par(mar = c(3, 3, 2, 1),
    mgp = c(1.5, .5, 0),
    tck = -.01)
plot(
  range(x),
  range(y1 - se1, y1 + se1),
  xlab = "Study",
  ylab = "Estimated treatment effect",
  bty = "l",
  type = "n",
  main = "Estimates reported in paper",
  cex.main = .9,
  xaxt='n',
  ylim=c(-1.5,4.5)
)
abline(0, 0, col = "gray")
axis(1, at = c(1,4,8,12,16,20,24))
points(x, theta_hat, pch = 20)
for (j in 1:J) {
  lines(rep(x_plot[j], 2), theta_hat[j] + c(-1, 1) * theta_se[j])
}
dev.off()