functions {
  matrix gp_periodic_cov(real[] x, real alpha, real rho, real period) {
    int L = size(x);
    matrix[L, L] x_diff;
    for (i in 1:L)
      for (j in 1:L)
        x_diff[i, j] = fabs(x[i] - x[j]);
    return square(alpha) * exp(-2 * square(sin(pi() * x_diff / period)) / square(rho));
  }
}
data {
  int N;
  int J;
  vector[N] y;
  vector[N] z;
  vector[N] se;
  vector[J] x;
  int<lower=1, upper=J> expt_id[N];
}
transformed data {
  real delta = 1e-9;
  real x_array[J] = to_array_1d(x);
}
parameters {
  real mu_theta;
  real mu_b;
  real<lower=0> sigma_b;
  vector[J] eta_theta;
  vector[J] eta_b;
  real<lower=0> rho_theta;
  real<lower=0> alpha_theta;
  real<lower=0> eta_period;
}
transformed parameters {
  vector[J] theta;
  vector[J] b;
  real<lower=0> period = 30 + eta_period * 0.25;
  b = mu_b + sigma_b * eta_b;
  {
    matrix[J, J] L_K_theta;
    matrix[J, J] K_theta =
      gp_periodic_cov(x_array, alpha_theta, rho_theta, period);
    // diagonal elements
    for (j in 1:J)
      K_theta[j, j] = K_theta[j, j] + delta;
    L_K_theta = cholesky_decompose(K_theta);
    theta = mu_theta + L_K_theta * eta_theta;
  }
}
model {
  y ~ normal(b[expt_id] + theta[expt_id] .* z, se);
  eta_theta ~ normal(0, 1);
  eta_b ~ normal(0, 1);
  rho_theta ~ inv_gamma(5, 0.05);
  eta_period ~ normal(0, 1);
}
