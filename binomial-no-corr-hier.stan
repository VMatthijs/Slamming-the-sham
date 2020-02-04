data {
  int N;
  int J;
  int ntot[N];
  int nrem[N];
  vector[N] z;
  vector[J] x;
  int<lower=1, upper=J> expt_id[N];
}
parameters {
  real mu_theta;
  real mu_b;
  real<lower=0> sigma_theta;
  real<lower=0> sigma_b;
  vector[J] eta_theta;
  vector[J] eta_b;
}
transformed parameters {
  vector[J] theta;
  vector[J] b;
  theta = mu_theta + sigma_theta * eta_theta;
  b = mu_b + sigma_b * eta_b;
}
model {
  nrem ~ binomial_logit(ntot, b[expt_id] + theta[expt_id] .* z);
  eta_theta ~ normal(0, 1);
  eta_b ~ normal(0, 1);
}
