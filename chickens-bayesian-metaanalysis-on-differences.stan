data {
  int J;
  vector[J] diff;
  vector[J] diff_se;
  vector[J] x;
}
parameters {
  real mu_theta;
  real<lower=0> sigma_theta;
  vector[J] eta_theta;
}
transformed parameters {
  vector[J] theta;
  theta = mu_theta + sigma_theta * eta_theta;
}
model {
  diff ~ normal(theta, diff_se);
  eta_theta ~ normal(0, 1);
}
