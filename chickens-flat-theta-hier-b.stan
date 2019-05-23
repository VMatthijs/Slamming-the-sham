data {
  int N;
  int J;
  vector[N] y;
  vector[N] z;
  vector[N] se;
  vector[N] df;
  vector[J] x;
  int<lower=1, upper=J> expt_id[N];
}
parameters {
  vector[J] theta;
  real mu_b;
  real<lower=0> sigma_b;
  vector[J] eta_b;
}
transformed parameters {
  vector[J] b;
  b = mu_b + sigma_b * eta_b;
}
model {
  y ~ student_t(df,
                b[expt_id] + theta[expt_id] .* z,
                se);
  eta_b ~ normal(0, 1);
}
