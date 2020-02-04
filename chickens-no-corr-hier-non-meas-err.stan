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
  vector[J] y1;
  vector[J] se1;
  for (i in 1:J) {
    y1[i] = y[i + J] - y[i];
    se1[i] = sqrt(square(se[i + J]) + square(se[i]));
  }
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
  y1 ~ normal(theta, se1);
  eta_theta ~ normal(0, 1);
}
