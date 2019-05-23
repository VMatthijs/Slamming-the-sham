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
  real mu_theta;
  real mu_b;
  real<lower=0> sigma_theta;
  real<lower=0> sigma_b;
  matrix[J,2] eta;
  corr_matrix[2] Sigma_eta;
}
transformed parameters {
  vector[J] theta;
  vector[J] b;
  theta = mu_theta + sigma_theta * eta[,1];
  b = mu_b + sigma_b * eta[,2];
}
model {
  y ~ student_t(df,
                b[expt_id] + theta[expt_id] .* z,
                se);
  for (j in 1:J){
    eta[j,] ~ multi_normal(rep_vector(0, 2), Sigma_eta);
  }
}
