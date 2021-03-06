data {
  int N;
  int J;
  vector[N] y;
  vector[N] z;
  vector[N] se;
  vector[J] x;
  int<lower=1, upper=J> expt_id[N];
}
parameters {
  vector[J] theta;
  vector[J] b;
}
model {
  y ~ normal(b[expt_id] + theta[expt_id] .* z, se);
}
