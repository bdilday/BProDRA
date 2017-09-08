
data {
  int<lower=2> K; # classes
  int<lower=0> N; # data points
  int<lower=1> D; # number of predictors / random effects
  real<lower=0> RANEF_SIGMA[K, D]; # sd of random effects
  int<lower=0> LEVELS[D]; # number of levels of each predictor
  int<lower=0> SUM_LEVELS;
  int<lower=0> MAX_LEVEL;
  int<lower=1, upper=K> y[N];
  int<lower=1, upper=MAX_LEVEL> x[N, D];
}

transformed data {
  int CU_LEVELS[2, D];
  CU_LEVELS[1, 1] = 1;
  CU_LEVELS[2, 1] = LEVELS[1];
  for (d in 2:D) {
    CU_LEVELS[1, d] = CU_LEVELS[1, d-1] + LEVELS[d-1];
    CU_LEVELS[2, d] = CU_LEVELS[2, d-1] + LEVELS[d];
  }
}

parameters {
  real ALPHAX[K-1, SUM_LEVELS];
  real CX[K-1];
}

transformed parameters {
  real C[K];
  real ALPHA[K, SUM_LEVELS];

  for (k in 1:(K-1)) {
    C[k] = CX[k];
  }
    C[K] = 0.0;

  for (i in 1:SUM_LEVELS) {
   for (k in 1:(K-1)) {
       ALPHA[k, i] = ALPHAX[k, i];
     }
     ALPHA[K, i] = 0.0;
   }

}

model {
  vector[K] lambda[N];

  for (k in 1:K) {
    C[k] ~ normal(0, 10);
  }

  for (i in 1:SUM_LEVELS) {
    for (k in 1:K) {
      for (d in 1:D) {
        if (i >= CU_LEVELS[1,d] && i <= CU_LEVELS[2,d]) {
            ALPHA[k, i] ~ normal(0, RANEF_SIGMA[k, d]);
          }
      }
    }
  }

   for (n in 1:N) {
     for (k in 1:K) {
       lambda[n][k] = C[k];
       for (d in 1:D) {
         lambda[n][k] = lambda[n][k] + ALPHA[k, x[n][d]  ];
       }
     }
     }

 for (n in 1:N) {
  y[n] ~ categorical_logit(lambda[n]);
 }

}
