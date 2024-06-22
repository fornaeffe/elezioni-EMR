data {
  int<lower=0> L; // Numero di liste
  int<lower=0> NR; // Numero di differenze regionali
  array[NR] real diffR; // Differenze tra logit percentuali regionali
  array[NR] real tempo; // Intervalli di tempo per ogni differenza regionale
  array[NR] int<lower=1,upper=L> lista; // Lista per ogni differenza regionale
  array[L] real lPR; // Logit delle percentuali regionali alle ultime elezioni
  real tempo_prox; // Intervallo di tempo tra l'ultima e la prossima elezione
}

parameters {
  array[L] real<lower=0> sigma;
  array[L] real<lower=0> v;
}

model {
  for (i in 1:NR) {
    diffR[i] ~ normal(0, sigma[lista[i]] + tempo[i] * v[lista[i]]);
  }
  
  // Priors
  sigma ~ normal(0, 5);
  v ~ normal(0, 5);
  
}

generated quantities {
  array[L] real lPR_new;

  for (l in 1:L) {
    lPR_new[l] = lPR[l] + normal_rng(0, sigma[l] + tempo_prox * v[l]);
  }

}

