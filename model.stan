data {
    int<lower=1> L; // Numero di liste
    int<lower=1> E; // Numero di elezioni
    int<lower=1> P; // Numero di province

    // 1 = la lista è presente alle elezioni
    // 2 = la lista non si è presentata alle elezioni
    array[E, L] int<lower=0, upper=1> presente;

    // Voti ricevuti da ciascuna lista in ciascuna provincia in ciascuna 
    // elezione
    array[E, P, L] int<lower=0> voti; 

    // Prior parameters

    // Varianza del fattore che influenza la probabilità di passaggio
    // tra un'elezione e l'altra
    real sigma2_e;
    
    real mu_stesso;
    real sigma2_stesso;
    real mu_diverso;
    real sigma2_diverso;
}

transformed data {
   array[E, P] int elettori;

   // Calcolo il numero di aventi diritto al voto ad ogni elezione
   for (e in 1:E) {
        for (p in 1:P) {
            elettori[e, p] = sum(voti[e, p]);
        }
   }

   // Conversione dei parametri prior
   real alpha_stesso = mu_stesso^2 / sigma2_stesso;
   real beta_stesso = mu_stesso / sigma2_stesso;
   real alpha_diverso = mu_diverso^2 / sigma2_diverso;
   real beta_diverso = mu_diverso / sigma2_diverso;
}

parameters {
    // Per ogni provincia ed ogni elezione,
    // proporzione di persone vicine a ciascuna lista
    array[P] simplex[L] frazione_base;
    array[E - 1, P, L] simplex[L] passaggi;
    array[E] matrix<lower=0>[L, L] matrice_passaggi;
    matrix<lower=0>[L, L] matrice_base;
}

transformed parameters {
   array[E, P] simplex[L] frazione;
    
   // Calcolo la frazione per ciascuna elezione partendo dalla prima
   // e calcolandola come somma dei passaggi di elettori dalle liste
   // dell'elezione precedente
   frazione[1] = frazione_base;
   for (e in 2:E) {
        for (p in 1:P) {
            for (l in 1:L) {
                frazione[e, p, l] = 0;

                for (l2 in 1:L) {
                    frazione[e, p, l] += 
                        frazione[e-1, p, l2] * passaggi[e-1, p, l2, l];
                }
            }
        }
        
   }
}

model {
    // I voti devono rispecchiare le frazioni di elettori.
    // Evito di considerare le liste che non si presentano,
    // ed evito di considerare l'astensione visto che comprenderà
    // sia la frazione legata all'astensione sia la frazione
    // legata alle liste che non si presentano.
    // TODO: esplicitare questa somma
    // TODO2: ampliare questo concetto alle liste che si presentano insieme.
    for (e in 1:E) {
        for (p in 1:P) {
            for (l in 1:(L-1)) {
                if (presente[e, l] == 1) {
                    voti[e, p, l] ~ binomial(elettori[e, p], frazione[e, p, l]);
                }
            }
        }
    }

    for (e in 1:(E-1)) {
        for (p in 1:P) {
            for (l in 1:L) {
                passaggi[e, p, l] ~ dirichlet(matrice_passaggi[e, ,l]);
            }
        }

        for (l in 1:L) {
            for (l2 in 1:L) {
                real alpha = matrice_base[l2, l]^2 / sigma2_e;
                real beta = matrice_base[l2, l] / sigma2_e;

                matrice_passaggi[e, l2, l] ~ gamma(alpha, beta);
            }
        }
    }

    for (l in 1:L) {
        for (l2 in 1:L) {
            if (l == l2) {
                matrice_base[l2, l] ~ gamma(alpha_stesso, beta_stesso);
            } else {
                matrice_base[l2, l] ~ gamma(alpha_diverso, beta_diverso);
            }
        }
    }

}

