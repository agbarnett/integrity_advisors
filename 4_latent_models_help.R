# 4_latent_models_help.R
# latent variable models to test effect of randomising the question order
# called by 4_census_results.Rmd
# outcome = amount of training 
# Feb 2023
library(nimble)

## data
for_model = filter(data, 
                   !is.na(q4_3_do_1), # must have seen question
                   !is.na(q4_3), # outcome cannot be missing
                   q4_3 != 'Unsure')  %>%  # cannot deal with `unsure` on ordinal scale
  mutate(outcome = case_when( # make ordinal dependent variable
    q4_3 == "None of the time" ~ 1,
    q4_3 == "Some of the time" ~ 2,
    q4_3 == "About half the time" ~ 3,
    q4_3 == "Most of the time" ~ 4,
    q4_3 == "All of the time" ~ 5
  ))

# outcome needs to be a binary matrix
M = 5 # outcome levels
outcome_matrix = matrix(data = 0, ncol = M, nrow = nrow(for_model))
for (k in 1:M){
  outcome_matrix[,k] = as.numeric(for_model$outcome == k)
}
bdata = list(outcome = outcome_matrix ) # needs to be a matrix
constants = list(N = nrow(for_model),
                 M = M,
                 random = for_model$q4_3_do_1) # randomised order

# Latent variable model
code = nimbleCode({
  for (i in 1:N){
    outcome[i,1:M] ~ dmulti(pi[i,1:M],1);
    # Cumulative probability of > category k given cut-point
    for (k in 1:(M-1)){
      logit(Q[i,k]) <- intercept + order*random[i] - C[k];
    }
    # Calculate probabilities
    pi[i,1] <- 1 - Q[i,1]; # Pr(cat=1) = 1 - Pr(cat>1);
    pi[i,2] <- Q[i,1] - Q[i,2]; # Pr(cat>k-1) - Pr(cat>k);
    pi[i,3] <- Q[i,2] - Q[i,3]; # Pr(cat>k-1) - Pr(cat>k);
    pi[i,4] <- Q[i,3] - Q[i,4]; # Pr(cat>k-1) - Pr(cat>k);
    pi[i,5] <- Q[i,4]; # Pr(cat=k) = Pr(cat>k-1);
  }
  # priors
  intercept ~ dnorm(0, sd=10000)
  order ~ dnorm(0, sd=10000)
  # ordered cut-offs
  C[1] <- 0; # for identifiability
  C[2] <- C[1] + R[1] # add small random constant
  C[3] <- C[2] + R[2]
  C[4] <- C[2] + R[3]
  for(k in 1:3){R[k] ~ dexp(1)}
})

## initial values
R = rep(0.1, M-2)
inits <- list(intercept = 0, order = 0, R = R)

# parameters to store
parms = c('intercept', 'order', 'C')

# models
model <- nimbleModel(code = code, 
                     data = bdata, 
                     inits = inits, 
                     constants = constants)

# MCMC samples
source('99_mcmc.R')
mcmc <- nimbleMCMC(model = model,
                   inits = inits,
                   monitors = parms,
                   niter = MCMC*2*thin, # times 2 for burn-in 
                   thin = thin,
                   nchains = n.chains, 
                   nburnin = MCMC,
                   summary = TRUE, 
                   setSeed = seed,
                   WAIC = FALSE)

# get table of summary estimates
to_table = data.frame(mcmc$summary$all.chains) %>%
  tibble::rownames_to_column() %>%
  janitor::clean_names() %>%
  filter(rowname != 'C[1]') %>%
  rename('lower' = 'x95_ci_low',
         'upper' = 'x95_ci_upp') %>%
  select(-median, -st_dev)

# check chains
plot(mcmc$samples$chain1[,6], type='l')

## save results
save(to_table, file = 'results/4_random_latent_help.RData')
