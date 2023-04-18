# 4_latent_models_training.R
# latent variable models to test effect of randomising the question order
# called by 4_census_results.Rmd
# outcome = amount of training 
# Feb 2023
library(nimble)

## data
for_model = filter(data, !is.na(q3_2_do_1)) %>% # must have seen question
  mutate(outcome = case_when( # make ordinal dependent variable
    q3_2 == "Too little" ~ 1,
    q3_2 == "About right" ~ 2,
    q3_2 == "Too much" ~ 3
  ))
# outcome needs to be a binary matrix
outcome_matrix = matrix(data = 0, ncol=3, nrow = nrow(for_model))
for (k in 1:3){
  outcome_matrix[,k] = as.numeric(for_model$outcome == k)
}
bdata = list(outcome = outcome_matrix ) # needs to be a matrix
constants = list(N = nrow(for_model),
                 random = for_model$q3_2_do_1) # randomised order

# Latent variable model
code = nimbleCode({
  for (i in 1:N){
    outcome[i,1:3] ~ dmulti(pi[i,1:3],1);
    # Cumulative probability of > category k given cut-point
    for (k in 1:2){
      logit(Q[i,k]) <- intercept + order*random[i] - C[k];
    }
    # Calculate probabilities
    pi[i,1] <- 1 - Q[i,1]; # Pr(cat=1) = 1 - Pr(cat>1);
    pi[i,2] <- Q[i,1] - Q[i,2]; # Pr(cat>k-1) - Pr(cat>k);
    pi[i,3] <- Q[i,2]; # Pr(cat=k) = Pr(cat>k-1);
  }
  # priors
  intercept ~ dnorm(0, sd=10000)
  order ~ dnorm(0, sd=10000)
  # ordered cut-offs
  C[1] <- 0; # for identifiability
  C[2] ~ dunif(C[1],10);
  # predicted probabilities over 24 hours, not the weekend and version set to reference
  for(x in 1:2){
    for(k in 1:2){
      logit(Q.pred[x,k]) <- intercept + (order*(x-1)) - C[k];
    }
    # Calculate probabilities
    pi.pred[x,1] <- 1 - Q.pred[x,1]; # Pr(cat=1) = 1 - Pr(cat>1);
    pi.pred[x,2] <- Q.pred[x,1] - Q.pred[x,2]; # Pr(cat>k-1) - Pr(cat>k);
    pi.pred[x,3] <- Q.pred[x,2]; # Pr(cat=k) = Pr(cat>k-1);
  }
})

## initial values
inits <- list(intercept = 0, order = 0, C = c(NA, 1))

# parameters to store
parms = c('intercept', 'order', 'C', 'pi.pred')

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

## save results
save(to_table, file = 'results/4_random_latent_training.RData')
