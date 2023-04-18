# 0_estimate_census_size.R 
# estimate size of census for RIA (used in ethics application)
# November 2022
TeachingDemos::char2seed('arsenal')

# number of institutions from 0_read_institutions.R
n_institutes = 86 # number of unis/institutes with recent NHMRC application(s) - see protocol
n_sim = 1000

est = upper = NULL
for (k in 1:n_sim){
  ind = 1+rpois(lambda=2, n=n_institutes)
  upper = c(upper, max(ind)) # highest number
  total = sum(ind) # add up random Poisson over institutes
  est = c(est, total)
}
quantile(est, probs=c(0.1,0.5,0.9)) # 80% interval
quantile(est, probs=c(0.05,0.5,0.95)) # 90% interval
summary(upper)
