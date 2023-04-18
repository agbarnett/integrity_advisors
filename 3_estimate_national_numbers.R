# 3_estimate_national_numbers.R
# estimate the national number of advisors and the uncertainty
# Feb 2023
library(dplyr)
library(readxl)

## start with data from the sheets
contacts = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 'contact_details') 
institutes = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 'all_institutes') 

## get the known advisor numbers per institute
known = filter(contacts,
         !is.na(inst_ria), # only those with a number
         inst != 'Dummy university') # remove one dummy row
numbers = group_by(known, inst) %>%
  tally() %>%
  ungroup()

## get the number of institutes with an unknown number of advisors
unknown = filter(contacts,
                 is.na(inst_ria)) %>%
  select(inst) %>%
  unique()
# remove any not applicable (do not need advisers)
not_needed = filter(institutes, rias_shared == 'N/A') %>%
  select(inst)
index = !unknown$inst %in% not_needed$inst
unknown = unknown[index,] # only keep those that we think need RIAs

## create distribution of possible numbers
dist_possible = group_by(numbers, n) %>%
  tally(name = 'N') %>%
  rename('advisors' = 'n')
# add zeros
zeros = data.frame(advisors = 0, N = nrow(not_needed))
dist_possible = bind_rows(zeros, dist_possible) %>%
  mutate(prob = N / sum(N))

## now bootstrap
TeachingDemos::char2seed('harrogate')
N_boot = 1000
all_boot = NULL
for (b in 1:N_boot){
  rsample = sample(dist_possible$advisors, size = nrow(unknown), replace = TRUE, prob = dist_possible$prob) # random sample of RIA numbers for unknown institutions
  this_un = mutate(unknown, n = rsample) # add unknown numbers
  boot = bind_rows(numbers, this_un) # combine known and estimated
  frame = data.frame(boot = b, total = sum(boot$n))
  # option: could prevent those that cannot be zero? but can never be totally sure that jobs is not being outsourced
  all_boot = bind_rows(all_boot, frame)
}
summary(all_boot$total)

# save the results
unknown = nrow(unknown) # add the total numbers known and unknown
known = nrow(numbers)
save(numbers, unknown, known, all_boot, file = 'results/3_estimate_national.RData')
