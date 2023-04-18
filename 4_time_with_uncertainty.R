# 4_time_with_uncertainty.R
# estimating total time in role with uncertainty
# Feb 2023
TeachingDemos::char2seed('colchester')

# key numbers
not_eligible = 12
invalid_email = 7
max_possible = 494 - invalid_email - not_eligible # maximum number of respondents
#
not_missing = filter(data, !is.na(q3_3))
n_responded = nrow(not_missing)

results = bootstrap_uncertainty(indata = data,
                      variable = 'q3_3',
                      n_boot = 1000,
                      max_possible = max_possible,
                      n_responded = n_responded)
