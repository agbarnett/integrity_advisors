# 4_training_with_uncertainty.R
# estimating amount of training with uncertainty
# March 2023
TeachingDemos::char2seed('colchester')

# key numbers
not_eligible = 12
invalid_email = 7
max_possible = 494 - invalid_email - not_eligible # maximum number of respondents

# a) students
students = bootstrap_uncertainty(indata = data,
                                variable = 'q4_1_p',
                                
                      n_boot = 1000,
                      max_possible = max_possible,
                      n_responded = n_responded)

# b) staff
staff = bootstrap_uncertainty(indata = data,
                                variable = 'q4_1_s',
                      n_boot = 1000,
                      max_possible = max_possible,
                      n_responded = n_responded)
