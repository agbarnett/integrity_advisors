# 4_help_with_uncertainty.R
# estimating amount of help with uncertainty
# March 2023
TeachingDemos::char2seed('doncaster')

# key numbers
not_eligible = 12
invalid_email = 7
max_possible = 494 - invalid_email - not_eligible # maximum number of respondents

# a) initial
initial_not_missing = filter(data, !is.na(q3_1_1)) %>%
  pull(q3_1_1)
N = length(initial_not_missing)
boot_res = NULL
for (b in 1:1000){
  sample = rep(NA, max_possible)
  sample[1:N] = initial_not_missing
  sample[(N+1):max_possible] = sample(initial_not_missing, replace=TRUE, size = max_possible - N)
  total_yes = sum(sample=='Yes')
  frame = data.frame(boot = b, total = total_yes)
  boot_res = bind_rows(boot_res, frame)
}
lower = 100 * as.numeric(quantile(boot_res$total, 0.025)/max_possible)
upper = 100 * as.numeric(quantile(boot_res$total, 1 - 0.025)/max_possible)
initial = data.frame(lower = lower, upper = upper)

# b) ongoing
ongoing_not_missing = filter(data, !is.na(q3_1_2)) %>%
  pull(q3_1_2)
N = length(ongoing_not_missing)
boot_res = NULL
for (b in 1:1000){
  sample = rep(NA, max_possible)
  sample[1:N] = ongoing_not_missing
  sample[(N+1):max_possible] = sample(ongoing_not_missing, replace=TRUE, size = max_possible - N)
  total_yes = sum(sample=='Yes')
  frame = data.frame(boot = b, total = total_yes)
  boot_res = bind_rows(boot_res, frame)
}
lower = 100 * as.numeric(quantile(boot_res$total, 0.025)/max_possible)
upper = 100 * as.numeric(quantile(boot_res$total, 1 - 0.025)/max_possible)
ongoing = data.frame(lower = lower, upper = upper)
