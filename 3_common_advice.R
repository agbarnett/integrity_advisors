# 3_common_advice.R
# convert amount of advice given to common scale, including uncertainty
# called by 3_read_qualtrics_api.R
# February 2023

# useful for checking results
original = data 

# response options, separate for students and staff:
# q4_1_number_s = None option for staff
# q4_1_number_p = None option for students
# q4_1_number_2_s = Total number - staff
# q4_1_number_2_p = Total number - students
# q4_1_number_3_s = Average per month - staff
# q4_1_number_3_p = Average per month - students
# q4_1_number_4_s = Average per year - staff
# q4_1_number_4_p = Average per year - students

## specific data edits
# none yet

## if people complete all three options, then take total variable
# staff
check_s = select(data, 'id', starts_with('q4_1_')) %>% select(ends_with('_s')) %>%
  mutate_all(replace_miss) # in 99_functions.R
multiple_s = rowSums(is.na(check_s)) < 2 # multiple times completed?
check_s[multiple_s,]
data$q4_1_number_3_s[multiple_s] = NA # blank months and years
data$q4_1_number_4_s[multiple_s] = NA
# students
check_p = select(data, 'id', starts_with('q4_1_')) %>% select(ends_with('_p')) %>%
  mutate_all(replace_miss) # in 99_functions.R
multiple_p = rowSums(is.na(check_p)) < 2 # multiple times completed?
check_p[multiple_p,]
data$q4_1_number_3_p[multiple_p] = NA # blank months and years
data$q4_1_number_4_p[multiple_p] = NA

# remove words from results
data = mutate(data,
              # total number
              q4_1_number_2_s = ifelse(q4_1_number_2_s == '<10', '9', q4_1_number_2_s),
              q4_1_number_2_s = str_replace_all(q4_1_number_2_s, '~', 'approx'), # use consistent wording
              #
              # years
              q4_1_number_4_s = ifelse(q4_1_number_4_s == '2-3 days', '2-3', q4_1_number_4_s), # hard to know how to edit this, assume 1 per day?
              q4_1_number_4_p = ifelse(q4_1_number_4_p == '2-3 days', '2-3', q4_1_number_4_p), # hard to know how to edit this, assume 1 per day?
              # - none yet
)

# now work on uncertainty 
data = mutate(data, 
              q4_1_uncertainty_s = str_detect(q4_1_number_2_s, '-|to|approx')|
                str_detect(q4_1_number_3_s, '-|to|approx')|
                str_detect(q4_1_number_4_s, '-|to|approx'),
              q4_1_uncertainty_p = str_detect(q4_1_number_2_p, '-|to|approx')|
                str_detect(q4_1_number_3_p, '-|to|approx')|
                str_detect(q4_1_number_4_p, '-|to|approx'),
              q4_1_number_2_s = str_remove(q4_1_number_2_s, 'approx'),
              q4_1_number_3_s = str_remove(q4_1_number_3_s, 'approx'),
              q4_1_number_4_s = str_remove(q4_1_number_4_s, 'approx'),
              q4_1_number_2_p = str_remove(q4_1_number_2_p, 'approx'),
              q4_1_number_3_p = str_remove(q4_1_number_3_p, 'approx'),
              q4_1_number_4_p = str_remove(q4_1_number_4_p, 'approx')) %>% # 
  separate(q4_1_number_2_s, into=c('q4_1_number_2_s_lower','q4_1_number_2_s_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q4_1_number_3_s, into=c('q4_1_number_3_s_lower','q4_1_number_3_s_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q4_1_number_4_s, into=c('q4_1_number_4_s_lower','q4_1_number_4_s_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q4_1_number_2_p, into=c('q4_1_number_2_p_lower','q4_1_number_2_p_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q4_1_number_3_p, into=c('q4_1_number_3_p_lower','q4_1_number_3_p_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q4_1_number_4_p, into=c('q4_1_number_4_p_lower','q4_1_number_4_p_upper'), sep ='to|-', remove=FALSE) %>% # can ignore warnings
  mutate(q4_1_uncertainty_s = ifelse(is.na(q4_1_uncertainty_s), FALSE, q4_1_uncertainty_s), # replace NA with false
         q4_1_uncertainty_p = ifelse(is.na(q4_1_uncertainty_p), FALSE, q4_1_uncertainty_p), # replace NA with false
         q4_1_number_2_s_lower = ifelse(q4_1_number_2_s_lower == q4_1_number_2_s, NA, q4_1_number_2_s_lower), # blank if lower limit just repeats mean
         q4_1_number_3_s_lower = ifelse(q4_1_number_3_s_lower == q4_1_number_3_s, NA, q4_1_number_3_s_lower), # blank if lower limit just repeats mean
         q4_1_number_4_s_lower = ifelse(q4_1_number_4_s_lower == q4_1_number_4_s, NA, q4_1_number_4_s_lower), # blank if lower limit just repeats mean
         q4_1_number_2_p_lower = ifelse(q4_1_number_2_p_lower == q4_1_number_2_p, NA, q4_1_number_2_p_lower), # blank if lower limit just repeats mean
         q4_1_number_3_p_lower = ifelse(q4_1_number_3_p_lower == q4_1_number_3_p, NA, q4_1_number_3_p_lower), # blank if lower limit just repeats mean
         q4_1_number_4_p_lower = ifelse(q4_1_number_4_p_lower == q4_1_number_4_p, NA, q4_1_number_4_p_lower), # blank if lower limit just repeats mean
         # convert to numbers (warnings are due to numbers with uncertainty)
         q4_1_number_2_s = as.numeric(q4_1_number_2_s),
         q4_1_number_3_s = as.numeric(q4_1_number_3_s),
         q4_1_number_4_s = as.numeric(q4_1_number_4_s),
         q4_1_number_2_p = as.numeric(q4_1_number_2_p),
         q4_1_number_3_p = as.numeric(q4_1_number_3_p),
         q4_1_number_4_p = as.numeric(q4_1_number_4_p),
         q4_1_number_2_s_lower = as.numeric(q4_1_number_2_s_lower),
         q4_1_number_2_s_upper = as.numeric(q4_1_number_2_s_upper),
         q4_1_number_3_s_lower = as.numeric(q4_1_number_3_s_lower),
         q4_1_number_3_s_upper = as.numeric(q4_1_number_3_s_upper),
         q4_1_number_4_s_lower = as.numeric(q4_1_number_4_s_lower),
         q4_1_number_4_s_upper = as.numeric(q4_1_number_4_s_upper),
         q4_1_number_2_p_lower = as.numeric(q4_1_number_2_p_lower),
         q4_1_number_2_p_upper = as.numeric(q4_1_number_2_p_upper),
         q4_1_number_3_p_lower = as.numeric(q4_1_number_3_p_lower),
         q4_1_number_3_p_upper = as.numeric(q4_1_number_3_p_upper),
         q4_1_number_4_p_lower = as.numeric(q4_1_number_4_p_lower),
         q4_1_number_4_p_upper = as.numeric(q4_1_number_4_p_upper),
         # use mean for central estimate if there's some uncertainty
         q4_1_number_2_s = ifelse(!is.na(q4_1_number_2_s_lower), round((q4_1_number_2_s_lower + q4_1_number_2_s_upper) /2), q4_1_number_2_s),
         q4_1_number_3_s = ifelse(!is.na(q4_1_number_3_s_lower), round((q4_1_number_3_s_lower + q4_1_number_3_s_upper) /2), q4_1_number_3_s),
         q4_1_number_4_s = ifelse(!is.na(q4_1_number_4_s_lower), round((q4_1_number_4_s_lower + q4_1_number_4_s_upper) /2), q4_1_number_4_s),
         q4_1_number_2_p = ifelse(!is.na(q4_1_number_2_p_lower), round((q4_1_number_2_p_lower + q4_1_number_2_p_upper) /2), q4_1_number_2_p),
         q4_1_number_3_p = ifelse(!is.na(q4_1_number_3_p_lower), round((q4_1_number_3_p_lower + q4_1_number_3_p_upper) /2), q4_1_number_3_p),
         q4_1_number_4_p = ifelse(!is.na(q4_1_number_4_p_lower), round((q4_1_number_4_p_lower + q4_1_number_4_p_upper) /2), q4_1_number_4_p))

## blank `none` if data in any of the three numbers columns
#
index = data$q4_1_number_s == 'None' & (!is.na(data$q4_1_number_2_s) | !is.na(data$q4_1_number_3_s) | !is.na(data$q4_1_number_4_s))
index[is.na(index)] = FALSE
data$q4_1_number_s[index] = ''
#
index = data$q4_1_number_p == 'None' & (!is.na(data$q4_1_number_2_p) | !is.na(data$q4_1_number_3_p) | !is.na(data$q4_1_number_4_p))
index[is.na(index)] = FALSE
data$q4_1_number_p[index] = ''

# record which response option they used
data = mutate(data,
              q4_1_option_s = case_when(
                q4_1_number_s == "None" ~ 'None',
                is.na(q4_1_number_2_s) == FALSE ~ 'Total number',
                is.na(q4_1_number_3_s) == FALSE ~ 'Average per month',
                is.na(q4_1_number_4_s) == FALSE ~ 'Average per year',
                TRUE ~ 'Missing'
                ),
              q4_1_option_p = case_when(
                q4_1_number_p == "None" ~ 'None',
                is.na(q4_1_number_2_p) == FALSE ~ 'Total number',
                is.na(q4_1_number_3_p) == FALSE ~ 'Average per month',
                is.na(q4_1_number_4_p) == FALSE ~ 'Average per year',
                TRUE ~ 'Missing'
                )
)


## now convert to common time of average per month
# a) none
none_s = filter(data, q4_1_option_s == 'None') %>%
  mutate(q4_1_s = 0,
         q4_1_s_lower = NA,
         q4_1_s_upper = NA)
none_p = filter(data, q4_1_option_p == 'None') %>%
  mutate(q4_1_p = 0,
         q4_1_p_lower = NA,
         q4_1_p_upper = NA)
# b) total, adjust to per month
total_s = filter(data, q4_1_option_s == 'Total number') %>%
  mutate(q4_1_s = q4_1_number_2_s / (years_in_role*12), # average per month
         q4_1_s_lower = q4_1_number_2_s_lower / (years_in_role*12),
         q4_1_s_upper = q4_1_number_2_s_upper / (years_in_role*12))
total_p = filter(data, q4_1_option_p == 'Total number') %>%
  mutate(q4_1_p = q4_1_number_2_p / (years_in_role*12), # average per month
         q4_1_p_lower = q4_1_number_2_p_lower / (years_in_role*12),
         q4_1_p_upper = q4_1_number_2_p_upper / (years_in_role*12))
# c) months, no conversion needed
months_s = filter(data, q4_1_option_s == 'Average per month') %>%
  mutate(q4_1_s = q4_1_number_3_s,
         q4_1_s_lower = q4_1_number_3_s_lower,
         q4_1_s_upper = q4_1_number_3_s_upper)
months_p = filter(data, q4_1_option_p == 'Average per month') %>%
  mutate(q4_1_p = q4_1_number_3_p,
         q4_1_p_lower = q4_1_number_3_p_lower,
         q4_1_p_upper = q4_1_number_3_p_upper)
# d) years -> month
years_s = filter(data, q4_1_option_s == 'Average per year') %>%
  mutate(q4_1_s = q4_1_number_4_s /12,
         q4_1_s_lower = q4_1_number_4_s_lower/12,
         q4_1_s_upper = q4_1_number_4_s_upper/12)
years_p = filter(data, q4_1_option_p == 'Average per year') %>%
  mutate(q4_1_p = q4_1_number_4_p /12,
         q4_1_p_lower = q4_1_number_4_p_lower/12,
         q4_1_p_upper = q4_1_number_4_p_upper/12)
#
missing_s = filter(data, q4_1_option_s == 'Missing') %>%
  mutate(q4_1_s = NA,
         q4_1_s_lower = NA,
         q4_1_s_upper = NA)
missing_p = filter(data, q4_1_option_p == 'Missing') %>%
  mutate(q4_1_p = NA,
         q4_1_p_lower = NA,
         q4_1_p_upper = NA)
data = bind_rows(none_s, total_s, months_s, years_s, missing_s)
students = bind_rows(none_p, total_p, months_p, years_p, missing_p) %>%
  select(id, q4_1_p, q4_1_p_lower, q4_1_p_upper)
data = left_join(data, students, by='id')

# few checks
select(data, starts_with('q4_1'))
uncertain = select(data, 'id', starts_with('q4_1')) %>% select(contains('_s')) %>% filter(q4_1_uncertainty_s == TRUE)
#compare = select(original, 'id', starts_with('q4_1')) %>% filter(id == 'National.120')
years = filter(data, q4_1_option_s == "Average per year") %>%
  select('id', 
         'q4_1_number_4_s', 'q4_1_number_4_s_lower', 'q4_1_number_4_s_upper',
         'q4_1_s','q4_1_s_lower','q4_1_s_upper') 
total = filter(data, q4_1_option_s == "Total") %>%
  select('id', 'years_in_role',
         'q4_1_number_2_s', 'q4_1_number_2_s_lower', 'q4_1_number_2_s_upper',
         'q4_1_s','q4_1_s_lower','q4_1_s_upper') 

# can drop original variables
data  = select(data, -starts_with('q4_1_number'))

## make "any advice" variable
data = mutate(data, 
              any_advice = !( (q4_1_s == 0|is.na(q4_1_s)) & (q4_1_p == 0|is.na(q4_1_p))),
              any_advice = ifelse(is.na(any_advice)==TRUE, TRUE, any_advice))
