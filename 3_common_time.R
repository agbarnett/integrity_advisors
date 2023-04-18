# 3_common_time.R
# convert amount of time in role to common scale, including uncertainty
# called by 3_read_qualtrics_api.R
# February 2023
# three time options:
# - q3_3_4 = total days
# - q3_3_5 = per month
# - q3_3_6 = FTE

## useful for later checks
original = data

## specific data edits
source('3_data_edits_time.R')

## if people complete all three time options, then take days variable (q3_3_4) if it is not missing
check = select(data, 'id', starts_with('q3_3_'))
check_small = select(check, 'id', starts_with('q3_3_')) %>%
  mutate_all(replace_miss) # in 99_functions.R
multiple = rowSums(is.na(check_small)) < 2 # multiple times completed?
check[multiple,]
# blank additional information in months and FTE
data$q3_3_5[multiple] = NA
data$q3_3_6[multiple] = NA

# remove words from results (see also 3_data_edits_time.R)
data = mutate(data,
              # total number
              q3_3_4 = str_remove(q3_3_4, pattern=' days?'),
              q3_3_4 = str_remove_all(q3_3_4, pattern='<'), # cannot cope with less than
              q3_3_4 = str_replace_all(q3_3_4, pattern='~', replacement = 'approx '), # consistent language
              q3_3_4 = str_squish(q3_3_4),
              # days per month
              q3_3_5 = str_remove_all(q3_3_5, pattern='<'), # cannot cope with less than
              q3_3_5 = str_replace_all(q3_3_5, pattern='~', replacement = 'approx '), # consistent language
              q3_3_5 = str_squish(q3_3_5),
              # FTE
              q3_3_6 = str_replace_all(q3_3_6, pattern='~', replacement = 'approx '), # consistent language
              q3_3_6 = str_squish(q3_3_6),
              # record which response option they used
              q3_3_option = case_when(
                is.na(q3_3_4) == FALSE ~ 'Days',
                is.na(q3_3_5) == FALSE ~ 'Average per month',
                is.na(q3_3_6) == FALSE ~ 'FTE',
                TRUE ~ 'Missing'
              )
)

# now work on uncertainty 
data = mutate(data, 
              q3_3_uncertainty = str_detect(q3_3_4, '-|to|approx')|
                                 str_detect(q3_3_5, '-|to|approx')|
                                 str_detect(q3_3_6, '-|to|approx'),
              q3_3_4 = str_remove(q3_3_4, 'approx'),
              q3_3_5 = str_remove(q3_3_5, 'approx'),
              q3_3_6 = str_remove(q3_3_6, 'approx')) %>% # 
  separate(q3_3_4, into=c('q3_3_4_lower','q3_3_4_upper'), sep ='to|-', remove=FALSE) %>% # can ignore warnings
  separate(q3_3_5, into=c('q3_3_5_lower','q3_3_5_upper'), sep ='to|-', remove=FALSE) %>%
  separate(q3_3_6, into=c('q3_3_6_lower','q3_3_6_upper'), sep ='to|-', remove=FALSE) %>%
  mutate(q3_3_uncertainty = ifelse(is.na(q3_3_uncertainty), FALSE, q3_3_uncertainty), # replace NA with false
         q3_3_4_lower = ifelse(q3_3_4_lower == q3_3_4, NA, q3_3_4_lower), # blank if lower limit just repeats mean
         q3_3_5_lower = ifelse(q3_3_5_lower == q3_3_5, NA, q3_3_5_lower), # blank if lower limit just repeats mean
         q3_3_6_lower = ifelse(q3_3_6_lower == q3_3_6, NA, q3_3_6_lower), # blank if lower limit just repeats mean
         # convert to numbers
         q3_3_4 = as.numeric(q3_3_4), # some errors because of uncertainty, but fixed just below by averaging
         q3_3_5 = as.numeric(q3_3_5),
         q3_3_6 = as.numeric(q3_3_6),
         q3_3_4_lower = as.numeric(q3_3_4_lower),
         q3_3_4_upper = as.numeric(q3_3_4_upper),
         q3_3_5_lower = as.numeric(q3_3_5_lower),
         q3_3_5_upper = as.numeric(q3_3_5_upper),
         q3_3_6_lower = as.numeric(q3_3_6_lower),
         q3_3_6_upper = as.numeric(q3_3_6_upper),
         # use mean for central estimate if there's some uncertainty
         q3_3_4 = ifelse(!is.na(q3_3_4_lower), round((q3_3_4_lower + q3_3_4_upper) /2), q3_3_4),
         q3_3_5 = ifelse(!is.na(q3_3_5_lower), round((q3_3_5_lower + q3_3_5_upper) /2), q3_3_5),
         q3_3_6 = ifelse(!is.na(q3_3_6_lower), round((q3_3_6_lower + q3_3_6_upper) /2), q3_3_6))
# few checks
select(data, starts_with('q3_3'))
uncertain = select(data, 'id', starts_with('q3_3')) %>% filter(q3_3_uncertainty == TRUE)
compare = select(original, 'id', starts_with('q3_3')) %>% filter(id == 'National.120')
# National.79, said 'approx' but did not give bounds

## now convert to common time of average per month
# a) days, convert to days per month
days = filter(data, q3_3_option == 'Days') %>%
  mutate(q3_3 = q3_3_4 / (years_in_role*12), # average per month
         q3_3_lower = q3_3_4_lower / (years_in_role*12),
         q3_3_upper = q3_3_4_upper / (years_in_role*12))
#View(select(days, 'q3_3', 'q3_3_4', 'years_in_role'))
# b) months, no conversion needed
months = filter(data, q3_3_option == 'Average per month') %>%
  mutate(q3_3 = q3_3_5,
         q3_3_lower = q3_3_5_lower,
         q3_3_upper = q3_3_5_upper)
# c) FTE
working_days_in_month = 48*5 / 12 # number of working days in the month, adjust for holidays, so times by 48 weeks per year, not 52
fte = filter(data, q3_3_option == 'FTE') %>%
  mutate(q3_3 = q3_3_6*working_days_in_month,
         q3_3_lower = q3_3_6_lower*working_days_in_month,
         q3_3_upper = q3_3_6_upper*working_days_in_month)
#
none = filter(data, q3_3_option == 'Missing') %>%
  mutate(q3_3 = NA,
         q3_3_lower = NA,
         q3_3_upper = NA)
data = bind_rows(days, months, fte, none)

## check a few results
# max and min
check.max = filter(data, !is.na(q3_3)) %>% filter(q3_3 == max(data$q3_3, na.rm=TRUE)) %>%
  select(id, starts_with('q3_3'))
str(check.max)
check.min = filter(data, !is.na(q3_3)) %>% filter(q3_3 == min(data$q3_3, na.rm=TRUE)) %>%
  select(id, starts_with('q3_3'))
str(check.min)
# days
check.days = filter(data, !is.na(q3_3)) %>% filter(q3_3_option == 'Days') %>%
  sample_n(1) %>%
  select(id, starts_with('q3_3') , 'years_in_role')
str(check.days )
# fte
check.fte = filter(data, !is.na(q3_3)) %>% filter(q3_3_option == 'FTE') %>%
  sample_n(1) %>%
  select(id, starts_with('q3_3'))
str(check.fte)

# can drop original variables
data  = select(data, -'q3_3_4', -'q3_3_4_lower', -'q3_3_4_upper',
               -'q3_3_5', -'q3_3_5_lower', -'q3_3_5_upper',
               -'q3_3_6', -'q3_3_6_lower', -'q3_3_6_upper')

# check missing
m = filter(data, is.na(q3_3)) %>% select('id')
miss = left_join(m, original, by='id') %>%
  select('id', 'years_in_role', starts_with('q3_3'))

# no longer needed
#remove(original)