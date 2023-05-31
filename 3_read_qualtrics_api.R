# 3_read_qualtrics_api.R
# read the latest survey data from Qualtrics using the API
# February 2023
library(tidyr)
library(qualtRics)
library(dplyr)
library(stringr)
library(janitor)
library(readxl)
library(lubridate) # for time zone
source('99_functions.R')

### Part 1: ordering and labels ### 

# read in variable ordering used for categorical variables (spreadsheet made by hand)
ordering = read.csv('data/done_ordering.csv') %>%
  arrange(question, order)

# get the variable names; from a qualtrics export to Excel with no data
names = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, n_max = 1) %>% 
  clean_names()
names = janitor::make_clean_names(names)  
# ... then get the labels
in_labels = read_excel('data/qualtrics_labels.xlsx', col_names = FALSE, skip=1, n_max = 1)
labs = as.character(matrix(in_labels))
# make a frame of labels
labels = bind_cols(names=names, labels=labs) %>%
  filter(!str_detect(names, 'end_date'), # drop variables that are not needed, some are qualtrics variables
         !str_detect(names, 'response_id'),
         !str_detect(names, 'user_language'),
         !str_detect(names, 'recipient'),
         !str_detect(names, 'policy'),
         !str_detect(names, 'sentiment')) 

# labels for generated variables
new_labels = read.table(header=TRUE,sep=',', text='
names,labels
duration_in_mins,Duration (in minutes)
progress_cat,Progress (%)
yrmon_start,Date started as RIA
email_provided,Email provided for focus group
years_in_role,Time as RIA (years)
q3_3,Time spent in role (average days per month)
q3_3_option,Time spent in role - answer option
q3_3_uncertainty,Time spent in role - uncertainty
q4_1_s,How many times have you provided advice for staff (average number per month)
q4_1_p,How many times have you provided advice for students (average number per month)
q4_1_option_s,How many times have you provided advice for staff - answer option
q4_1_option_p,How many times have you provided advice for students - answer option
q4_1_uncertainty_s,How many times have you provided advice for staff - uncertainty
q4_1_uncertainty_s,How many times have you provided advice for students - uncertainty
')
# just need one label
labels = bind_rows(labels, new_labels) %>% 
  unique() 

### Part 2: data ###
## set up API access
source('0_my_key.R')
set_up = function(){ # no longer needed?
  qualtrics_api_credentials(api_key = key, 
                            base_url = base_url,
                            overwrite = TRUE,
                            install = TRUE)
  readRenviron("~/.Renviron") # reload your environment 
}

## list all available surveys
surveys = all_surveys() %>%
  filter(str_detect(name, pattern='^Research integrity')) # just the RIA surveys
# Loop through multiple surveys (QIMR, Sydney, etc)
raw = NULL
for (k in 1:nrow(surveys)){
  ## get which version of the questionnaire it is
  qsource = str_remove_all(surveys$name[k], "Research integrity advisors| – ")
  if(qsource ==''){qsource = 'National'}
  ## now get data from Qualtrics
  this_survey = fetch_survey(surveyID = surveys$id[k], 
                        label = TRUE, # use text for surveys
                        add_column_map = FALSE,
                        force_request = TRUE,
                        verbose = TRUE)
  if(nrow(this_survey) > 0){
    this_survey = clean_names(this_survey) %>%
      select(-q_data_policy_violations) %>%
      mutate(source = qsource,
             id = paste(qsource, '.', 1:n(), sep='')) %>% # make unique ID number
      mutate_at(vars(starts_with("q")), funs(as.character)) %>% # change all questions to character, better for concatenating
      mutate_at(vars(starts_with("q3_3_")), funs(tolower)) %>% # convert time spent in role to lower case
      mutate(q2_1_2 = as.numeric(q2_1_2),
             q2_3_2 = as.numeric(q2_3_2)) # change back from character for two questions that are always years
    # concatenate
    raw = bind_rows(raw, this_survey)
  }
}
raw = select(raw, 'id', everything()) # move ID to first column

## Email
# Validate q5_4 as an email
index = str_detect(raw$q5_4, pattern='@') # is there an email
index[is.na(index)] = TRUE # no problems for missing data
raw$q5_4[!index]
# add email provided as a variable
raw = mutate(raw,
             email_provided = str_detect(q5_4, '@') & !is.na(q5_4), # at symbol and not missing
             email_provided = ifelse(email_provided==TRUE, 'Yes', 'No')) %>% 
  select(-q5_4) # remove email, not required for this project


## removals ##
# remove people with zero progress ... 
cat('There were ', nrow(filter(raw, progress==0)),' respondents with a zero progress.\n', sep='')
raw = filter(raw, progress > 0)
# ... and who did not answer any questions
comment_questions = c('q2_4','q3_4','q4_4','q5_1_9_text','q5_3') 
#
selected_questions = select(raw, 'response_id', starts_with('q')) %>% # 
  select(-contains('_do_'), -all_of(comment_questions)) %>% # remove comments
  mutate_all(as.character) %>%
  pivot_longer(cols = -response_id) %>%
  filter(!str_detect(name, pattern='q4_1_|q4_2')) %>% # remove remove long optional questions
  mutate(missing = is.na(value)) 
# overall missing
all_missing = group_by(selected_questions, response_id) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
cat('There were ', nrow(all_missing),' respondents who completed nothing\n', sep='')
raw = filter(raw, !response_id %in% all_missing$response_id)

## data edits
experience_levels = c("0 years (I am not a researcher)","Less than 3 years", "3 to 10 years", "11 years or more")
data = mutate(raw, 
              q5_2 = factor(q5_2, levels = experience_levels), # make as a factor for ordering
              q2_4 = str_squish(q2_4), # remove unwanted spaces from optional text
              q3_4 = str_squish(q3_4),
              q4_4 = str_squish(q4_4),
              q5_1_9_text = str_squish(q5_1_9_text),
              q5_3 = str_squish(q5_3)) %>% 
  select(-user_language, -distribution_channel, 
         -recorded_date, -end_date # just use start_date
  ) 

# question duration and progress
data = mutate(data,
              duration_in_mins = duration_in_seconds/60,
              progress_cat = cut(progress, c(-0.001,5,50,75,100)), # progress percent as categories
              start_date = with_tz(start_date + 60*60*6, "Australia/Brisbane"), # change time zones (had to add six hours to get right time)
              start_date = as.Date(start_date)) %>% # do not need time
  select(-duration_in_seconds)

### date and time started for RIA role
## data edits based on comments
source('3_data_edits.R')

#
months = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
data = mutate(data,
            # start
              month_start = as.numeric(factor(q2_1_1, levels=months)), # change month to number, keep original variable for missing stats
              month_start = ifelse(is.na(month_start)==TRUE, 6, month_start), # assume June if missing
              year_start = as.numeric(q2_1_2),
              yrmon_start = year_start + ((month_start-1)/12))
# still an advisor
data1 = filter(data, q2_2 == 'Yes') %>%
  mutate(current_year = as.numeric(format(start_date, "%Y")), # start_date is date they started the questions
         current_month = as.numeric(format(start_date, "%m")),
         yrmon_end = current_year + (current_month-1)/12)
# not a current advisor
data2 = filter(data, q2_2 == 'No') %>%
  mutate(month_end = as.numeric(factor(q2_3_1, levels=months)), # change month to number, keep original variable for missing stats
   month_end = ifelse(is.na(month_end)==TRUE, 6, month_end), # June if missing
   year_end = as.numeric(q2_3_2),
   yrmon_end = year_end + (month_end-1)/12)
# missing
data3 = filter(data, is.na(q2_2))
# calculate time in RIA role (months)
data = bind_rows(data1, data2, data3) %>%
  mutate(years_in_role = yrmon_end - yrmon_start) # 
# checks
#s = select(data, starts_with('q2_1'), starts_with('q2_2'), 'q2_3_1', 'q2_3_2', contains('yrmon'), 'year_start', 'month_start', 'year_end', 'month_end','years_in_role') %>% sample_n(5)
# check missing
#m = filter(data, is.na(years_in_role)) %>% select(starts_with('q2_1'), starts_with('q2_2'), 'q2_3_1', 'q2_3_2', contains('yrmon'), 'year_start', 'month_start', 'year_end', 'month_end','years_in_role','q2_4')

# create common time for time spent in role
source('3_common_time.R')

## rename some variables to simplify
# _s = staff, _p = [phd] students
data = rename(data, 
              'q4_1_number_s' = 'q4_1_number_1_1',
              'q4_1_number_p' = 'q4_1_number_1_2',
              'q4_1_number_2_s' = 'q4_1_number_2_1_1',
              'q4_1_number_2_p' = 'q4_1_number_2_2_1',
              'q4_1_number_3_s' = 'q4_1_number_3_1_1',
              'q4_1_number_3_p' = 'q4_1_number_3_2_1',
              'q4_1_number_4_s' = 'q4_1_number_4_1_1',
              'q4_1_number_4_p' = 'q4_1_number_4_2_1')
labels = mutate(labels, # rename in labels too
                names = case_when(
                  names == 'q4_1_number_1_1' ~ 'q4_1_number_s',
                  names == 'q4_1_number_1_2' ~ 'q4_1_number_p',
                  names == 'q4_1_number_2_1_1' ~ 'q4_1_number_2_s',
                  names == 'q4_1_number_2_2_1' ~ 'q4_1_number_2_p',
                  names == 'q4_1_number_3_1_1' ~ 'q4_1_number_3_s',
                  names == 'q4_1_number_3_2_1' ~ 'q4_1_number_3_p',
                  names == 'q4_1_number_4_1_1' ~ 'q4_1_number_4_s',
                  names == 'q4_1_number_4_2_1' ~ 'q4_1_number_4_p',
                  TRUE ~ as.character(names)
                ))

## convert amount of advice given to common scale
source('3_common_advice.R')

## can drop some of the random ordering variables
# need to keep all orders for q4_2 and q5_1 
data = select(data, 
              -status, -response_id , # not used
              -q3_2_do_2, -q3_2_do_3, # only need _1, as other order is determined
              -q4_3_do_2, -q4_3_do_3, -q4_3_do_4, -q4_3_do_5, -q4_3_do_6 # only need _1, as other order is determined
) %>%
  mutate(q3_2_do_1 = as.numeric(q3_2_do_1=='1'), # binary variable for order
         q4_3_do_1 = as.numeric(q4_3_do_1=='1')) %>%
  mutate_at(vars(starts_with("q4_2_do")), funs(as.numeric)) %>% # change into a number for modelling
  mutate_at(vars(starts_with("q5_1_do")), funs(as.numeric)) # change into a number for modelling


## Part 3: admin data on emails ##
sheets = excel_sheets('data/email_bounced_away.xlsx')
admin_data  = NULL
for (s in sheets){
  this = read_excel('data/email_bounced_away.xlsx', sheet=s) %>%
    clean_names() %>%
    mutate(response = s)
  admin_data = bind_rows(admin_data, this)
}
admin_data = select(admin_data, -checker, -check_date)

### save ###
# main version
save(data, labels, ordering, comment_questions, admin_data, file='rdata/3_AnalysisReady.RData')
cat('Number of data rows = ', nrow(data), '.\n', sep='')
# version for sharing
data = select(data, -all_of(comment_questions))
save(data, labels, file='rdata/3_AnalysisReady_share.RData')
