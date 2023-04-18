# 3_data_edits_time.R
# specific data edits to time answers
# Feb 2023

# find answers that might need changing
to_change = function(){
  #
  text = unique(data$q3_3_4)
  index1 = str_detect(text,'[^0-9]') # any non-numbers
  index2 = str_detect(text,'to|-') # ranges
  text[index1&!index2]
  #
  text = unique(data$q3_3_5)
  index1 = str_detect(text,'[^0-9]') # any non-numbers
  index2 = str_detect(text,'to|-') # ranges
  text[index1&!index2]
  #
  text = unique(data$q3_3_6)
  index1 = str_detect(text,'[^0-9]') # any non-numbers
  index2 = str_detect(text,'to|-') # ranges
  text[index1&!index2]
}

## answers in days
index = data$q3_3_4 == '0.05fte' # wrote FTE in days variable
index[is.na(index)] = FALSE
data$q3_3_4[index] = NA # blank days ...
data$q3_3_5[index] = NA # ... and months
data$q3_3_6[index] = '0.05' # move to FTE
#
index = data$q3_3_4 == '5 to 8 over 3.5 years' # 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '5 to 8' # remove time scale
#
index = data$q3_3_4 == "5% of workload"
index[is.na(index)] = FALSE
data$q3_3_4[index] = NA # blank days
data$q3_3_6[index] = '0.05' # add to FTE
#
index = data$q3_3_4 == "less than one day in total"
index[is.na(index)] = FALSE
data$q3_3_4[index] = '1' # assume one case took whole day
#
index = data$q3_3_4 == "one (1)"
index[is.na(index)] = FALSE
data$q3_3_4[index] = '1' # assume one case took whole day
#
index = data$q3_3_4 == "2 days" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '2' # assume one case per day
#
index = data$q3_3_4 == "10 days" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '10' # assume one case per day
#
index = data$q3_3_4 == "5 days" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '5' # assume one case per day
#
index = data$q3_3_4 == "12-15 days"
index[is.na(index)] = FALSE
data$q3_3_4[index] = '12-15' # assume one case per day
#
index = data$q3_3_4 == "12 days (~ 2 days/year)" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '12' # assume one case per day
#
index = data$q3_3_4 == "20 days over 7 years"
index[is.na(index)] = FALSE
data$q3_3_4[index] = '20' # assume one case per day
#
index = data$q3_3_4 == "about 12 full days"
index[is.na(index)] = FALSE
data$q3_3_4[index] = '12' # assume one case per day
#
index = data$q3_3_4 == "no idea" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = NA # blank
#
index = data$q3_3_4 == "two" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '2' 
#
index = data$q3_3_4 == "0 hours" 
index[is.na(index)] = FALSE
data$q3_3_4[index] = '0' 

## answers in months
index = data$q3_3_5 == '0,25 days per month' # used comma
index[is.na(index)] = FALSE
data$q3_3_5[index] = '0.25' 
# remove units
data = mutate(data,
              q3_3_5 = str_remove_all(q3_3_5, 'days? (per|a) month')) 
#
index = data$q3_3_5 == 'zero ' # 
index[is.na(index)] = FALSE
data$q3_3_5[index] = '0' 
#
index = data$q3_3_5 == 'half ' # `half day per month`
index[is.na(index)] = FALSE
data$q3_3_5[index] = '0.5' 
#
index = data$q3_3_5 == 'one' # 
index[is.na(index)] = FALSE
data$q3_3_5[index] = '1' 
#
index = data$q3_3_5 == '10 min' # !
index[is.na(index)] = FALSE
data$q3_3_5[index] = '0.001'  #

## answers in FTE
#
#index = original$q3_3_6 == "very little. once or twice in total" - do not need to edit, as they also gave an answer in days
#index[is.na(index)] = FALSE
#data$q3_3_5[index]
#
index = data$q3_3_6 == 'very little. once or twice in total' # 
index[is.na(index)] = FALSE
data$q3_3_6[index] = NA # blank FTE ...
data$q3_3_4[index] = '1' # move to days, assuming 1 working day for this answer
