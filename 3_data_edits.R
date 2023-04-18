# 3_data_edits.R
# data edits based on respondents comments for starting time
# February 2023

#
index = str_detect(data$q2_4, "I started in 2011")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2011
#
index = str_detect(data$q2_4, "in 2011")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2011
#
index = str_detect(data$q2_4, "My role actually started earlier than 2016")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2011
#
index = str_detect(data$q2_4, "My records indicate that I started in 2012")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2012
#
index = str_detect(data$q2_4, "I commenced in the role in 2010")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2010
#
index = str_detect(data$q2_4, "not allow me to enter 2011")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2011
#
index = str_detect(data$q2_4, "Approx 5 years")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2018
#
index = str_detect(data$q2_4, "I think it started about 4 years ago")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2019
#
index = str_detect(data$q2_4, "It comes with being a Deputy Dean")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2020
data$q2_1_1[index] = 'Septemeber'
#
index = str_detect(data$q2_4, "^I started in August 2015")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2015
data$q2_1_1[index] = 'August'
#
index = str_detect(data$q2_4, "I started my role in 2014" )
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2014
#
index = str_detect(data$q2_4, "October 2020" )
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2020
data$q2_1_1[index] = 'October'
#
index = data$q2_4=="2022"  # this is an ending one
index[is.na(index)] = FALSE
data$q2_3_2[index] = 2022 # end
data$q2_3_1[index] = 'June'
#
index = str_detect(data$q2_4, "May 2022" )
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2022
data$q2_1_1[index] = 'May'
#
index = str_detect(data$q2_4, "I think it started about 4 years ago")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2019
data$q2_1_1[index] = 'February'
#
index = data$q2_4 == '2 years'
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2021
data$q2_1_1[index] = 'February'
#
index = str_detect(data$q2_4, "August 2015")
index[is.na(index)] = FALSE
data$q2_1_2[index] = 2015 # month was already August

# check individual answers
dummy = function(){
 index = str_detect(data$q2_4, 'August 2015')
 index[is.na(index)] = FALSE
 data[index, c('q2_4','q2_1_2','q2_1_1','q2_2','q2_3_2','q2_3_1')]
}
# check if time is missing but comments are given
dummy2 = function(){
  check = filter(data, is.na(q2_1_2)) %>% # missing start
    select('id','q2_4','q2_1_2','q2_1_1','q2_2','q2_3_2','q2_3_1')
}

