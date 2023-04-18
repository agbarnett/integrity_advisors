# 2a_make_email_list.R
# make the email list for importing into qualtrics
# see https://www.qualtrics.com/support/survey-platform/contacts/contact-list-overview/
# and https://www.qualtrics.com/support/survey-platform/distributions-module/email-distribution/personal-links/
# Feb 2023
library(readxl)
library(dplyr)
library(janitor)
library(stringr)

# get the names of RIAs - includes myself as a dummy email
sample = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 2) %>%
  clean_names() %>%
  select(row_number, inst, title, first_name, last_name, email, searched_on_or_added_on) %>%
  filter(!is.na(email))  # only those with an email

# export for qualtrics, had to have quote = TRUE
first_sample = filter(sample, searched_on_or_added_on < as.Date('2023-02-13')) %>%
  select(-searched_on_or_added_on)
write.csv(first_sample, file = 'list/ria_list_for_qualtrics.csv', quote=TRUE, row.names=FALSE)

# then generate personal links in Qualtrics , file read in using 2_email_census.R

