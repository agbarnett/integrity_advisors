# 0_read_ria_emails.R
# read in the RIA email data entered by David
# March 2023
library(janitor)
library(dplyr)
library(readxl)
library(stringr)

# institutions
institutions = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 'all_institutions') %>%
  clean_names() %>%
  select(-starts_with('x')) %>% # remove unwanted columns
  filter(!str_detect(website, pattern = '^Same as')) # remove six duplicates institutions

# Research Integrity Advisors
rias = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 'contact_details') %>%
  clean_names() %>%
  filter(inst !='Dummy university')

## check the data
# emails without at symbol - should be no rows
filter(rias, 
       !is.na(email),
       !str_detect(email, '@')) # 
filter(institutions, 
       rias_available  == FALSE, # only need email where there's no public data
       !is.na(email),
       !str_detect(email, '@')) # 
# emails without au
filter(rias, 
       !is.na(email),
       !str_detect(email, '\\.au')) %>% # 
  select(inst, email)

# save
save(institutions, rias, file = 'data/0_emails.RData')
