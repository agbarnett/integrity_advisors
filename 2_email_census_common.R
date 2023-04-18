# 2_email_census_common.R
# common code used by 2_email_census_[].R
# Feb 2023

## get the personal links from Qualtrics, see 2a_make_email_list.R, links generated on 13-Feb-2023
# get the links
links1 = read.csv(file='list/export-EMD_ysaKEkCu9WmrtNc-2023-02-13T00-07-52Z.csv') %>%
  clean_names() %>%
  select(email, link) %>%
  mutate(email = str_squish(tolower(email))) # for merge
# extra from ACU (arrived after original distribution)
links2 = read.csv(file='list/export-EMD_GeISbltlYr3uGvB-2023-02-16T11-43-08Z.csv') %>%
  clean_names() %>%
  select(email, link) %>%
  mutate(email = str_squish(tolower(email))) # for merge
links = bind_rows(links1, links2)

# get the more detailed data with names and addresses
census_detail = read_excel('data/RIA-main_13-01-2023.xlsx', sheet = 2) %>%
  clean_names() %>%
  select(row_number, inst, title, first_name, last_name, email) %>%
  filter(!is.na(email)) %>% # only those with an email
  mutate(email = str_squish(tolower(email)), # for merge
         title = ifelse(is.na(title), '', title),
         to = paste(title, first_name, last_name), # make `to`
         to = str_squish(to),
         approach = TRUE) # start off approaching everyone
# merge links with detailed data by email
census = full_join(census_detail, links, by='email')
