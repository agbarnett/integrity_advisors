# 2_email_census_reminder.R
# email the RIA census; reminder email
# code copied from career disruption project
# February 2023
library(janitor)
library(dplyr)
library(readxl)
library(stringr)
library(RDCOMClient) # for sending emails direct from R (installed from github)

## get the personal links from Qualtrics, see 2a_make_email_list.R, links generated on 13-Feb-2023
source('2_email_census_common.R')

# check no missing institutions
summary(nchar(census$inst))

## Section 1: add responders/withdrawals to avoiding emailing twice
# find responders based on link; csv exported from Qualtrics
infile1 = 'data/export-EMD_8cHPjvWxMPQBVJu-2023-02-26T21-42-43Z.csv'
infile2 = 'data/export-EMD_ysaKEkCu9WmrtNc-2023-02-26T21-42-32Z.csv'
responders1 = read.csv(infile1, stringsAsFactors = FALSE) %>%
  clean_names()
responders2 = read.csv(infile2, stringsAsFactors = FALSE) %>%
  clean_names()
responders = bind_rows(responders1, responders2)%>%
  filter(status == 'Survey Finished') %>% # so still send to `Survey Partially Finished` and `Survey Started`
  select(email_address ) %>%
  mutate(email_address  = str_squish(tolower(email_address)))
# knock out responders
index = census$email %in% responders$email_address
table(index)
census$approach[index] = FALSE # do not approach these people

## 1b) Hand-entered data on withdrawals, not eligible, etc
# withdrawn
withdrawn = read_excel('data/email_bounced_away.xlsx', sheet='withdrawn') %>%
  mutate(email = str_squish(tolower(email)))
index = tolower(census$email) %in% withdrawn$email # find matches
if(sum(index) != nrow(withdrawn)){
  cat('Warning, not a perfect match\n')
  withdrawn$email [withdrawn$email %in% census$email == FALSE]
}
census$approach[index] = FALSE # do not approach these people
# Not eligible
not_eligible = read_excel('data/email_bounced_away.xlsx', sheet='Not eligible') %>%
  mutate(email = str_squish(tolower(email)))
index = tolower(census$email) %in% not_eligible$email # find matches
if(sum(index) != nrow(not_eligible)){cat('Warning, not a perfect match\n')}
census$approach[index] = FALSE # do not approach these people
# Bounced
bounced = read_excel('data/email_bounced_away.xlsx', sheet='Bounced') %>%
  mutate(email = str_squish(tolower(email)))
index = tolower(census$email) %in% bounced$email # find matches
if(sum(index) != nrow(bounced)){cat('Warning, not a perfect match\n')} # one missing, checked email and will just send again
census$approach[index] = FALSE # do not approach these people

# remove few with missing institution
census = filter(census, !is.na(inst))
N_census = nrow(census)

## Section 2: create individual emails ##
## 1st reminder email - with median time ##
email.body = list()
for (k in 1:N_census){
  # opening
  email.body[[k]] = ''
  email.body[[k]] = paste(email.body[[k]], 'Dear ', census$to[k], ',<br><br>', sep='')
  # main body
  email.body[[k]] = paste(email.body[[k]], 'We are interested in hearing from Research Integrity Advisors. Your input via our questionnaire could be helpful to guide policy, but we haven’t yet received your response. ', sep='')
  email.body[[k]] = paste(email.body[[k]], 'We would be grateful if you complete the questions, even if you are no longer in the role at ', census$inst[k], '. ', sep='')
  email.body[[k]] = paste(email.body[[k]], 'From the responses so far, the questions have taken a median of 8 minutes to complete. ', sep='')
  email.body[[k]] = paste(email.body[[k]], 'You can read the participant information sheet and complete the questions here: ', census$link[k],'. Please do not pass on this email to others, as this link is specific to you. Your name and email will be deleted once data collection has ended.<br><br>', sep='')
  email.body[[k]] = paste(email.body[[k]], 'We want to hear from all advisors, including those who have yet to provide any advice.<br><br>', sep='')
  email.body[[k]] = paste(email.body[[k]], 'If you don’t want to be contacted again, then simply reply to this email with the word “No”.<br><br>', sep='')
  email.body[[k]] = paste(email.body[[k]], 'This study has been approved by the QUT Human Research Ethics Committee (approval number LR `2022-6395-11521`).<br><br>', sep='')
  # closing text
  email.body[[k]] = paste(email.body[[k]], 'Regards,<br><br>Prof Adrian Barnett<br>Queensland University of Technology<br>Phone: 07 3138 6010<br>Internal reference number: ', k, sep='') # 
}

#### Section 3: send emails ###
# 361 reminders sent on 27 feb
min.send = 201 # used to run in batches
#max.send = 200
max.send = nrow(census) # uncapped
for (k in min.send:max.send){ 
  # only create email if its okay to approach them
  if(census$approach[k] == TRUE){
    # start the app
    OutApp <- COMCreate("Outlook.Application")
    # create an email 
    outMail = OutApp$CreateItem(0)
    outMail[["To"]] = census$email[k] # commented out for testing
    #    outMail[["To"]] = 'dn.borg@qut.edu.au' # for testing
    outMail[["subject"]] = "Research integrity advisor"
    outMail[["HTMLbody"]] = email.body[[k]]
    # send it                     
    outMail$Send()
    Sys.sleep(2) # short break
  } # end of if
}

