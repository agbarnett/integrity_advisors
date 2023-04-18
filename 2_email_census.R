# 2_email_census.R
# email the RIA census; original email
# code copied from career disruption project
# February 2023
library(janitor)
library(dplyr)
library(readxl)
library(stringr)
library(RDCOMClient) # for sending emails direct from R (installed from github)

## get the personal links from Qualtrics, see 2a_make_email_list.R, links generated on 13-Feb-2023
source('2_email_census_common.R')
N_census = nrow(census)

# check no missing institutions
summary(nchar(census$inst))

## Section 2: create individual emails ##
email.body = list()
for (k in 1:N_census){
  # opening
  email.body[[k]] = ''
  email.body[[k]] = paste(email.body[[k]], 'Dear ', census$to[k], ',<br><br>', sep='')
  # main body
  email.body[[k]] = paste(email.body[[k]], 'My name is Adrian Barnett, I am a professor at Queensland University of Technology. Our group is conducting research on Research Integrity Advisors using a short online questionnaire. ', sep='')
  email.body[[k]] = paste(email.body[[k]], 'You were selected because we believe you are Research Integrity Advisor at ', census$inst[k], '. We want to hear from you even if you have left your Advisor role. ', sep='')
  email.body[[k]] = paste(email.body[[k]], 'You can read the participant information sheet and complete the questions here: ', census$link[k],' (which should take 5 to 10 mins). Please do not pass on this email to others, as this link is specific to you. Your name and email will be deleted once data collection has ended.<br><br>', sep='')
  email.body[[k]] = paste(email.body[[k]], 'If you don’t want to be contacted again then simply reply to this email with the word “No”.<br><br>', sep='')
  email.body[[k]] = paste(email.body[[k]], 'This study has been approved by the QUT Human Research Ethics Committee (approval number LR `2022-6395-11521`).<br><br>', sep='')
  # closing text
  email.body[[k]] = paste(email.body[[k]], 'Regards,<br><br>Prof Adrian Barnett<br>Queensland University of Technology<br>Phone: 07 3138 6010<br>Internal reference number: ', k, sep='') # 

}



#### Section 3: send emails ###
# 357 sent for first email (including me as a test)
# + 5 more for ACU
max.send = 1 # **** temporary for testing ****
#max.send = nrow(census) # uncapped
min.send = 201 # used to run in batches
max.send = nrow(census)
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
