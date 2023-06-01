# 1_consort_diagram.R
# CONSORT flow diagram of inclusions
# December 2022
library(diagram)
library(readxl)
library(dplyr)

## Data
# get the data from 0_read_ria_emails.R
load('data/0_emails.RData')

## Institutions
# number of Institutions
N_inst = nrow(institutions)
available = sum(institutions$rias_available == TRUE) # publicly available RIAs
not_available = sum(institutions$rias_available == FALSE)
not_contactable = 3 # Deakin plus two that were missed
not_available = not_available - not_contactable # remove not contactable from not available
# number of RIAs
rias_first_pass = sum(rias$email != '', na.rm = TRUE) # number of publicly available RIAs with email
# number in second pass (shared or part shared)
inst_second_pass = filter(rias, second_pass) %>% select(inst) %>% unique() %>% nrow()
rias_second_pass = filter(rias, second_pass, !is.na(email)) %>% nrow() # fully shared
# if missing email, then institute passed on invite on our behalf: 
inst_pass = filter(rias, second_pass) %>% 
  group_by(inst) %>%
  summarise(miss = sum(is.na(email)), 
                       n = n()) %>%
  filter(n == miss) %>% # all emails from institution are missing
  ungroup()
rias_inst_pass = summarise(inst_pass, n = sum(n)) # number of RIAs that were passed on invite
#
rias_with_email = rias_first_pass + rias_second_pass + rias_inst_pass$n
# institutions without publicly available information, but who gave emails:
inst_supplied = sum(institutions$rias_shared=='TRUE', na.rm = TRUE) + sum(institutions$rias_shared=='PARTLY', na.rm = TRUE) 
# institutions that did not need RIAs
inst_not_required = sum(institutions$rias_shared=='N/A', na.rm = TRUE) 
inst_not_supplied = not_available - inst_supplied - nrow(inst_pass) - inst_not_required # remainder of Institutions

## Research integrity advisers
# number who responded
load('rdata/3_AnalysisReady.RData') # from 3_read_qualtrics_api.R
rias_responded = nrow(data)
# bounced emails:
invalid_email = read_excel(path = 'data/email_bounced_away.xlsx', sheet = 'Bounced') %>% nrow()
# not eligible:
not_eligible = read_excel(path = 'data/email_bounced_away.xlsx', sheet = 'Not eligible') %>% nrow()
# did not consent
not_consent = sum(data$q1_2=='No')
# not an RIA
not_ria = sum(data$q1_3=='No', na.rm = TRUE)
#
rias_analysed = rias_responded - not_consent - not_ria

# labels, big N for institutions, little n for RIAs
l1 = paste('Number of institutions\n(N=', N_inst, ')', sep='') # 
l2 = paste('RIAs not contacted\n(N=', not_contactable, ')', sep='') # 
l3 = paste('RIAs not publicly\navailable (N=', not_available, ')', sep='') # 
l4 = paste('RIAs publicly\navailable (N=', available, ')', sep='') # 
# 
l5 = paste('Institution did not respond\nor was unwilling to share\nemails (N=', inst_not_supplied, ')', sep='') # 
l6 = paste('RIAs not\nrequired (N=', inst_not_required, ')', sep='') # 
l7 = paste('Institution passed\non our invite (N=', nrow(inst_pass), ')', sep='') # 
l8 = paste('Institution shared\nsome or all\nemails (N=', inst_supplied, ')', sep='') # 
#
l9 = paste('RIAs invited\n(n=', rias_with_email, ')', sep='') # 
l10 = paste('Invalid email\n(n=', invalid_email, ')', sep='') # 
l11 = paste('Not eligible\n(n=', not_eligible, ')', sep='') # 
l12 = paste('RIAs responded\n(n=', rias_responded, ')', sep='') # 
l13 = paste('Excluded\nNot consented (n=', not_consent, ')\n',
            'Not an RIA (n=', not_ria, ')', sep='') # 
l14 = paste('RIAs analysed\n(n=', rias_analysed, ')', sep='') # 
null = ''
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, null)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.3	0.12
2	0.23	0.82	white	square	0.3	0.12
3	0.5	0.82	white	square	0.3	0.12
4	0.77	0.82	white	square	0.3	0.12
5	0.23	0.71	white	square	0.29	0.145
6	0.2	0.57	white	square	0.45	0.10
7	0.425	0.57	white	square	0.44	0.10
8	0.65	0.57	white	square	0.43	0.10
9	0.77	0.39	white	square	0.3	0.12
10	0.18	0.26	white	square	0.46	0.07
11	0.38	0.26	white	square	0.46	0.07
12	0.61	0.26	white	square	0.3	0.11
13	0.88	0.20	white	square	0.34	0.12
14	0.61	0.12	white	square	0.3	0.11
15	0.61	0.20	transparent	square	0.001	0.001')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 1] = "' '"
M[4, 1] = "' '"
M[5, 3] = "' '"
M[6, 3] = "' '"
M[7, 3] = "' '"
M[8, 3] = "' '"
M[9, 4] = rias_first_pass
M[9, 8] = rias_second_pass
M[9, 7] = rias_inst_pass$n
M[10, 9] = "' '" # 
M[11, 9] = "' '" # 
M[12, 9] = "' '" # 
M[14, 12] = "' '" # 
M[13, 15] = "' '" # 
# colours
tcol = rep('black', n.labels)

## make figure 
jpeg('figures/consort_flow.jpg', width=7.5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        dtext = 0.12, # controls the position of arrow text relative to arrowhead
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# dotted line separating Institutions and advisors
lines(x = c(-1,1), y = c(0.46,0.46), type='l', lty=2)
text(x = 0.05, y = 0.46, labels = 'Institutions', srt = 90, adj=-0.1)
text(x = 0.05, y = 0.46, labels = 'Advisors', srt = 90, adj=1.1)
# footnote:
text(x = 0.1, y = 0.04, cex = 0.8, labels = 'RIA = research integrity advisor; N = number of institutions, n = number of RIAs', adj=c(0,0.5))
dev.off()

## Calculate response percent by invite type
r1 = filter(data, source != 'National') %>% nrow()
r2 = filter(data, source == 'National') %>% nrow()
f1 = data.frame(type = 'Behalf', r = r1, n = rias_inst_pass$n)
f2 = data.frame(type = 'Direct', r = r2, n = rias_first_pass + rias_second_pass)
f3 = data.frame(type = 'Total', r = rias_responded, n = rias_first_pass + rias_second_pass + rias_inst_pass$n)
response_frame = bind_rows(f1, f2, f3) %>%
  mutate(p = round(100*r/n))

# save for 4_census_results.Rmd
save(N_inst,
     not_contactable,
     not_available,
     available,
     inst_not_supplied,
     inst_not_supplied,
     inst_supplied,
     rias_first_pass, 
     rias_with_email,
     rias_responded, 
     response_frame,
     file = 'rdata/flow_numbers.RData')
