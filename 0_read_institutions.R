# 0_read_institutions.R
# find the number of eligible institutions
# October 2022
library(dplyr)

# from the list of NHMRC summary files, see https://www.nhmrc.gov.au/funding/data-research/outcomes
# 
applied2020 = read.table('data/list_of_institutions_2020.txt', quote='', header = FALSE, row.names = NULL, sep='\t')
applied2021 = read.table('data/list_of_institutions_2021.txt', quote='', header = FALSE, row.names = NULL, sep='\t')
applied2022 = read.table('data/list_of_institutions_2022.txt', quote='', header = FALSE, row.names = NULL, sep='\t')
# added by investigators
added = c('The Florey Institute','Peter MacCallum Cancer Centre')
#
all = bind_rows(applied2020, applied2021, applied2022, added) %>%
  unique() %>%
  arrange(V1) %>%
  rename('inst' = 'V1')
nrow(all)

# export to latex
outfile = 'data/for_latex.txt'
ofile = file(outfile, 'w')
for (k in 1:nrow(all)){
  cat('\\item ', all$inst[k], '\n', sep='', file=ofile)
}
close(ofile)

# export to CSV
write.csv(all, file = 'data/all_institutes.csv', quote=FALSE, row.names=FALSE)

# More complete list of who was approached in 3_estimate_national_numbers.R