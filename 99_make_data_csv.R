# 99_make_data_csv.R
# make the saved data sets into Excel files
# May 2023
library(openxlsx)

### Analysis data
# get data without qual answers
load('rdata/3_AnalysisReady_share.RData')
#
filename = "rdata/3_AnalysisReady_share.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "data")
addWorksheet(wb, sheetName = "labels")
writeDataTable(wb, sheet = 1, x = data,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 2, x = labels,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)


### Other numbers
load('rdata/flow_numbers.RData')
list = data.frame(available = available,
                  inst_not_supplied = inst_not_supplied,
                  inst_supplied = inst_supplied,
                  N_inst = N_inst,
                  not_available = not_available,
                  not_contactable = not_contactable,
                  rias_first_pass = rias_first_pass,
                  rias_responded = rias_responded,
                  rias_with_email = rias_with_email)
#
filename = "rdata/flow_numbers.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "Response numbers by approach")
addWorksheet(wb, sheetName = 'numbers')
writeDataTable(wb, sheet = 1, x = response_frame,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 2, x = list,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)
