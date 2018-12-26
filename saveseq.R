# save sequences into excel file
library(openxlsx)
library(dplyr)
library(stringr)
index = data.frame(filenames=list.files(pattern = "*.RData"))
index = index %>% mutate(condans = str_extract(filenames,"[A-z]+")) %>% arrange(condans)
index = index %>% group_by(condans) %>% mutate(count = row_number())
table(substr(index$condans,1,3),substr(index$condans,4,6))

wb = createWorkbook("VSPT_EEG_sequences")
for(i in 1:nrow(index)){
  sheetname = paste0(index$condans[i],index$count[i])
  addWorksheet(wb, sheetname)
  load(as.character(index$filenames[i]))
  writeData(wb, sheet = i, output.nice)
}

saveWorkbook(wb, "all_seq.xlsx", overwrite = TRUE)