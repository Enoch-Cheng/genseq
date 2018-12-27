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

saveWorkbook(wb, "all_seq.xlsx", overwrite = FALSE)

checking = data.frame(cond="---",stim="---",ans="---",ans_rep="---")
# check seq
for(i in 1:nrow(index)){
  tmp = data.frame(cond=as.character(index$filenames[i]),stim="---",ans="---",ans_rep="---")
  load(as.character(index$filenames[i]))
  checking = rbind(checking,tmp,output.nice[1:3,])
  print(i)
}

write.xlsx(checking, "check152seq.xlsx")