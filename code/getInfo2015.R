library(synapseClient)
synapseLogin()
library(XLConnect)

aacr2015 <- synGet('syn5650504', downloadLocation = 'data')
aacr2015 <- readWorksheetFromFile("data/2015_abstracts.xlsx",sheet="AM_2015_Interactome_Data_Reg_Ab")
inter2015 <- readWorksheetFromFile("data/abstract_2015.xlsx",sheet="Hoja1")

temp <- merge(aacr2015, inter2015, by.x = "CONTROL.NUMBER", by.y = "CONTROLNUMBER")

write.csv(temp, 'interactome_presenter_2015.csv')
