#Diriye Ibrahim
#Materials Recycled Per Month
library(openxlsx) 
xlsxFileData < - data.frame()
xlsxFileData <- read.xlsx("MRPM Data.xlsx",
                          sheet = 1,
                          startRow = 1,
                          colNames = TRUE,
                          rowNames = FALSE, 
                          detectDates = TRUE, 
                          skipEmptyRows = TRUE, 
                          skipEmptyCols = FALSE, 
                          rows = NULL,
                          cols = NULL, 
                          check.names = FALSE,
                          sep.names = ".",
                          namedRegion = NULL,
                          na.strings = "NA", 
                          fillMergedCells = FALSE
)
print(head(xlsxFileData,10))
#Initial Data Extraction
PrintTwoColDescriptStat <- xlsxFileData[,c(1,4)] #row,column
print(PrintTwoColDescriptStat)
FinalTwoColumns <- PrintTwoColDescriptStat[PrintTwoColDescriptStat$Material.Type == "Wood",]
print(FinalTwoColumns)
#Descriptive Stats for Wood
summary(FinalTwoColumns)
#Descriptive Stats for Metal
PrintTwoColDescriptStat1 <- xlsxFileData[xlsxFileData$Material.Type == "Metal", c(1,4)]
print(PrintTwoColDescriptStat1)
summary(PrintTwoColDescriptStat1)
#Descriptive Stats for C&D Debris
PrintTwoColDescriptStat2 <- xlsxFileData[xlsxFileData$Material.Type == "C&D Debris", c(1,4)]
print(PrintTwoColDescriptStat2)
summary(PrintTwoColDescriptStat2)
#Descriptive Stats for Concrete
PrintTwoColDescriptStat3 <- xlsxFileData[xlsxFileData$Material.Type == "Concrete", c(1,4)]
print(PrintTwoColDescriptStat3)
summary(PrintTwoColDescriptStat3)
#Descriptive Stats for Yardwaste/Greenwaste
PrintTwoColDescriptStat4 <- xlsxFileData[xlsxFileData$Material.Type == "Yardwaste/Greenwaste", c(1,4)]
print(PrintTwoColDescriptStat4)
summary(PrintTwoColDescriptStat4)
#Descriptive Stats for Foodwaste/MRF
PrintTwoColDescriptStat4 <- xlsxFileData[xlsxFileData$Material.Type == "Foodwaste/MRF", c(1,4)]
print(PrintTwoColDescriptStat4)
summary(PrintTwoColDescriptStat4)
#Descriptive Stats for Refuse
PrintTwoColDescriptStat5 <- xlsxFileData[xlsxFileData$Material.Type == "Refuse", c(1,4)]
print(PrintTwoColDescriptStat5)
summary(PrintTwoColDescriptStat5)
#Descriptive Stats for Mix Debris
PrintTwoColDescriptStat6 <- xlsxFileData[xlsxFileData$Material.Type == "Mix Debris", c(1,4)]
print(PrintTwoColDescriptStat6)
summary(PrintTwoColDescriptStat6)
#Descriptive Stats for Organics
PrintTwoColDescriptStat7 <- xlsxFileData[xlsxFileData$Material.Type == "Organics", c(1,4)]
print(PrintTwoColDescriptStat7)
summary(PrintTwoColDescriptStat7)
#ANOVA DOE Hypothesis Testing
fit <- aov(xlsxFileData$TONS~Material.Type+Semester, data = xlsxFileData)
summary(fit)
summary.lm(fit)
#TukeyTest
TukeyHSD(fit)
plot(TukeyHSD(fit))