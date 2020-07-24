#Diriye Ibrahim
#Bin Removal Frequency
library(openxlsx) 
xlsxFileData < - data.frame()
xlsxFileData <- read.xlsx("BRF Data.xlsx",
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
FinalTwoColumns <- PrintTwoColDescriptStat[PrintTwoColDescriptStat$Bin.Type == "Roll Off Bins",]
print(FinalTwoColumns)
#Descriptive Stats for Roll Off Bins
summary(FinalTwoColumns)
#Descriptive Stats for Front Load Bins
PrintTwoColDescriptStat1 <- xlsxFileData[xlsxFileData$Bin.Type == "Front Load Bins", c(1,4)]
print(PrintTwoColDescriptStat1)
summary(PrintTwoColDescriptStat1)
#ANOVA DOE Hypothesis Testing
fit <- aov(xlsxFileData$Truck.Arrivals~Bin.Type+Semester, data = xlsxFileData)
summary(fit)
summary.lm(fit)
#TukeyTest
TukeyHSD(fit)
plot(TukeyHSD(fit))