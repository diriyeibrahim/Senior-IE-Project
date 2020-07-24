#Diriye Ibrahim
#Waste Per Location
library(openxlsx) 
xlsxFileData < - data.frame()
xlsxFileData <- read.xlsx("WPL Data.xlsx",
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
FinalTwoColumns <- PrintTwoColDescriptStat[PrintTwoColDescriptStat$Location == "Art Foundary",]
print(FinalTwoColumns)
#Descriptive Stats for Art Foundary
summary(FinalTwoColumns)
#Descriptive Stats for Business Classrooms
PrintTwoColDescriptStat1 <- xlsxFileData[xlsxFileData$Location == "Business Classrooms", c(1,4)]
print(PrintTwoColDescriptStat1)
summary(PrintTwoColDescriptStat1)
#Descriptive Stats for Corporation Yard
PrintTwoColDescriptStat2 <- xlsxFileData[xlsxFileData$Location == "Corporation Yard", c(1,4)]
print(PrintTwoColDescriptStat2)
summary(PrintTwoColDescriptStat2)
#Descriptive Stats for Dining Commons
PrintTwoColDescriptStat3 <- xlsxFileData[xlsxFileData$Location == "Dining Commons", c(1,4)]
print(PrintTwoColDescriptStat3)
summary(PrintTwoColDescriptStat3)
#Descriptive Stats for Duncan Hall
PrintTwoColDescriptStat4 <- xlsxFileData[xlsxFileData$Location == "Duncan Hall", c(1,4)]
print(PrintTwoColDescriptStat4)
summary(PrintTwoColDescriptStat4)
#Descriptive Stats for Engineering Courtyard
PrintTwoColDescriptStat4 <- xlsxFileData[xlsxFileData$Location == "Engineering Courtyard", c(1,4)]
print(PrintTwoColDescriptStat4)
summary(PrintTwoColDescriptStat4)
#Descriptive Stats for Facilities
PrintTwoColDescriptStat5 <- xlsxFileData[xlsxFileData$Location == "Facilities", c(1,4)]
print(PrintTwoColDescriptStat5)
summary(PrintTwoColDescriptStat5)
#Descriptive Stats for Housing
PrintTwoColDescriptStat6 <- xlsxFileData[xlsxFileData$Location == "Housing", c(1,4)]
print(PrintTwoColDescriptStat6)
summary(PrintTwoColDescriptStat6)
#Descriptive Stats for Industrial Studies
PrintTwoColDescriptStat7 <- xlsxFileData[xlsxFileData$Location == "Industrial Studies", c(1,4)]
print(PrintTwoColDescriptStat7)
summary(PrintTwoColDescriptStat7)
#Descriptive Stats for MLK Library
PrintTwoColDescriptStat8 <- xlsxFileData[xlsxFileData$Location == "MLK Library", c(1,4)]
print(PrintTwoColDescriptStat8)
summary(PrintTwoColDescriptStat8)
#Descriptive Stats for Recycling Yard at Central
PrintTwoColDescriptStat9 <- xlsxFileData[xlsxFileData$Location == "Recycling Yard at Central", c(1,4)]
print(PrintTwoColDescriptStat9)
summary(PrintTwoColDescriptStat9)
#Descriptive Stats for Spartan Shops
PrintTwoColDescriptStat10 <- xlsxFileData[xlsxFileData$Location == "Spartan Shops", c(1,4)]
print(PrintTwoColDescriptStat10)
summary(PrintTwoColDescriptStat10)
#Descriptive Stats for Spartan Stadium
PrintTwoColDescriptStat11 <- xlsxFileData[xlsxFileData$Location == "Spartan Stadium", c(1,4)]
print(PrintTwoColDescriptStat11)
summary(PrintTwoColDescriptStat11)
#Descriptive Stats for Student Union
PrintTwoColDescriptStat12 <- xlsxFileData[xlsxFileData$Location == "Student Union", c(1,4)]
print(PrintTwoColDescriptStat12)
summary(PrintTwoColDescriptStat12)
#Descriptive Stats for Student Union Loading Dock
PrintTwoColDescriptStat13 <- xlsxFileData[xlsxFileData$Location == "Student Union Loading Dock", c(1,4)]
print(PrintTwoColDescriptStat13)
summary(PrintTwoColDescriptStat13)
#ANOVA DOE Hypothesis Testing
fit <- aov(xlsxFileData$TONS~Location+Semester, data = xlsxFileData)
summary(fit)
summary.lm(fit)
#TukeyTest
TukeyHSD(fit)
plot(TukeyHSD(fit))