library(dplyr)
library(readxl)
library(tidyr)
xl_data <- "/Users/Sam/Desktop/Project/TimeSheets.xlsx" # creating a variable name for the file path
excel_sheets(path = xl_data) # viewing the names of the 7 tabs

## reading in each of the months
August <- read_excel(path = xl_data, sheet = "August")
September <- read_excel(path = xl_data, sheet = "September")
October <- read_excel(path = xl_data, sheet = "October")
November <- read_excel(path = xl_data, sheet = "November")
January <- read_excel(path = xl_data, sheet = "January")
February <- read_excel(path = xl_data, sheet = "February")
March <- read_excel(path = xl_data, sheet = "March")

## binding the months together
sheets <- rbind(August, September, October, November, January, February, March)


## formatting the first rows to set a header
sheets <- sheets[-c(0:2),] 
names(sheets) <- as.matrix(sheets[1, ])
sheets <- sheets[-1, ]
sheets[] <- lapply(sheets, function(x) type.convert(as.character(x)))


## Manipulate to get just the info we need
sheets <- as_tibble(sheets) 
sheets <- unique(sheets)
sheets <- sheets %>% drop_na("Clock In Time") 
sheets <- sheets %>% select(-"Clock Out Date", -"Break Start",-"Break End", -"Break Type", -"Payroll ID", -"Scheduled Hours", -"Total Paid Hours", -"Regular Hours",-"Estimated Overtime Hours",-"Unpaid Breaks",-"Blue",-"Cash Tips",-"Credit Tips",-"Variance",-"Total Tips",-"No-Show Reason") 
sheets <- sheets %>% filter(`First Name` != "First Name" & `First Name` != "-")
View(sheets)

