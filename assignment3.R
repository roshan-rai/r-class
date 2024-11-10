#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
bill.data <-read.csv(file="BillingInfoAssignment.csv", header=TRUE,sep=",")
vital.data<-read.csv(file="VitalInfoAssignment.csv", header=TRUE,sep=",")
colnames(bill.data)
colnames(vital.data)
#inner join on first name and last name
innerjoin <- merge(bill.data, vital.data, by.x = c("FirstName", "LastName"), by.y = c("First", "Last"))
#appering in either of the dataset
outerjoin <- merge(bill.data, vital.data, by.x = c("FirstName", "LastName"), by.y = c("First", "Last"), all = TRUE)
#"Se" starting insurance in merged dataset 
subsetSE <- subset(innerjoin, grepl("^Se", Insurance, ignore.case = TRUE))
#mean of blood pressure in mergred dataset
meanSystolic <- mean(innerjoin$SystolicBP, na.rm = TRUE)
meanDiastolic <- mean(innerjoin$DiastolicBP, na.rm = TRUE)
#deleting last row
innerjoin <- innerjoin[1:(nrow(innerjoin)-1), ]
dim(innerjoin)
# column “YearVisited” + extract the year information from date column
innerjoin$YearVisited <- format(as.Date(innerjoin$Date, format="%m/%d/%y"), "%y")
#sort in desc order by carged in dollars
innerjoin <- innerjoin[order(-innerjoin$ChargesInDollars), ]
#h unique values of insurance type using data generated 
unique_insurance <- unique(innerjoin$Insurance)
#i delete prioirVisit and Opinion columns
innerjoin <- innerjoin[, !names(innerjoin) %in% c("PriorVisits", "Opinion")]

