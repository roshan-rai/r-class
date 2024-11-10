#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
med.data <-read.csv(file="Billing-Info.csv", header=TRUE,sep=",")
#Drop a specific set of columns
med.data.sample <- subset(med.data, select=-c(ChargesInDollars, VisitTimeInMin))
#keep a specific set of columns
med.data.specific.cols <- subset(med.data, select=c(ID,SysBloodPressure,DiaBloodPressure))
print(med.data.specific.cols)
#filter data
filter.data <-subset(med.data, Opinion == 5 & Gender=="F")

filter.data.extreme.opinion <- subset(med.data, Opinion > 4 | Opinion <2)
med.data[(med.data$Opinion > 4) | (med.data$Opinion < 2),  ]

#sort data asending order
mmed.data.age.sorted <- med.data[order(med.data$AgeInYears),]

#sort data (descending order)
med.data.age.sorted.desc <- med.data[order(med.data$AgeInYears, decreasing=TRUE),]
print(med.data.age.sorted.desc)

#pattern matching and replacement replace pay with pay self
new.text <- sub("Pay","Pay Self",med.data$Insurance)
print(new.text)
#pattern matching and replacment self with group
new.text <- sub("Self","Group",med.data$Insurance)
print(new.text)
gsub("self","Group",new.text)#replace only the first instances of self if used sub

#new column with just state abbreviation
help(substr)
#Number of characters in a string
end.position <- nchar(med.data$State) #start
start.position <- nchar(med.data$State)-1 #Stop

substr(med.data$State, start.position, end.position)

#make a column new one
med.data$state.abb <- substr(med.data$State, start.position, end.position)
