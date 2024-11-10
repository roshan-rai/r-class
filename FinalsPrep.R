#week 7 final week

students <- data.frame(name = c("Joe", "Fred", "Bill"), age = c(32, 24, 28))

students.name 

#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
med.data <-read.csv(file="CoreBillingData.csv", header=TRUE,sep=",")
med.data$NewCol<-rep("No data", nrow(med.data))
if (med.data$ChargesInDollars[1]>50)
{
  med.data$NewCol[1]<-"High maintaince"
} else
{
  med.data$NewCol[1]<-"Low maintaince"
}

a<-med.data$Insurance.char
med.data$Insurance.char[is.na(med.data$Insurance.char)] <- "BCS"

if(is.null(med.data$Insurance))
  {
    med.data$Insurance<-"Worked"
  }else
  {
  }
help(nchar)
a<-nchar(as.character(med.data$Insurance.char[4]))-1
med.data$Insurance[4].char
a<-"cat"
nchar(as.character(a))-1
med.data$NewCol <- ifelse(grepl("^A", med.data$State), "Yay", "Nay")

#for loop
i<-0
for(i in 1:nrow(med.data)){
  if(med.data$ChargesInDollars[i]>50){
    med.data$NewCol[i]<-"High"
  }else{
    med.data$NewCol[i]<-"Low"
  }
}

#creating own function
functionName<-function(med.data)
{
   med.data$label.values<-rep("no data",nrow(med.data))
  i<-0
  for(i in 1:nrow(med.data))
  {
    if(med.data$ChargesInDollars[i]>50)
    {
      med.data$label.values[i]<-"high"
    }else
    {
      med.data$label.values[i]<-"low"
    }
    return(med.data)
  }
}


#Week 4 code
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
getwd()
beer.data<-read.csv(file="BeerDataExample.csv", sep=",", header=TRUE)
nrow(beer.data)
summary.data<-table(beer.data$Brand)
barplot(beer.data$Brand, main="Beer Preference", xlab="Types of Beer")
#the above code wont work and the one beneth too why ?
barplot(beer.data$Brand)
#label
labels.data<-c("Bud Light","Busch","Michelob","Coors","Miller","Natural","Other")
#to make it work
barplot(summary.data, main="Beer Preference", xlab="Types of Beer",names.arg=labels.data)
#label only different in pie chart
pie(summary.data, main="Beer Preference", xlab="Types of Beer", labels.data)
#Another data its money 40.12 now know what kind of diagram should it have ?
bills.data <- read.csv(file = "BillsExample.csv", sep = ",", header =TRUE)
#histogram ma summary chahindaina hist(bills.data$Bills matra garyo bhaneni chalcha)
hist(bills.data$Bills,ylim=c(0,60),xlab="Telephone Bills", ylab="Number of Customers", main="Telephone Bills Distribution")
#another data
housing.data <- read.csv(file ="HousingDataExample.csv", sep = ",", header = TRUE)
#has size and price
plot(housing.data$Size, housing.data$Price,main="Price VS Size", xlab="Size",ylab="Price",col=blues9)
cor<-cor(housing.data$Size, housing.data$Price)

#week 5
bill.data <- read.csv(file = "Billing-Info.csv", header =  TRUE, sep = ",")
#select columns using select=c make a med.data.subset with only two tables ID and charges indollars
med.data.subset<-subset(bill.data, select=c(ID, ChargesInDollars))
#remove column ageinyears and gender using subset
med.data<-subset(bill.data, select=-c(AgeInYears, Gender))
#Filter data using get data with opinion =5 and gender only male
maleopinion.data<-subset(bill.data, Opinion==5 & Gender=='M')
#replacig using sub replace the pay with day in insurance table
med.data$Insurance<-sub('Pay','day',med.data$Insurance,ignore.case = TRUE)
#replace using gsub
med.data$Insurance<-gsub('Self','public', med.data$Insurance,ignore.case=TRUE)
#extract the last two character
substr(med.data$State,nchar(med.data$State)-1,nchar(med.data$State))

#week 6 code
med.data <-read.csv(file="CoreBillingData.csv", header=TRUE,sep=",")
vital.data<-read.csv(file="VitalDataSubset.csv", header=TRUE,sep=",")
health.data<-read.csv(file="HealthcareData.csv", header=TRUE,sep=",")
#left outerjoin merge the ded and vital with ID in common
leftjoin <- merge(med.data, vital.data, by="ID",all.x=TRUE)
#right outer join
rightjoin <- merge(med.data, vital.data, by="ID", all.y=TRUE)
#inner join
innerjoin <- merge(med.data, vital.data, by="ID")
#change the format of the date into data
as.Date(med.data$Date, format="%m/%d/%y")
#find the diferences in the date
date<-as.Date(med.data$Date)[2]-as.Date(med.data$Date)[1]
#find the differences in the format of the number
dateDays<-as.numeric(as.Date(med.data$Date)[2]-as.Date(med.data$Date)[1])
library("ggplot2")
#make a canvas on side
ggplot(mtcars, aes(x=wt,y=mpg))
#add a layer of scatterplot
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()
#add another layer
ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+xlim(c(2,5)+ylim(c(0,30))
                                                  #add title
                                                  ggplot(mtcars, aes(x=wt,y=mpg))+geom_point()+ggtitle("mpg vs wt", subtitle="Year 2020")+xlab("Weight")+ylab("Miles Per Gallon")
                            
                                                  unique(mtcars$cyl)                
                                                  length(unique(mtcars$cyl))
                                                  class(mtcars$cyl)
                                                  mtcars$cylfact<- as.factor(mtcars$cyl)
                                                  ggplot(mtcars, aes(x=wt,y=mpg))+geom_point(aes(col=cylfact),size=3)+ggtitle("mpg vs wt", subtitle="Year 2020")+xlab("Weight")+ylab("Miles Per Gallon")
                                                  ggplot(mtcars, aes(fill=cylfact,x=gear, y=mpg))+geom_bar(position = "dodge", stat="identity")
                                                  #create histogram
                                                  ggplot(mtcars, aes(mpg))+geom_histogram(color="blue",fill="lightblue", bins = 8)+ggtitle("MPG Distribution")
                                                  #average
                                                  vital.sub$average.BP<-rowMeans(cbind(vital.sub$SystolicBP, vital.sub$DiastolicBP))
                                                  head(vital.sub)
                                                  
#assignment 2
shoppers.data <- read.csv(file="ShoppersData.csv", sep=",", header=TRUE)
table(shoppers.data$Day)  
#First step is to make the table in what you want to sort in and then only after the table calculate
tableOnly<-aggregate(Customer~Day,data=shoppers.data,FUN=length)
#total spent everyday 
totalSpent<-aggregate(AmountSpentInDollars~Day,data=shoppers.data, FUN=sum)
#mean average spent each day
avg<-aggregate(AmountSpentInDollars~Day,data=shoppers.data, FUN=mean)
#table
table(shoppers.data$Browser)
#totalDollarspent
totalSpentBrowser<-aggregate(AmountSpentInDollars~Browser,data=shoppers.data,FUN=sum)
#mean spent on each browser
mean<-aggregate(AmountSpentInDollars~Browser,data=shoppers.data, FUN=mean)

#assignment3
vital.data<-read.csv(file="VitalInfoAssignment.csv", sep=",", header=TRUE)
bill.data<-read.csv(file="BillingInfoAssignment.csv", sep=",", header=TRUE)
#inner join
innerJoin<-merge(vital.data,bill.data,by.x= c("First","Last"),by.y=c("FirstName","LastName"))
#outerjoin
outJoin<-merge(vital.data,bill.data,by.x= c("First","Last"),by.y=c("FirstName","LastName"),all=TRUE)
#patients records that starts with "Se"
insurance<-innerjoin[substr(innerjoin$Insurance,1,2) == "Se",]
subsetSE <- subset(innerjoin, grepl("^Se", Insurance, ignore.case = TRUE))
#systolic
sysMean<-mean(innerjoin$SystolicBP,na.rm=TRUE)
dysMean<-mean(innerjoin$DiastolicBP,na.rm=TRUE)
combinedMean<-cbind(sysMean,dysMean)
#delete last row in merged dataset
innerjoin<-innerjoin[1:nrow(innerjoin)-1,]
#make another table and get 2014 extracted
innerjoin$YearVisited <- substr(as.Date(innerjoin$Date,format = "%m/%d/%y"),1,4)  
#g)	Sort the data by ChargesInDollars (in descending order)
innerjoin$YearVisited<-order(innerjoin$YearVisited,decreasing = TRUE)
#sort the data of initial thing but by decreasing order of charges in dollars
innerjoin <- innerjoin$YearVisited[order(innerjoin$ChargesInDollars, decreasing = TRUE)]
#unique values
unique(innerJoin$Insurance)
#Delete two tables
colnames(innerJoin)
innerJoin <- subset(innerJoin, select = -c(PriorVisits, Opinion), subset = TRUE)


#create a function that calculates the coefficient of variation (I think that's what it's called) which is standard deviation divided by the mean. 
#And then if the mean is "0" then you return a -9999. Otherwise calculate that coefficient of variation.

coeVar<-function(med.data)
{
  avg<-mean(med.data$ChargesInDollars)
  std<-sd(med.data$ChargesInDollars)
  if(avg==0)
  {
    return(-9999)
  }else
  {
    return(std/avg)
  }
}
result <- coeVar(med.data)
print(result)

dayBrows<-aggregate(Customer~(Day+Browser), data=shoppers.data, FUN=length)
i<-0
for(i in 1:nrow(shoppers.data))
{
  if(shoppers.data$AmountSpentInDollars[i]>50){
    shoppers.data$NewCol[i]<-"High"
  }else{
    shoppers.data$NewCol[i]<-"Low"
  }
  
}

setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
getwd()
vital.data<-read.csv(file="VitalDataSubset.csv",header=TRUE,sep=",")
health.data<-read.csv(file="HealthcareData.csv",header=TRUE,sep=",")
left<-merge(vital.data,health.data,by="ID",all.x=TRUE)
right<-merge(vital.data,health.data,by="ID",all.y=TRUE)
inner<-merge(vital.data,health.data,by.y=c("SysBloodPressure","DiaBloodPressure"),by.x=c("SystolicBP","DiastolicBP"))
outer<-merge(vital.data,health.data,by="ID",all=TRUE)
help(mean)
i<-0
for(i in 1:nrow(outer))
{
  if (is.na(outer$SystolicBP[i])) {
    outer$SystolicBP[i]<-mean(outer$SystolicBP,na.rm=TRUE)
  }
}
i<-0
for (i in 1:nrow(outer)) {
  if (is.na(outer$DiastolicBP[i])) {
    outer$DiastolicBP[i]<-mean(outer$DiastolicBP,na.rm=TRUE)
  }else
  {}
}
i<-1
for(i in 1:nrow(outer))
{
  if(is.na(outer$AgeInYears[i]>25)){
    outer$Big[i]<-"Adult"
  }else{
    outer$Big[i]<-"Young"
  }
}
#making own function
functionName<-function(outer)
{
  i<-0
  for(i in 1:nrow(outer))
  {
    if(is.na(outer$Opinion[i]>3))
    {
      outer$CustomerSatisfaction[i]<-"Satisfied"
    }else{
      outer$CustomerSatisfaction[i]<-"Not Satisfied"
    }
    return(outer)
  }
}
#calling function
functionName(outer)

as.Date(outer$Date[nrow(outer)-1])
a<-as.numeric(as.Date(outer$Date)[nrow(outer)-3]-as.Date(outer$Date)[1])
byInsurance<-aggregate(ChargesInDollars~Insurance, data=outer,FUN=sum)
byGender<-aggregate(ChargesInDollars~Gender, data=outer,FUN=mean)
combinedData<-merge(byInsurance, byGender, by="ChargesInDollars")
outer<-outer[1:nrow(outer)-1,]
outer<-subset(outer,select=-c(Big,DiaBloodPressure))
se<-subset(outer,grepl("^A",State))
outer$Gender<-gsub("M","Male",outer$Gender,ignore.case = TRUE)
outer$Gender<-gsub("F","Female",outer$Gender,ignore.case = TRUE)

