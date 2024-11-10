#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
med.data <-read.csv(file="CoreBillingData.csv", header=TRUE,sep=",")
vital.sub<-read.csv(file="VitalDataSubset.csv", header=TRUE,sep=",")
health.data<-read.csv(file="HealthcareData.csv", header=TRUE,sep=",")
#left outer join it should have 20
leftjoin.data<-merge(med.data, vital.sub, by = "ID", all.x = TRUE)
dim(leftjoin.data)
a<-head(input.data)
#right outer join 14 should be there because ...idk
rightjoin.data<-merge(med.data, vital.sub, by = "ID", all.y = TRUE)
#inner join
innerjoin <- merge(med.data,vital.sub)
dim(innerjoin)

as.Date(health.data$Date, format="%m/%d/%y")
days<-as.Date(health.data$Date, format="%m/%d/%y")[2]-as.Date(health.data$Date, format="%m/%d/%y")[1]

diff<-as.numeric(as.Date(health.data$Date, format="%m/%d/%y")[2]-as.Date(health.data$Date, format="%m/%d/%y")[1])

#vector data with character data
company.name <- c("Google","IBM", "HP")
class(company.name)
length(company.name)
is.character(company.name)

#numeric data
company.innovation.index<-c(0.9,0.6,0.5)
class(company.innovation.index)
is.numeric(company.innovation.index)

#integer data
company.employees <- c(80000L,250000L,70000L)
class(company.employees)
is.integer(company.employees)

#factor by default R stores in character
employees.status <- c("Permanent","Temporary","Others")
employees.status <- factor(c("Permanent","Temporary","Others"))
is.factor(employees.status)
levels(employees.status)
as.numeric(employees.status)

#create matrix matrix with 2 rows and 3 columns. 
input.matrix<-matrix(c(2,3,6,7,3,5),2,3)
input.matrix[2,1]<-"Gender"#made everything string

dim(mtcars)
head(mtcars)
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

