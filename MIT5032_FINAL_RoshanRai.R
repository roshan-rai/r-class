.a<-c(9,8,9)
sd(.a)
help(sd)
9a<-9
9**10

trunc(1340.2*10**decimal.places)/10**decimal.places

CustomCVar(input.data,decimal.places)
input.data=c(1,2,3,4,5,6,7,8,9)
decimal.places=3

#question 2 considering input.data has only one column
#make function
CustomCVar <- function(input.data,decimal.places)
    {
        #compute median,mean, range
        median.input.data <- median(input.data, na.rm=TRUE)
        mean.input.data <- mean(input.data,na.rm=TRUE)
        range.input.data<-(max(input.data)-min(input.data))
        #to go through each of the input.data and see if has na
        i<-0
        
        for(i in 1:nrow(input.data))
        {
          #if na then put mean
          if(is.na(input.data[i]))
          {
            input.data[i]<-mean(input.data, na.rm=TRUE)
          }
        }
    #if mean and sd equals 0 then calculate and put the coeff var
    if(mean(input.data)==0 & sd(input.data)==0)
        {
          coeff.var <- sd(input.data)/mean(input.data)
          
          #truncate the coefficient variable to decimal place
          coeff.var <- trunc(coeff.var*10**decimal.places)/10**decimal.places
          
          #diff.input.data<-rep(0,nrow(input.data))
          diff.input.data<-0
          i<-0
          for(i in 1:nrow(input.data))
          {
            diff.input.data[i]<-(input.data[i+1]-input.data[i])
          }
          output.var<-paste("Mean:",mean.input.data, "Median: ", median.input.data,"Coeff of Variation: ",coeff.var, "Average Diff:", mean(diff.input.data, na.rm=TRUE,"Range: ", range.input.data))
          return(output.var)
        }  else {return("Both mean and sd are zero.")}
}

#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
info.data <-read.csv(file="StoreInfo-2023.csv", header=TRUE,sep=",")
trans.data<-read.csv(file="StoreTrans-2023.csv", header=TRUE,sep=",")
#get column names to see
colnames(info.data)
colnames(trans.data)

# inner join on commmon field
inner <- merge(info.data, trans.data, by = "RetailerStoreName")
#just in case its not numeric
inner$Revenue <- as.numeric(inner$Revenue)
#check
class(inner$Revenue)
#checking to see if
dim(inner)

#by group of QuarterYear
#a<-aggregate(QuarterYear~(Revenue+RetailerStoreName), data=inner, FUN=length)
#a<-aggregate(Revenue~QuarterYear, data=inner, FUN=mean)
a<-aggregate(Revenue ~ QuarterYear + Year + ProductLine + ProductType + RetailerStoreName, data = inner, FUN = sum)


# if revenue > 440000
i<-0
for(i in 1:nrow(a))
{
  if(a$Revenue[i]>4400000)#(!is.na(a$Revenue[i])>4400000)
  {
    revenue <- subset(inner, select = c("QuarterYear", "Year", "ProductLine", "ProductType", "RetailerStoreName"))
    
    #this isnt working but this is correct
    #revenue[i]<-subset(inner$Revenue[i],select=c("QuarterYear", "Year", "ProductLine", "ProductType", "RetailerStoreName"), subset=TRUE)
  }
}
dim(revenue)
dim(inner)

