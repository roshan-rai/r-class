#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
med.data <-read.csv(file="CoreBillingData.csv", header=TRUE,sep=",")
vital.sub<-read.csv(file="VitalDataSubset.csv", header=TRUE,sep=",")
health.data<-read.csv(file="HealthcareData.csv", header=TRUE,sep=",")
#initializing a new column
med.data$label.values <- rep('NO DATA',nrow(med.data))
#
if(med.data$ChargesInDollars[1] > 50){
  med.data$label.values[1]<-"High Maintenance"
}
else
{
  med.data$label.values[1]<-"Low Maintenance"
}
#step 2
i<-0
for (i in 1:nrow(med.data)) {

  if(med.data$ChargesInDollars[i] > 50){
    med.data$label.values[i]<-"High Maintenance"
  }
  else
  {
    med.data$label.values[i]<-"Low Maintenance"
  }
}
med.data$label.values.ifelse<-rep('NoDATA', nrow(med.data))
ifelse(med.data$ChargesInDollars > 50, "High Maint", "Low Maint")

# create a user deinfed fuction add label
addlabel<-function(input.data)
{
  input.data$label.values <- rep('No data', nrow(input.data))
  i<-0
  for(i in 1:nrow(input.data))
  {
    if(input.data$ChargesInDollars[i]>50)
    {
      input.data$label.values[i]<-"High Maintenance"
    }
    else
    {
      input.data$label.values[i]<-"Low Maintance"
    }
    return(input.data)
  }
}












