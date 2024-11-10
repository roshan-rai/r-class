#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
input.data <-read.csv(file="FurnitureData.csv", header=TRUE,sep=",")
#Create a subset of the dataset only having Sofa as Furniture type
sofa_subset <- input.data[input.data$FurnitureType == "Sofa", c('FurnitureType', 'Quantity', 'Month')]
# Print the subset
print(sofa_subset)
# Create a subset with Quantity greater than 5
quantity.dataset <- input.data[input.data$Quantity > 5, ]
# Print the filtered dataset
print(quantity.dataset)
#Create a subset with Quantity greater than 6 and Month is May or June
filtered.dataset <- input.data[input.data$Quantity > 6 & (input.data$Month == "May" | input.data$Month == "June"), ]
# Print the filtered dataset
print(filtered.dataset)
