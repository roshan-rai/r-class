#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")

#get working directory
getwd()

#read data
beer.data <- read.csv(file="BeerDataExample.csv", sep=",", header=TRUE)

#number of rows
nrow(beer.data)

#barplot diagram
barplot(beer.data$Brand)

#labels for the graph
labels.data <- c("Bud Light", "Busch", "Michelob", "Coors", "Miller","Natural", "Other")

#summarize data table is getting the count
summary.data <- table(beer.data$Brand)

#creating a bar plot to visualize summary of data, with x axis label
barplot(summary.data, main="Beer Preference", xlab = "Types of Beer")

#to increase the y limit, to have multiple colors,also label, title
barplot(summary.data, main="Beer Preference", xlab = "Types of Beer", ylab = "Number of Student", ylim=c(0,100), col=rainbow(7), names.arg = labels.data)

#creating a pie chart using the summary data
pie(summary.data)

#creating a pie chart to visualize summary of data, with label, color, title
pie(summary.data, main="Beer Preference", xlab = "Types of Beer", col=rainbow(7), labels = labels.data)


#Bills
# Read Telephone Bill Data
bills.data <- read.csv(file = "BillsExample.csv", sep = ",", header =TRUE)

#making histogram, setting the limit on yaxis, label for x and y, color, and main label being set
hist(bills.data$Bills, ylim = c(0,60), xlab = "Telephone Bills (in $)", ylab ="Number of customers", col = rainbow(12), main = "Telephone Bills Distribution")


# Read Housing Data
housing.data <- read.csv(file ="HousingDataExample.csv", sep = ",", header = TRUE)

# Scatter Plot with labels of x,y and main, color
plot(housing.data$Size, housing.data$Price, main ="Price vs Size Relationship", xlab = "Size (Sq. Feet)", ylab = "Price (Million $)", col = "blue")



