# Set the working directory to the location of your data file
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")

# Load the data from the CSV file
shoppers.data <- read.csv(file="ShoppersData.csv", sep=",", header=TRUE)

# View the first few rows of the data to understand its structure
head(shoppers.data)

# Summary statistics for the length of time spent on the website
summary(shoppers.data$TimeInMins)

# Histogram for the length of time spent on the website
hist(shoppers.data$TimeInMins, main="Length of Time Spent on Website", xlab="Time (minutes)",ylab="Number of customer",ylim = c(0,20), col="lightblue")

# Summary statistics for the number of pages viewed during each transaction
summary(shoppers.data$PagesViewed)

#labeling labels.data <- c(1,50,1)

# Histogram for the number of pages viewed
hist(shoppers.data$PagesViewed, main="Number of Pages Viewed", xlab="Pages Viewed",ylab="Number of customer", col="lightgreen", ylim = c(0,12))

# Summary statistics for the amount spent per transaction
summary(shoppers.data$AmountSpentInDollars)

min_value <- min(shoppers.data$AmountSpentInDollars)
max_value <- max(shoppers.data$AmountSpentInDollars)

# Set the scale and adjust the bin width and Histogram for the amount spent per transaction
hist(shoppers.data$AmountSpentInDollars, 
     main="Amount Spent per Transaction", 
     xlab="Amount(dollars)", 
     ylab="number of people",
     col="lightcoral",
     xlim=c(0,200)     
     )  

# Calculate the number of transactions per day
cdata <- aggregate(Customer ~ Day, data = shoppers.data, FUN = length)
colnames(cdata) <- c("Day", "TransactionCount")


# Calculate the total amount spent per day
adata <- aggregate(AmountSpentInDollars ~ Day, data = shoppers.data, FUN = sum)
colnames(adata) <- c("Day", "TotalAmountSpent")

# Calculate the mean amount spent per day
mdata <- aggregate(AmountSpentInDollars ~ Day, data = shoppers.data, FUN = mean)
colnames(mdata) <- c("Day", "MeanAmountSpent")

# Combine the results into a single data frame
combined_data <- merge(cdata, adata, by = "Day")

#combine the results everything
combined_data <- merge(combined_data, mdata, by = "Day")

# Print the combined results
print(combined_data)

# Print the mean results print(mdata)

#calculating the number of customer per browser
cdata <- aggregate(Customer~Browser, data = shoppers.data, FUN = length)
colnames(cdata)<-c("Browser","Customers")
#print(cdata)

#calculating the sum of total money spent on each browser
adata <- aggregate(AmountSpentInDollars~Browser,shoppers.data, sum)
colnames(adata)<-c("Browser", "$Total spent")
#merging to make one table
combined_data <- merge(cdata, adata, by="Browser")

#calculting the average money spent on each browser
mdata <- aggregate(AmountSpentInDollars~Browser, shoppers.data, mean)
colnames(mdata) <- c("Browser", "Average spent")

#merging to make one table
combined_data<-merge(combined_data,mdata, by="Browser")

#printing the table
print(combined_data)

#scarterplot for time and money with x and y labels
plot(shoppers.data$TimeInMins, shoppers.data$AmountSpentInDollars, main = "Time and Money Spent", xlab = "Time Spent on Website (In Min)", ylab = "Dollar spent (in $)", col = "blue", xlim = c(0, 35), ylim = c(0, 200))

#scaterplot for ages viewed and money spend with x and y lables
plot(shoppers.data$PagesViewed, shoppers.data$AmountSpentInDollars, main="Number of website and Money spent", xlab="Pages Viewed", ylab="Money spent (in $)", col="red")

#scarterplot for pages viewed and time spent with x and y axis with labels
plot(shoppers.data$PagesViewed, shoppers.data$TimeInMins, main="Time Pages viewed", xlab = "Pages Viewed", ylab="Time (in Mins)", col="green ")
