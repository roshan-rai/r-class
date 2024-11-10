# Weeks 4 Sample Codes 

# Set the working directory. 
# Note: You may want to change the path/directory  based on where you have posted the files on your computer 
setwd("/Users/XXXX/")

# Read Beer Data
beer.data <- read.csv(file = "BeerDataExample.csv", sep = "," , header = TRUE)

# Beer Labels - character vector for labeling the bars plots
beer.labels <- c("Bud Light", "Busch", "Michelob",  "Coors", "Miller", "Natural", "Other")

#summary table
summary.info <- table(beer.data$Brand)

# Bar plot
barplot(summary.info, names.arg  = beer.labels, ylim = c(0,120), col = rainbow(12), 
        main = "Beer Preference", xlab = "Types of Beer", ylab = "Number of Students")
#Note : Expand the plot window to see all the labels for the corresponding bars in the plot


# Pie Chart
pie(summary.info, labels = beer.labels, col = rainbow(20), main="Pie Chart")

# Read Housing Data
housing.data <- read.csv(file = "HousingDataExample.csv", sep = ",", header = TRUE)

# Scatter Plot
plot(housing.data$Size, housing.data$Price, main = "Price vs size", xlab = "Size", ylab = "Price")   

# Compute correlation coefficient between house size and price 
cor(housing.data$Size, housing.data$Price)     


# mean
mean(med.data$AgeInYears)

# median
median(med.data$AgeInYears)

# range
diff(range(med.data$AgeInYears))

# Standard deviation
sd(med.data$AgeInYears)
