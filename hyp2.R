#setting the working directory
setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
hyp.data <-read.csv(file="Hypothesis2.csv", header=TRUE,sep=",")
#check column names
colnames(hyp.data)
# print to see the payratio format
print(hyp.data$pay_ratio)
#make a new column
hyp.data$new_column <- sub(":.*", "", hyp.data$pay_ratio)
#print out the new column
print(hyp.data$new_column)
hyp.data$new_column <- as.numeric(gsub(",", "", hyp.data$new_column))
max(hyp.data$new_column)

# Scatterplot with color based on industry
plot(hyp.data$S.P.Rank, hyp.data$new_column,
     main = "Rank VS ratio", 
     xlab = "S&P Rank", 
     ylab = "Pay Ratio",
     col=blues9,
     ylim = c(0,6500),
     clim = c(0,500))

# Add the correlation line
abline(correlation_line, col = "black")

# Get the correlation coefficient
cor_coefficient <- cor(hyp.data$S.P.Rank, hyp.data$new_column)


