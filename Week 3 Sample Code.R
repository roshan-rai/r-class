# Weeks 3 Sample Codes 

# Set the working directory. 
# Note: You may want to change the path/directory  based on where you have posted the files on your computer 
setwd("/Users/XXXXX/")

# Read input data
input.data<-read.csv(file = 'SampleCatapult Data.csv', header = TRUE, sep=',')

# access column names
input.data.columns<-colnames(input.data)

#there is also a command called ncol to see how many columns there are
ncol(input.data)

#using ncol to select columns (from column 2 to whatever the last column is) 
# and specific rows
input.data[c(2:4, 8,11),(2:ncol(input.data))]

# Display data from the last two rows and columns
input.data[(nrow(input.data)-1):(nrow(input.data)),   (ncol(input.data)-1):(ncol(input.data))]

#how to display everything except certain columns, using indexing
input.data[ , -c(2:5)]

#how to display everything except certain columns, using column names
input.data[ ,c("LSL", "USL")]

#how to remove column names
input.data[ ,c("LSL", "USL")] = NULL


####accessing specific column####
input.data$ShooterName

#now storing shootername variable
shooter.name<- input.data$ShooterName

#how to see what are the unique shooter names
unique(shooter.name)

# Average of release angle
mean(input.data$ReleaseAngle)

# Average of short distance measured by inspector 1 (ignore missing values)
mean(input.data$ShotDistanceInspector1, na.rm = T)

# Display summary statistics
summary(input.data)


