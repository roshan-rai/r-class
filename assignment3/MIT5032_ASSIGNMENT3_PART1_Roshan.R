# Set working directory

setwd("/Users/eugeniagalan/Desktop/Personal/Grad School/MIT 5032/directory/")
getwd()
ceo.data <- read.csv(file = "ceo_data_pay_merged_r3000.csv", header = TRUE, sep=",")

########################################################
# Data attributes
ceo.data.columns <- colnames(ceo.data)
nrow(ceo.data)
colnames(ceo.data)
summary(ceo.data)
dim(ceo.data)

library(ggplot2)


########################################################
# ------- Replacing Industry with gsub() functions
check.subs<-unique(ceo.data$industry)
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)
unique(ceo.data$industry)








########################################################
#                Dictionary Reference
########################################################
communication <- ceo.data[ceo.data$industry == "Communication Services", ]
consumer.d <- ceo.data[ceo.data$industry == "Consumer Discretionary", ]
consumer.s <- ceo.data[ceo.data$industry == "Consumer Staples", ]
energy <- ceo.data[ceo.data$industry == "Energy", ]
financials <- ceo.data[ceo.data$industry == "Financials", ]
healthcare <- ceo.data[ceo.data$industry == "Health Care", ]
industrials <- ceo.data[ceo.data$industry == "Industrials", ]
infotechnology <- ceo.data[ceo.data$industry == "Information Technology", ]
materials <- ceo.data[ceo.data$industry == "Materials", ]
real.estate <- ceo.data[ceo.data$industry == "Real Estate", ]
utilities <- ceo.data[ceo.data$industry == "Utilities", ]



########################################################
#                Creating Subset
########################################################

fin.ceo<-subset(ceo.data, industry == "Financials" )
it.ceo<-subset(ceo.data, industry == "Information Technology" )
health.ceo<-subset(ceo.data, industry == "Health Care" )

########################################################
#              Rename Column
########################################################
SP_rank<-names(ceo.data)[names(ceo.data) == "Column1"]
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)






#################################################################################
# ------- HYPOTHESIS 1
######### Male CEO’s in the Information Technology have a higher pay ratio than 
######### Female CEOs in the Information Technology. 
#################################################################################
# Violin Plot 1
# CATEGORICAL NOMINAL M/F

colnames(it.ceo)



#### R VALUE Cramers
cor_table_1 <- table(ceo.data$ceo_gender, as.factor(ceo.data$pay_ratio_int))
cramers_v<-assocstats(cor_table_1)$cramersV
cat("Cramér's V:", cramers_v, "\n")

t_test_result <- t.test(pay_ratio ~ ceo_gender, data = df)
wilcox.test(pay_ratio ~ gender, data = df)


# Start here

ggplot(it.ceo, aes(x = ceo_gender, y = pay_ratio_int, fill = ceo_gender)) +
  geom_violin(trim = FALSE, alpha = 0.3) +
  labs(title = "CEO-Worker Salary Ratios Across IT by Gender",
       x = "Gender",
       y = "CEO-Worker Pay Ratio",
       fill = "CEO Gender") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 14),   # Adjust font size for x-axis label
        axis.title.y = element_text(size = 14)) 

summary(it.ceo$pay_ratio_int)

















#################################################################################
# ------- HYPOTHESIS 2
######### Financial companies with a higher Rusell 3000 Ranking have a lower 
######### CEO-worker salary ratio.
#################################################################################
--------------------------------------------------------------------------------
  
  #setting the working directory
  setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
ceo.data <-read.csv(file="ceo_data_pay_merged_r3000.csv", header=TRUE,sep=",")
colnames(ceo.data)
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)
fin.ceo<-subset(ceo.data, industry == "Financials" )


library(ggplot2)

# Assuming you have a data frame named 'fin.ceo'
# Replace 'fin.ceo' with the actual name of your data frame

# Convert median worker pay to integer
fin.ceo$median_worker_pay_cat <- as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))

# Categorize
fin.ceo$pay_Ratio_int <- as.numeric(sub(":.*", "", fin.ceo$pay_ratio))


# Convert median worker pay to integer
fin.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))


# Categorize
fin.ceo$median_worker_pay_cat<- ifelse(fin.ceo$median_worker_pay_cat < 5000, "below 5K",
                                       ifelse(fin.ceo$median_worker_pay_cat >= 5000 & fin.ceo$median_worker_pay_cat <= 10000, "5-10K",
                                              ifelse(fin.ceo$median_worker_pay_cat > 10000 & fin.ceo$median_worker_pay_cat <= 50000, "10-50K",
                                                     "above 50K")))


#### R value

fin.ceo$median_worker_pay<-as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))
fin.ceo$pay_Ratio_int <- as.numeric(sub(":.*", "", fin.ceo$pay_ratio))

cor_coeff_3<- cor(fin.ceo$pay_Ratio_int,fin.ceo$median_worker_pay,)
cor_coeff_2<- cor(fin.ceo$X,fin.ceo$median_worker_pay,)
cor_coeff_1<- cor(fin.ceo$X,fin.ceo$pay_Ratio_int,)

colnames(health.ceo)

fin.ceo$median_worker_pay <- as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))
#############################################
# Function to remove outliers based on IQR


fin.ceo <- na.omit(fin.ceo)

# Scatter plot without outliers
ggplot(fin.ceo, aes(y = pay_Ratio_int, x = X, color = median_worker_pay)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_smooth(method = "gam", se = FALSE, color = "tomato") +
  labs(title = "Scatter Plot of Rank vs Pay Ratio of CEO-to-Worker (Financial)",
       y = "pay_Ratio_int",
       x = "Rank",
       color = "Median Worker Pay ($)") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
----------------------------------------------------------------------------------------------------------
  #setting the working directory
  setwd("C:/Users/Roshan Rai/OneDrive - University of Oklahoma/Rclass")
#getting the working directory
getwd()
#making the data store in the variable
ceo.data <-read.csv(file="ceo_data_pay_merged_r3000.csv", header=TRUE,sep=",")
colnames(ceo.data)
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)


library(ggplot2)

# Assuming you have a data frame named 'fin.ceo'
# Replace 'fin.ceo' with the actual name of your data frame

# Convert median worker pay to integer
fin.ceo$median_worker_pay_cat <- as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))

# Categorize
fin.ceo$pay_Ratio_int <- sub(":.*", "", fin.ceo$pay_ratio)

# Scatter plot with additional elements
ggplot(fin.ceo, aes(x = median_worker_pay, y = pay_Ratio_int, color = median_worker_pay_cat)) +
  geom_point(aes(size = X), position = position_dodge(width = 0.8), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  
  
  
  labs(title = "Relationship between Median Worker Pay and CEO-to-Worker Pay Ratio (Financial Companies)",
       x = "Median Worker Pay",
       y = "CEO-to-Worker Pay Ratio",
       color = "Median Worker Pay Category") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = "bottom") +
  scale_size_continuous(name = "S&P Rank") +
  scale_color_brewer(palette = "Set1")  # Using a color palette for better visibility

expand_limits(x = c(0, 150000000), y = c(0, 150000))

































colnames(fin.ceo)



# Convert median worker pay to integer
fin.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))


# Categorize
fin.ceo$median_worker_pay_cat<- ifelse(fin.ceo$median_worker_pay_cat < 5000, "below 5K",
                                       ifelse(fin.ceo$median_worker_pay_cat >= 5000 & fin.ceo$median_worker_pay_cat <= 10000, "5-10K",
                                              ifelse(fin.ceo$median_worker_pay_cat > 10000 & fin.ceo$median_worker_pay_cat <= 50000, "10-50K",
                                                     "above 50K")))


#### R value

fin.ceo$median_worker_pay<-as.numeric(gsub("[$,]", "", fin.ceo$median_worker_pay))
fin.ceo$pay_Ratio_int <- as.numeric(sub(":.*", "", fin.ceo$pay_ratio))

cor_coeff_3<- cor(fin.ceo$pay_Ratio_int,fin.ceo$median_worker_pay,)
# -0.3608818
colnames(fin.ceo)



##################################

ggplot(fin.ceo, aes(y = median_worker_pay, x = pay_Ratio_int, color = X)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_smooth(method = "gam", se = FALSE, color = "tomato") +
  labs(title = "CEO-Worker Salary Ratios of Financial Companies",
       y = "Median Worker Pay ($)",
       x = "CEO-Worker Pay Ratio",
       color = "Russell Rank") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )

-----------------------------------------------------------------------------------------------
  
  ######## Convert 'company_cat' to a factor with custom levels
  ceo.company.data$company_cat <- factor(ceo.company.data$company_cat, 
                                         levels = c("Micro Business", "Medium Business","Large Business", "Enterprise"))


######## Financial Companies
cor_coeff_2<- cor(fin.ceo$S_P_Rank, fin.ceo$pay_ratio_int)
# -0.05953562


#### color scheme
ggplot(fin.ceo, aes(x = Column1, y = pay_ratio_int, color = Column1)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +
  geom_errorbar(aes(ymin = mean(pay_ratio_int) - sd(pay_ratio_int), ymax = mean(pay_ratio_int) + sd(pay_ratio_int)), 
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "CEO-Worker Salary Ratios of Financial Companies",
       x = "S&P Rank",
       y = "CEO-Worker Pay Ratio",
       color = "S&P Ranking") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 14),   # Adjust font size for x-axis label
        axis.title.y = element_text(size = 14)) 











#################################################################################
# ------- HYPOTHESIS 3
######### Healthcare companies have the highest median worker pay, 
######### and the lowest CEO-worker salary ratio.
##########################################################################################


colnames(health.ceo)



# Convert median worker pay to integer
health.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", health.ceo$median_worker_pay))


# Categorize
health.ceo$median_worker_pay_cat<- ifelse(health.ceo$median_worker_pay_cat < 5000, "below 5K",
                                          ifelse(health.ceo$median_worker_pay_cat >= 5000 & health.ceo$median_worker_pay_cat <= 10000, "5-10K",
                                                 ifelse(health.ceo$median_worker_pay_cat > 10000 & health.ceo$median_worker_pay_cat <= 50000, "10-50K",
                                                        "above 50K")))


#### R value

health.ceo$median_worker_pay<-as.numeric(gsub("[$,]", "", health.ceo$median_worker_pay))

cor_coeff_3<- cor( health.ceo$pay_ratio_int,health.ceo$median_worker_pay,)
# -0.3608818
colnames(health.ceo)

##################################

ggplot(health.ceo, aes(y = median_worker_pay, x = pay_ratio_int, color = S_P_Rank)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_smooth(method = "gam", se = FALSE, color = "tomato") +
  labs(title = "CEO-Worker Salary Ratios of Healthcare Companies",
       y = "Mean Worker Pay ($)",
       x = "CEO-Worker Pay Ratio",
       color = "S&P Rank") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )















## ------- HYPOTHESIS 4
######### Female CEO’s in the Top 100 companies have higher CEO-worker salary ratio 
######### than the Male CEO’s in the top 100. Companies with Male CEO’s have a 3:1
######## ratio and have a higher median work pay. 


####### Top 100 Companies
##################################


top_100 <-ceo.data[order(ceo.data$pay_ratio_int, decreasing = TRUE), ][1:100, ]
colnames(ceo.data)



####### Bottom 100 Companies
##################################
sorted_data <- ceo.data[order(ceo.data$pay_ratio_int), ]
bottom_100 <- sorted_data[1:100, ]


dim(bottom_100)

#### count of industry #####
bottom_100_count<-(c(bottom_100$industry))
top_100_count<-c(top_100$industry)

count_unique<- function(top_100_count) {
  result_table<- table (top_100_count)
  return(result_table)
}

data_count<-count_unique(top_100_count)
bottom_100$industry_count<-count_unique(bottom_100_count)


top_frame<-data.frame(
  value= c(7, 54, 8 ,2 ,4 , 4, 18, 3),
  Industry= c("Communication Services","Consumer Discretionary",'Consumer Staples','Financials','Health Care',
              'Industrials',"Information Technology","Materials")
)
ggplot(bottom_frame, aes(x = "", y = value, fill = Industry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Top 100 Companies")+
  theme_minimal()

bottom_frame<-data.frame(
  value= c(29,18, 17,12,6,5, 5,4,2,1,1),
  Industry= c('Health Care','Financials','Information Technology','Real Estate','Industrials',
              'Consumer Discretionary','Communication Services','Energy','Materials','Consumer Staples','Utilities')
)
ggplot(bottom_frame, aes(x = "", y = value, fill = Industry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Bottom 100 Companies")+
  theme_minimal()

count_unique<- function(bottom_100_count) {
  result_table<- table (bottom_100_count)
  return(result_table)
}



top_100$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", top_100$median_worker_pay))

#get R value

cor_coeff_4<- cor(top_100$median_worker_pay_cat, top_100$pay_ratio_int)
# 0.0008506226

ggplot(top_100, aes(y = median_worker_pay_cat, x = pay_ratio_int, color = ceo_gender)) +
  geom_point(size=3) +
  geom_errorbar(
    aes(ymin = mean(pay_ratio_int) - sd(pay_ratio_int),
        ymax = mean(pay_ratio_int) + sd(pay_ratio_int)),
    position = position_dodge(width = 0.8),
    width = 0.2, alpha=0.3
  ) +
  labs(
    title = "Average CEO-Worker Salary Ratios by Gender",
    x = "CEO-Worker Pay Ratio",
    y = "Median Worker Pay",
    color = "CEO Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )+
  scale_color_manual(values = c("M"="deepskyblue", "F" = "firebrick1"))











# ------- HYPOTHESIS 5
#########. Industrial companies with a higher pay ratio indicate in general a higher CEO-worker salary. 
# This shows a positive correlation relationship. 
# Additionally, CEOS in the industrial sector are heavily male with only 6% of CEOS 
# represented in the dataset being female. 

cor_coeff_5<- cor(ind.ceo$pay_ratio_int, ind.ceo$salary_cat)
#  0.4349431

ind.ceo<-subset(ceo.data, industry == "Industrials" )
colnames(ind.ceo)

ind.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", ind.ceo$median_worker_pay))
ind.ceo$salary_cat<-as.numeric(gsub("[$,]", "", ind.ceo$salary))
ind.ceo$combined_x <- paste(ind.ceo$salary_cat, ind.ceo$pay_ratio_int, sep = " - ")
unique(ceo.data$industry)


# First axis
ggplot(ind.ceo, aes(x = pay_ratio_int, y = salary_cat, color=ceo_gender)) +
  geom_point(alpha=0.7, size = 2) +
  labs(title = "Pay Ratio vs CEO-Worker Salary in Industrial Companies",
       x = "Pay Ratio",
       y = "Median Worker Salary"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)) +
  scale_color_manual(name="Gender of CEO",values = c("M" = "deepskyblue", "F" = "firebrick1"))


# Percentage of CEO
ind.ceo$ceo_gender



