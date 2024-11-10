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
table(ceo.data$industry)


library(ggplot2)
# ------- Replacing Industry with gsub() functions
########################################################
check.subs<-unique(ceo.data$industry)
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)
unique(ceo.data$industry)
ceo.data$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", ceo.data$median_worker_pay))


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
#                Summary
########################################################
summary(communication)
summary(consumer.d)
summary(consumer.s)
summary(energy)
summary(financials)
summary(healthcare)
summary(industrials)
summary(infotechnology)
summary(materials)
summary(real.estate)
summary(utilities)


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

##################################
# Data Inference -- Pie Charts


##################################
####### Top 100 Companies
##################################


top_100 <-ceo.data[order(ceo.data$pay_ratio_int, decreasing = TRUE), ][1:100, ]
colnames(ceo.data)

head(top_100)


##################################
####### Bottom 100 Companies
##################################
sorted_data <- ceo.data[order(ceo.data$pay_ratio_int), ]
bottom_100 <- sorted_data[1:100, ]


dim(bottom_100)


##################################
####### Pie Charts
##################################
#### count of industry #####
bottom_100_count<-(c(bottom_100$industry))
top_100_count<-c(top_100$industry)

count_unique<- function(top_100_count) {
  result_table<- table (top_100_count)
  return(result_table)
}

top_frame<-data.frame(
  value= c(7, 54, 8 ,2 ,4 , 4, 18, 3),
  Industry= c("Communication Services","Consumer Discretionary",'Consumer Staples','Financials','Health Care',
              'Industrials',"Information Technology","Materials")
)
ggplot(top_frame, aes(x = "", y = value, fill = Industry)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Top 100 Companies")+
  theme_minimal()



data_count<-count_unique(top_100_count)
bottom_100$industry_count<-count_unique(bottom_100_count)




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





#################################################################################
# ------- HYPOTHESIS 1
######### Male CEO’s in the Information Technology have a higher pay ratio than 
######### Female CEOs in the Information Technology. 
#################################################################################
# Violin Plot 1
# CATEGORICAL NOMINAL M/F

summary(it.ceo)
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
######### Financial companies with a higher S&P Ranking have a lower 
######### CEO-worker salary ratio.
#################################################################################


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



# ------- Replacing Industry with gsub() functions
check.subs<-unique(ceo.data$industry)
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)
unique(ceo.data$industry)


#                Creating Subset
fin.ceo<-subset(ceo.data, industry == "Financials" )


#              Rename Column
SP_rank<-names(ceo.data)[names(ceo.data) == "Column1"]
ceo.data$industry<-gsub("%20", " ", ceo.data$industry)

######### Financial companies with a higher Russell 3000 Ranking have a lower CEO-worker salary ratio.
fin.ceo$pay_ratio_int <- as.numeric(sub(":.*", "", fin.ceo$pay_ratio))

######## Financial Companies
cor_coeff_2<- cor(fin.ceo$X, fin.ceo$pay_ratio_int)
# -0.05953562


#### color scheme
ggplot(fin.ceo, aes(x = X, y = pay_ratio_int, color = X)) +
  geom_point(position = position_dodge(width = 0.8), size = 4) +
  geom_errorbar(aes(ymin = mean(pay_ratio_int) - sd(pay_ratio_int), ymax = mean(pay_ratio_int) + sd(pay_ratio_int)), 
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(title = "CEO-Worker Salary Ratios of Financial Companies",
       x = "Russell 3000 Rank",
       y = "CEO-Worker Pay Ratio",
       color = "R 3000 Ranking") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.title.x = element_text(size = 14),   # Adjust font size for x-axis label
        axis.title.y = element_text(size = 14)) + correlation_line

correlation_line <- geom_smooth(method = "lm", se = FALSE, color = "red")



#################################################################################
# ------- HYPOTHESIS 3
######### Healthcare companies have the highest median worker pay, 
######### and the lowest CEO-worker salary ratio.
#################################################################################

## Categorical Ordinal R 3000 rank

summary(health.ceo)
colnames(health.ceo)



# Convert median worker pay to integer
#####################################################################################################
health.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", health.ceo$median_worker_pay))


# Categorize
#####################################################################################################
cut_points <- c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, Inf)
labels <- c("Below 10k", "10-20k", "20-30k", "30-40k", "40-50k", "50-60k", "60-70k", "70-80k", "Above 80K")

health.ceo$median_worker_pay_new <- cut(health.ceo$median_worker_pay, breaks = cut_points, labels = labels, include.lowest = TRUE)

table(health.ceo$median_worker_pay_new)

#### R value
###########################################################
health.ceo$median_worker_pay<-as.numeric(gsub("[$,]", "", health.ceo$median_worker_pay))

cor_coeff_3<- cor( health.ceo$pay_ratio_int,health.ceo$median_worker_pay,)
# -0.3608818


##################################
# Plot of Healthcare Companies
##################################

ggplot(health.ceo, aes(y = median_worker_pay_new, x = pay_ratio_int, color = S_P_Rank)) +
  geom_point(shape = 15,stat = "identity", position = position_dodge(width = 0.8),  size = 6) +
  labs(title = "CEO-Worker Salary Ratios of Healthcare Companies",
       subtitle = "By R 3000 Rank",
       y = "Median Worker Pay ($)",
       x = "CEO-Worker Pay Ratio",
       color = "R 3000") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) 

#### Scatter Plot


ggplot(health.ceo, aes(y = median_worker_pay, x = pay_ratio_int, color = S_P_Rank)) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  geom_smooth(method = "gam", se = FALSE, color = "tomato") +
  labs(title = "CEO-Worker Salary Ratios of Healthcare Companies",
       subtitle = "By R 3000 Rank",
       y = "Mediann Worker Pay ($)",
       x = "CEO-Worker Pay Ratio",
       color = "R 3000") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )



################################################################################################################
## ------- HYPOTHESIS 4
######### Female CEO’s in the Top 100 companies have higher CEO-worker salary ratio 
######### than the Male CEO’s in the top 100. Companies with Male CEO’s have a 3:1
######## ratio and have a higher median work pay. 
################################################################################################################





#get R value

cor_coeff_4<- cor(top_100$median_worker_pay_cat, top_100$pay_ratio_int)
# 0.0008506226


#### Plot Scatterplot
######################
ggplot(top_100, aes(x = median_worker_pay_cat, y = pay_ratio_int, fill = ceo_gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black") +
  labs(
    title = "Average CEO-Worker Salary Ratios by Gender",
    x = "Median Worker Pay",
    y = "CEO-Worker Pay Ratio",
    fill = "CEO Gender"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_fill_manual(values = c("M" = "deepskyblue", "F" = "firebrick1"))

unique(top_100$industry)

### With encircle



industry_colors <- c(
  "Consumer Staples" = "red",
  "Consumer Discretionary" = "green",
  "Communication Services" = "blue",
  "Information Technology" = "orange",
  "Industrials" = "purple",
  "Health Care" = "brown",
  "Materials" = "cyan",
  "Financials" = "pink"
)

ggplot(top_100, aes(x = median_worker_pay_cat, y = pay_ratio_int, color = industry)) +
  geom_count(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), size = 3) +
  
  geom_encircle(aes(x = median_worker_pay_cat, y = pay_ratio_int, group = ceo_gender, color = ceo_gender), 
                data = NULL, expand = 0.01, size = 1.5, linetype = "dashed") +
  
  labs(
    title = "Top 100 CEO-Worker Salary Ratios by Gender",
    x = "Median Worker Pay",
    y = "CEO-Worker Pay Ratio",
    color = "Industry"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  ) +
  scale_color_manual(
    values = industry_colors,
    name = "Industry",
    breaks = names(industry_colors),
    labels = names(industry_colors)
  ) +
  guides(color = guide_legend(title = "Industry"))


###########################################################################################
# ------- HYPOTHESIS 5
#########. Industrial companies with a higher pay ratio indicate in general a higher CEO-worker salary. 
# This shows a positive correlation relationship. 
# Additionally, CEOS in the industrial sector are heavily male with only 6% of CEOS 
# represented in the dataset being female. 
###########################################################################################

cor_coeff_5<- cor(ind.ceo$pay_ratio_int, ind.ceo$salary_cat)
#  0.4349431

ind.ceo<-subset(ceo.data, industry == "Industrials" )
colnames(ind.ceo)

ind.ceo$median_worker_pay_cat<-as.numeric(gsub("[$,]", "", ind.ceo$median_worker_pay))
ind.ceo$salary_cat<-as.numeric(gsub("[$,]", "", ind.ceo$salary))
ind.ceo$combined_x <- paste(ind.ceo$salary_cat, ind.ceo$pay_ratio_int, sep = " - ")
unique(ceo.data$industry)


# Plot
#######################
ggplot(ind.ceo, aes(x = pay_ratio_int, y = salary_cat, color = ceo_gender)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(aes(col=ind.ceo$ceo_gender), method="lm", se=F, alpha = 0.5) +
  geom_count(position = position_dodge(width = 0.8), show.legend = FALSE, size=2) + 
  labs(title = "Pay Ratio vs CEO-Worker Salary in Industrial Companies",
       x = "Pay Ratio",
       y = "Median Worker Salary") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)) +
  scale_color_manual(name = "Gender of CEO", values = c("M" = "deepskyblue", "F" = "firebrick1"))



##### ensure ggplot2 available

install.packages("ggplot2")
library(ggplot2)

##### Another analysis
##### Bubble Plot
#######################
ggplot(ind.ceo, aes(x = pay_ratio_int, y = salary_cat, color = ceo_gender, size = after_stat(n))) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(col=manufacturer), method="lm", se=F)+
  labs(title = "Pay Ratio vs CEO-Worker Salary in Industrial Companies",
       x = "Pay Ratio",
       y = "Median Worker Salary",
       size = "Count") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14)
  ) +
  scale_color_manual(name = "Gender of CEO", values = c("M" = "deepskyblue", "F" = "firebrick1")) +
  scale_size_continuous(name = "Count", range = c(2, 10)) +
  guides(size = "legend", color = "legend")




# Percentage of CEO
ind.ceo$ceo_gender



