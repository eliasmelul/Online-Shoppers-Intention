library(tidyverse)
#####         Data Understanding          #####

online_shoppers_intention <- read.csv("~/MQM/Term 2 - Fall 1/Data Science for Business/Term Project/online_shoppers_intention.csv")
# After importing the online_shoppers_intention dataset, we first investigate the data
online <- online_shoppers_intention

summary(online)
# Things that stand out:
  # 1. Some duration times are negative. *Must look into it/alter
  # 2. Bounce rates in decimal terms, not "percentage"
  # 3. Product related duration stands out as being the most active and long lasting visits (Good bc that's where benefits will come from: selling product)
  # 4. There are 13 types of Browsers (from 1 to 13)
  # 5. There are 9 unique regions (1 thru 9)
  # 6. There are 20 types of traffic (1 thru 20)
  # 7. There are 3 types of visitors (New, Returning and Other)
  # 8. Revenue is a logical variable (whether revenue will be generated or not T/F) *
  # 9. There's a Weekend logical variable indicating if it was a weekend day or not (T/F) *
  # 10. The months in which the 12330 sessions are collected are: May to December, and February and March
  # 11. Administrative, Informational and Product Related Vlauea are unclear. Are these just specific sites? *

# 1. Durations times are negative. How many?
online %>%
  filter(Administrative_Duration < 0) %>%
  tally()
online %>%
  filter(Informational_Duration <0) %>%
  tally()
online %>%
  filter(ProductRelated_Duration <0) %>%
  tally()
## Seems like there are exactly 33 records with a negative duration value in the three duration categories.
## Suspicious that the three types of duration have the same number of rows with negative durations. Are they the same?
online %>%
  filter(Administrative_Duration < 0 | Informational_Duration <0 | ProductRelated_Duration <0) %>%
  tally()
# Seems like the same records hold the negative values. Let's check it out
View(online[online$ProductRelated_Duration <0 | online$Administrative_Duration <0 | online$Informational_Duration < 0,])
# About the records with the negative durations:
  # They all seems to have ALL values of duration negative, meaning that spent a total of 0 "real" time in the website.
  ## This indicated that we should drop these records since they don't provide any usefull information
  # All the records don't collect revenue (Revenue is FALSE)
  # The VisitorType is always a Returning Visitor

### Drop the 33 records associated with empty sessions
online <- subset(online, Administrative_Duration >= 0)
# If we now run the summary() command on our cleaner dataset, we will see that the minimum value of the durations is 0

# 11. What about Administrative, Product Related and Informational Values. What are these?
hist(online$Administrative) # Range of values 0 to 25. Highly concentrated at 0
hist(online$Informational) # Range of values 0 to 20. Highly concentrated at 0 with almost no deviation
hist(online$ProductRelated) # Range of values 0 to 600. Highly concentrated in 0-40 range.

# It looks as if these numbers are counts for the number of websites visited in each session depending on the type.
# If this hypotesis were to be true, we would expect that, on average, the duration of a category increases with the category's value
# Example: Product Related durations versus productrelated value
ggplot(online, aes(x = ProductRelated, y = ProductRelated_Duration/60)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Duration vs Website Count") +
  xlab("Product Related Website Count") +
  ylab("Duration (minutes)")+
  theme_bw()
cor(online$ProductRelated, online$ProductRelated_Duration)
# As we can observe from the graph, as ProductRelated value increases, so does the duration, and the correlation is very strong (0.86)
# The distribtions shown by the hisrograms make sense now, with most people visiting the website for a short time and for a few number of websites

### What are other things we would like to change before further analyzing?
# Potentially, we might want to change Revenue to binary classification to build a logistic regression model



#####         Exploratory Data Analysis         #####
# Now that we understand the data, lets look into every specfic column and what it can convey

#####1. EDA: Revenue #####
#library(scales)
Revs <- data.frame(Revenue = online$Revenue,
                   Total = 1)
numLoss <- nrow(subset(online, Revenue == FALSE))
numTOT <- nrow(online)
cutoff <- numLoss/numTOT

ggplot(Revs, aes(x = Total, fill = reorder(Revenue, -Revenue)))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Percent Revenue")+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept = cutoff, col = "black", linetype = 2)+
  annotate("text", 0.85, cutoff, vjust = -1.5, label = "15.53%", size = 4, col = "orangered4")+
  annotate("text", 0.85, cutoff, vjust = 2, label = "84.47%", size = 4, col = "seagreen")+
  ggtitle("Percent Sessions that culminate \nin Revenue-Generating Operations")+
  guides(fill=guide_legend(title="Revenue"))

# Here, we can observe that most of the sessions don't lead to revenue generating activities. 
# Only about 16% of sessions end up leading to Revenue stream contribution

#####2. EDA: Weekend          #####
#Since weekend has the same setup as Revenue, we will copy what we did with Revenue and use it again for Weekend
Wknd <- data.frame(Weekend = online$Weekend,
                   Total = 1)
numWknd <- nrow(subset(online, Weekend == TRUE))
numTOT <- nrow(online)
cutoff_wknd <- numWknd/numTOT

ggplot(Wknd, aes(x = Total, fill = Weekend))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Percent Weekend")+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept = cutoff_wknd, col = "black", linetype = 2)+
  annotate("text", 0.75, cutoff_wknd, vjust = 2, label = paste(round(cutoff_wknd*100,2), "%"), size = 4, col = "seagreen")+
  annotate("text", 0.75, cutoff_wknd, vjust = -1.5, label = paste(round(100-(cutoff_wknd*100),2), "%"), size = 4, col = "orangered4")+
  ggtitle("Percent Sessions that are \nin Weekends")

# Here, we can observe that online shopping occurs predominantly during weekdays, not only in terms
# of aggregate sessions, but also in terms of sessions per weekday

# However, this does not mean that the ratio of revenue is less or more during weekends. Maybe online weekend
# shoppers are more prone to purchasing items. Lets investigate

# During weekends, what percent of sessions end up with revenue stream?
weekrev_matrix <- online%>%
  group_by(Weekend, Revenue) %>%
  tally()
weekendRev_perc = round(100*weekrev_matrix[4,3]/sum(weekrev_matrix[3:4,3]),2)
weekdayRev_perc = round(100*weekrev_matrix[2,3]/sum(weekrev_matrix[1:2,3]),2)
weekendNA_perc = round(100*weekrev_matrix[3,3]/sum(weekrev_matrix[3:4,3]),2)
weekdayNA_perc = round(100*weekrev_matrix[1,3]/sum(weekrev_matrix[1:2,3]),2)

Percs_table <- matrix(c(weekdayNA_perc, weekdayRev_perc, weekendNA_perc, weekendRev_perc), ncol = 2, byrow = TRUE)
rownames(Percs_table) <- c("Weekday", "Weekend")
colnames(Percs_table) <- c("No Revenue", "Revenue")
print(Percs_table)
# As we can observe from the table, Weekends do have a higher revenue rate than weekdays, indicating that
# weekend customers are more likely to buy something than weekday. However, the quantity (and relative profit margin)
# generated by each session might not be equal. So we ignore all questions regarding magnitude of revenue

##### 3. EDA: Visitor Type          #####
visitor <- data.frame(VisitorType = online$VisitorType,
                   Total = 1)
numNew <- nrow(subset(online, VisitorType == "New_Visitor"))
numOther <- nrow(subset(online, VisitorType == "Other"))
numReturn <- nrow(subset(online, VisitorType == "Returning_Visitor"))
numTOT <- nrow(online)
percNew <- numNew/numTOT
percOther <- numOther/numTOT
percReturn <- numReturn/numTOT

ggplot(visitor, aes(x = Total, fill = VisitorType))+
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  ylab("Cumulative Percent Visitor Type")+
  scale_y_continuous(labels = scales::percent)+
  geom_hline(yintercept = percReturn, col = "darkblue", linetype = 2)+
  geom_hline(yintercept = percReturn+percOther, col = "black", linetype = 5)+
  annotate("text", 0.8, percReturn, vjust = 1.8, label = paste(round(percReturn*100,2), "%"), size = 4, col = "darkblue")+
  annotate("text", 1.25, percReturn+percOther, vjust = 1.3, label = paste(round(percOther*100,2), "%"), size = 4, col = "darkgreen")+
  annotate("text", 0.8, percReturn, vjust = -1.2, label = paste(round(percNew*100,2), "%"), size = 4, col = "darkred")+
  ggtitle("Types of Visitors")

# Here, we learn that new visitors only account for about 14% of sessions; instead, returning visitors account for 86% of the sessions.
# Which type of customer is more prone to purchasing? 

# Since the 'Other' category of the sessions are few and irrelevant, and the 'Other' category
# doesnt convey any information, we will ignore it to answer this question.

visitrev_matrix <- online%>%
  filter(VisitorType != "Other")%>%
  group_by(VisitorType, Revenue) %>%
  tally()
returnRev_perc = round(100*visitrev_matrix[4,3]/sum(visitrev_matrix[3:4,3]),2)
newRev_perc = round(100*visitrev_matrix[2,3]/sum(visitrev_matrix[1:2,3]),2)
returnNA_perc = round(100*visitrev_matrix[3,3]/sum(visitrev_matrix[3:4,3]),2)
newNA_perc = round(100*visitrev_matrix[1,3]/sum(visitrev_matrix[1:2,3]),2)

visit_table <- matrix(c(newNA_perc, newRev_perc, returnNA_perc, returnRev_perc), ncol = 2, byrow = TRUE)
rownames(visit_table) <- c("New Customer", "Returning Customer")
colnames(visit_table) <- c("No Revenue", "Revenue")
print(visit_table)


# Even though the majority of the sessions occur from returning visitors, new customers' sessions
# have a significantly higher chance of generating revenue than returning customers. It might
# show a tendency of returning customers to just search online on their free time but not purchasing anything.
# Additionally, new customers bring more value than simply the revenue generated, since a good
# purchase might lead to more purchases by the new customer.

# It's entirely possible that we may include variables such as Browser, Region, and OperatingSystem,
# in our models, but exploring this data doesnt increase interest past a summary command
summary(online)

#write the target as online$Variable
# use only with discrete variables
cumRev <- function(target){
  ## First, print the ggplot without specifications and nice notations
  picturethis <- data.frame(Y = target,
                        Total = 1)
  
  g <- ggplot(picturethis, aes(x = Total, fill = reorder(Y, -Y)))+
    geom_bar(aes(y = (..count..)/sum(..count..)))+
    theme_bw()+
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    ylab("Percent Revenue")+
    scale_y_continuous(labels = scales::percent)+
    guides(fill=guide_legend(title="Distinct Values"))
  print(g)
  
  sub1 <- data.frame(target, online$Revenue)
  names(sub1) <- c("target", "Revenue")
  temp_matrix <- sub1 %>%
    group_by(target, Revenue)%>%
    tally()
  comb_df <- data.frame()
  solo <- unique(temp_matrix$target)
  for (i in unique(temp_matrix$target)){
    med1 <- subset(temp_matrix, target == i)
    rev1 <- med1[2,"n"]
    denom1 <- sum(med1[,"n"], na.rm = TRUE)
    ratiorev <- round(100*(rev1/denom1),2)
    ratioloss <- round((100 - ratiorev), 2)
    temp_df <- data.frame(ratiorev, ratioloss)
    rownames(temp_df) <- c(i)
    colnames(temp_df) <- c("Revenue", "No Revenue")
    comb_df <- rbind(comb_df, temp_df)
  }
  return(comb_df)
}

#####4. EDA: Traffic Type #####
cumRev(online$TrafficType)
#####5. EDA: Region #####
cumRev(online$Region)
#####6. EDA: Browser #####
cumRev(online$Browser)
#####7. EDA: Operating Systems #####
cumRev(online$OperatingSystems)
#####8. EDA: Month #####
cumRev(online$Month)
# While not all analysis in cumRev function works with Month, we notive that February has a rather
# rare extreme low ratio of Rev / Sessions as compared to the rest. 
summary(online$Month)
# As we can see, Frebruary has a significant low number of sessions. June is also low. 
# It seems like Sep, Oct, Jul, and Aug are borderline. Actually, these months seem to have 
# large-ish revenue ratios, but low n. What the average revenue ratio?
meanbar <- cumRev(online$Month)
mean(meanbar$Revenue)
# Yes, the months with unusually high percentages, with November as an exception, have low N

#####9. EDA: Special Day #####
# Since 'Special Day' only takes 6 values...
cumRev(online$SpecialDay)
# The data shows that the percent of Revenue generating visits is significantly higher when
# it is NOT a Special Day (SpecialDay = 0), and significantly lower otherwise.

#####10. EDA: Administrative + Duration #####
# Histogram of Administrative color coded for Revenue
ggplot(online, aes(x = Administrative, fill = Revenue))+
  geom_histogram(binwidth = 1)+
  theme_bw()
#What's the median and range?
summary(online$Administrative) 
#What about the total count per integer of Admnistrative?
summary(as.factor(online$Administrative))

# Histogram of Administrative Duration
# Find the mean of the administrative duration to set a logical bin width
summary(online$Administrative_Duration)
# OK so about 80. But 60 more convenient since every bin would equate a minute
ggplot(online, aes(x = Administrative_Duration, fill = Revenue))+
  geom_histogram(binwidth = 60)+
  theme_bw()+
  xlim(c(0,1000))+
  ylim(c(0,3000))
# Since we could barely take away any observations from the full histogram,
# we changed the x and y axis to show us a histogram regresenting over 99% of the data

#As we can observe, most website visits last no more than 4 minutes. However, it is not uncommon
# to see the administrative duration visit be up to 8 minutes. There are occurrances that are more

#####11. EDA: Informational + Duration #####
# Histogram of Informational color coded for Revenue
ggplot(online, aes(x = Informational, fill = Revenue))+
  geom_histogram(binwidth = 1)+
  theme_bw()
#What's the median and range?
summary(online$Informational) 
#What about the total count per integer of Admnistrative?
summary(as.factor(online$Informational))
# Run the cumRev function to see a matrix of the Revenue percentages
cumRev(online$Informational)
# There are some standouts in Informational Revenue generating websites. This is, websites that have
# higher revenue generating occurance rates. 
##  When Informational is 0, we assume that no informational website has been visited. If it is 
##  otherwise, then that website has a lower than usual revenue margin
##  Informational == 5, 8, 9, and 12 have noticeably high values of Revenue. However, we can 
##  attribute this number to the Law of Small Numbers, since their relative mass is so small.

# Histogram of Informational Duration
# Whats the mean of the informational duration?
summary(online$Informational_Duration)
# Seen as the mean is almost 35 and the range is over 2500, we expect a highly skewed histogram 
# with tendency to 0
ggplot(online, aes(x = Informational_Duration, fill = Revenue))+
  geom_histogram(binwidth = 60)+
  theme_bw()+
  xlim(c(0,1000))+
  ylim(c(0,1000))

#Similar to administrative. 

#####12. EDA: ProductRelated + Duration #####
# Histogram of ProductRelated color coded for Revenue
ggplot(online, aes(x = ProductRelated, fill = Revenue))+
  geom_histogram(binwidth = 1)+
  theme_bw()
#What's the mean, median and range?
summary(online$ProductRelated) 
# as we can observe, the majority of the product related visits are from 
# the products that sell the most. Or so we could assume. 

# Histogram of ProductRelated Duration
# Whats the mean of the informational duration?
summary(online$ProductRelated_Duration)
# Mean is 1200, median is 600 and max is 64000
ggplot(online, aes(x =ProductRelated_Duration, fill = Revenue))+
  geom_histogram(binwidth = 60)+
  theme_bw()+
  xlim(c(0,15000))+
  ylim(c(0,900))

#this histogram shows that for the vast majority of purchases, a session product related is open for
# up to 50 minutes (3000 seconds). We would expect to see an S shaped curte to denote
# the relationship betweem duration and revenue. At first a customer is simply browsing. 
# After a certain threshold, the customer is considering the purchase. # If the customer
# stays in the same website for too long, it probably means that he is second-guissing him/herself.

#####13. EDA: Bounce Rates #####
summary(online$BounceRates)
#####14. EDA: Exit Rates #####
summary(online$ExitRates)


