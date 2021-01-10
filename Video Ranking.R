#cleaning the data enviroment and setting options and working directory
rm(list=ls())
options(scipen = 999)
setwd("C:/Users/Omer Gralnik/Desktop/PlayStudios")
list.files()

#loading the "video_count" dataset
video_count <- read.csv("video_count.csv", header = TRUE)

#let's check the data's summary and sturcture
summary(video_count)
str(video_count)

#verifing there are no missing values
colSums(is.na(video_count))

#the date&video_id variables' type needs to be changed
video_count$date <- as.Date(video_count$date)
video_count$video_id <- as.factor(video_count$video_id)

#let's check the new data's summary and sturcture
summary(video_count)
str(video_count)

#how many days are in the dataset?
max(video_count$date)-min(video_count$date) #approximatley half a year

#Descriptive Statistics of the data
quant <- seq(0,1,by = 0.1) 
quantile(video_count$count,probs = quant)


#loading relevant libraries
library(dplyr)
library(ggplot2)
library(lubridate)

#generating some visualizations of the data
ggplot(video_count)+ aes(count)+
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 30)+
  geom_density(alpha=.2, fill="#FF6666")+
  labs(title =  'Count of views per day histogram')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_vline(aes(xintercept = mean(count)),col='red',size=1)+
  geom_text(aes(label=round(mean(count),0),y=0,x=mean(count+3)),
            vjust=-1,col='red',size=4)

ggplot(data = video_count, aes(x = date, y = count)) +
    geom_bar(stat = "identity", fill = "purple", width = 0.3)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d")


#Loading of the 'video_features' dataset
video_features <- read.csv("video_features.csv", header = TRUE, stringsAsFactors = TRUE)

#let's check the data's summary and sturcture
summary(video_features)
str(video_features)

#verifing there are no missing values
colSums(is.na(video_features))

#the date&video_id variables' type needs to be changed
video_features$video_upload_date <- as.Date(video_features$video_upload_date)
video_features$video_id <- as.factor(video_features$video_id)

#let's check the new data's summary and sturcture
summary(video_features)
str(video_features)

#the quality levels needs to be ordered correctly
video_features$video_quality <- ordered(video_features$video_quality, levels = c('240p','360p','480p','720p','1080p'))

#how many videos are in each quality by each language
by(video_features$video_quality, video_features$video_language, summary)

#let's plot this information
ggplot(video_features, aes(x = video_quality, fill = video_quality))+
  geom_bar()+
  facet_wrap(video_features$video_language)+
  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+
  labs(title = '# of videos by each quality level')+
  theme(plot.title = element_text(hjust = 0.5))

#combinig this two tables together
total <- merge(video_count, video_features, by = 'video_id')

#generating a variable determines how many days have passed since the video was uploaded
total$days_passed <- as.numeric(total$date - total$video_upload_date)

#what is the correlation between the number of days passed and the total views of the video
cor(total$days_passed, total$count) #negative correlation

#what is the distribution of video_length variable
quantile(video_features$video_length,probs = quant)
by(total$video_length, total$video_language, mean)
ggplot(video_features, aes(x = video_length))+
  geom_histogram(bins = 30, binwidth = 0.2)+
  facet_grid(video_features$video_language)

#what is the correlation between video length and the total views
cor(total$video_length, total$count) #strong negative correlation

ggplot(total, aes(x = video_length, y = count))+
  geom_bar(stat = 'identity')+
  facet_grid(total$video_language~total$video_quality)


#the correlation is the same even when the video just uploaded
cor(subset(total, days_passed == 0)$video_length,subset(total, days_passed == 0)$count)

#what is the average views count by video quality
by(total$count, total$video_quality, mean) #seems that the higher the quality there are more views 

#let's see if there is any difference between the languages type
ggplot(total, aes(x = video_quality, y = count, fill = video_quality))+
  geom_bar(stat = 'identity')+
  facet_wrap(total$video_language)+
  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+
  labs(title = 'Total views by video quality',
       y = 'total views')+
  theme(plot.title = element_text(hjust = 0.5))

#is there any difference between the languages regarding total views
ggplot(total , aes(x = video_language, y = count, fill = video_language))+
  geom_bar(stat = 'identity', width = 0.5)+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")+
  labs(title = 'Total views by video language',
       y = 'total views')+
  theme(plot.title = element_text(hjust = 0.5))
  
#What is the correlation between the total views and the video's quality
total$quality <- as.numeric(total$video_quality)
cor(total$quality, total$count)


#What is the average view per day and total views for each video
views_by_id <- video_count %>% group_by(video_id) %>% summarise(Avg_views = round(mean(count),0),
                                                                Total_views = sum(count))

#let's add this data to the "video_features" dataframe
video_features <- merge(video_features, views_by_id, by = 'video_id')

#checking again the summary of this dataset
summary(video_features)

#seems that the distribution of the total views variable is normal
hist(video_features$Total_views,
     breaks = seq(2000,16000,2000),
     main = 'Total views histogram',
     xlab = 'Total views',
     col = 'pink')
abline(v=10000,col="red", lwd = 4)

quantile(video_features$Total_views, quant)



#what are the characteristics of the videos with 10000+ total views
ggplot(subset(video_features, Total_views > 10000), aes(x = video_quality, fill = video_quality))+
  geom_bar()+
  facet_wrap(subset(video_features, Total_views > 10000)$video_language)+
  scale_fill_brewer(palette="Dark2")+
  theme_minimal()+
  labs(title = '# of videos with total views > 10000')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(video_features, Total_views > 10000) , 
       aes(x = video_language, fill = video_language))+
  geom_bar(width = 0.5)+
  theme_minimal()+
  scale_fill_brewer(palette="Dark2")+
  scale_y_continuous(breaks = seq(0,14,2))+
  labs(title = 'Total videos by video language with total views > 10000')+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(subset(video_features, Total_views > 10000), 
       aes(x = video_length, fill = video_language))+
  geom_histogram(bins = 30, )+
  facet_wrap(subset(video_features, Total_views > 10000)$video_language)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(15,24,1))+
  theme(panel.spacing = unit(1, "lines"),plot.title = element_text(hjust = 0.5))+
  labs(title = "Video's length histogram")+
  scale_fill_brewer(palette="Dark2")

#in order to estimate the linear model correctly the "video_quality" variable needs to be adjusted
total$video_quality <- factor( total$video_quality , ordered = FALSE )

#Estimation of linear model contains the relevant variables
summary(lm(count ~ days_passed + video_length + video_quality, data = total))

library(stargazer)

#export the estimation's results to a table
#stargazer(lm(count ~ days_passed + video_length + video_language + video_quality, data = total),
#          lm(count ~ days_passed + video_length + video_quality, data = total),
#          out = 'Linear Regressions.html',intercept.top = T, intercept.bottom = F)


#####################################################################################################
#####################################################################################################

#There is an effect to the number of days passed since the video was uploaded
#in order to estimate this effect I will generate a table of each video and how many day have passed
#since it was uploaded until the # of views reached to its peak

library(data.table)

dt <- data.table(total)

max_count_by_id <- dt[, .SD[which.max(count)], by = 'video_id']

#now let's see what is the average # of days that take to a video reach his maximum views
summary(max_count_by_id$days_passed)

#demonstrate this information on a graph
ggplot(max_count_by_id, aes(x = days_passed, fill = days_passed))+
  geom_histogram(bins = 50, fill="#69b3a2", color="#e9ecef")+
  theme_minimal()+
  geom_vline(aes(xintercept = mean(days_passed)),col='red',size=1)+
  geom_text(aes(label=round(mean(days_passed),1),y=10,x=mean(days_passed)+1),col='red',size=6)+
  labs(title = 'Distribution of days passed until videos got to max views')+
  theme(plot.title = element_text(hjust = 0.5))

##so it takes about 5 days to a video to reach his peak
##subset the data by the first 5 days of each video
test <- subset(total, days_passed <= 5)

#how many views each video had in 5 days
views_untill_peak <- test %>% group_by(video_id) %>% summarise(views_untill_peak = sum(count))

#let's add this information to the relevant dataset
video_features <- merge(video_features, views_untill_peak, by = 'video_id')

#######################################################################################################

#now the classification model needs to be generated
set.seed(1234)

#converting the video_quality variable into numeric so it could be calculated
video_features$quality <- as.numeric(video_features$video_quality)

library(factoextra)

#I will use the K-Means algorithm in order to classify each video
kmc <- kmeans(scale(video_features[,c(2,8,9)]), 3, nstart = 25) 

#how many objects are in each cluster?
kmc$size

#visualization of the clusters
fviz_cluster(kmc, data = scale(video_features[,c(2,8,9)]),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())+xlab("")+ylab("")+
  theme(plot.title = element_text(hjust = 0.5))

#adding the model results to the dataset
video_features <- cbind(video_features, cluster = kmc$cluster)

#export the information to a html table
#stargazer(aggregate(video_features[,c(2,8,9)], by=list(cluster=kmc$cluster), mean),
#          out = 'clusters_characteristics.html', summary = F,font.size = "small", 
#          column.separate = c(1,1,1))


#generating a dataset contains the classification
classified_data <- video_features[,c(1,10)]
classified_data$cluster <- as.factor(classified_data$cluster)
levels(classified_data$cluster) <- c('Hot','Stable and Popular','Everything Else')


#export the classification results to a csv file
#write.csv(classified_data, 'Video Ranking.csv', row.names = F) ## RUN ONLY ONCE
