# Import data from excel, make it into a dataframe, and clean it
MX_Campaigns_by_Demographics2023<-as.data.frame(MX_Campaigns_by_Demographics2023)
MX_Campaigns_by_Demographics2023<-clean_names(MX_Campaigns_by_Demographics2023)

#Graph 1
library(dplyr)
library(ggplot2)

# Filter out 'NA' age
MX_Campaigns_by_Demographics2023 <- MX_Campaigns_by_Demographics2023 %>%
  filter(age != "NA") %>%
  filter(!is.na(clicks_all))

# Calculate average clicks for each age group
avg_clicks <- MX_Campaigns_by_Demographics2023 %>%
  group_by(age) %>%
  summarise(avg_clicks = mean(clicks_all))

# Define the colors for different shades of blue
blue_colors <- c("18-24" = "#08306b", "25-34" = "#08519c", "35-44" = "#2171b5",
                 "45-54" = "#4292c6", "55-64" = "#6baed6", "65+" = "#9ecae1")

# Create a summary dataframe to count the number of occurrences of each age group
age_counts <- MX_Campaigns_by_Demographics2023 %>%
  count(age)

# Bar Chart
ggplot(avg_clicks, aes(x = factor(age), y = avg_clicks, fill = factor(age))) +
  geom_bar(stat = "identity") +
  geom_text(data = age_counts, aes(x = age, y = -10, label = n), vjust = 0, size = 3, color = "white") +  # Add text labels below each bar
  scale_fill_manual(values = blue_colors) +
  labs(title = "Average Clicks by Age Group", x = "Age", y = "Average Clicks") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#1a1a1a"),  # Set background color to dark grey
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        panel.border = element_blank(),  # Remove plot border
        axis.line = element_line(color = "white"),  # Set color of axis lines to white
        axis.text = element_text(color = "white"),  # Set color of axis text to white
        axis.title = element_text(color = "white"),  # Set color of axis titles to white
        plot.title = element_text(color = "white"),  # Set color of plot title to white
        legend.text = element_text(color = "white"))  # Set color of legend text to white
# Graph 2
# Import data from excel, make it into a dataframe, and clean it
Mx_campaigns_Engagement2023<-as.data.frame(Mx_campaigns_Engagement2023)
Mx_campaigns_Engagement2023<-clean_names(Mx_campaigns_Engagement2023)

# Replace 'NA' values with 0 in post_comments, post_reactions, and post_shares columns
Mx_campaigns_Engagement2023 <- Mx_campaigns_Engagement2023 %>%
  mutate(post_comments = replace_na(post_comments, 0),
         post_reactions = replace_na(post_reactions, 0),
         post_shares = replace_na(post_shares, 0)) %>%
  mutate(post_interactions = post_reactions + post_comments + post_shares)

# Filter out 'NA' ad_name and calculate post_interactions
Mx_campaigns_Engagement2023 <- Mx_campaigns_Engagement2023 %>%
  filter(ad_name != "NA")

# Define different shades of green for the bars
green_colors <- c("Productores 1" = "#238b45", "Ahorros 20 años" = "#41ab5d",
                  "45L" = "#74c476", "Sistema 400 50 off" = "#a1d99b",
                  "Amplia oportunidades de negocio" = "#c7e9c0", "Descuento Aniversario 1" = "#e5f5e0",
                  "Ylang 2" = "#238b45", "11 tamaños" = "#41ab5d",
                  "Ylang 1" = "#74c476", "Granjas, producción y tratamiento 50 off" = "#a1d99b",
                  "Más de 10 tamaños para ti" = "#c7e9c0", "2 o 200 cerdos" = "#e5f5e0",
                  "Publicación: \"🏢Representa en #Panamá a la empresa más grande...\"" = "#238b45",
                  "Sistema 20" = "#41ab5d", "Aniversario 1" = "#74c476",
                  "Publicación: \"¡Con un biodigestor Sistema.bio sácale el mayor...\"" = "#a1d99b",
                  "Productores 2 - Copia" = "#c7e9c0", "Ylang 3" = "#e5f5e0",
                  "Red de distribuidores" = "#238b45", "Estufa de regalo 50 off" = "#41ab5d",
                  "Ylang 4" = "#74c476", "Diversifica tu negocio" = "#a1d99b",
                  "Diversifica tu negocio Panamá" = "#c7e9c0", "Profesionistas 2" = "#e5f5e0",
                  "Aniversario 6 tamaños" = "#238b45", "Aniversario 20 años" = "#41ab5d",
                  "Publicación: \"🔴 Estamos en vivo #webinar \"Biodigestores en...\"" = "#74c476",
                  "Sistemas Casasano 50 off" = "#a1d99b", "Profesionistas 1" = "#c7e9c0")

ad_name_counts <- Mx_campaigns_Engagement2023 %>%
  count(ad_name)

ggplot(Mx_campaigns_Engagement2023, aes(x = ad_name, y = post_interactions, fill = ad_name)) +
  geom_bar(stat = "identity") +
  geom_text(data = ad_name_counts, aes(x = ad_name, y = 0, label = n), vjust = 1, size = 3, fontface = "bold", color = "black") +  # Add text labels below each bar
  scale_fill_manual(values = green_colors) +
  labs(title = "Post Interactions by Ad Name", x = "Ad Name", y = "Post Interactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),  # Rotate x-axis labels vertically
        plot.background = element_rect(fill = "darkgrey"))  # Set background color to dark grey

#Graph 3:

library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)

mydata<- MX_Campaigns_Performance_2023
sum(is.na(mydata))

overall_performance <- mydata %>%
  summarise(Total_Reach = sum(Reach, na.rm = TRUE),
            Total_Impressions = sum(Impressions, na.rm = TRUE),
            Total_Clicks = sum(`Clicks (all)`, na.rm = TRUE))
print(overall_performance)

mydata <- mutate(mydata, Month = floor_date(as.Date(Starts), "month"))

monthly_trends <- mydata %>%
  group_by(Month) %>%
  summarise(Total_Spent = sum(`Amount spent`, na.rm = TRUE),
            Total_Clicks = sum(`Clicks (all)`, na.rm = TRUE))

ggplot(monthly_trends, aes(x = Month)) +
  geom_line(aes(y = Total_Spent, color = "Total Spent")) +
  geom_line(aes(y = Total_Clicks, color = "Total Clicks")) +
  scale_y_continuous(sec.axis = sec_axis(~./max(monthly_trends$Total_Clicks)*max(monthly_trends$Total_Spent), name = "Total Clicks")) +
  labs(x = "Month", y = "Total Spent (MXN)", title = "Monthly Spend and Clicks Trend") +
  theme_minimal()

efficiency_metrics <- mydata %>%
  summarise(Average_CPC = mean(`CPC (all)`, na.rm = TRUE),
            Total_Spent = sum(`Amount spent`, na.rm = TRUE))
print(efficiency_metrics)

campaign_effectiveness <- mydata %>%
  select(`Campaign name`, Reach, Impressions) %>%
  arrange(desc(Impressions))

print(head(campaign_effectiveness, 10))

mydata <- mutate(mydata, CTR = `Clicks (all)` / Impressions * 100)

ctr_analysis <- mydata %>%
  select(`Campaign name`, CTR) %>%
  arrange(desc(CTR))

print(head(ctr_analysis, 10))

mydata$Starts <- as.Date(mydata$Starts)
mydata$Month <- format(mydata$Starts, "%Y-%m")

monthly_kpi <- mydata %>%
  group_by(Month) %>%
  summarise(Total_Reach = sum(Reach, na.rm = TRUE),
            Total_Impressions = sum(Impressions, na.rm = TRUE),
            Total_Clicks = sum(`Clicks (all)`, na.rm = TRUE),
            Average_CTR = mean(CTR, na.rm = TRUE),
            Total_Spent = sum(`Amount spent`, na.rm = TRUE))

ggplot(monthly_kpi, aes(x = as.Date(paste0(Month, "-01")))) +
  geom_line(aes(y = Total_Reach, colour = "Reach")) +
  geom_line(aes(y = Total_Impressions/10, colour = "Impressions (x10)"))

# R-code for Mobile devices
library(readxl)
Website_data_Formatted_sheet <- read_excel("Assignments/Website data_Formatted_sheet.xlsx", 
                                           sheet = "Data_Devices_ 20230101-20231231")

marketing_data<-as.data.frame(Website_data_Formatted_sheet)

# Print the structure of the dataframe
str(marketing_data)

# Summary statistics
summary(marketing_data)

# Distribution of variables
ggplot(marketing_data, aes(x = marketing_data$`Mobile Device Info`, y = Users)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Users by Mobile Device Info", x = "Mobile Device Info", y = "Users") +
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))

# R code for In-market Segment EDA Analysis

library(readxl)
Website_data_Formatted_sheet <- read_excel("Assignments/Website data_Formatted_sheet.xlsx", 
                                           sheet = "In-marketsegment20230101-202312")

in_market_data<-as.data.frame(Website_data_Formatted_sheet)
cleaned_data <- na.omit(in_market_data)
str(in_market_data)
colSums(is.na(in_market_data))
# Histogram for Users
ggplot(in_market_data, aes(x = Users)) +
  geom_histogram(binwidth = 500, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Users")

# Scatter plot between Users and Sessions
ggplot(in_market_data, aes(x = Users, y = Sessions)) +
  geom_point() +
  labs(title = "Scatter Plot of Users vs Sessions")

# Bar plot for In-Market Segment
ggplot(in_market_data, aes(x = `In-Market Segment` , y = Users)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Users by In-Market Segment")

#R-code for  Affinity Category EDA Analysis

library(readxl)
Website_data_Formatted_sheet <- read_excel("Assignments/Website data_Formatted_sheet.xlsx", 
                                           sheet = "AffinityCat# 20230101-20231231")

affinity_data <- as.data.frame(Website_data_Formatted_sheet)
str(affinity_data)
# Summary statistics
summary(affinity_data)

# Distribution of numerical variables
hist(affinity_data$Users, main = "Distribution of Users")
hist(affinity_data$New_Users, main = "Distribution of New Users")
hist(affinity_data$Sessions, main = "Distribution of Sessions")

# Scatter plot between Users and Sessions
plot(affinity_data$Users, affinity_data$Sessions, main = "Scatter Plot of Users vs Sessions", xlab = "Users", ylab = "Sessions")

# Correlation matrix
cor(affinity_data[, c("Users", "New Users", "Sessions", "Bounce Rate", "Pages / Session")])

# Bar plot for Affinity Category with the highest Users
top_category <- affinity_data[which.max(affinity_data$Users), "Affinity_Category"]
ggplot(affinity_data, aes(x = reorder(`Affinity Category (reach)`, -Users), y = Users)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  
        axis.text.y = element_text(face = "bold")) +
  labs(title = paste("Distribution of Users by Affinity Category\n(Top Category:", top_category, ")"),
       x = "Affinity Category")

# R-code for Facebook /Instagram Followers EDA Analysis

# Facebook audience data
fb_age <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
fb_women_followers <- c("1.4%", "12.8%","7.2%","2.8%","1.1%","0.7%")
fb_men_followers <- c("3.6%","32.3%","22%","10.2%","4%","1.9%")

# Remove percentage sign and convert to numeric
fb_women_followers_numeric <- as.numeric(sub("%", "", fb_women_followers)) / 100
fb_men_followers_numeric <- as.numeric(sub("%", "", fb_men_followers)) / 100

# Create a new data frame
fb_followers_data <- data.frame(Age = fb_age, Women = fb_women_followers_numeric, Men = fb_men_followers_numeric)

# Plotting
library(ggplot2)
ggplot(fb_followers_data, aes(x = Age)) +
  geom_bar(aes(y = Women, fill = "Women"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Men, fill = "Men"), stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Followers (Proportion)", fill = "Gender") +
  ggtitle("Facebook Followers by Gender and Age") +
  theme_minimal() +
  theme(legend.position = "top")


# Instagram Audience data
Insta_age <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
Insta_women_followers <- c("2%", "7.7%","5.4%","2.6%","0.6%","0.3%")
Insta_men_followers <- c("8.1%","32%","25.6%","10.6%","3.7%","1.4%")

# Remove percentage sign and convert to numeric
Insta_women_followers_numeric <- as.numeric(sub("%", "", Insta_women_followers)) / 100
Insta_men_followers_numeric <- as.numeric(sub("%", "", Insta_men_followers)) / 100

# Create a new data frame
Insta_followers_data <- data.frame(Age = Insta_age, Women = Insta_women_followers_numeric, Men = Insta_men_followers_numeric)

# Plotting
library(ggplot2)
ggplot(Insta_followers_data, aes(x = Age)) +
  geom_bar(aes(y = Women, fill = "Women"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Men, fill = "Men"), stat = "identity", position = "dodge") +
  labs(x = "Age", y = "Followers (Proportion)", fill = "Gender") +
  ggtitle("Instagram Followers by Gender and Age") +
  theme_minimal() +
  theme(legend.position = "top")

# R code for METALATAM _Reach

library(readxl)
Meta_LATAM <- read_excel("Assignments/Meta_LATAM.xlsx")

str(Meta_LATAM)
data<-as.data.frame(Meta_LATAM)

# Convert Date column to proper date-time format
data$Date <- as.POSIXct(data$Date, format = "%Y-%m-%dT%H:%M:%S")

# Assuming your data frame is named 'data'
filtered_data <- data %>%
  filter(data$`Facebook reach` != 0)

str(filtered_data)

# Convert Facebook reach column to numeric
filtered_data$`Facebook reach` <- as.numeric(filtered_data$`Facebook reach`)

# Extract year and month from Date column
filtered_data <- filtered_data %>%
  mutate(Year = format(Date, "%Y"),  # Extract year
         Month = format(Date, "%Y-%m"))  # Extract year and month

# Aggregate data by month and year
monthly_yearly_aggregated_data <- filtered_data %>%
  group_by(Year, Month) %>%
  summarise(Total_Facebook_reach = sum(`Facebook reach`))

# Print the aggregated data
print(monthly_yearly_aggregated_data)

# Plot
ggplot(monthly_yearly_aggregated_data, aes(x = paste(Month, sep = "-"), y = Total_Facebook_reach, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Facebook Page Likes Over Time",
       x = "Year-Month",
       y = "Total Facebook Page Likes",
       fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#META_LATAM _Visits
library(readxl)
Meta_LATAM_Visits <- read_excel("Assignments/Meta_LATAM.xlsx", 
                                sheet = "MetaLATAM_Visits")

str(Meta_LATAM_Visits)
data_v<-as.data.frame(Meta_LATAM_Visits)

# Convert Date column to proper date-time format
data_v$Date <- as.POSIXct(data_v$Date, format = "%Y-%m-%dT%H:%M:%S")

#filtering the non zero rows
filtered_data_v <- data_v %>%
  filter(data_v$`Facebook Page likes` != 0)

# Convert Facebook Page likes column to numeric
filtered_data_v$`Facebook Page likes` <- as.numeric(filtered_data_v$`Facebook Page likes`)

# Remove rows with NA values
filtered_data_v <- na.omit(filtered_data_v)

# Extract year and month from Date column
filtered_data_v <- filtered_data_v %>%
  mutate(Year = format(Date, "%Y"),  
         Month = format(Date, "%Y-%m")) 

# Aggregate data by month and year
monthly_yearly_aggregated_data <- filtered_data_v %>%
  group_by(Year, Month) %>%
  summarise(Total_Facebook_page_likes = sum(`Facebook Page likes`))

# Print the aggregated data
print(monthly_yearly_aggregated_data)

# Plot
ggplot(monthly_yearly_aggregated_data, aes(x = paste(Month, sep = "-"), y = Total_Facebook_page_likes)) +
  geom_line() +
  geom_point() +
  labs(title = "Total Facebook Page Likes Over Time",
       x = "Year-Month",
       y = "Total Facebook Page Likes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Plot
ggplot(monthly_yearly_aggregated_data, aes(x = paste(Month, sep = "-"), y = Total_Facebook_page_likes, fill = Month)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Total Facebook Page Likes Over Time",
       x = "Year-Month",
       y = "Total Facebook Page Likes",
       fill = "Month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






#R_code for Website_Audience _data
library(readxl)
Website_data_Formatted_sheet <- read_excel("Assignments/Website data_Formatted_sheet.xlsx", 
                                           sheet = "Data Audience20230101-20231231")
df<-as.data.frame(Website_data_Formatted_sheet)
df$`Day Index`<- as.Date(df$`Day Index`)
# Extract month and year from 'Day Index'
df$Month <- format(df$`Day Index`, "%Y-%m")
# Group by month and calculate total users
monthly_users <- df %>%
  group_by(Month) %>%
  summarise(Total_Users = sum(Users))
print(monthly_users)
# Plot total users by month
ggplot(monthly_users, aes(x = Month, y = Total_Users)) +
  geom_line(color = "#0072B2", size = 1) +  # Line color and size
  geom_point(color = "#0072B2", size = 3, shape = 21, fill = "#56B4E9") +  
  labs(title = "Total Users by Month", x = "Month", y = "Total Users") +
  theme_minimal() +  # Use minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        plot.title = element_text(size = 16, face = "bold"),  
        axis.title = element_text(size = 14), 
        legend.position = "none")  


# R-code for Website_Channels_data

library(readxl)
Website_data_Formatted_sheet <- read_excel("Assignments/Website data_Formatted_sheet.xlsx", 
                                           sheet = "data_export(1)_20230501_2023123")
Channels_data<-as.data.frame(Website_data_Formatted_sheet)
Channels_data <- Channels_data[, -c(8, 9)]
str(Channels_data)

# Check for missing values
colSums(is.na(Channels_data))


# Summary Statistics
summary(Channels_data)

# Bar plot of New users by channel
ggplot(Channels_data, aes(x = `First user default channel group`, y = `New users`)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total New Users by Channel",
       x = "Channel",
       y = "Total New Users") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)



# Bubble plot of New users vs Engagement rate by channel
ggplot(Channels_data, aes(x = `New users`, y = `Engagement rate`, size = `New users`, color = `Engagement rate`, label = `First user default channel group`)) +
  geom_point(alpha = 0.6) +
  geom_text(hjust = -0.2, vjust = 0.2, size = 3, color = "black", fontface = "bold")
labs(title = "New Users vs Engagement Rate by Channel",
     x = "New Users",
     y = "Engagement Rate") +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal()


#Modeling with sentiment analysis
#Linear model and correlation matrix
# Load necessary libraries
library(tidyverse)
library(readxl)
library(janitor)
library(dplyr)
# Import and clean the datasets
engagement_data <- read_excel("2023_Mx_campaigns-Engagement.xlsx") %>%
  clean_names() %>%
  slice(-1) %>%
  replace_na(list(post_comments = 0, post_reactions = 0, post_shares = 0)) %>%
  mutate(post_interactions = post_reactions + post_comments + post_shares)

performance_data <- read_excel("2023_MX_Campaigns-Performance.xlsx") %>%
  clean_names() %>%
  slice(-1)

# Merge datasets by 'ad_name' and 'campaign_name'
merged_df <- merge(performance_data, engagement_data, by = c("ad_name", "campaign_name"), all = TRUE) %>%
  clean_names()

# Convert date columns and calculate campaign length
merged_df <- merged_df %>%
  mutate(starts = as.Date(starts, format="%Y-%m-%d"),
         ends = as.Date(ends, format="%Y-%m-%d"),
         campaign_length = as.numeric(ends - starts) + 1)

# Calculate and print correlation matrix
cor_matrix <- cor(merged_df[,c("reach", "impressions", "amount_spent", "post_reactions", "clicks_all", "campaign_length")], use="complete.obs")
print(cor_matrix)

# Fit a linear model to predict clicks_all
lm_model <- lm(clicks_all ~ reach + impressions + amount_spent + post_reactions + cpc_all + campaign_length, data = merged_df)
summary(lm_model)

#Clustering 
#Load in the excels & remove the summary stats in the first row

library(tidyverse)
library(dplyr)
library(janitor)
library(stringr)
library(dendextend)
library(factoextra)
Performance <- as.data.frame(X2023_MX_Campaigns_Performance)
Performance <- slice(Performance,-1)
Engagement <- as.data.frame(X2023_Mx_campaigns_Engagement)
Engagement <- slice(Engagement, -1)
(unique(Performance$"Campaign name"))
(unique(Performance$"Ad name"))
unique_pairs_count <- sum(!duplicated(Performance[, c("Campaign name", "Ad name")]))
unique_pairs_count
merged_df <- merge(Performance, Engagement, by = c("Ad name", "Campaign name"), all = TRUE)
merged_df <- merged_df[, -c(27,26,24,18,17,16,14,15,9)] #remove duplicate columns & ones with no meaning(reporting starts/ends & currency since currency is same)
df <- clean_names(merged_df)
#remove Na
df <- replace(df, is.na(df), 0)
#create a column that shows which month a campaign started in
df$start_month <- str_extract(df$starts, "(?<=\\d{4}-)\\d{2}(?=-\\d{2})")
#remove campaigns that didnt go through or something of the sort
df <- df %>% filter(ad_delivery_x != "inactive" & ad_delivery_x != "rejected")
write.csv(df, "dataPreparedForCluster.csv", row.names = FALSE)
#cluster analysis
df_cluster <- df %>% select(6:10, 13:19)
df_cluster$start_month <- as.integer(df$start_month)
distance = dist(df_cluster)
dfclusterh = hclust(distance)
plot(dfclusterh)

kmeans_results <- lapply(1:10, function(k) kmeans(df_cluster, centers = k))

# Compute total within-cluster sum of squares
wss <- sapply(kmeans_results, function(k) k$tot.withinss)

# Plot the scree plot
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)",
     ylab = "Total within-clusters sum of squares")
kmeans_result <- kmeans(df_cluster, centers = 4)

print(kmeans_result)

# Assign cluster labels to the original dataframe
df_cluster$cluster <- as.factor(kmeans_result$cluster)

cluster_means <- df_cluster %>%
  group_by(cluster) %>%
  summarise_all(mean)

count_start_month <- table(df_cluster$start_month)
print(count_start_month)
ggplot(df_cluster, aes(x = factor(start_month))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Frequency of Start Months", x = "Month", y = "Frequency")

# R code for Topic modeling/Sentiment analysis
install.packages("sentimentr")
library(sentimentr)
library(readxl)
install.packages("tm")
install.packages("topicmodels")
library(tm)
library(topicmodels)
library(syuzhet)
SistemaComments <- read_excel("Assignments/SistemaComments.xlsx")
# Example comments data
comments_df <- as.data.frame(SistemaComments)

#word clouds
wordcloud(comments_df$Comment, max.words = 100, random.order = FALSE,col="red")

review1<-as.character(comments_df$Comment)
#obtain sentiment scores
get_nrc_sentiment('happy')
get_nrc_sentiment('abuse')

s1<-get_nrc_sentiment(review1)

review_sentiment1<-cbind(comments_df$Comment,s1)

#Bar plot for sentiments
barplot(colSums(s1),col=rainbow(10), ylab='count', main ='feedback')


# Create a corpus
corpus <- Corpus(VectorSource(comments_df$Comment))

# Filter out empty documents from the corpus
non_empty_corpus <- corpus[sapply(corpus, function(x) length(unlist(strsplit(as.character(x), " ")))) > 0]


# Preprocessing the corpus
corpus <- tm_map(corpus, content_transformer(tolower)) # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
#corpus <- tm_map(corpus, removeNumbers) # Remove numbers
corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stopwords
corpus <- tm_map(corpus, stripWhitespace) # Strip whitespace

# Filter out empty documents from the corpus
non_empty_corpus <- corpus[sapply(corpus, function(x) length(unlist(strsplit(as.character(x), " ")))) > 0]


# Create a document-term matrix
dtm <- DocumentTermMatrix(non_empty_corpus)

# deleting the rows with all-zero's
row.sum=apply(dtm,1,FUN=sum)
dtm_Nonzero=dtm[row.sum!=0,]

# Run LDA topic modeling
num_topics <- 3 # Number of topics
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# View the topics
terms(lda_model, 10) # Display top 10 terms for each topic

# Assuming 'lda_model' is your LDA model
beta_values <- terms(lda_model)

# View the beta values
print(beta_values)

# Extract and print the top terms for each topic

df_topics<-tidy(lda_model,matrix="beta")
df_topics

#grouping the terms by topic
df_topterms<-df_topics %>% 
  group_by(topic) %>% 
  slice_max(beta,n=10) %>% 
  ungroup()%>% 
  arrange(topic, -beta)


#display the grouped terms on the charts
df_topterms %>% 
  mutate(term=reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill=factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic,scales="free") +
  scale_y_reordered()

#filter topic by terms
tidy(dtm_Nonzero) %>% 
  filter(document == 3) %>% 
  arrange(desc(count))


#probability of topic to document
df_gamma<-tidy(lda_model,matrix="gamma")
df_gamma
df_top_docs<-df_gamma %>% group_by(topic) %>% slice_max(gamma,n=10)   
df_top_docs

#create a dataframe with gamma results
doc_gamma.df<-data.frame(df_gamma)
doc_gamma.df$chapter<-rep(1:dim(dtm_Nonzero))


#plot gamma results
ggplot(data=doc_gamma.df,aes(x=chapter, y=gamma, group=factor(topic),
                             color=factor(topic)))+geom_line()+facet_wrap(~factor(topic),ncol=1)



#optimal number of clusters
cluster_no<-FindTopicsNumber (dtm_Nonzero, topics = seq(2,3),
                              metrics = c("Griffiths2004",
                                          "caojuan2009",
                                          "Arun2010",
                                          "Deveaud2014"),
                              method = "Gibbs",control=list(seed=2024),
                              mc.cores=2L,verbose=TRUE)

#R code for Sentiment analysis

SistemaComments <- read_excel("Assignments/SistemaComments.xlsx")
# Example comments data
comments_df <- as.data.frame(SistemaComments)
str(comments_df)

# Load the data frame
comments <- comments_df$Comment

# Tokenize the comments
tokens <- comments %>%
  tibble(text = .) %>%
  unnest_tokens(word, text)

# Load the AFINN lexicon for sentiment analysis
afinn <- get_sentiments("afinn")

# Join the tokens with the AFINN lexicon to get sentiment scores
sentiment_scores <- tokens %>%
  inner_join(afinn, by = "word")

# Calculate the sentiment score for each comment
comment_sentiment <- sentiment_scores %>%
  group_by(word) %>%
  summarise(sentiment_score = sum(value))



# Classify sentiment based on score
comment_sentiment$sentiment <- ifelse(comment_sentiment$sentiment_score > 0, "Positive",
                                      ifelse(comment_sentiment$sentiment_score < 0, "Negative", "Neutral"))

# View the results
print(comment_sentiment)


# Plot
ggplot(comment_sentiment, aes(x = word, y = sentiment_score, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sentiment Analysis",
       x = "Word",
       y = "Sentiment Score",
       fill = "Sentiment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))