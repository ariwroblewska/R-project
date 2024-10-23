install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("readr")
library(readr)
install.packages("tidyr")
library(tidyr)
install.packages("car")
library(car)
install.packages("corrgram")
library(corrgram)
install.packages("NbClust")
library("NbClust")
install.packages("flexclust")
library(flexclust)

# reading the data about hotel bookings
hotel<- read.csv("C:\\Users\\Aurelia Wróblewska\\Desktop\\R studio\\hotel_bookings.csv", 
                 header = TRUE)
head(hotel)
summary(hotel) # there are 4 missing values (NA's) in the children column
hotel_clean <- drop_na(hotel)
summary(hotel_clean)
str(hotel)

# after looking at the data in an excel file there is a lot of NULL values in column company
# as well as in the column agent and since values in those columns are just ID numbers
# I will be ommiting these columns in the analysis

hotel_clean <- hotel %>% select(-agent, -company) #using "dplyr" package
str(hotel_clean)

colnames(hotel_clean)

# Add a column for total children
hotel_clean <- hotel_clean %>% mutate(total_children = children + babies)
hotel_clean <- hotel_clean %>% mutate(total_guests = adults + children + babies)
table(hotel_clean$total_guests)
hotel_clean <- filter(hotel_clean, total_guests > 0)
table(hotel_clean$total_guests)

hotel_clean <- hotel_clean %>% mutate(total_stay = stays_in_weekend_nights + 
                                      stays_in_week_nights)
colnames(hotel_clean)

hotel_clean$guest_category <- with(hotel_clean, 
                                   ifelse(total_guests == 1, "solo",
                                   ifelse(total_guests >= 2 & total_guests <= 4, "family", 
                                   ifelse(total_guests > 4, "big family", NA))))
head(hotel_clean)

stay_by_children <- hotel_clean %>%
  group_by(total_children) %>%
  summarise(mean_stay = mean(total_stay, na.rm = TRUE))
stay_by_children

# Create a bar plot
ggplot(data = stay_by_children, aes(x = factor(total_children), y = mean_stay)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(mean_stay, 1)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "Average Duration of Stay by Total Number of Children",
       x = "Total Number of Children",
       y = "Average Duration of Stay (nights)")


#replace negative values with a suitable value.
negatives <- which(hotel_clean$adr < 0)
hotel_clean$adr[negatives] <- mean(hotel_clean$adr, na.rm = TRUE)

sum(is.na(hotel_clean$adr))

hotel_counts <- table(hotel_clean$hotel)
hotel_counts

mean_stays_week <- mean(hotel_clean$stays_in_week_nights, na.rm = TRUE)
mean_stays_weekend <- mean(hotel_clean$stays_in_weekend_nights, na.rm = TRUE)
mean_tot_guests <- mean(hotel_clean$total_guests, na.rm = TRUE)
mean_tot_children <- mean(hotel_clean$total_children, na.rm = TRUE)

min_stays_week <- min(hotel_clean$stays_in_week_nights, na.rm = TRUE)
min_stays_weekend <- min(hotel_clean$stays_in_weekend_nights, na.rm = TRUE)
min_tot_guests <- min(hotel_clean$total_guests, na.rm = TRUE)
min_tot_children <- min(hotel_clean$total_children, na.rm = TRUE)

max_stays_week <- max(hotel_clean$stays_in_week_nights, na.rm = TRUE)
max_stays_weekend<- max(hotel_clean$stays_in_weekend_nights, na.rm = TRUE)
max_tot_guests<- max(hotel_clean$total_guests, na.rm = TRUE)
max_tot_children<- max(hotel_clean$total_children, na.rm = TRUE)

stays_week <- data.frame(
  category = c("mean", "min", "max"),
  count = c(mean_stays_week, min_stays_week, max_stays_week))
stays_weekend <- data.frame(
  category = c("mean", "min", "max"),
  count = c(mean_stays_weekend, min_stays_weekend, max_stays_weekend))
tot_guests <- data.frame(
  category = c("mean", "min", "max"),
  count = c(mean_tot_guests, min_tot_guests, max_tot_guests))
tot_children <- data.frame(
  category = c("mean", "min", "max"),
  count = c(mean_tot_children, min_tot_children, max_tot_children))

ggplot(data = stays_week, aes(x = category, y = count)) +
  geom_bar(stat = "identity", fill = "orange") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black") + 
  theme_minimal() + 
  labs(title = "Descriptive Statistics", x = "Stays Weekdays", y = "Count") 

ggplot(data = stays_weekend, aes(x = category, y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black") + 
  theme_minimal() + 
  labs(title = "Descriptive Statistics", x = "Stays Weekends", y = "Count") 

ggplot(data = tot_guests, aes(x = category, y = count)) +
  geom_bar(stat = "identity", fill = "violet") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black") + 
  theme_minimal() + 
  labs(title = "Descriptive Statistics", x = "Total Guests", y = "Count") 

ggplot(data = tot_children, aes(x = category, y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") + 
  geom_text(aes(label = count), vjust = -0.5, color = "black") + 
  theme_minimal() + 
  labs(title = "Descriptive Statistics", x = "Total Children", y = "Count") 

ggplot(data = hotel_clean,aes(hotel,fill=hotel))+
  geom_bar(fill = "pink")+
  labs(title = "City Hotel VS Resort Hotel", x = "Hotel", y = "Count")

ggplot(data = hotel_clean,aes(arrival_date_month,fill=arrival_date_month))+
  geom_bar()+
  theme(axis.text.x=element_text(angle =  30 ))+
  labs(title = "Most Visited Months", x = "Month", y = "Count" )

ggplot(data = hotel_clean,aes(guest_category,fill=hotel))+
  geom_bar(fill = "lightblue")+
  labs(title = "Categories of guests", x = "Category", y = "Count")


#select top countries
# Identify the top 10 countries by number of customers
top_countries <- hotel_clean %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10, wt = count) %>%
  pull(country)

filtered_top_countries <- hotel_clean %>%
  filter(country %in% top_countries)

ggplot(filtered_top_countries, aes(x = country, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(title = "Hotel Preference by Country (top 10 countries)",
       x = "Country",
       y = "Count",
       fill = "Hotel Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


correlation_matrix <- cor(hotel_clean[, c("total_of_special_requests",
                                          "is_canceled")], use = "pairwise.complete.obs")
correlation_matrix

corrgram(correlation_matrix, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Corrgram of special requests and cancellations intercorrelations")


linear_request<-lm(total_of_special_requests ~ is_canceled, data=hotel_clean)
linear_request

summary(linear_request)
fitted(linear_request)
par(mfrow=c(1,1))
plot(hotel_clean$total_of_special_requests, hotel_clean$is_canceled,
     ylab="Is Canceled",
     xlab = "Nr. of special requests",
     main = "Special Requests vs. Cancellation")
abline(linear_request)

var_request <- data.frame(is_canceled = c(0, 1))
var_request
predict(linear_request, newdata = var_request)
predict(linear_request, newdata = var_request, interval = 'confidence')


linear_adr<-lm(adr ~ total_stay, data=hotel_clean)
linear_adr

summary(linear_adr)
fitted(linear_adr)
par(mfrow=c(1,1))
plot(hotel_clean$adr, hotel_clean$total_stay,
     ylab="Total Stay",
     xlab = "Average Daily Rate",
     main = "Total Stay vs. Average Daily Rate")
abline(linear_adr)

var_adr <- data.frame(total_stay = c(12, 13, 14))
var_adr
predict(linear_adr, newdata = var_adr)
predict(linear_adr, newdata = var_adr, interval = 'confidence')

#polynomial regression lead time
hotel_clean <- hotel_clean[order(hotel_clean$total_guests), ]

polynomial_guests<-lm(lead_time ~ total_guests + I(total_guests^2), data = hotel_clean)
summary(polynomial_guests)

plot(hotel_clean$lead_time, hotel_clean$total_guests, ylab = "Total Guests",
     xlab = "Lead Time")
fitted(polynomial_guests)

lines(hotel_clean$lead_time, fitted(polynomial_guests))

#polynomial regression days in waiting list
hotel_clean <- hotel_clean[order(hotel_clean$total_stay), ]

polynomial_list<-lm(total_stay ~ days_in_waiting_list + 
                      I(days_in_waiting_list^2), data = hotel_clean)
summary(polynomial_list)

plot(hotel_clean$total_stay, hotel_clean$days_in_waiting_list, 
     ylab = "Days In Waiting List",
     xlab = "Total Stay")
fitted(polynomial_list)

lines(hotel_clean$days_in_waiting_list, fitted(polynomial_list))

#multiple children
scatterplotMatrix(~ total_children  + total_stay + adr, 
                  data = hotel_clean,spread = FALSE, smoother.args = list(lty = 2),
                  main = "Scatterplot Matrix")

multi_children<-lm(total_children ~ total_stay + adr, data = hotel_clean)
summary(multi_children)

new_data<-data.frame(total_children = c(10, 11, 12), total_stay = c(12, 13, 16))
new_data

predict(multi_children, newdata = new_data)
predict(multi_children, newdata = new_data, interval = 'confidence')
fitted(multi_children)

# Selecting variables for clustering

cluster_data<-hotel_clean[, c("total_guests", "days_in_waiting_list",
                              "total_stay")]
cluster_data_1 <- hotel_clean[, c("total_stay", "total_children")]
cluster_data_2<-hotel_clean[, c("lead_time", "is_canceled")]
cluster_data_3<-hotel_clean[, c("is_repeated_guest", "days_in_waiting_list")]


nc <- NbClust(cluster_data, distance="euclidean",
              min.nc=2, max.nc=15, method="ward.D2")
table(nc$Best.n[1,])

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Indices")


head(cluster_data, 4)

cluster_data_scaled<-scale(cluster_data)
head(cluster_data_scaled, 4)

dist_matrix_3<- dist(cluster_data_scaled,method = "euclidean")
dist_matrix_3
as.matrix(dist_matrix_3)[1:4, 1:4]
hc_3 <- hclust(dist_matrix_3, method = "ward.D2")
hc_3
par(mfrow=c(1,1))
plot(hc_3, cex = 0.6, hang = -1, 
     main = "Hierarchical Clustering (Total Guests vs Total Stay vs Days In Waiting List)")
hierarchical_labels <- cutree(hc_3, k = 2)
rect.hclust(hc_3, k=2)

hierarchical_sizes <- table(hierarchical_labels)
print(hierarchical_sizes)

set.seed(123)
sample_size <- 0.1 * nrow(cluster_data_1) # Use 10% of the data
sampled_data <- cluster_data_1[sample(1:nrow(cluster_data_1), size = sample_size), ]

# Run NbClust on the sampled data
library(NbClust)
nc <- NbClust(sampled_data, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "ward.D2")
table(nc$Best.n[1,])

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Indices")
head(cluster_data_1, 4)

cluster_data_1_scaled<-scale(cluster_data_1)
head(cluster_data_1_scaled, 4)

dist_matrix<- dist(cluster_data_1_scaled,method = "euclidean")
dist_matrix
as.matrix(dist_matrix)[1:4, 1:4]
hc <- hclust(dist_matrix, method = "ward.D2")
hc
par(mfrow=c(1,1))
plot(hc, cex = 0.6, hang = -1, 
     main = "Hierarchical Clustering (Total Stay vs Total Children)")
hierarchical_labels <- cutree(hc, k = 4)
rect.hclust(hc_3, k=4)

hierarchical_sizes <- table(hierarchical_labels)
print(hierarchical_sizes)

# kmeans
library("NbClust")
nc <- NbClust(cluster_data_2, distance="euclidean",
              min.nc=2, max.nc=15, method="ward.D2")
table(nc$Best.n[1,])

par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 30 Indices")

library(cluster)
# install.packages("factoextra") 
library(factoextra)

cluster_data_2_scaled<-scale(cluster_data_2)
head(cluster_data_2_scaled, 4)

km.res1<-kmeans(cluster_data_2_scaled, 2, nstart = 25)
km.res1
fviz_cluster(km.res1, data = cluster_data_2_scaled,
             palette = "jco", ggtheme = theme_minimal(),
             xlab = "Lead Time",
             ylab = "Is cancelled")

cluster_data_3_scaled<-scale(cluster_data_3)
head(cluster_data_3_scaled, 4)

km.res2<-kmeans(cluster_data_1_scaled, 5, nstart = 25)
km.res2
fviz_cluster(km.res2, data = cluster_data_1_scaled, 
             palette = "jco", ggtheme = theme_minimal())
