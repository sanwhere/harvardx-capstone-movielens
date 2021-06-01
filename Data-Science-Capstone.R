## HarvardX: PH125.9x
## Data Science: Capstone - MovieLens
## Sanver Gozen
## April 05, 2021

############################# introduction ###############################

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(dplyr)) install.packages("dplyr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", 
                                        repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                        repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", 
                                      repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", 
                                        repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", 
                                       repos = "http://cran.us.r-project.org")

library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(data.table)
library(GGally)
library(kableExtra)
library(Metrics)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, 
                                          "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, 
                                  p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

#Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

############################# Analysis ###############################
## Data Summary

# Let's see rows, columns and general statistics in the data edx dataset.

# Head of the subset
head(edx) %>%  
  kable() %>% kable_styling(font_size = 12, position = "center")

# Summary of the subset
summary(edx) %>%  
  kable() %>% kable_styling(font_size = 12, position = "center")

# Head of the subset
head(validation) %>%  
  kable() %>% kable_styling(font_size = 12, position = "center")

# Summary of the subset
summary(validation) %>%  
  kable() %>% kable_styling(font_size = 12, position = "center")

# NA check
anyNA(edx)

## Data Analysis

# See how many unique films,users and genres we have in the edx data:
edx %>% summarise(
  Unique_Movies = n_distinct(movieId),
  Unique_Users = n_distinct(userId),
  Combined_Genres = n_distinct(genres),
  Average_Rating = mean(validation$rating)) %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))


# Ratings distribution
edx %>% 
  ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+ 
  scale_y_continuous(labels = scales::label_number_si())+
  # Draw average line
  geom_vline(xintercept = mean(edx$rating),col = "red", linetype = "dashed") +
  # Give a name to the average
  annotate("text", x = mean(edx$rating), y = 1200000, angle = 90, 
           label = paste("Average =", round(mean(edx$rating),digits=2)), 
           vjust = -1, colour ="red")+
  # x label
  xlab("Rating") +
  # y label
  ylab("Number of Ratings") +
  # title
  ggtitle("Rating Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Number of rating per movie 
edx %>% count(movieId) %>% ggplot(aes(n))+
  geom_histogram(binwidth = 0.5, color = "black")+
  xlab("Number") +
  ylab("Count") +
  ggtitle("Number of Ratings per Movie")+
  scale_x_log10(labels = scales::label_number_si())+
  theme(plot.title = element_text(hjust = 0.5))

# Mean Number of Rates
mean_number_of_rates <- edx %>% count(userId) %>% summarize(Average = mean(n))
mean_number_of_rates

ggplot(edx, aes(x = userId, y = rating)) + geom_point()

# Number of rating per user
edx %>% count(userId) %>% ggplot(aes(n))+
  geom_histogram(binwidth = 0.1, color = "black")+
  # Draw average line
  geom_vline(xintercept = mean_number_of_rates$Average,col = "red", 
             linetype = "dashed") + 
  # Give a name to the average
  annotate("text", x = mean_number_of_rates$Average, y = 2000, angle = 90, 
           label = paste("Average =",round(mean_number_of_rates$Average)), 
           vjust = -1, colour ="red")+
  xlab("Number of Ratings") +
  ylab("Users") +
  ggtitle("Number of Ratings per User")+
  scale_x_log10()+
  theme(plot.title = element_text(hjust = 0.5))

# Separate group of genres
edx_genres <- edx %>% separate_rows(genres, sep = "\\|")

# Find Unique Genres
edx_genres %>% group_by(genres) %>% summarise()

# Number of rating for each movie genres
edx_genres %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = genres, y = count)) + 
  geom_point() +
  xlab("Genres") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = scales::label_number_si())+
  labs(title = " Number of Ratings for Each Genre")+
  theme(axis.text.x  = element_text(angle= 90))

# Average Ratings by Genres
edx_genres %>% group_by(genres) %>%
  summarise(n = n(), avg = mean(rating)) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg)) + 
  geom_point() +
  xlab("Genres") +
  ylab("Average Ratings") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average Ratings by Genres")

# Most Rated 20 Movies
edx %>%
  group_by(title, genres) %>%
  summarise(count=n()) %>%
  arrange(desc(count))%>% head(20)%>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

# Most rated Genres
edx_genres %>%group_by(genres) %>% 
  summarise(avg = mean(rating),count=n()) %>% arrange(desc(count))%>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

# Number of Rating per Movie
edx %>% count(movieId) %>% ggplot(aes(n))+
  geom_histogram(binwidth = 0.4, color = "black")+
  scale_x_log10()+
  xlab("Movie") +
  ylab("Number of Rating") +
  ggtitle("Number of Rating per Movie") +
  theme_light()

# Mean movie rating per User
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(average = mean(rating)) %>%
  ggplot(aes(average)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  scale_x_continuous(breaks=seq(0, 5, by= 0.5))+ 
  # Draw average line
  geom_vline(xintercept = mean(edx$rating),col = "red", linetype = "dashed") +
  # Give a name to the average
  annotate("text", x = mean(edx$rating), y = 1200, angle = 90, 
           label = paste("Average =", round(mean(edx$rating),digits=2)), 
           vjust = -1, colour ="red")+
  xlab("Mean rating") +
  ylab("Number of users") +
  ggtitle("Average movie ratings given by users") +
  theme_light()

# Unique users rated 1 or below to at least 100 movie
edx %>%
  group_by(userId) %>%
  filter(n() >= 100) %>% filter(rating <= 1) %>%
  summarize() %>% nrow

############################# Modeling ###############################

### Train and Test Sets
# Split edx data to train and test data by portion of %20-%80
set.seed(2105, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

### Basic Model: Average Movie Rating
# Set an option for decimals
options(digits = 10)
# Use train subset for train our model, 
mu <- mean(train_set$rating)
# Use test subset to test our model 
basic_model_rmse <- rmse(test_set$rating,mu)
# Write the result shown in table
all_rmse <- data.frame(Linear_Model = "Basic Model", RMSE = basic_model_rmse)
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

### Movie Effect Model
# Calculate the difference between each movie and all movie ratings. 
movie_average <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
# test our model
predicted_ratings <- mu + test_set %>%
  left_join(movie_average, by='movieId') %>%
  pull(b_i)
movie_effect_model <- RMSE(predicted_ratings, test_set$rating, na.rm = TRUE)
# Write the result on the table
all_rmse <- bind_rows(all_rmse, data.frame(Linear_Model = "Movie Effect Model", 
                       RMSE = movie_effect_model))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

### User Effect Model
# Calculate the difference between each user average and all users average.
user_average <- train_set %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))
# test our model
predicted_ratings <- mu + test_set %>%
  left_join(user_average, by='userId') %>%
  pull(b_u)
user_effect_model <- RMSE(predicted_ratings, test_set$rating, na.rm = TRUE)
# Write the result shown in table
all_rmse <- bind_rows(all_rmse, data.frame(Linear_Model = "User Effect Model", 
                                           RMSE = user_effect_model))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))


### Movie and User Effect Model
# I calculate the model by combining user and movie effect 
user_average <- train_set %>%
  left_join(movie_average, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
# test our model
predicted_ratings <- test_set %>%
  left_join(movie_average, by='movieId') %>%
  left_join(user_average, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
movie_user_model <- RMSE(predicted_ratings, test_set$rating, na.rm = TRUE)
# Write the result shown in table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "Movie and User Effect Model", 
                                           RMSE = movie_user_model))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))


### Regularized Movie and User Effect Model
# Determine best lambda from a sequence
lambdas <- seq(0, 10, 0.25)
# Calculate best lambda
rmses <- sapply(lambdas, function(l){
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  # Run our previous model with lambda  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating, na.rm = TRUE))
})
# See all lambdas by RMSEs
qplot(lambdas, rmses)
# Select the best lambda
min_lambda <- lambdas[which.min(rmses)]
min_lambda
# Select the best RMSE
rmse_regularisation <- min(rmses)
# Write the result shown in table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "Regularized Movie and User Effect Model", 
                                 RMSE = rmse_regularisation))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center",
                            latex_options = c("HOLD_position"))

### Validation Model
# Calculate regularized movie effect
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+min_lambda))
# Calculate regularized user effect
b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+min_lambda))
# Calculate the predictions on validation set based on these above terms
predicted_ratings <- validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
# output RMSE of our final model
validation_model <- RMSE(predicted_ratings, validation$rating)
# Write the result shown in table
all_rmse <- bind_rows(all_rmse, 
                      data.frame(Linear_Model = "Validation", 
                                 RMSE = validation_model))
all_rmse %>%  
  kable() %>% kable_styling(font_size = 12, position = "center")

# Results 
## Discussion
# Conclusion 