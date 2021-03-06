# Loading the required libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org") 
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org") 
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") 

library(tidyverse) 
library(caret) 
library(data.table)
library(stringr) 
library(broom)  
library(lubridate)

##########################################################
# Create 'dataset' set, 'validation' set (final hold-out test set)
##########################################################

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
dataset <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in dataset set
validation <- temp %>% 
  semi_join(dataset, by = "movieId") %>%
  semi_join(dataset, by = "userId")

# Add rows removed from validation set back into dataset set
removed <- anti_join(temp, validation)
dataset <- rbind(dataset, removed)

#separate rows for extracting different genres
dataset <- dataset %>% separate_rows(genres, sep = "\\|")
validation <- validation %>% separate_rows(genres, sep = "\\|")

#removing 'timestamp' and 'title'
dataset <- dataset %>% select(-timestamp, -title)

#splitting the training data into training and testing sets
inTrain <- createDataPartition(y = dataset$rating, times = 1, p = 0.7, list = F)

training <- dataset[inTrain,]
test <- dataset[-inTrain,]

#RMSE function
RMSE <- function(predictions, actuals){
  d <- predictions - actuals
  d <- d^2
  sqrt(mean(as.numeric(d), na.rm = TRUE))
}


#The mean of ratings in training set
raw_mean <- mean(training$rating)

#genres effect
genres_effect <- training %>% group_by(genres) %>% summarise(genreseffect = mean(rating - raw_mean))

#movie effect
movie_effect <- training %>% left_join(genres_effect, by = 'genres') %>% group_by(movieId) %>% summarise(movieeffect = mean(rating - raw_mean - genreseffect))

#user effect
user_effect <- training %>% left_join(genres_effect, by = 'genres') %>% left_join(movie_effect, by = 'movieId') %>% group_by(userId) %>% summarise(usereffect = mean(rating - raw_mean - genreseffect - movieeffect))

#Predictor function
predictions <- function(testSet){
  #predicting with the model
  pred <- testSet %>% left_join(movie_effect, by = 'movieId') %>% left_join(user_effect, by = 'userId') %>% left_join(genres_effect, by = 'genres') %>% mutate(preds = raw_mean + movieeffect + usereffect + genreseffect) %>% .$preds
  #adjusting our predictions according to the expected range
  pred[pred < 0.5] <- 0.5
  pred[pred > 5] <- 5
  pred
}
