################################
# Create train set, validation set 
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataSet:
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
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in train set
validation <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into train set
removed <- anti_join(temp, validation)
train <- rbind(train, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#function which calculates RMSE
RMSE <- function(actual_rating, predicted_rating){
  sqrt(mean((actual_rating - predicted_rating)^2))
}

#predicted rating = mean rating(mu) + movie effect(bm) + user effect(bu)

mu <- mean(train$rating)

bm <- train %>% group_by(movieId) %>% summarise(bm = mean(rating - mu))

bu <- train %>% left_join(bm, by = 'movieId') %>% group_by(userId) %>% summarise(bu = mean(rating - bm - mu))

#RMSE for this model for validation set

rmse_bm_bu <- validation %>% left_join(bm, by = 'movieId') %>% left_join(bu, by = 'userId') %>% 
  mutate(predicted_rating = mu + bm + bu) %>% summarise(RMSE(rating, predicted_rating))

rmse_bm_bu <- rmse_bm_bu[1,]