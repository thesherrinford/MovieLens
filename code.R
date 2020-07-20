# Loading the required libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org") 
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org") 
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") 
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org") 
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org") 
if(!require(biglm)) install.packages("biglm", repos = "http://cran.us.r-project.org") 
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org") 
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org") 
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org") 
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")

library(tidyverse) 
library(ggplot2) 
library(caret) 
library(rpart) 
library(rpart.plot) 
library(randomForest) 
library(gbm) 
library(stringr) 
library(biglm) 
library(broom) 
library(data.table) 
library(lubridate) 
library(recommenderlab) 
library(recosystem)

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























#splitting the training data into training and testing sets
inTrain <- createDataPartition(y = edx$rating, times = 1, p = 0.7, list = F)

training <- edx[inTrain,]
test <- edx[-inTrain,]