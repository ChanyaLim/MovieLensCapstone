##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(doParallel)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


#registerDoParallel(makePSOCKcluster(detectCores()))

#registerDoSEQ()
#stopCluster(makePSOCKcluster(detectCores()))
#gc(reset=TRUE)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
#movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           #title = as.character(title),
                                           #genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                         title = as.character(title),
                                         genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

### Quiz ###
#Quiz Q1
str(edx)
#Quiz Q2
edx %>% group_by(rating) %>% summarize(number=n())
#Quiz Q3
uniqueN(edx$movieId)
#Quiz Q4
uniqueN(edx$userId)
#Quiz Q5
sum(str_detect(edx$genres,"Drama"))
sum(str_detect(edx$genres,"Comedy"))
sum(str_detect(edx$genres,"Thriller"))
sum(str_detect(edx$genres,"Romance"))
#Quiz Q6
edx %>% group_by(title) %>% summarize(number=n())%>% arrange(desc(number))
#Quiz Q7
edx %>% group_by(rating) %>% summarize(number=n())%>% arrange(desc(number))
#Quiz Q8
edx %>% group_by(rating) %>% summarize(number=n()) %>% 
  mutate(rating_group = ifelse(rating%%1==0,"whole","decimal")) %>% 
  group_by(rating_group)%>%summarize(sum(number))

