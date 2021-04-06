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

############## Project ##############
### Data cleaning ###
head(edx)
str(edx)
str(validation)
## extract movie year from title ##
edx <- edx %>% mutate(movie_year=str_sub(title,str_length(title)-4,str_length(title)-1))
## Change movie_year type to numeric
edx <- edx %>% mutate(movie_year=as.numeric(movie_year))
## NA checking ##
any(is.na(edx))

#### Exploration Data Analysis(EDA) and visulization####
### Check relationship of each variable with movie rating###
## plot distribution of user id Vs rating : rating vary by userId##
edx %>% group_by(userId) %>% summarize(Mean_rating = mean(rating)) %>%
  ggplot(aes(x=userId,y=sort(Mean_rating))) +
  geom_bar(stat="identity")+
  ylab("Mean rating")+
  xlab("MovieId")

## plot distribution of movie id Vs rating : rating vary by movieId ##
edx %>% group_by(movieId) %>% summarize(Mean_rating = mean(rating)) %>%
  ggplot(aes(x=movieId,y=sort(Mean_rating))) +
  geom_bar(stat="identity")+
  ylab("Mean rating")+
  xlab("UserId")

## plot distribution of movie year Vs rating : ##
edx %>% group_by(movie_year) %>% summarize(Mean_rating = mean(rating)) %>%
  ggplot(aes(x=movie_year,y=sort(Mean_rating))) +
  geom_bar(stat="identity")+
  ylab("Mean rating")+
  xlab("Movie year")

### Distribution ###
## userId count validation for Regularization consideration

edx %>% group_by(userId) %>% 
  summarize(Count_by_user=n()) %>% ggplot(aes(Count_by_user))+
  geom_histogram(binwidth = 10)+
  ggtitle("Distribution of count by user")

user_less <- edx %>% group_by(userId)%>%summarize(user_count=n(),mean_rating=mean(rating)) %>%
  arrange(user_count) %>% slice(0:10) %>% pull(userId)
edx %>% filter(userId %in% user_less) %>% group_by(userId)%>%
  ggplot(aes(as.factor(userId),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 less active users with rating")+
  xlab("UserId")+
  ylab("Rating")

user_most <- edx %>% group_by(userId)%>%summarize(user_count=n(),mean_rating=mean(rating)) %>%
  arrange(desc(user_count)) %>% slice(0:10) %>% pull(userId)
edx %>% filter(userId %in% user_most) %>% group_by(userId)%>%
  ggplot(aes(as.factor(userId),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 most active users with rating")+
  xlab("UserId")+
  ylab("Rating")

## movieId count validation for Regularization consideration

edx %>% group_by(movieId) %>% 
  summarize(Count_by_movie=n()) %>% ggplot(aes(Count_by_movie))+
  geom_histogram(binwidth = 10)+
  ggtitle("Distribution of count by movie")

edx %>% group_by(movieId) %>% 
  summarize(Count_by_movie=n()) %>% ggplot(aes(Count_by_movie))+
  geom_histogram(binwidth = 10)+
  ylim(0,500)+xlim(0,5000)+
  ggtitle("Distribution of count by movie(Zoom)")

movie_less <- edx %>% group_by(movieId)%>%summarize(movie_count=n(),mean_rating=mean(rating)) %>%
  arrange(movie_count) %>% slice(0:10) %>% pull(movieId)
edx %>% filter(movieId %in% movie_less) %>% group_by(movieId)%>%
  ggplot(aes(as.factor(movieId),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 less rated movies with rating")+
  xlab("MovieId")+
  ylab("Rating")

movie_most <- edx %>% group_by(movieId)%>%summarize(movie_count=n(),mean_rating=mean(rating)) %>%
  arrange(desc(movie_count)) %>% slice(0:10) %>% pull(movieId)
edx %>% filter(movieId %in% movie_most) %>% group_by(movieId)%>%
  ggplot(aes(as.factor(movieId),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 most rated movies with rating")+
  xlab("MovieId")+
  ylab("Rating")

## movie year count validation for Regularization consideration

edx %>% group_by(movie_year) %>% 
  summarize(Count_by_year=n()) %>% ggplot(aes(Count_by_year))+
  geom_histogram()+
  ggtitle("Distribution of count by movie year")

movieY_less <- edx %>% group_by(movie_year)%>%summarize(movieY_count=n(),mean_rating=mean(rating)) %>%
  arrange(movieY_count) %>% slice(0:10) %>% pull(movie_year)
edx %>% filter(movie_year %in% movieY_less) %>% group_by(movie_year)%>%
  ggplot(aes(as.factor(movie_year),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 less rated movie year with rating")+
  xlab("Movie year")+
  ylab("Rating")

movieY_most <- edx %>% group_by(movie_year)%>%summarize(movieY_count=n(),mean_rating=mean(rating)) %>%
  arrange(desc(movieY_count)) %>% slice(0:10) %>% pull(movie_year)
edx %>% filter(movie_year %in% movieY_most) %>% group_by(movie_year)%>%
  ggplot(aes(as.factor(movie_year),rating))+
  geom_boxplot()+
  geom_hline(yintercept=mean(edx$rating), color="red")+
  ggtitle("Top 10 most rated movie year with rating")+
  xlab("Movie year")+
  ylab("Rating")

#### Recommendation system ####
### RMSE function ###
RMSE <- function(true_data,pred_data){
  sqrt(mean((true_data-pred_data)^2))
}
### create training set and test set ###
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(edx$rating,times = 1,p=0.2,list=FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]
test_set <- test_set %>% semi_join(train_set,by="movieId")%>%
            semi_join(train_set,by="userId")

### model 1 : Naive mean ###
mu_hat <- mean(train_set$rating)
mu_hat
naive_rmse <- RMSE(test_set$rating,mu_hat)

RMSE_result <- data.frame(Method="Naive mean",RMSE=naive_rmse)
knitr::kable(RMSE_result)

### model 2 : movieId effect ###
mu <- mean(train_set$rating)
movie_avgs <- train_set %>% group_by(movieId) %>%
              summarize(b_i = mean(rating-mu))
movie_avgs %>% ggplot(aes(b_i))+geom_histogram()

predicted_rating_movie <- mu + test_set %>%
  left_join(movie_avgs,by="movieId") %>% 
  pull(b_i)
movie_rmse <- RMSE(test_set$rating,predicted_rating_movie)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Movie Effect",RMSE=movie_rmse))
knitr::kable(RMSE_result)

### model 3 : userId and movieId effect ###
movie_user_avgs <- train_set %>% 
                   left_join(movie_avgs,by="movieId") %>%
                   group_by(userId)%>%
                   summarize(b_u=mean(rating-mu-b_i))
movie_user_avgs %>% ggplot(aes(b_u))+geom_histogram()
predicted_rating_movie_user <- test_set %>%
                               left_join(movie_avgs,by="movieId")%>%
                               left_join(movie_user_avgs,by="userId")%>%
                               mutate(pred=mu+b_i+b_u)%>%
                               pull(pred)
movie_user_rmse <- RMSE(predicted_rating_movie_user,test_set$rating)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Movie and User Effect",RMSE=movie_user_rmse))
knitr::kable(RMSE_result)

### model 4 : userId,movieId and movie year effect ###
movie_user_movieYear_avgs <- train_set %>% 
  left_join(movie_avgs,by="movieId") %>%
  left_join(movie_user_avgs,by="userId") %>%
  group_by(movie_year)%>%
  summarize(b_y=mean(rating-mu-b_i-b_u))
movie_user_movieYear_avgs %>% ggplot(aes(b_y))+geom_histogram()

predicted_rating_movie_user_movieYear <- test_set %>%
  left_join(movie_avgs,by="movieId")%>%
  left_join(movie_user_avgs,by="userId")%>%
  left_join(movie_user_movieYear_avgs,by="movie_year")%>%
  mutate(pred=mu+b_i+b_u+b_y)%>%
  pull(pred)
movie_user_movieYear_rmse <- RMSE(predicted_rating_movie_user_movieYear,test_set$rating)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Movie,User and Movie year Effect",RMSE=movie_user_movieYear_rmse))
knitr::kable(RMSE_result)

## Model 5 Regularized Movie,User and Movie year Effect ##
## cross validation to pick up lambdas with movieId,userId, movie year ##
lambdas <- seq(0,10,0.25)
rmses4 <- sapply(lambdas,function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i=sum(rating-mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i,by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u=sum(rating-b_i-mu)/(n()+l))
  b_y <- train_set %>%
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId")%>%
    group_by(movie_year) %>%
    summarize(b_y=sum(rating-b_i-b_u-mu)/(n()+l))
  predicted_rating <- test_set %>%
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    left_join(b_y,by="movie_year") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    pull(pred)
  return(RMSE(predicted_rating,test_set$rating))
})
qplot(lambdas,rmses4)
lambdas[which.min(rmses4)]
Reg_movie_user_movieYear_rmse <- min(rmses4)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Regularized Movie,User and Movie year Effect",RMSE=Reg_movie_user_movieYear_rmse))
knitr::kable(RMSE_result)

## Model 6 Regularized Movie,User and Movie year with adjusting Effect ##
## cross validation to pick up lambdas with movieId,userId, movie year with round ##
rmses5 <- sapply(lambdas,function(l){
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i=sum(rating-mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i,by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u=sum(rating-b_i-mu)/(n()+l))
  b_y <- train_set %>%
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId")%>%
    group_by(movie_year) %>%
    summarize(b_y=sum(rating-b_i-b_u-mu)/(n()+l))
  predicted_rating <- test_set %>%
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>%
    left_join(b_y,by="movie_year") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    mutate(pred1=ifelse(pred>5,max(train_set$rating),pred))%>%
    mutate(pred2=ifelse(pred1<0,min(train_set$rating),pred1))%>%
    pull(pred2)
  return(RMSE(predicted_rating,test_set$rating))
})
qplot(lambdas,rmses5)
lambdas[which.min(rmses5)]
Reg_movie_user_movieYear_round_rmse <- min(rmses5)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Regularized Movie,User and Movie year with adjusting Effect",RMSE=Reg_movie_user_movieYear_round_rmse))
knitr::kable(RMSE_result)

## keep lambda for final model testing with validation dataset 
lambda <- lambdas[which.min(rmses5)]

### Final model is model 6 with picked up lambda ###
### Start validation with validation set ###

## extract movie year from title for validation set ##
validation <- validation %>% mutate(movie_year=str_sub(title,str_length(title)-4,str_length(title)-1))
## Change movie_year type to numeric for validation set ##
validation <- validation %>% mutate(movie_year=as.numeric(movie_year))

## Finding bias of movieId, userId and movie_year of EDX
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i=sum(rating-mu)/(n()+lambda))
b_u <- edx %>%
  left_join(b_i,by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u=sum(rating-b_i-mu)/(n()+lambda))
b_y <- edx %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId")%>%
  group_by(movie_year) %>%
  summarize(b_y=sum(rating-b_i-b_u-mu)/(n()+lambda))
predicted_rating_validation <- validation %>%
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>%
  left_join(b_y,by="movie_year") %>%
  mutate(pred = mu + b_i + b_u + b_y) %>%
  mutate(pred1=ifelse(pred>5,max(edx$rating),pred))%>%
  mutate(pred2=ifelse(pred1<0,min(edx$rating),pred1))%>%
  pull(pred2)
rmse_validation <- RMSE(validation$rating,predicted_rating_validation)

RMSE_result <- rbind(RMSE_result,data.frame(Method="Validation result",RMSE=rmse_validation))
knitr::kable(RMSE_result)

#########################################################################

##### Appendix #####

### model : Regularization for movie id ###
## regularized estimate with lambda(l) = 3 ##
#l <- 3
#movie_reg_avgs <- train_set %>%
#  group_by(movieId) %>%
#  summarize(b_i=sum(rating-mu)/(n()+l))
#hist(movie_reg_avgs$b_i)
## find RMSE to see if regularization improve our model ##
#predicted_rating_reg_movie <- test_set %>%
#  left_join(movie_reg_avgs,by="movieId") %>%
#  mutate(pred=mu+b_i)%>%
#  pull(pred)
#Reg_movie_rmse <- RMSE(predicted_rating_reg_movie,test_set$rating)
#Reg_movie_rmse

## cross validation to pick up lambda with movieId ##
#lambdas <- seq(0,10,0.25)
#just_the_sum <- train_set %>%
#  group_by(movieId)%>%
#  summarize(s=sum(rating-mu),n_i=n())
#rmses<-sapply(lambdas,function(l){
#  pred_rating <- test_set %>%
#    left_join(just_the_sum,by="movieId") %>%
#    mutate(b_i=s/(n_i+l))%>%
#    mutate(pred = mu+b_i)%>%
#    pull(pred)
#  return(RMSE(pred_rating,test_set$rating))
#})
#qplot(lambdas,rmses)
#lambdas[which.min(rmses)]
#Reg_movie_rmse_best <- min(rmses)

#RMSE_result <- rbind(RMSE_result,data.frame(Method="Regularized Movie Effect",RMSE=Reg_movie_rmse_best))
#knitr::kable(RMSE_result)

## Model Regularization for movie id and user id ##
## cross validation to pick up lambdas with movieId and userId ##
#rmses2 <- sapply(lambdas,function(l){
#  b_i <- train_set %>%
#    group_by(movieId) %>%
#    summarize(b_i=sum(rating-mu)/(n()+l))
#  b_u <- train_set %>%
#    left_join(b_i,by="movieId") %>%
#    group_by(userId) %>%
#summarize(b_u=sum(rating-b_i-mu)/(n()+l))
#  predicted_rating <- test_set %>%
#    left_join(b_i,by="movieId") %>%
#    left_join(b_u,by="userId") %>%
#    mutate(pred=mu+b_i+b_u) %>%
#    pull(pred)
#  return(RMSE(predicted_rating,test_set$rating))
#})
#qplot(lambdas,rmses2)
#lambdas[which.min(rmses2)]
#Reg_movie_user_rmse <- min(rmses2)

#RMSE_result <- rbind(RMSE_result,data.frame(Method="Regularized Movie and User Effect",RMSE=Reg_movie_user_rmse))
#knitr::kable(RMSE_result)

## Model : Modify from regularized movie,user 
##If rating over 5 keep as 5, if below 0 keep as 0 
#rmses3 <- sapply(lambdas,function(l){
#  b_i <- train_set %>%
#    group_by(movieId) %>%
#    summarize(b_i=sum(rating-mu)/(n()+l))
#  b_u <- train_set %>%
#    left_join(b_i,by="movieId") %>%
#    group_by(userId) %>%
#    summarize(b_u=sum(rating-b_i-mu)/(n()+l))
#  predicted_rating <- test_set %>%
#    left_join(b_i,by="movieId") %>%
#    left_join(b_u,by="userId") %>%
#    mutate(pred=mu+b_i+b_u) %>%
#    mutate(pred1=ifelse(pred>5,max(train_set$rating),pred))%>%
#    mutate(pred2=ifelse(pred1<0,min(train_set$rating),pred1))%>%
#    pull(pred2)
#  return(RMSE(predicted_rating,test_set$rating))
#})
#qplot(lambdas,rmses3)
#lambdas[which.min(rmses3)]
#Reg_movie_user_round_rmse<- min(rmses3)

#RMSE_result <- rbind(RMSE_result,data.frame(Method="Regularized Movie and User with round Effect",RMSE=Reg_movie_user_round_rmse))
#knitr::kable(RMSE_result)


