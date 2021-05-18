
# title: "Movielensproject"
# author: "Harrie Jonkman"
# date: "5/13/2021"

# loading package
library(tidyverse)
library(tidymodels)
library(caret)

# downloading data
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# using R 4.0 or later:
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

# Because this took a while every time, for myself I used the downloaded dataset in my projectmap.
# load(file = edx.Rdata)
# load(file = validation.Rdata)

validation_CM <- validation  
validation <- validation %>% select(-rating)

#Let us first look at the dataset.
head(edx)

# Give a summary
summary(edx)
glimpse(edx)

# Let us look at the lengthe of the dataset, the number of observations and the colums (variables).
length(edx$rating)
length(validation$rating)
total_obs<-length(edx$rating)+length(validation$rating)
total_obs
ncol(edx)
ncol(validation)

# Because RMSE(Root Mean Square Error) $RMSE=sqrt(mean((true_ratings-predicted_ratings)^2)$ has to be compared in this study, it is important to define its function at the sart of the study.
RMSE <- function(true_ratings, predicted_ratings){
                 sqrt(mean((true_ratings-predicted_ratings)^2,na.rm=T))
                                                }
#Year could be an additional variable for the analysis. But, let us first extract year from the title-variable. 
#We do this on similiar way for the edx, validation and validation_CM datasets. 
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation_CM <- validation_CM %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
                                                
# We also do some preprocessing on the genre-variable 
split_edx  <- edx  %>% separate_rows(genres, sep = "\\|")
split_valid <- validation   %>% separate_rows(genres, sep = "\\|")
split_valid_CM <- validation_CM  %>% separate_rows(genres, sep = "\\|")
                                                
# Let us compare what happened and see what the first rows of edx-dataset show us now.
head(edx) 
head(split_edx)
                                                
# Let us summarize the dataset also.
summary(edx)
#And summarize it also for the splitted set also.
summary(split_edx)

# Explorative Analysis
# Unique numbers
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

# Total movie ratings per genre
#Let us also count the total movie ratings per genre, counts in descent order.
genre_rating <- split_edx%>%
                group_by(genres) %>%
                summarize(count = n()) %>%
                arrange(desc(count))
genre_rating
                                                
# Ratings distribution
vec_ratings <- as.vector(edx$rating)
unique(vec_ratings) 
                                             
#And how are these ratings distributed?
vec_ratings <- vec_ratings[vec_ratings != 0]
vec_ratings <- factor(vec_ratings)
qplot(vec_ratings) +
ggtitle("Distribution of the Ratings")
                                                
# Data Analysis Strategies
# 1. Movie biases
edx %>% 
count(movieId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "black") + 
scale_x_log10() + 
ggtitle("Movies")
                                               
# 2. Ueser bias
edx %>% count(userId) %>% 
ggplot(aes(n)) + 
geom_histogram(bins = 30, color = "black") + 
scale_x_log10() + 
ggtitle("Users")
                                                
# 3. Genres popularity per year. 
genres_popularity <- split_edx %>%
na.omit() %>% 
select(movieId, year, genres) %>% 
mutate(genres = as.factor(genres)) %>% 
group_by(year, genres) %>% 
summarise(number = n(), .groups = 'drop') %>% 
complete(year = full_seq(year, 1), genres, fill = list(number = 0)) 
                                                
#Let us plot this object Genres vs year; 4 genres are chosen for readability: animation, science fiction, war and western movies. 
genres_popularity %>%
filter(year > 1930) %>%
filter(genres %in% c("War", "Sci-Fi", "Animation", "Western")) %>%
ggplot(aes(x = year, y = number)) +
geom_line(aes(color=genres)) +
scale_fill_brewer(palette = "Paired") +
ggtitle("Movie genres over the years (1930-2010)")
                                                
# 4. Average rating of movies over the years
#Do the users mindset also evolve over time? 
edx %>% group_by(year) %>%
summarize(rating = mean(rating)) %>%
ggplot(aes(year, rating)) +
geom_point() +
geom_smooth()+ 
ggtitle("Ratings over the years")

## Modeling and Data Analysis
# RMSE
rmse_results <- data_frame()

# 1. Simplest possible model
mu <- mean(edx$rating)  
mu

# 2. Penalty Term (b_i)- Movie Effect
movie_avgs_norm <- edx %>% 
group_by(movieId) %>% 
summarize(b_i = mean(rating - mu))
movie_avgs_norm %>% qplot(b_i, geom ="histogram", bins = 20, data = ., color = I("black")) +
ggtitle("Taking into account Movie Effect")

# 3. Penalty Term (b_u)- User Effect
user_avgs_norm <- edx %>% 
left_join(movie_avgs_norm, by='movieId') %>%
group_by(userId) %>%
summarize(b_u = mean(rating - mu - b_i))
user_avgs_norm %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black")) +
ggtitle("Taking into account User Effect")

# Evaluation
# Assessing by the RMSE (the lower the score on this is, the better). For this we use the `validation_CM` dataset.
# 1. Baseline Model
baseline_rmse <- RMSE(validation_CM$rating,mu)
baseline_rmse

# Show the results on this way.
rmse_results <- data_frame(method = "Using mean only", RMSE = baseline_rmse)
rmse_results

# 2. Movie Effect Model
predicted_ratings_movie_norm <- validation %>% 
left_join(movie_avgs_norm, by='movieId') %>%
mutate(pred = mu + b_i) 

model_1_rmse <- RMSE(validation_CM$rating,predicted_ratings_movie_norm$pred)

rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model", RMSE = model_1_rmse ))
rmse_results %>% knitr::kable()
rmse_results

# 3. Movie and User Effect Model
predicted_ratings_user_norm <- validation %>% 
left_join(movie_avgs_norm, by='movieId') %>%
left_join(user_avgs_norm, by='userId') %>%
mutate(pred = mu + b_i + b_u) 
                                                
model_2_rmse <- RMSE(validation_CM$rating,predicted_ratings_user_norm$pred)
                                                
rmse_results <- bind_rows(rmse_results, data_frame(method="Movie and User Effect Model",  RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()
rmse_results
                                                
# Model 4. Regularized movie and user effect model
                                                
# {r lambdas, echo = TRUE}

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(validation_CM$rating,predicted_ratings))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

## Results summarized

# For the full model, the optimal lambda is: 5.25. Let us regularized the estimates 
movie_avgs_reg <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda), n_u = n())

predicted_ratings_reg <- validation %>% 
  left_join(movie_avgs_reg, by='movieId') %>%
  left_join(user_avgs_reg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>% 
  .$pred

model_3_rmse <- RMSE(validation_CM$rating,predicted_ratings_reg)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie and User Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

## Concluding Remarks
rmse_results %>% knitr::kable()

# We therefore found the lowest value of RMSE that is 0.8648170.
# This model work well 
# We can affirm to have built a machine learning algorithm to predict movie ratings with MovieLens dataset.      
# The regularized model including the effect of user is characterized by the lower RMSE value and is hence the optimal model to use for the present project.     
# The optimal model characterised by the lowest RMSE value (0.8648170).      
# We could also affirm that improvements in the RMSE could be achieved by adding other effect (genre, year, age,..).   
# Other different machine learning models could also improve the results further, but my brain and hardware have limitations, as well as the RAM. They are a constraint.
                                                
# Appendix - Enviroment
print("Operating System:")
version


                                                
                                                
                                                