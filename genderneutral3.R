#Introduction

#One important decision parents make is naming their children.  In this study, we will look at 
#popular names and gender neutral names. A soon-to-be parent who is researching such an important 
#decision may want to consider data on a name to see how neutral the name is considered to be.
#Choosing a name that is almost equally chosen for both sexes can be
#the goal for parents.  We will consider several names that have been labeled gender neutral
#and consider how they have been used by both biological sexes historically and
#we will use a model that predicts when the name is considered male or female based on 
#it's use in the US. We will also look at whether specific names are considered outdated.

#Need to download packages if not done already

## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org" 
options(repos=r)
})

install.packages("remotes") # if necessary
remotes::install_github("lmullen/gender")



install.packages('plyr', repos = "http://cran.us.r-project.org")
install.packages("babynames")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("magrittr")
install.packages("devtools")
install.packages("tidyverse")
install.packages("caret")

#I had to install psych, naivebayes, gender, and genderdata through RStudio instead
library(tidyverse)
library(caret)
library(plyr)
library(naivebayes)
library(psych)
library(gender)
library(tibble)
library(devtools)
library(babynames) 
library(dplyr) 
library(tidyr)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(zoo)



data(babynames)
head(babynames)
tail(babynames)

#Methods
#The methods used in this study are probability and Bayes Theorem.  
#Data exploration and visualization were conducted, comparing the popularity of specific
#names that were labeled gender neutral. Probabilities of gender prediction based on these
#names were calculated.  Finally, Bayes Theorem was utilized for the probability of someone 
#being alive by their name or name and gender. 

#Finding out how many people were named X name is year X (sample)

entered_name <- "Charlie"
entered_year <- 2017
result <- babynames %>% filter(name == entered_name) %>%
  filter(year == entered_year) %>%
  summarize(count = sum(n))
result

#Number of male and female names in dataset
babynames %$%
  split(., sex) %>%
  lapply(. %$% length(unique(name)))

#Gender Neutral Names by Sex from 1880-2017
#For each chart, you can view the popularity of the name for use in both biological 
#sexes between 1880-2017. I took a sample of random names from websites that identify gender neutral names
#the prospective parents could visit using a Google search 
#The names that were tested were taken from a few popular websites, as that is likely 
#the place where expectant parents would look.  Some examples are:
#https://www.popsugar.com/family/Gender-Neutral-Baby-Names-34485564
#https://www.mother.ly/child/top-50-gender-neutral-baby-names-youll-obsess-over-

babynames %>% 
  filter(name == "Kaelin") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Kaelin, by Sex")
#The name Kaelin seems to be used by box sexes but has fallen in popularity.
 

babynames %>% 
  filter(name == "Charlie") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Charlie, by Sex")
#Charlie is another name for Charles and was traditionally used by males.  
#However, it has grown in popularity for both genders

babynames %>% 
  filter(name == "Shane") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Shane, by Sex")
#Shane is a name that was traditionally given to males but has decreased in popularity
babynames %>% 
  filter(name == "Quinn") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Quinn, by Sex")
#Quinn is a name that has been used by box sexes, but has grown in popularity in females
babynames %>% 
  filter(name == "Morgan") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Morgan, by Sex")
#Morgan is a name that has historically been used by both sexes, but sharply rose among 
#females 20 years ago.  It has fallen in usage in females since then to meet male usage

babynames %>% 
  filter(name == "Finley") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "FInley, by Sex")
#Finley has grown in usage for both sexes, but more for females
babynames %>% 
  filter(name == "Leslie") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Leslie, by Sex")
#Leslie is a name that was historically used in both genders, although it's use in males 
#has decreased over the last 60 years.  It was popular for females in the last half 
#of the last century.  It has fallen in popularity overall.
babynames %>% 
  filter(name == "Jessie") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Jessie, by Sex")

#Jessie a name that was historically used in both genders and has fallen in popularity
babynames %>% 
  filter(name == "Sidney") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Sidney, by Sex")
#Sidney is a name that was historically used in both genders and has fallen in popularity 
#for both genders

babynames %>% 
  filter(name == "Skyler") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Skyler, by Sex")
#Skyler a name that was historically used in both genders and has risen in popularity
#in the last two decades
babynames %>% 
  filter(name == "Kendall") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Kendall, by Sex")
#Kendall a name that was historically used in both genders but has become more 
#popular for females

babynames %>% 
  filter(name == "Clarke") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Clarke, by Sex")
#Clarke is a name that was historically used for males but has increased in 
#popularity for females


babynames %>% 
  filter(name == "Jackie") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Jackie, by Sex")
#Jackie is a name that was historically used in both genders and has fallen in popularity 
#for both genders
babynames %>% 
  filter(name == "Nicky") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Nicky, by Sex")
#Nicky is a name that was historically used in both genders and has fallen in popularity 
#for both genders

babynames %>% 
  filter(name == "Logan") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Logan by Sex")
#Logan is a name that has generally been given to males

babynames %>% 
  filter(name == "Ashley") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Ashley, by Sex")
#Ashley is a name that has generally been given to females.  Gone With the Wind was an anomaly.

babynames %>% 
  filter(name == "Tyler") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Tyler, by Sex")
#Tyler is a name that has generally been given to males

babynames %>% 
  filter(name == "Oakley") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Oakley, by Sex")
#Oakley is the closest to gender neutral out of this data analysis and is extremely popular.   
babynames %>% 
  filter(name == "Frankie") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Frankie, by Sex")
#Frankie is a name that was historically used in both genders and is rising in popularity in females.

babynames %>% 
  filter(name == "Justice") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Justice, by Sex")
#Justice is a name that was historically used in both genders and is a newer name compared
#to many others.  
babynames %>% 
  filter(name == "Royal") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Royal, by Sex")
#Royal is a name that was historically used for males but has risen in female in the past decade.


#What name has been the most popular over time for males? For females? 
babynames %>% group_by(sex, name) %>%
  dplyr::summarize(median_prop = median(prop)) %>%
  top_n(1)



#This is the number of names given each year in US (1880-2017).  The number is rising, which means 
#more names will be given for our data point. 
ggplot(namescount, aes(x = year,y = value, group="variable")) + geom_line(alpha = 0.4) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(label=" Number of  names in a given year") + geom_smooth(method="loess")


#We can look at the popular names and see how gender neutral they appear
babynames %>% 
  filter(name == "Ava") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Ava, by Sex")

babynames %>% 
  filter(name == "Liam") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Liam, by Sex")

babynames %>% 
  filter(name == "Noah") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Noah, by Sex")

babynames %>% 
  filter(name == "Olivia") %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(aes(color = sex)) + labs(x = "Year", y = "Number Born", 
                                     title = "Olivia, by Sex")

#The most popular names in 2017 are not considered gender neutral.  A parent
#would would be concerned about this would be unikely to choose these names. 

#Prediction of gender by name

#install gender and genderdata packages and all applicable libraries
#The gender package in cran contains only demonstration data. 
#For full data analysis, I had to download the genderdata package. 

head(gender)

#I used (method = "ssa"): United States from 1930 to 2012. 

ssa_names <- c("Charlie", "Royal", "Morgan", "Skyler",
               "Frankie", "Oakley", "Justice")
ssa_years <- c(rep(c(2009, 2012), 3), 2012)
ssa_df <- tibble(first_names = ssa_names,
                 last_names = LETTERS[1:7],
                 years = ssa_years,
                 min_years = ssa_years - 3,
                 max_years = ssa_years + 3)

ssa_df

#This dataset connects first names to years but there are columns 
#for minimum and maximum years for possible age range since birth dates are not always exact. 
#We pass this to gender_df() function, which assigns the method that we wish to use and the names of the columns that contain the names and the birth years. The result is a tibble of predictions.

results <- gender_df(ssa_df, name_col = "first_names", year_col = "years",
                     method = "ssa")


results
#gender_df() function calculates genders only for unique 
#combinations of first names and years

ssa_df %>% 
  left_join(results, by = c("first_names" = "name", "years" = "year_min"))

gender_df(ssa_df, name_col = "first_names",
          year_col = c("min_years", "max_years"), method = "ssa")

#Now, we use gender_df() to predict gender by passing it the columns 
#minimum and maximum years to be used for each name

ssa_df %>% 
  left_join(results, by = c("first_names" = "name", "years" = "year_min"))

gender_df(ssa_df, name_col = "first_names",
          year_col = c("min_years", "max_years"), method = "ssa")

ssa_df %>% 
  distinct(first_names, years) %>% 
  rowwise() %>% 
  do(results = gender(.$first_names, years = .$years, method = "ssa")) %>% 
  do(bind_rows(.$results))

ssa_df %>% 
  distinct(first_names, years) %>% 
  group_by(years) %>% 
  do(results = gender(.$first_names, years = .$years[1], method = "ssa")) %>% 
  do(bind_rows(.$results))

#Logistic Regression Model

library(plyr)
neutral_names <- babynames %>%
  select(-prop) %>%
  #filter only names between years 1930 and 2012  
  filter(year >= 1930, year <= 2012)  %>%
  #get the number of female and male for each name per year
  spread(key = sex, value = n, fill = 0)   %>%
  #Calculate the measure of gender-neutrality
  mutate(prop_F = 100 * F / (F+M), se = (50 - prop_F)^2) %>%
  group_by(name) %>%  
  #per name, find the total number of babies and measure of gender-neutrality
  dplyr::summarise(n = n(), female = sum(F), male=sum(M), total = sum(F + M),
            mse = mean(se)) %>%  
  #take only names that occurs every year and occurs greater than 9000 times
  filter(n == 83, total > 9000) %>%  
  #sort by gender neutrality
  arrange(mse) %>%  
  #get only the top 10
  head(10)  

neutral_names
 
#Random Forest Classification

library(randomForest)
neutral_names <- babynames %>%
  select(-prop) %>%
  #Filter only names between years 1930 and 2012
  filter(year >= 1930, year <= 2012) %>%
  #Get the number of female and male for each name per year
  spread(key = sex, value = n, fill = 0) %>%
  #Calculate the measure of gender-neutrality
  mutate(prop_F = 100 * F / (F+M), se = (50 - prop_F)^2) %>%
  group_by(name) %>%
  #Find the total number of babies and measure of gender-neutrality per name
  dplyr::summarise(n = n(), female = sum(F), male=sum(M), total = sum(F + M),
            mse = mean(se)) %>%
  #Take only names that occurs every year and occurs greater than 9000 times
  filter(n == 83, total > 9000) %>%
  #Sort by gender neutrality
  arrange(mse) %>%
  #Add variable to represent gender neutral namse. Assumes an mse <= 2000
  mutate(isNeutral = ifelse(mse <= 2000,1,0))

#Convert the response variable to factor
neutral_names$isNeutral <- as.factor(neutral_names$isNeutral)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(neutral_names), 0.7*nrow(neutral_names), replace = FALSE)
TrainSet <- neutral_names[train,]
ValidSet <- neutral_names[-train,]
summary(TrainSet)
summary(ValidSet)

# Create a Random Forest model with default parameters
model1 <- randomForest(isNeutral ~ ., data = TrainSet, importance = TRUE)
model1

# Predicting on train set
predTrain <- predict(model1, TrainSet, type = "class")

# Checking classification accuracy using the confusionMatrix() function in caret package
caret::confusionMatrix(predTrain, TrainSet$isNeutral)

# Predicting on test set
predValid <- predict(model1, ValidSet, type = "class")

# Checking classification accuracy using the confusionMatrix() function in caret package
caret::confusionMatrix(predValid, ValidSet$isNeutral)


#Train data accuracy is 100% that indicates all the values classified correctly.

#Predicting on test data
predTest <- predict(model1, ValidSet, type = "class")
caret::confusionMatrix(predTest, ValidSet$isNeutral)

#Validation data accuracy is 100% that indicates all the values classified correctly.

#Naive Bayes Classification

#Comparing model 1 of Random Forest with Naive Bayes model
model <- naive_bayes(isNeutral ~ ., data = TrainSet, usekernel = T)
model
plot(model)

#Prediction using naive bayes on training data
p <- predict(model, TrainSet, type = 'prob')

head(cbind(p, TrainSet))

#Confusion matrix for train data
p1 <- predict(model, TrainSet)
(tab1 <- table(p1, TrainSet$isNeutral))

#Calculate misscalculation/error
miscalc <- (1 - sum(diag(tab1)) / sum(tab1)) * 100

#Calculate model accuracy
accuracy <- (100- miscalc)
accuracy

#The model has an accuracy of 99.90357 on training data

#Predicting whether someone is alive by their name could be useful if someone 
#was interested in a historical name or wanted to avoid naming their baby something that might
#feel too dated for them. 

# Results #

#We can use logistic regression to make a prediction of gender from a name, we can use Random Forest Classification and Naive Bayes to make whether a name is gender neutral with close to 100% and over 99% accuracy, respectively. These methods are effective in determining whether a name is considered gender neutral based on its usage between genders.  Using these methods indicate that the methods of classification between genders is highly accurate.  

# Conclusion #

#The results indicate the name and the proportion of each biological sex given that name and a prediction of whether the name is generally considered male or female.  By using this data, a prospective parent can consider how names are viewed regarding gender neutrality based on statistical data from the ssa dataset. 


