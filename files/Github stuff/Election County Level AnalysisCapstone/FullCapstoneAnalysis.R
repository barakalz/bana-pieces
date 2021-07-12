library(data.table)
library(tidyverse)
library(readxl)
library(rpart)
library(MASS)
library(boot)
library(ROCR)
setwd("C:/Users/Laith/Documents/University of Cincinnati/SpringSem20/Capstone")
#Import and data transformation

##importing labor statistics 
file.urls <- c('Labor force datasets/laucnty12.xlsx',
                'Labor force datasets/laucnty13.xlsx',
                'Labor force datasets/laucnty14.xlsx',
                'Labor force datasets/laucnty15.xlsx',
                'Labor force datasets/laucnty16.xlsx')

labor12 <- read_excel(file.urls[1], skip = 4)
labor12 <- labor12[-1,]
labor12$FIPS <- paste(labor12$Code...2, labor12$Code...3, sep = "")

labor13 <- read_excel(file.urls[2], skip = 4)
labor13 <- labor13[-1,]
labor13$FIPS <- paste(labor13$Code...2, labor13$Code...3, sep = "")

labor14 <- read_excel(file.urls[3], skip = 4)
labor14 <- labor14[-1,]
labor14$FIPS <- paste(labor14$Code...2, labor14$Code...3, sep = "")

labor15 <- read_excel(file.urls[4], skip = 4)
labor15 <- labor15[-1,]
labor15$FIPS <- paste(labor15$Code...2, labor15$Code...3, sep = "")

labor16 <- read_excel(file.urls[5], skip = 4)
labor16 <- labor16[-1,]
labor16$FIPS <- paste(labor16$Code...2, labor16$Code...3, sep = "")

labor <- rbind(labor12,labor13,labor14,labor15,labor16)
head(labor)

glimpse(labor)

##importing election results 
countyres.url <- "https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ"
elect.res <- fread(countyres.url, stringsAsFactors = TRUE)
glimpse(elect.res)

electiontable <- elect.res %>% 
  dplyr::select(FIPS, year, state, county, candidate, candidatevotes) %>% 
  filter(year == "2016" | year == "2012") %>% 
  spread(candidate,candidatevotes) %>% 
  arrange(FIPS)
glimpse(electiontable)
head(electiontable)

##unworkable joins
###labor %>% 
###  select(FIPS, Year, Force, Employed, Unemployed) %>% 
###  filter(FIPS == c("03220", "03221", "03222","06442", "06443", "06444", "09664","09665", "09666", '12886', "12887", "12888", "16108", "16109","16110"))

###labor %>% 
###  select(FIPS, Year, Force, Employed, Unemployed) %>% 
###  spread(Year, Force)

head(electiontable)
head(labor)

electiontable$FIPS <- as.character(electiontable$FIPS)
labor$FIPS <- if_else(str_starts(labor$FIPS,"0"),
                      str_sub(labor$FIPS,2),
                      labor$FIPS)

##join election and labor tables
finaltable <- electiontable %>% 
  left_join(labor, by = "FIPS") %>% 
  dplyr::select(FIPS, year, state, county, `Donald Trump`, `Hillary Clinton`, Other, Year, Force, Employed, Unemployed, `Mitt Romney`, `Barack Obama`)
str(finaltable)
head(finaltable)

###spread function for multiple columns
myspread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}

##final spread function
finaltable <- finaltable %>% myspread(Year, c(Force, Employed, Unemployed))
str(finaltable)
summary(finaltable)
head(finaltable, n = 20)

##drop unneeded columns
DF <- data.frame(
  finaltable
  )
drops <- c("NA_Employed","NA_Force", "NA_Unemployed")
finaltable <- DF[, !(names(DF) %in% drops)]
str(finaltable)

##solidify last table
elect_labor <- data.frame(finaltable) 
summary(elect_labor)
str(elect_labor)
boxplot(elect_labor$Donald.Trump)

elect_labor12 <- elect_labor %>% 
  dplyr::select(FIPS,year,Mitt.Romney,Barack.Obama) %>% 
  filter(year == 2012)

elect_labor16 <- elect_labor %>%
  dplyr::select(FIPS,
         state,
         county,
         year,
         Donald.Trump,
         Hillary.Clinton,
         X2012_Employed,X2012_Force,X2012_Unemployed,
         X2013_Employed,X2013_Force,X2013_Unemployed,
         X2014_Employed,X2014_Force,X2014_Unemployed,
         X2015_Employed,X2015_Force,X2015_Unemployed,
         X2016_Employed,X2016_Force,X2016_Unemployed
        ) %>% 
  filter(year == 2016)

elect_labor <- elect_labor12 %>% 
  inner_join(elect_labor16, by = "FIPS")
head(elect_labor, n = 20)
str(elect_labor)

#Data cleaning and EDA
head(elect_labor, n = 10)
summary(elect_labor) 
glimpse(elect_labor)

##building binary variables
elect_labor$TrumpWin <- if_else(elect_labor$Donald.Trump > elect_labor$Hillary.Clinton, 1, 0)
elect_labor$ObamaWin <- if_else(elect_labor$Barack.Obama > elect_labor$Mitt.Romney, 1, 0)
elect_labor$TrumpFlip <- if_else(elect_labor$ObamaWin == 1 & elect_labor$TrumpWin == 1, 1, 0)

##gut checking total popular vote numbers 
elect_labor %>% 
  summarize(
    MittVotes = sum(Mitt.Romney, na.rm = TRUE),
    ObamaVotes = sum(Barack.Obama, na.rm = TRUE),
    HillVotes = sum(Hillary.Clinton, na.rm = TRUE),
    TrumpVotes = sum(Donald.Trump, na.rm = TRUE)
  )
##pretty close to the real thing!
##let's proceed. 

##building change variables
elect_labor$unemp_growth_12to13 <- (elect_labor$X2013_Unemployed - elect_labor$X2012_Unemployed)/elect_labor$X2013_Force
elect_labor$unemp_growth_13to14 <- (elect_labor$X2014_Unemployed - elect_labor$X2013_Unemployed)/elect_labor$X2014_Force
elect_labor$unemp_growth_14to15 <- (elect_labor$X2015_Unemployed - elect_labor$X2014_Unemployed)/elect_labor$X2015_Force
elect_labor$unemp_growth_15to16 <- (elect_labor$X2016_Unemployed - elect_labor$X2015_Unemployed)/elect_labor$X2016_Force

summary(elect_labor,digits = max(2, getOption("digits")-3))

elect_labor %>% 
  summarize(
    CountyPercentTrumpWon = mean(TrumpWin, na.rm = TRUE),
    CountyPercentObamaWon = mean(ObamaWin, na.rm = TRUE),
    CountyPercentTrumpFlip = mean(TrumpFlip, na.rm = TRUE)
  )

par(mfrow=c(2,2))
plot(elect_labor$unemp_growth_12to13)
plot(elect_labor$unemp_growth_13to14)
plot(elect_labor$unemp_growth_14to15)
plot(elect_labor$unemp_growth_15to16)

par(mfrow=c(2,2))
hist(elect_labor$unemp_growth_12to13)
hist(elect_labor$unemp_growth_13to14)
hist(elect_labor$unemp_growth_14to15)
hist(elect_labor$unemp_growth_15to16)

par(mfrow=c(1,1))
hist(elect_labor$TrumpFlip)
##problematically for our hypothesis, it looks like unemployment did not grow extensively over the 4-year period leading up to the 2016 election. 

##correlation study
glimpse(elect_labor)
cor(elect_labor[,28:31], use = "pairwise")

#eliminate the few NA rows for model building
summary(elect_labor)
elect_labor <- elect_labor[complete.cases(elect_labor), ]


#Regression Models - unemployment growth rate by county year to year

##step development

elect.glm.full <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14+unemp_growth_12to13, family = binomial, data = elect_labor)
summary(elect.glm.full)

model_step_f <- step(elect.glm.full, trace = 0, direction = "forward")
summary(model_step_f)

model_step_fbic <- step(elect.glm.full, direction = c("forward"), criterion = "BIC")
summary(model_step_fbic)

##full model

index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.glm.full.train <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14+unemp_growth_12to13, family = binomial, data = elect_labor_train)
elect.glm.part.train <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14, family = binomial, data = elect_labor_train)
anova(elect.glm.full.train,elect.glm.part.train, test = "Chisq")

pred.glm.train <- predict(elect.glm.full.train, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC curve for Unemployment Growth Rate Training Data")
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.glm.full.train, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC curve for Unemployment Growth Rate Testing Data")
unlist(slot(performance(pred, "auc"), "y.values"))

##partial model

elect.glm.full <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14+unemp_growth_12to13, family = binomial, data = elect_labor_train)
summary(elect.glm.full)

model_step_fbic <- step(elect.glm.full, direction = c("forward"), criterion = "BIC")
summary(model_step_fbic)

index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.glm.part.train <- model_step_fbic
pred.glm.train <- predict(elect.glm.part.train, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.glm.full.train, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

###same results as full model

##exploring probit and loglog?
###probit
index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.glm.full.train.probit <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14+unemp_growth_12to13, family = binomial(link="probit"), data = elect_labor_train)

pred.glm.train <- predict(elect.glm.full.train.probit, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.glm.full.train.probit, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

###loglog
elect.glm.full.train.loglog <- glm(TrumpFlip~unemp_growth_15to16+unemp_growth_14to15+unemp_growth_13to14+unemp_growth_12to13, family = binomial(link="cloglog"), data = elect_labor_train)

pred.glm.train <- predict(elect.glm.full.train.loglog, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.glm.full.train.loglog, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

#Regression Models - unemployment rate at election year

##variable generation
elect_labor$unemp.rate16 <- elect_labor$X2016_Unemployed / elect_labor$X2016_Force
head(elect_labor)

##stepwise variable selection - unneeded since one variable is used 

##test set cross validation

index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.rate.glm <- glm(TrumpFlip ~ unemp.rate16, family ='binomial', data = elect_labor_train)
pred.glm.train <- predict(elect.rate.glm, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC curve for 2016 unemployment rate training data")
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.rate.glm, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC curve for 2016 unemployment rate testing data")
unlist(slot(performance(pred, "auc"), "y.values"))

##exploring probit and loglog?
###probit
index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.rate.glm.probit <- glm(TrumpFlip~unemp.rate16, family = binomial(link="probit"), data = elect_labor_train)

pred.glm.train <- predict(elect.rate.glm.probit, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.glm.full.train.probit, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)

###loglog
elect.rate.glm.loglog <- glm(TrumpFlip~unemp.rate16, family = binomial(link="cloglog"), data = elect_labor_train)

pred.glm.train <- predict(elect.rate.glm.loglog, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.rate.glm.loglog, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE)



#Regression Models - state

##stepwise variable selection - unneeded since one variable is used 

##test set cross validation
index <- sample(nrow(elect_labor), nrow(elect_labor)*.80)
elect_labor_train <-  elect_labor[index,]
elect_labor_test <- elect_labor[-index,]

elect.state.glm <- glm(TrumpFlip ~ state, family ='binomial', data = elect_labor_train)
pred.glm.train <- predict(elect.state.glm, type = "response")
pred <- prediction(pred.glm.train, elect_labor_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve for State Model, training dataset")
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.state.glm, newdata = elect_labor_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve for State Model, testing dataset")
unlist(slot(performance(pred, "auc"), "y.values"))
summary(elect.state.glm)
write.csv(elect_labor, "elect_labor.csv")


#income regression model
income <- read_excel("income.xls", skip = 3)
glimpse(income)
length(income)
income$FIPS <- paste(income$`State FIPS Code`, income$`County FIPS Code`, sep = "")
income$FIPS <- if_else(str_starts(income$FIPS, "0"), str_sub(income$FIPS, start = 2, end = 5), income$FIPS)
income <- income[,c("FIPS", "Median Household Income")]
elect_labor_income <- left_join(elect_labor, income, by = "FIPS")
glimpse(elect_labor_income)
elect_labor_income$med_income <- as.numeric(elect_labor_income$`Median Household Income`)


index <- sample(nrow(elect_labor_income), nrow(elect_labor_income)*.80)
elect_labor_income_train <-  elect_labor_income[index,]
elect_labor_income_test <- elect_labor_income[-index,]

elect.income.glm <- glm(TrumpFlip ~ med_income, family ='binomial', data = elect_labor_income_train)
pred.glm.train <- predict(elect.income.glm, type = "response")
pred <- prediction(pred.glm.train, elect_labor_income_train$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve for income Model, training dataset")
unlist(slot(performance(pred, "auc"), "y.values"))

pred.glm0.test <- predict(elect.income.glm, newdata = elect_labor_income_test, type = "response")
pred <- prediction(pred.glm0.test, elect_labor_income_test$TrumpFlip)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, main = "ROC Curve for income Model, testing dataset")
unlist(slot(performance(pred, "auc"), "y.values"))
