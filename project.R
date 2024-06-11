

# laoding and preparing the data
data <- read.csv("protests.csv",header = TRUE,skip = 1)
# chaniging treu false to 0,1
data[which(data$Success. == FALSE),"Success."] <- 0
data[which(data$Success. == TRUE),"Success."] <- 1

data[which(data$greater.than.3.5..of.pop.involved. == FALSE),"greater.than.3.5..of.pop.involved."] <- 0
data[which(data$greater.than.3.5..of.pop.involved. == TRUE),"greater.than.3.5..of.pop.involved."] <- 1

data[which(data$Problem. == "chill"),"Problem."] <- 0
data[which(data$Problem. == "PROBLEM"),"Problem."] <- 1

# bringing the variables into the right format (mostly datatype)
data$participants.as...population <- gsub("%","",data$participants.as...population)
data$participants.as...population <- as.double(data$participants.as...population)

data$MAX.of.participants <- gsub(",","",data$MAX.of.participants)
data$MAX.of.participants <- as.numeric(data$MAX.of.participants)

data$Pop.in.that.year <- gsub(",","",data$Pop.in.that.year)
data$Pop.in.that.year <- as.numeric(data$Pop.in.that.year)

data$Problem. <- as.numeric(data$Problem.)

# grouping countrie into continents
library(countrycode)

data$continent <- countrycode(data$Location,"country.name","continent")
data$continent[which(data$Location == "Czechoslovakia")] <- "Europe"
data$continent[which(is.na(data$continent))] <- "Asia" # 3 different Yemens
data$continent <- as.factor(data$continent)

# renaming the columns
names <- c("campaign","population_involved","success","max_participants","ID","year_max_involvement","first_year","last_year","location","pop_during_height", "more_than_3.5_involved","violent","continent")
colnames(data) <- names

# creating a "duration" variable instead of 3 variables representing years
data$duration <- data$last_year-data$first_year +1

summary(data)
str(data)

# explore the data
pairs(data[,-c(1,9)])

hist(data$max_participants) # large outliers so a log transform might be good
data$logMax_participants <- log2(data$max_participants)
hist(data$logMax_participants) # better for sure

hist(data$pop_during_height) # same with overall pop
data$logPop_during_height <- log2(data$pop_during_height)
hist(data$logPop_during_height) #still alittle skewed but ok

hist(data$population_involved)
data$logPop_involved <- log2(data$population_involved)
data$logPop_involved[which(data$logPop_involved == "-Inf")] <- 0
hist(data$logPop_involved)

pairs(data[,-c(1,5,6,7,8,9,11)])

data_mod <- data[,-c(1,2,4,5,6,7,8,9,10,11)]



# first glm

logit <- glm(success~.,data = data_mod, family = "binomial")
summary(logit)

# the diagnostics from the script
# checking deviance with monte carlo
B <- 999
Devs <- rep(0,B)
for(b in 1:B){
  newData <- data_mod
  newData$Survival <- simulate(logit)$sim_1
  newmodel <- glm(success~., data=newData, family=binomial)
  Devs[b] <- newmodel$deviance
}
hist(Devs,main="", xlab="deviance")
abline(v=logit$deviance, col="red", lwd=2)

# plotting standard errors
plot(rstandard(logit))
smoother <- loess(rstandard(logit)~c(1:nrow(data_mod)))
ind <- sort(c(1:nrow(data_mod))) # need x-axis ordered to plot a line
points(ind,fitted(smoother)[ind],type="l")

# model submodel test
logit_sub1 <- glm(success~violent+logPop_involved+duration,data = data_mod, family = binomial)
anova(logit,logit_sub1,test="LRT") # submodel is significantly better

summary(logit_sub1)
logit_sub2 <- glm(success~logPop_involved+duration, data = data_mod, family = binomial)
anova(logit_sub1,logit_sub2,test="LRT")

summary(logit_sub2)
plot(logit_sub2)


