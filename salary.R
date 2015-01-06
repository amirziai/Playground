train <- read.csv(file.choose())
train_salary <- read.csv(file.choose())
test <- read.csv(file.choose())

t <- merge(train,train_salary, by="jobId")

formula = salary ~ companyId + jobType + degree + major + industry + yearsExperience + milesFromMetropolis
fit <- rpart(formula, data=t, method="anova")
summary(fit)
plot(fit)
text(fit)
fancyRpartPlot(fit)

#plot(salary ~ milesFromMetropolis, data=t)
library(randomForest)
set.seed(310)
formula = salary ~ jobType + degree + major + industry + yearsExperience + milesFromMetropolis
fit <- randomForest(formula, data=ts, importance=TRUE, ntree=200) 
plot(fit)
print(fit)
summary(fit)

# visualizations
library(ggplot2)
ggplot(t, aes(degree, salary)) + geom_point(aes(colour=industry, alpha = .5))
library(scatterplot3d)
scatterplot3d(t$degree,t$industry,t$salary)
summary(t$salary)
ts <- t[sample(1:nrow(t), 1000,replace=FALSE),]
print(fit)
importance(fit) 
predict(fit,test)
library(party)
fit2 <- cforest(formula, data=ts, controls=cforest_control(mtry=2, mincriterion=0))
library(evtree)
fit3 <- evtree(formula, data=ts)
print(fit3)
summary(fit3)
plot(fit3)

tnj <- t[t$jobType != 'JANITOR',]
tnjs <- tnj[sample(1:nrow(tnj), 10000, replace=FALSE),]
plot(tnjs$yearsExperience, tnjs$salary)
model1 <- lm(salary ~ yearsExperience, data = tnjs)
summary(model1)
library(car)
scatterplot(salary ~ yearsExperience, data=tnjs)

plot(salary ~ industry, data = tnjs)
library(ggplot2)
tnjs_sub <- tnjs[tnjs$industry == 'EDUCATION' | tnjs$industry == 'OIL',]
ggplot(tnjs_sub, aes(yearsExperience, salary)) + geom_point(aes(colour=industry, alpha = .1))

summary(tnjs$industry)
tnjs_ind <- tnjs[tnjs$industry == 'AUTO',]
model_ind <- lm(salary ~ yearsExperience, data = tnjs_ind)
plot(tnjs$yearsExperience, tnjs$salary)
summary(model_ind)

# 
summary(tnjs$major)
x <- tnjs[tnjs$degree == 'DOCTORAL',]
plot(salary ~ industry, data = x)

prop.table(table(tnjs$industry, tnjs$degree))

# dplyr
library(dplyr)
summary(tnjs$degree)
x <- filter(tnjs, industry == 'WEB' & jobType == 'CEO' & degree == 'DOCTORAL')
x
hist(x$salary)
arrange(tnjs, desc(salary))
select(tnjs,companyId,industry,degree)
select(tnjs,-companyId)
distinct(select(tnjs,companyId,jobType))
summarise(tnjs, x = mean(salary))

#
library(reshape2)
#m <- melt(tnjs, id.vars = c("industry",""))
tnjs$experience <- ifelse(tnjs$yearsExperience < 10,"low",
                          ifelse(tnjs$yearsExperience <20,"med","high"))
hist(tnjs$yearsExperience)
summary(tnjs$yearsExperience)
grouped <- group_by(tnjs, industry, degree, jobType,companyId,experience)
grouped
x <- summarise(grouped, mean=mean(salary), sd=sd(salary),count=length(salary))
dim(x)

