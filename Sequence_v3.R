# load and clean up
x<- read.csv(file.choose(), header= TRUE)

# create variables
x$first_year <- substr(x$first_date,1,4)
x$first_products <- rowSums(x[c(3:11)])
x$second_products <- rowSums(x[c(13:21)])
x$third_products <- rowSums(x[c(23:31)])

x$play_1_bool <- ifelse(x$play_1>=1,1,0)
x$play_3_bool <- ifelse(x$play_3>=1,1,0)
x$play_5_bool <- ifelse(x$play_5>=1,1,0)
x$connect_bool <- ifelse(x$connect>=1,1,0)
x$connect_amp_bool <- ifelse(x$connect_amp >=1, 1, 0)
x$sub_bool <- ifelse(x$sub >=1, 1, 0)
x$bridge_bool <- ifelse(x$bridge >= 1, 1, 0)
x$boost_bool <- ifelse(x$boost>=1, 1, 0)
x$playbar_bool <- ifelse(x$playbar>=1, 1, 0)

x$play_1_bool_2nd <- ifelse(x$play_1_2nd>=1,1,0)
x$play_3_bool_2nd <- ifelse(x$play_3_2nd>=1,1,0)
x$play_5_bool_2nd <- ifelse(x$play_5_2nd>=1,1,0)
x$connect_bool_2nd <- ifelse(x$connect_2nd>=1,1,0)
x$connect_amp_bool_2nd <- ifelse(x$connect_amp_2nd >=1, 1, 0)
x$sub_bool_2nd <- ifelse(x$sub_2nd >=1, 1, 0)
x$bridge_bool_2nd <- ifelse(x$bridge_2nd >= 1, 1, 0)
x$boost_bool_2nd <- ifelse(x$boost_2nd>=1, 1, 0)
x$playbar_bool_2nd <- ifelse(x$playbar_2nd>=1, 1, 0)

x$play_1_bool_3rd <- ifelse(x$play_1_3rd>=1,1,0)
x$play_3_bool_3rd <- ifelse(x$play_3_3rd>=1,1,0)
x$play_5_bool_3rd <- ifelse(x$play_5_3rd>=1,1,0)
x$connect_bool_3rd <- ifelse(x$connect_3rd>=1,1,0)
x$connect_amp_bool_3rd <- ifelse(x$connect_amp_3rd >=1, 1, 0)
x$sub_bool_3rd <- ifelse(x$sub_3rd >=1, 1, 0)
x$bridge_bool_3rd <- ifelse(x$bridge_3rd >= 1, 1, 0)
x$boost_bool_3rd <- ifelse(x$boost_3rd>=1, 1, 0)
x$playbar_bool_3rd <- ifelse(x$playbar_3rd>=1, 1, 0)

dim(x)



x_2013 <- x[x$first_year == 2013, ]

# data doesn't seem to be very crazy
plot(x_2013$ratio, x_2013$second_products + x_2013$first_products)

# 
library(ggplot2)
ggplot(x_2013, aes(y=third_products, x= after_3rd)) + geom_point(alpha = .3)
ggplot(x_2013, aes(y=third_products, x= second_products)) + geom_point(alpha = .3)

# remove last quarter
x_2013_no_xmas <- x_2013[ as.numeric(as.character(substring(x_2013$first_date,6,7))) <= 9 , ]
nrow(x_2013_no_xmas) / nrow(x_2013) # 64% of data is before xmas
x_2013_no_xmas$second_date <- as.Date(x_2013_no_xmas$first_date) + x_2013_no_xmas$after_2nd
# remove if second purchase is in xmas
x_2013_no_xmas <- x_2013_no_xmas[ as.numeric(as.character(substring(x_2013_no_xmas$second_date,6,7))) <= 9 , ]

ggplot(x_2013_no_xmas, aes(y=second_products, x= after_2nd)) + geom_point(alpha = .3)
ggplot(x_2013_no_xmas, aes(y=second_products, x= first_products)) + geom_point(alpha = .3)

# data set used for analysis from this point forward
x_original <- x_2013_no_xmas

# k means
library(fpc)
x_use <- x_original[,c(36:62)]
fit <- kmeans(x_use, 4)
aggregate(x_use,by=list(fit$cluster),FUN=mean)
plotcluster(x_use, fit$cluster)
write.excel(aggregate(x_use,by=list(fit$cluster),FUN=mean))

# # of distinct first products
x_original$distinct_first <- rowSums(x_original[,c(25:33)])

# formula
formula = second_products ~ distinct_first + first_products + play_1_bool + play_3_bool + play_5_bool + connect_bool + connect_amp_bool + sub_bool + bridge_bool + boost_bool + playbar_bool

grep("play_1_bool", colnames(x_original))
grep("playbar_bool_3rd", colnames(x_original))

# decision tree
library(rpart)
fit <- rpart(formula, data=x_original, method="anova")
plotcp(fit)
plot(fit)
text(fit)

# random forest
library(randomForest)
fit <- randomForest(formula, data=x_original)
print(fit)
importance(fit)
#getTree()
plot(fit)
plot(importance(fit), lty=2, pch=16)
#lines(importance(fit))

# linear regression
fit <- lm(after_2nd ~ first_products, data=x_original)
summary(fit)
library(MASS)
fit <- lm(second_products ~ first_products + as.factor(play_1_bool) + as.factor(play_3_bool) + as.factor(play_5_bool) + as.factor(connect_bool) + as.factor(connect_amp_bool) + as.factor(sub_bool) + as.factor(bridge_bool) + as.factor(boost_bool) + as.factor(playbar_bool), data=x_original)
step <- stepAIC(fit, direction="both")
step$anova # display results
summary(fit)



# 
# buy 2nd time? boolean
x_original$return_customer <- ifelse(x_original$second_products >=1, 1, 0)

#formula = third_products ~ first_products + second_products + play_1_bool + play_3_bool + play_5_bool + connect_bool + connect_amp_bool + sub_bool + bridge_bool + boost_bool + playbar_bool
formula = third_products ~ first_products + second_products + play_1_bool + play_3_bool + play_5_bool + connect_bool + connect_amp_bool + sub_bool + bridge_bool + boost_bool + playbar_bool + play_1_bool_2nd + play_3_bool_2nd + play_5_bool_2nd + connect_bool_2nd + connect_amp_bool_2nd + sub_bool_2nd + bridge_bool_2nd + boost_bool_2nd + playbar_bool_2nd

nrow(as.factor(x_original$return_customer))
length(x_original$return_customer)

# decision tree
library(rpart)
fit <- rpart(formula, data=x_original, method="anova")
plotcp(fit)
plot(fit)
text(fit)

min(x_original$return_customer)

plot(x_original$distinct_first, x_original$return_customer)
ggplot(x_original, aes(y=return_customer, x= distinct_first)) + geom_point(alpha = .1)
boxplot(return_customer ~ distinct_first, data=x_original)



# explore

