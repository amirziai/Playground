x<- read.csv(file.choose(), header= TRUE)

# create variables
x$first_year <- substr(x$first_date,1,4)
x$first_products <- rowSums(x[c(3:11)])
x$second_products <- rowSums(x[c(13:21)])

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

dim(x)
x_2013 <- x

formula = after_2nd ~ first_products + play_1_bool + play_3_bool + play_5_bool + connect_bool + connect_amp_bool + sub_bool + bridge_bool + boost_bool + playbar_bool

x_2013$after_2nd[x_2013$after_2nd > 20]

plot(x_2013$first_products, x_2013$second_products)

ggplot(x_2013, aes(y=second_products, x= first_products)) + geom_point(aes(size = after_2nd))

# tree
library(rpart)
#fit <- rpart(formula, data=x, method="anova")
fit <- rpart(formula, data=x_2013, method="class")
plotcp(fit)
plot(fit)
text(fit)

# random forest
library(randomForest)
fit <- randomForest(formula, data=x)
print(fit)
importance(fit)
getTree()
plot(fit)
plot(importance(fit), lty=2, pch=16)
lines(importance(fit))


# k-means
#x_use <- x[,c(25:33)]
x_use <- x[,c(34:42)]
fit <- kmeans(x_use, 3)
aggregate(x_use,by=list(fit$cluster),FUN=mean)
plotcluster(x_use, fit$cluster)

write.excel(aggregate(x_use,by=list(fit$cluster),FUN=mean))
