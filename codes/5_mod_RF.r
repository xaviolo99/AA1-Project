library(randomForest)

set.seed(1433)

churn <- get(load("churn.RData"))
colnames(churn)

ind <- sapply(churn, is.numeric)
churn[ind] <- lapply(churn[ind], scale)

test_churn <- churn[4001:5000,]
churn <- churn[1:4000,]

drops <- c("state")
churn <- churn[,!(names(churn) %in% drops)]
test_churn <- test_churn[,!(names(test_churn) %in% drops)]

##################################################
# Forest 300 tree

(model.rf1 <- randomForest (churned ~ ., data=churn, ntree=300, proximity=FALSE))
plot(model.rf1)
pred.rf1 <- predict (model.rf1, test_churn, type="class")

# test error
(ct <- table(Truth=test_churn$churned, Pred=pred.rf1))

prop.table(ct, 1)
sum(diag(ct))/sum(ct)
round(100*(1-sum(diag(ct))/sum(ct)),2)

##################################################
# Forest 50 tree

(model.rf1 <- randomForest (churned ~ ., data=churn, ntree=50, proximity=FALSE))
plot(model.rf1)
pred.rf1 <- predict (model.rf1, test_churn, type="class")

# test error
(ct <- table(Truth=test_churn$churned, Pred=pred.rf1))

prop.table(ct, 1)
sum(diag(ct))/sum(ct)
round(100*(1-sum(diag(ct))/sum(ct)),2)