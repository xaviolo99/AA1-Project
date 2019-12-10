# Load the Data, Set Seed and generate a Test Set

churn <- get(load("churn.RData")) # IMPORTANT: set the working directory to source file location!
set.seed(1433)

# 80% for training/validation (4000), 20% for testing (1000)
churned = churn[,17]
churn <- churn[,-17]
test_churn <- churn[4001:5000,]
test_churned <- churned[4001:5000]
churn <- churn[1:4000,]
churned <- churned[1:4000]

length(churned[churned=='no'])/4000 # 86.25% is the prior of 'no' for the training/validation set
length(test_churned[test_churned=='no'])/1000 # 84.3% is the prior of 'no' for the test set

# Disclaimer: It is not needed to use the test set in LDA, QDA and Naive Bayes because there are no hyperparameters to be set, but for consistency test error is checked.

################################################
################################################

# LDA

cont_churn = churn[c(3,6,7,8,9,10,11,12,13,14,15,16)]

# cont_churn with binary categorical variables included (worse accuracy)
#cont_churn <- churn[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
#cont_churn[,2] <- as.numeric(cont_churn[,2])
#cont_churn[,3] <- as.numeric(cont_churn[,3])

table(churned)

library(klaR)

(ldamod <- lda(x=cont_churn, grouping=churned))
plot(ldamod)
ldamod$prior

# We see a separation, but it is not significant at all

loadings <- as.matrix(cont_churn) %*% as.matrix(ldamod$scaling)

plot(loadings[churned=='yes',],col='red')
points(loadings[churned=='no',],col='blue')
abline(mean(loadings[churned=='yes',]),col='red',0)
abline(mean(loadings[churned=='no',]),col='blue',0)

(tr_err <- table(Truth=churned, Pred=predict(ldamod, scaled_cont_churn)$class))
sum(diag(prop.table(tr_err)))

# NORMALIZED LDA (as it can be expected, normalized QDA/LDA obtain the exact same results) (though not exactly the same... ¿precision problems?)

scaled_cont_churn = scale(cont_churn)
(ldamod2 <- lda(x=scaled_cont_churn, grouping=churned))

(tr_err <- table(Truth=churned, Pred=predict(ldamod2, scaled_cont_churn)$class))
sum(diag(prop.table(tr_err)))

# LOO CV ERROR

ldamod_cv <- lda(x=cont_churn,grouping=churned,CV=TRUE)

ldamod_cv$class[1:10]
ldamod_cv$posterior[1:10]

(ct <- table(Truth=churned, Pred=ldamod_cv$class))
sum(diag(prop.table(ct)))
# The obtained LOO CV accuracy is 86.325%

# Test set accuracy
test_cont_churn <- test_churn[c(3,6,7,8,9,10,11,12,13,14,15,16)]

# test_cont_churn with binary categorical variables included (worse accuracy)
#test_cont_churn <- test_churn[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
#test_cont_churn[,2] <- as.numeric(test_cont_churn[,2])
#test_cont_churn[,3] <- as.numeric(test_cont_churn[,3])

(test_err <- table(Truth=test_churned, Pred=predict(ldamod, test_cont_churn)$class))
sum(diag(prop.table(test_err)))


################################################
################################################

# QDA

(qdamod <- qda(x=cont_churn,grouping=churned))

# CV QDA

qdamod_cv <- qda(x=cont_churn,grouping=churned,CV=TRUE)
head(qdamod_cv$posterior)

(ct <- table(Truth=churned, Pred=qdamod_cv$class))
sum(diag(prop.table(ct)))
# The LOOCV accuracy improves to 89.18%

# Test set accuracy
(test_err <- table(Truth=test_churned, Pred=predict(qdamod, test_cont_churn)$class))
sum(diag(prop.table(test_err)))

################################################
################################################

# kNN

library(class)

knn_churn <- churn[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
knn_churn[,2] <- as.numeric(knn_churn[,2])
knn_churn[,3] <- as.numeric(knn_churn[,3])

test_knn_churn <- test_churn[c(3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
test_knn_churn[,2] <- as.numeric(test_knn_churn[,2])
test_knn_churn[,3] <- as.numeric(test_knn_churn[,3])

# Warning: Takes a noticeable amount of time to execute!
# LOO CV kNN

knn.preds <- rep(NA, nrow(knn_churn))
y <- vector()
for (a in 1:20){
  for (i in 1:nrow(knn_churn)){
    knn.preds[i] <- knn (knn_churn[-i,], knn_churn[i,], churned[-i], k = a) 
  }
  (tab <- table(Truth=churned, Preds=knn.preds))
  print(sum(tab[row(tab)==col(tab)])/sum(tab))
  y <- c(y, sum(tab[row(tab)==col(tab)])/sum(tab))
}

plot(y,xlim=c(0,20),ylim=c(0.81,0.91),type="l",col='red')

# 89.55% LOO CV accuracy when k = 5, 9
# Test accuracy with best k:

predicted <- knn(knn_churn, test_knn_churn, churned, k=9)

(ct <- table(Truth=test_churned, Pred=predicted))
sum(diag(prop.table(ct)))

# LOO CV NORMALIZED kNN

knn_churn <- scale(knn_churn)
knn.preds <- rep(NA, nrow(knn_churn))
y <- vector()
for (a in 1:20){
  for (i in 1:nrow(knn_churn)){
    knn.preds[i] <- knn (knn_churn[-i,], knn_churn[i,], churned[-i], k = a) 
  }
  (tab <- table(Truth=churned, Preds=knn.preds))
  print(sum(tab[row(tab)==col(tab)])/sum(tab))
  y <- c(y, sum(tab[row(tab)==col(tab)])/sum(tab))
}

plot(y,xlim=c(0,20),ylim=c(0.875,0.91),type="l",col='red')

# 90.72% LOO CV accuracy when k = 7
# Test accuracy with best k:

predicted <- knn(knn_churn, test_knn_churn, churned, k=9)

(ct <- table(Truth=test_churned, Pred=predicted))
sum(diag(prop.table(ct)))

# KNN gets a boost when normalized (in this dataset at least)

################################################
################################################

# Naive Bayes LOOCV

library(e1071)

naive_churn <- churn[,1:16]

naive.preds <- rep(NA, nrow(naive_churn))
for (i in 1:nrow(naive_churn)){
  naive.preds[i] <- predict(naiveBayes(naive_churn[-i,], churned[-i]), naive_churn[i,])
}
(tab <- table(Truth=churned, Preds=naive.preds))
print(sum(tab[row(tab)==col(tab)])/sum(tab))

# test error

test_naive_churn <- test_churn[,1:16]
nbtp <- predict(naiveBayes(naive_churn, churned), test_churn)

(tab <- table(Truth=churned, Preds=nbtp))
print(sum(tab[row(tab)==col(tab)])/sum(tab))

