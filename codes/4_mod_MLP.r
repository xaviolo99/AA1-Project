library(nnet)
library(caret)

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


train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

errors <- function (model)
{
  options(digits=4)
  p1 <- as.factor(predict (model, type="class"))
  t1 <- table(p1,churned)
  cat ("Train = ", 100*(1-sum(diag(t1))/4000),"%\n")
  p2 <- as.factor(predict (model, newdata=test_churn, type="class"))
  t2 <- table(p2,test_churn$churned)
  cat ("Test = ", 100*(1-sum(diag(t2))/1000),"%\n")
}

###############

# size 10 decay 0.1

model.nnet <- nnet(churned ~., data = churn, size=10, maxit=500, decay=0.1)
summary(model.nnet)
errors(model.nnet)

# The time elapsed in my computer is 110 seconds (1 min 50 secs) per 5 times 10 fold CV and 10 hidden

#############################################
# 10 hidden neurons with different regularizations

(decays <- 10^seq(-2,0,by=0.2))

start_time <- Sys.time()
model.10CV <- train(churned ~., data = churn, method='nnet', maxit = 500, trace = FALSE,
                        tuneGrid = expand.grid(.size=10,.decay=decays), trControl=train_control)
end_time <- Sys.time()

end_time - start_time

# Results
model.10CV$results
model.10CV$bestTune

#Time difference of 21.17 mins
#size   decay Accuracy  Kappa AccuracySD KappaSD
#1    10 0.01000   0.9293 0.6836    0.01326 0.06324
#2    10 0.01585   0.9333 0.7028    0.01414 0.06348
#3    10 0.02512   0.9375 0.7177    0.01391 0.06770
#4    10 0.03981   0.9409 0.7333    0.01155 0.05496
#5    10 0.06310   0.9385 0.7214    0.01271 0.06061
#6    10 0.10000   0.9385 0.7202    0.01423 0.06667
#7    10 0.15849   0.9401 0.7259    0.01274 0.06124
#8    10 0.25119   0.9395 0.7222    0.01303 0.06368
#9    10 0.39811   0.9381 0.7131    0.01215 0.05995
#10   10 0.63096   0.9346 0.6907    0.01052 0.05364
#11   10 1.00000   0.9314 0.6672    0.01109 0.05812

# 0.04 and 0.16 seem the best options
# Choices between 0.04 and 0.25 are all good choices

##########################################
# 20 hidden neurons with different regularizations
(decays <- 10^seq(-1.5,0.5,by=0.2))

start_time <- Sys.time()
model.10CV <- train(churned ~., data = churn, method='nnet', maxit = 500, trace = FALSE,
                    tuneGrid = expand.grid(.size=20,.decay=decays), trControl=train_control)
end_time <- Sys.time()

end_time - start_time

# Results
model.10CV$results
model.10CV$bestTune

#Time difference of 54.74 mins
#size   decay Accuracy  Kappa AccuracySD KappaSD
#1    20 0.03162   0.9039 0.5962   0.013308 0.05057
#2    20 0.05012   0.9125 0.6262   0.012697 0.05258
#3    20 0.07943   0.9173 0.6371   0.011487 0.05240
#4    20 0.12589   0.9265 0.6732   0.012837 0.06024
#5    20 0.19953   0.9281 0.6707   0.011827 0.05684
#6    20 0.31623   0.9339 0.6960   0.011250 0.05434
#7    20 0.50119   0.9348 0.6945   0.008970 0.04621
#8    20 0.79433   0.9345 0.6884   0.009543 0.04734
#9    20 1.25893   0.9319 0.6656   0.009551 0.05366
#10   20 1.99526   0.9248 0.6181   0.008358 0.05083
#11   20 3.16228   0.9134 0.5401   0.008598 0.05454

##########################################
# 7 hidden neurons with different regularizations
(decays <- 10^seq(-2.25,-0.25,by=0.2))

start_time <- Sys.time()
model.10CV <- train(churned ~., data = churn, method='nnet', maxit = 500, trace = FALSE,
                    tuneGrid = expand.grid(.size=7,.decay=decays), trControl=train_control)
end_time <- Sys.time()

end_time - start_time

# Results
model.10CV$results
model.10CV$bestTune

#Time difference of 12.82 mins
#size    decay Accuracy  Kappa AccuracySD KappaSD
#1     7 0.005623   0.9326 0.6933   0.015774 0.06809
#2     7 0.008913   0.9386 0.7177   0.011933 0.05711
#3     7 0.014125   0.9399 0.7258   0.012402 0.05654
#4     7 0.022387   0.9410 0.7298   0.012361 0.06242
#5     7 0.035481   0.9396 0.7238   0.012444 0.05973
#6     7 0.056234   0.9446 0.7456   0.011643 0.05516
#7     7 0.089125   0.9434 0.7418   0.011603 0.05426
#8     7 0.141254   0.9427 0.7353   0.010459 0.05049
#9     7 0.223872   0.9408 0.7270   0.008622 0.04285
#10    7 0.354813   0.9365 0.7002   0.010644 0.05111
#11    7 0.562341   0.9368 0.7005   0.007458 0.03843

# Size 10 optimal decay 0.04
model.nnet <- nnet(churned ~., data = churn, size=7, maxit=500, decay=0.056)
summary(model.nnet)
errors(model.nnet)
