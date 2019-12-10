library(caret)

set.seed(1433)

churn <- get(load("churn.RData"))
colnames(churn)

glm_churn <- churn[1:4000,]
ind <- sapply(glm_churn, is.numeric)
glm_churn[ind] <- lapply(glm_churn[ind], scale)

glm_churn_test <-churn[4001:5000,]
ind <- sapply(glm_churn_test, is.numeric)
glm_churn_test[ind] <- lapply(glm_churn_test[ind], scale)

train_control <- trainControl(method = "repeatedcv", number = 100, repeats = 10)

detach(glm_churn)
attach(glm_churn)

############### CLASSICAL FITTING

# Obvious model
model <- glm(churned ~ state+area_code+account_length+international_plan+voice_mail_plan+number_vmail_messages+
             total_day_minutes+total_day_calls+total_eve_minutes+total_eve_calls+total_night_minutes+total_night_calls+total_intl_minutes+total_intl_calls+
             total_charge+number_customer_service_calls, family=binomial(link=logit)) 
# According to model, most relevant variables are international_plan, voice_mail_plan, number_vmail_messages, total_intl_calls, number_customer_service_calls

#model <- glm(churned ~ state+area_code+account_length+voice_mail_plan+number_vmail_messages*number_customer_service_calls*total_charge+
#               total_day_minutes*total_day_calls+total_eve_minutes*total_eve_calls+total_night_minutes*total_night_calls+total_intl_minutes*total_intl_calls*international_plan
#             , family=binomial(link=logit)) 

# Good model 1
model <- glm(churned ~ 
               voice_mail_plan*number_vmail_messages*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_charge+
               total_intl_minutes*total_intl_calls*international_plan*total_charge
             , family=binomial(link=logit)) 

# Good model 2
model <- glm(churned ~ 
               number_vmail_messages*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_charge+
               total_intl_minutes*total_intl_calls*international_plan*total_charge
             , family=binomial(link=logit)) 

# Good model 3
model <- glm(churned ~ 
               voice_mail_plan*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_charge+
               total_intl_minutes*total_intl_calls*international_plan*total_charge
             , family=binomial(link=logit)) 

# Good model 4 BEST SMALL
model <- glm(churned ~ 
               voice_mail_plan*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_charge+
               total_intl_minutes*total_intl_calls*international_plan
             , family=binomial(link=logit)) 

# Good model 5 BEST BIG
model <- glm(churned ~ 
               voice_mail_plan*international_plan*total_charge+
               number_customer_service_calls*total_intl_minutes*total_intl_calls*international_plan*total_charge
             , family=binomial(link=logit)) 

# Good model 6 BEST MEDIUM
model <- glm(churned ~ 
               voice_mail_plan*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_charge+
               number_customer_service_calls*international_plan*total_intl_minutes*total_intl_calls
             , family=binomial(link=logit)) 

# Model 7 MONSTER (spoiler: OVERFITS)
model <- glm(churned ~ 
               voice_mail_plan*international_plan*total_charge*number_customer_service_calls*total_intl_minutes*total_intl_calls*account_length
             , family=binomial(link=logit)) 

############### 100-FOLD CV

# Obvious model
(cv_model <- train(churned ~ state+area_code+account_length+international_plan+voice_mail_plan+number_vmail_messages+
               total_day_minutes+total_day_calls+total_eve_minutes+total_eve_calls+total_night_minutes+total_night_calls+total_intl_minutes+total_intl_calls+
               total_charge+number_customer_service_calls,
               data = glm_churn,
               trControl = train_control,
               method = "glm",
               family=binomial(link=logit)))

# Good model 1
(cv_model <- train(churned ~ voice_mail_plan*number_vmail_messages*international_plan*total_charge+
                 number_customer_service_calls*international_plan*total_charge+
                 total_intl_minutes*total_intl_calls*international_plan*total_charge,
               data = glm_churn,
               trControl = train_control,
               method = "glm",
               family=binomial(link=logit)))

# Good model 2
(cv_model <- train(churned ~ number_vmail_messages*international_plan*total_charge+
                  number_customer_service_calls*international_plan*total_charge+
                  total_intl_minutes*total_intl_calls*international_plan*total_charge,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

# Good model 3
(cv_model <- train(churned ~ voice_mail_plan*international_plan*total_charge+
                  number_customer_service_calls*international_plan*total_charge+
                  total_intl_minutes*total_intl_calls*international_plan*total_charge,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

# Good model 4 BEST SMALL
(cv_model <- train(churned ~ voice_mail_plan*international_plan*total_charge+
                  number_customer_service_calls*international_plan*total_charge+
                  total_intl_minutes*total_intl_calls*international_plan,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

# Good model 5 BEST BIG
(cv_model <- train(churned ~ voice_mail_plan*international_plan*total_charge+
                  number_customer_service_calls*total_intl_minutes*total_intl_calls*international_plan*total_charge,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

# Good model 6 BEST MEDIUM
(cv_model <- train(churned ~ voice_mail_plan*international_plan*total_charge+
                  number_customer_service_calls*international_plan*total_charge+
                  number_customer_service_calls*international_plan*total_intl_minutes*total_intl_calls,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

# Model 7 MONSTER (spoiler: OVERFITS)
(cv_model <- train(churned ~ voice_mail_plan*international_plan*total_charge*number_customer_service_calls*total_intl_minutes*total_intl_calls*account_length,
                data = glm_churn,
                trControl = train_control,
                method = "glm",
                family=binomial(link=logit)))

############### PROPERTIES

summary(model)
coef(model)
length(coef(model))

############### TEST

preds <- predict (model, glm_churn_test,type="response")
preds <- as.factor(round(preds))
levels(preds) = c("no","yes")

(tab <- table(Truth=glm_churn_test$churned, Preds=preds))
sum(tab[row(tab)==col(tab)])/sum(tab)

