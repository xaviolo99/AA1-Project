# 1: READ THE DATA

churn = read.csv("churn.csv", header = TRUE) # IMPORTANT: set the working directory to source file location!

dim(churn)
names(churn)

churn[1:4,1:7]
churn[1:4,8:12]
churn[1:4,13:17]
churn[1:4,18:21]

# 2: BASIC INSPECTION

summary(churn)
attach(churn)

hist(state, 51)
hist(account_length, 20)
hist(area_code, 100)
hist(phone_number, 100)

hist(number_vmail_messages)

hist(number_customer_service_calls)

hist(total_intl_calls, 15)

mean(account_length)

hist(international_plan, 2)
sum(international_plan==0)/(sum(international_plan==0)+sum(international_plan==1))
sum(voice_mail_plan==0)/(sum(voice_mail_plan==0)+sum(voice_mail_plan==1))

sum(class==0)/(sum(class==0)+sum(class==1)) # priors for the variable we want to predict

# 3: DEALING WITH MISSING VALUES

# No missings, as seen in basic inspection

# 4: TREATMENT OF MIXED DATA TYPES

state = as.factor(state)
area_code = as.factor(area_code)
international_plan = as.factor(international_plan)
voice_mail_plan = as.factor(voice_mail_plan)
churned = as.factor(class)

# 5 states + district of columbia
levels(state) = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL","GA","HI","IA","ID","IL","IN","KS","KY","LA","MA","MD","ME","MI","MN","MO","MS","MT","NC","ND","NE","NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")
levels(international_plan) = c("no","yes")
levels(voice_mail_plan) = c("no","yes")
levels(churned) = c("no","yes")

# 5: DERIVATION AND REMOVAL OF VARIABLES

# PHONE NUMBER
# We will not be including the variable phone number (not relevant)

# CHARGES AND MINUTES
pairs(data.frame(total_day_minutes, total_day_calls))
# We can observe a linear trend... probably because calls cost 17 cents/min

pairs(data.frame(total_eve_minutes, total_eve_charge))
# The same is observed, but calls cost 8.5 cents/min

pairs(data.frame(total_night_minutes, total_night_charge))
# The same, calls cost 4.5 cents/min

pairs(data.frame(total_intl_minutes, total_intl_charge))
# International calls cost 27 cents/min

# We will remove the charges since these are too redundant, and create a variable called total_charge
total_charge = total_day_charge + total_eve_charge + total_night_charge + total_intl_charge

# ACCOUNT_LENGTH
hist(account_length)
# We can see that it is quite normal, we dont know which order of magnitde uses
# but it does not matter, so no need to change it

pairs(data.frame(account_length, total_day_minutes))
# And that account length and minutes are not correlated
# meaning that total_x_y is a measure of last month or mean data, not cumulated. probably LAST MONTH

pairs(data.frame(account_length, number_customer_service_calls))
pairs(data.frame(account_length, number_vmail_messages))
# The same holds for customer service and vmail messages

pairs(data.frame(voice_mail_plan, number_vmail_messages))
hist(number_vmail_messages)
hist(number_vmail_messages[voice_mail_plan=="yes"])
# We can remove voice_mail_plan, since in general it does not provide additional information
# However, I think it may be a good idea to include it because there are 2 clear groups

hist(total_intl_charge)
hist(total_intl_charge[international_plan=="no"])
hist(total_intl_charge[international_plan=="yes"])
pairs(data.frame(international_plan, total_intl_calls))
# We will include international_plan, snce it seems to provide data not explained by total_intl_x

pairs(data.frame(total_day_calls+total_eve_calls+total_night_calls, total_intl_calls))
pairs(data.frame(total_day_charge+total_eve_charge+total_night_charge-total_intl_charge, total_intl_charge))
# There seems to be no correlation between total calls/charge and intl cars/charge
# we will treat them as separate calls from day/eve/night ones

# 6: CREATE NEW DATAFRAME

raw_churn = churn
churn = data.frame(state,area_code,account_length,international_plan,voice_mail_plan,number_vmail_messages,
                   total_day_minutes,total_day_calls,total_eve_minutes,total_eve_calls,
                   total_night_minutes,total_night_calls,total_intl_minutes,total_intl_calls,
                   total_charge,number_customer_service_calls,churned)
detach(churn)
attach(churn)

# 7: Gaussianity

barplot(table(state))
pie(table(area_code))
# OK

boxplot(account_length)
hist(account_length)
# Normal

pie(table(international_plan))
# Not balanced, but nothing problematic

hist(number_vmail_messages)
hist(total_intl_calls)
sum(total_intl_calls==0)/sum(total_day_calls)*100

hist(total_day_minutes)
hist(total_eve_minutes)
hist(total_night_minutes)
hist(total_intl_minutes)
# All these variables seem Gaussian

hist(total_charge)
hist(total_charge[churned=='no'])
hist(total_charge[churned=='yes'])
# This is really interesting, since it seems that user tends to churn when the total charge is not normal

hist(number_customer_service_calls, 10)

# 7: Shuffle

set.seed (1433)
churn = churn[sample.int(nrow(churn)),]

# 8: Save

save(churn, file = "churn.Rdata")

