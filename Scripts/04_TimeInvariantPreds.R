library(tidyverse)
library(caret)
library(catboost)
load("Data/tmp.Rdata")

#Time-invariant vars include Opportunity Atlas, Social Capital Atlas, and PUMA Wealth inequality (2020) vars.

#We additionally train and test using data from all years regardless of the fact that the predictors are the same across multiple years.
#The goal is to get a typical measurement independent of the year; we don't want the predictions to rely too much on a specific year's data.

pumaWealth2020 <- read_csv("Data/puma_wealth_inequality.csv") |> 
  filter(year == 2020) |> 
  select(statefip, puma, wealth_median, wealth_bottom50, own_outright) |> 
  rename(FIPS = statefip)
df <- df |> select(!geometry) |> as_tibble() |> 
  mutate(puma = as.numeric(puma)) |> 
  select(Year, Test, NCES_ID, puma, FIPS, tract2010, tract2020, zip, SAT_Total, SAT_Math, SAT_ERW) |> 
  left_join(pumaWealth2020, by=c("FIPS", "puma"))
rm(pumaWealth2020)

#Here we join with Social Capital data. 
scZIP <- read_csv("Data/Social_Capital/social_capital_zip.csv") |> 
  select(zip, civic_organizations_zip)
df <- semi_join(df, scZIP, by="zip") |> 
  left_join(scZIP, by="zip")
rm(scZIP)

scHS <- read_csv("Data/Social_Capital/social_capital_high_school.csv") |> 
  rename(NCES_ID = high_school) |> 
  select(NCES_ID, ec_high_own_ses_hs, ec_high_parent_ses_hs, volunteering_rate_hs, exposure_parent_ses_hs)
df <- semi_join(df, scHS, by="NCES_ID") |> 
  left_join(scHS, by="NCES_ID")
rm(scHS)

#Now add Opportunity Atlas data (Shown not to be useful; see further analysis in [make new script])
tractUM <- read_csv("Data/Opportunity_Atlas/tract_inc_age35_allSubgroups.csv") |> 
  select(tract, contains("rP")) |> 
  select(tract, contains("gP")) |> 
  rename(tract2010 = tract)
#df <- df |> 
#  mutate(tract2010 = as.numeric(tract2010)) |> 
#  semi_join(tractUM, by="tract2010") |> 
#  left_join(tractUM, by="tract2010")
rm(tractUM)

tractNC <- read_csv("Data/Opportunity_Atlas/tract_allNeighborhoodCharacteristics.csv") |> 
  select(!c(id2, name, aland_sqmi))  |> 
  rename(tract2010 = tract)
#df <- df |> 
#  mutate(tract2010 = as.numeric(tract2010)) |> 
#  semi_join(tractNC, by="tract2010") |> 
#  left_join(tractNC, by="tract2010")
rm(tractNC)
save(df, file="Data/tmp_time_invar.Rdata")

#Now begin modeling. Train catboost on RMSE, R^2, and MAE
set.seed(1667)
frame <- drop_na(df) |> 
  distinct()
ind <- createDataPartition(frame$SAT_Total, p = 0.9, list = FALSE)
train <- frame[ind,]
test <- frame[-ind,]
ctrl <- trainControl(method = "cv", 
                     number = 10
)
model1 <- train(x = train[,12:19],
                y = train$SAT_Total,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
varImp(model1)
defaultSummary(cbind(tibble(obs = test$SAT_Total), tibble(pred = predict(model1, test[,12:19]))))
save(model1, train, test, ind, frame, file="Models/TimeInvarR2Catboost.Rdata")

model1 <- train(x = train[,12:19],
                y = train$SAT_Math,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
varImp(model1)
defaultSummary(cbind(tibble(obs = test$SAT_Math), tibble(pred = predict(model1, test[,12:19]))))
save(model1, train, test, ind, frame, file="Models/TimeInvarMathR2Catboost.Rdata")

model1 <- train(x = train[,12:19],
                y = train$SAT_ERW,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
varImp(model1)
defaultSummary(cbind(tibble(obs = test$SAT_ERW), tibble(pred = predict(model1, test[,12:19]))))
save(model1, train, test, ind, frame, file="Models/TimeInvarERWR2Catboost.Rdata")

#Make prediction data frame:
load("Models/TimeInvarR2Catboost.Rdata")
df <- as_tibble(drop_na(df)) |> 
  distinct()
row <- tibble(row = 1:35835)
df <- cbind(df, row)
df <- mutate(df, total_timeinvar = predict(model1, df[row,12:19]))

load("Models/TimeInvarMathR2Catboost.Rdata")
df <- mutate(df, math_timeinvar = predict(model1, df[row,12:19]))

load("Models/TimeInvarERWR2Catboost.Rdata")
df <- mutate(df, erw_timeinvar = predict(model1, df[row,12:19]))
df <- df |> 
  select(Year, Test, NCES_ID, puma, FIPS, tract2010, tract2020, zip, SAT_Total, SAT_Math, SAT_ERW, total_timeinvar, math_timeinvar, erw_timeinvar)
timeinvar <- df

#Save:
save(timeinvar, file="Data/timeinvar.Rdata")
