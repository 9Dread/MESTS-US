library(tidyverse)
library(sf)
library(catboost)
library(caret)
load("Data/tmp.Rdata")

#Here we join the data to "time-invariant" variables which we will use to predict the score percentiles. "Time-invariant" variables are variables
#that only have one observation per school irrespective of time, meaning if we were to join them to the dataset normally, there would be duplicate
#variable observations for any school included more than once in the dataset. To make time-invariant variables more interpretable for the machine
#learning models, we combine all of them into one predictor variable by predicting test score percentiles with these variables alone.

#Time-invariant vars include Opportunity Atlas, Social Capital Atlas, and PUMA Wealth inequality (2020) vars.
#Also, there are many NA points in the data. To preserve as many schools in our data as possible, we thus use catboost which allows NA values.
#We do, however, remove observations for which no match exists.

#We additionally train and test using data from all years regardless of the fact that the predictors are the same across multiple years.
#The goal is to get a typical measurement independent of the year; we don't want the predictions to rely too much on a specific year's data.
#Year-based dependencies are for the year-based variables we introduce in the next step of the modeling stage.




pumaWealth2020 <- read_csv("Data/puma_wealth_inequality.csv") |> 
  filter(year == 2020) |> 
  select(!contains("sd")) |>
  select(!c(cntygp97, cntygp98)) |> 
  rename(FIPS = statefip) |> 
  select(!year)
df <- as_tibble(df) |> 
  mutate(puma = as.numeric(puma)) |> 
  select(Year, NCES_ID, puma, FIPS, tract2010, tract2020, zip, SAT_Total_Pctile, SAT_Math_Pctile, SAT_ERW_Pctile) |> 
  left_join(pumaWealth2020, by=c("FIPS", "puma"))
rm(pumaWealth2020)

#Here we join with Social Capital data. This will reduce our data points. We conduct testing on whether this is worth it for
#prediction accuracy.

scZIP <- read_csv("Data/Social_Capital/social_capital_zip.csv") |> 
  select(c(zip, ec_zip, ec_se_zip, civic_organizations_zip, support_ratio_zip))
df <- semi_join(df, scZIP, by="zip") |> 
  left_join(scZIP, by="zip")
rm(scZIP)

scHS <- read_csv("Data/Social_Capital/social_capital_high_school.csv") |> 
  rename(NCES_ID = high_school) |> 
  select(!contains("_se_")) |> 
  select(!c(high_school_name, zip, county))

df <- semi_join(df, scHS, by="NCES_ID") |> 
  left_join(scHS, by="NCES_ID")
rm(scHS)

#Now add Opportunity Atlas data

tractUM <- read_csv("Data/Opportunity_Atlas/tract_inc_age35_allSubgroups.csv") |> 
  select(tract, contains("rP")) |> 
  select(tract, contains("gP")) |> 
  rename(tract2010 = tract)

df <- df |> 
  mutate(tract2010 = as.numeric(tract2010)) |> 
  semi_join(tractUM, by="tract2010") |> 
  left_join(tractUM, by="tract2010")

rm(tractUM)
save(df, file="Data/tmp_time_invar.Rdata")

#Now begin modeling. Train catboost on RMSE, R^2, and MAE
set.seed(1667)
frame <- as_tibble(drop_na(df)) |> 
  select(!geometry)
ind <- createDataPartition(frame$SAT_Total_Pctile, p = 0.9, list = FALSE)
train <- frame[ind,]
test <- frame[-ind,]
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = "final"
                    )
model1 <- train(x = train[,11:65],
               y = train$SAT_Total_Pctile,
               method = catboost.caret,
               trControl = ctrl,
               metric = "Rsquared"
              )
model2 <- train(x = train[,11:65],
                y = train$SAT_Total_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "RMSE"
                )
model3 <- train(x = train[,11:65],
                y = train$SAT_Total_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "MAE"
                )
save(model1, train, test, ind, frame, file="Models/TimeInvarR2Catboost.Rdata")
save(model2, train, test, ind, frame, file="Models/TimeInvarRMSECatboost.Rdata")
save(model3, train, test, ind, frame, file="Models/TimeInvarMAECatboost.Rdata")

#Math predictions
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = "final"
)
model1 <- train(x = train[,11:65],
                y = train$SAT_Math_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
model2 <- train(x = train[,11:65],
                y = train$SAT_Math_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "RMSE"
)
model3 <- train(x = train[,11:65],
                y = train$SAT_Math_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "MAE"
)
save(model1, train, test, ind, frame, file="Models/TimeInvarMathR2Catboost.Rdata")
save(model2, train, test, ind, frame, file="Models/TimeInvarMathRMSECatboost.Rdata")
save(model3, train, test, ind, frame, file="Models/TimeInvarMathMAECatboost.Rdata")

#ERW predictions
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = "final"
)
model1 <- train(x = train[,11:65],
                y = train$SAT_ERW_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
model2 <- train(x = train[,11:65],
                y = train$SAT_ERW_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "RMSE"
)
model3 <- train(x = train[,11:65],
                y = train$SAT_ERW_Pctile,
                method = catboost.caret,
                trControl = ctrl,
                metric = "MAE"
                )
save(model1, train, test, ind, frame, file="Models/TimeInvarERWR2Catboost.Rdata")
save(model2, train, test, ind, frame, file="Models/TimeInvarERWRMSECatboost.Rdata")
save(model3, train, test, ind, frame, file="Models/TimeInvarERWMAECatboost.Rdata")
