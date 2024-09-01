library(tidyverse)
library(catboost)
library(caret)
library(MLmetrics)

set.seed(1667)
#Here we analyze the results of the 3 trained catboost models using time-invariant variables.

load("Models/TimeInvarR2Catboost.Rdata")
load("Models/TimeInvarRMSECatboost.Rdata")
load("Models/TimeInvarMAECatboost.Rdata")

#Variable importance for the 3 models:
varImp(model1)
varImp(model2)
varImp(model3)

#We see that consistently the top 4 variables are exposure_parent_ses_hs, volunteering_rate_hs, ec_high_own_ses_hs, and white.
#In particular, exposure_parent_ses_hs is always the most important by far.

#Here we see error measures on the test set predictions:
defaultSummary(cbind(tibble(obs = test$SAT_Total_Pctile), tibble(pred = predict(model1, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_Total_Pctile), tibble(pred = predict(model2, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_Total_Pctile), tibble(pred = predict(model3, test[,11:65]))))

#They are very similar, robust to the training metric. For some reason the R^2 and MAE models perform exactly the same.

#Let's compute predictions for all data points to see if there are any systematic patterns in error.
row <- tibble(row = 1:36545)
frame <- cbind(frame, row)
frame <- mutate(frame, pred = predict(model1, frame[row,11:65]))
frame <- mutate(frame, abs_error = abs(pred - SAT_Total_Pctile))

mean_error_by_year <- frame |> 
  group_by(Year) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_year
#The highest errors seem to be in years that are closer to the endpoints of the time interval. This makes sense because earlier years
#are represented by less states (so the model is less fitted to them), and later years were disrupted because of the pandemic.

#TX = 48, TN = 47, SC = 45, NJ = 34, NC = 37, MA = 25, IA = 19, GA = 13, CO = 8
mean_error_by_state <- frame |> 
  group_by(FIPS) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_state
#Highest errors for IA and NC. Lowest for CO and MA.


#Math:
load("Models/TimeInvarMathR2Catboost.Rdata")
load("Models/TimeInvarMathRMSECatboost.Rdata")
load("Models/TimeInvarMathMAECatboost.Rdata")

varImp(model1)
varImp(model2)
varImp(model3)
#Similar top variables, but white and ec_high_own_ses_hs seem to matter a lot more.

#Error measures on the test set predictions:
defaultSummary(cbind(tibble(obs = test$SAT_Math_Pctile), tibble(pred = predict(model1, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_Math_Pctile), tibble(pred = predict(model2, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_Math_Pctile), tibble(pred = predict(model3, test[,11:65]))))
#Same exact models regardless of error message

frame <- mutate(frame, pred = predict(model1, frame[row,11:65]))
frame <- mutate(frame, abs_error = abs(pred - SAT_Total_Pctile))

mean_error_by_year <- frame |> 
  group_by(Year) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_year
#The highest errors again pool towards endpoints of time interval. Error is generally higher compared to total score.

#TX = 48, TN = 47, SC = 45, NJ = 34, NC = 37, MA = 25, IA = 19, GA = 13, CO = 8
mean_error_by_state <- frame |> 
  group_by(FIPS) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_state
#Again highest errors for IA and NC. Lowest for CO and MA.


#ERW:
load("Models/TimeInvarERWR2Catboost.Rdata")
load("Models/TimeInvarERWRMSECatboost.Rdata")
load("Models/TimeInvarERWMAECatboost.Rdata")

varImp(model1)
varImp(model2)
varImp(model3)
#Similar top variables, but white and ec_high_own_ses_hs seem to matter a lot more.

#Error measures on the test set predictions:
defaultSummary(cbind(tibble(obs = test$SAT_ERW_Pctile), tibble(pred = predict(model1, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_ERW_Pctile), tibble(pred = predict(model2, test[,11:65]))))
defaultSummary(cbind(tibble(obs = test$SAT_ERW_Pctile), tibble(pred = predict(model3, test[,11:65]))))
#Same exact models regardless of error message

frame <- mutate(frame, pred = predict(model1, frame[row,11:65]))
frame <- mutate(frame, abs_error = abs(pred - SAT_Total_Pctile))

mean_error_by_year <- frame |> 
  group_by(Year) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_year
#The highest errors again pool towards endpoints of time interval. Error is generally higher compared to total score.

#TX = 48, TN = 47, SC = 45, NJ = 34, NC = 37, MA = 25, IA = 19, GA = 13, CO = 8
mean_error_by_state <- frame |> 
  group_by(FIPS) |> 
  summarize(MeanAbsErr = mean(abs_error))
mean_error_by_state
#Again highest errors for IA and NC. Lowest for CO and MA.
