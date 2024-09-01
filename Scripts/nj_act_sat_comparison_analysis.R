library(tidyverse)
library(Boruta)
library(catboost)
library(caret)
library(MLmetrics)

#Boruta Analysis:
load("Data/NJ_ACTSAT_Comparison_Train_Test/full_frame.Rdata")
ggplot(data=mutate(fullSchools, flag = ifelse(Year == 2017 | Year == 2018 | Year == 2019, "2017-19", "2020-2023"))) +
  geom_point(mapping = aes(x = ACT_Total, y = SAT_Total, color = Year)) +
  geom_errorbar(data = ACTSATConcordance, mapping = aes(x = ACT_Total, ymin = SAT_Low, ymax = SAT_High)) +
  facet_wrap(vars(flag))
#Many schools have SAT data but no ACT data, and the ones that do have ACT data mostly have higher SAT participation
#In the concordance study (https://satsuite.collegeboard.org/media/pdf/guide-2018-act-sat-concordance.pdf) researchers looked at students
#that took both tests. In this case performance differentials are likely due to differences between groups that take the SAT vs ACT.
#In other words, there are large differences in participation of the ACT and SAT, which likely impacts conversion error (in addition to random error)
borutaFrame <- fullSchools |> 
  select(!c(School_ID, ACT_Total, ACT_English, ACT_Reading, ACT_Math, ACT_Science, SAT_Total, SAT_ERW, SAT_Math)) |> 
  drop_na()
set.seed(1667)
boruta <- Boruta(SAT_Conversion_Error~., data=borutaFrame, doTrace=2)
print(boruta)
attStats(boruta)
#We see that Year, ACT_Participation, and Participation_Differential are the most important predictors by far
#Let's train a model:
frame <- drop_na(fullSchools, SAT_Conversion_Error)
ind <- createDataPartition(frame$SAT_Conversion_Error, p = 0.9, list = FALSE)
train <- frame[ind,]
test <- frame[-ind,]
ctrl <- trainControl(method = "cv", 
                     number = 5,
                     savePredictions = "final"
                    )

grid <- expand.grid(depth = c(2, 4, 6, 8),
                    learning_rate = c(0.1, 0.15, 0.2),
                    iterations = 100,
                    l2_leaf_reg = c(1e-3, 1e-6),
                    rsm = 0.95,
                    border_count = c(64, 128))

model <- train(x = train[,11:18],
               y = train$SAT_Conversion_Error,
               method = catboost.caret,
               trControl = ctrl,
               tuneGrid = grid,
               metric = "Rsquared"
              )
varImp(model)
#ACT_Participation is most important.
defaultSummary(cbind(tibble(obs = test$SAT_Conversion_Error), tibble(pred = predict(model, test[,11:18]))))

#Let's train a model for each year now:
years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
for(y in seq_along(years)) {
  load(paste0("Data/NJ_ACTSAT_Comparison_Train_Test/modeldata_",years[y],".Rdata"))
  set.seed(years[y]*7) 
  ind <- createDataPartition(tmp$SAT_Conversion_Error, p = 0.8, list = FALSE)
  train <- tmp[ind,]
  test <- tmp[-ind,]
  ctrl <- trainControl(method = "cv", 
                       number = 3,
                       savePredictions = "final"
                       )
  model <- train(x = train[,c(11:16, 18)],
                 y = train$SAT_Conversion_Error,
                 method = catboost.caret,
                 trControl = ctrl,
                 tuneLength = 5,
                 metric = "RMSE"
                )
  save(model, test, file = paste0("Models/NJ_ACTSAT_Comparison/catboost_",years[y],".Rdata"))
}

#Now, test the model on data from all years and report RMSE
years <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023)
for(y in seq_along(years)) {
  load(paste0("Models/NJ_ACTSAT_Comparison/catboost_",years[y],".Rdata"))
  rmseFrame <- tribble(
    ~Year, ~RMSE
  )
  for(i in seq_along(years)) {
    #If making predictions for own year, use the test set
    #Else predict on the entire dataset
    if(i == y) {
      load(paste0("Models/NJ_ACTSAT_Comparison/catboost_",years[i],".Rdata"))
      preds <- tibble(preds = predict(model, test))
      test <- cbind(test, preds) |> 
        mutate(SE = (SAT_Conversion_Error - preds)^2)
      RMSE <- test |> 
        group_by(Year) |> 
        summarize(RMSE = (mean(SE))^0.5)
      rmseFrame <- rbind(rmseFrame, RMSE)
    } else {
      load(paste0("Data/NJ_ACTSAT_Comparison_Train_Test/modeldata_",years[i],".Rdata"))
      preds <- tibble(preds = predict(model, tmp))
      tmp <- cbind(tmp, preds) |> 
        mutate(SE = (SAT_Conversion_Error - preds)^2)
      RMSE <- tmp |> 
        group_by(Year) |> 
        summarize(RMSE = (mean(SE))^0.5)
      rmseFrame <- rbind(rmseFrame, RMSE)
    }
  }
  #Make plot of RMSE against year, save
  ggplot(rmseFrame |> mutate(Year = as.character(Year))) +
    geom_point(aes(x = Year, y = RMSE)) +
    labs(title = "RMSE of Conversion Error Prediction by Year",
         subtitle = paste0("Using CatBoost Model Trained on ",years[y]," Data"))
  ggsave(filename = paste0("catboost_",years[y],"_rmse_plot.png"), path = "Plots/NJ_ACTSAT_Plots")
}
