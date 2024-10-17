library(tidyverse)
library(caret)
library(catboost)
library(sf)
load("Data/tmp.Rdata")
load("Data/timeinvar.Rdata")

timeinvar <- timeinvar |> select(!c(tract2010, tract2020, puma))
df <- as_tibble(df) |> select(!geometry) |> left_join(timeinvar, by=c("Year", "Test", "NCES_ID", "FIPS", "zip", "SAT_Total", "SAT_Math", "SAT_ERW")) |> distinct(NCES_ID, Year, Test, Part_Rate, Num_Tests, SAT_Total, SAT_Math, SAT_ERW, .keep_all = TRUE) |> drop_na()
df <- relocate(df, SAT_ERW, SAT_Math, SAT_Total, Target_Grp, Year, State, FIPS, Test, TestType, NCES_ID, zip, puma, SAT_Total_Pctile, SAT_Math_Pctile, SAT_ERW_Pctile)
df <- mutate(df, leaid = str_sub(NCES_ID, 1, 7)) |> 
  relocate(leaid)
df <- left_join(df, df1 |> mutate(Year = as.character(Year)), by=c("Year", "NCES_ID"))
df <- inner_join(df, IPR |> select(!NAME), by=c("Year", "NCES_ID"))
df <- inner_join(df, data, by=c("Year", "leaid"))
df <- inner_join(df, chronicabsentee, by = c("NCES_ID", "Year"))


save(df, file="Data/finalframe.Rdata")


set.seed(563725)
ind <- createDataPartition(df$SAT_Total, p = 0.9, list = FALSE)
train <- df[ind,]
test <- df[-ind,]
ctrl <- trainControl(method = "cv", 
                     number = 10
)
catboost <- train(x = train[,c(20, 21, 26)],
                y = train$SAT_Total,
                method = catboost.caret,
                trControl = ctrl,
                metric = "Rsquared"
)
varImp(catboost)
defaultSummary(cbind(tibble(obs = test$SAT_Total), tibble(pred = predict(catboost, test[,c(20, 21, 26)]))))
defaultSummary(tibble(obs = test$SAT_Total, pred = test$total_timeinvar))

#linear model between invar pred and actual score
ggplot(df) +
  geom_point(aes(x = total_timeinvar, y = SAT_Total))
linear <- lm(SAT_Total ~ total_timeinvar, data = train)

defaultSummary(cbind(tibble(obs = test$SAT_Total), tibble(pred = predict(linear, test[,26]))))
summary(linear)

#Create stacked model
df <- cbind(df, tibble(total_catboost = predict(catboost, df[,c(20, 21, 26)])))
df <- cbind(df, tibble(total_linear = predict(linear, tibble(total_timeinvar = df[,26]))))

train <- df[ind,]
test <- df[-ind,]
stacked <- train(x = train[,c(29,30)],
                  y = train$SAT_Total,
                  method = catboost.caret,
                  trControl = ctrl,
                  metric = "Rsquared"
)
varImp(stacked)
defaultSummary(cbind(tibble(obs = test$SAT_Total), tibble(pred = predict(stacked, test[,c(29,30)]))))
