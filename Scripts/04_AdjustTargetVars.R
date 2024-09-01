library(tidyverse)

#The goal is to adjust test score data to quantiles within the Year/TestType/TargetGroup groups
#Hopefully this *should* make it easier to use these mixed groups together in one model
load("Data/tmp.Rdata")

ggplot(df |> as_tibble()) +
  geom_histogram(aes(x = SAT_Total)) +
  facet_wrap(vars(TestType, Target_Grp))

pctiles <- df |> as_tibble() |> group_by(Target_Grp, TestType, Year) |> 
  reframe(SAT_Total_Pctile = percent_rank(SAT_Total), SAT_Math_Pctile = percent_rank(SAT_Math), SAT_ERW_Pctile = percent_rank(SAT_ERW), Test=Test, NCES_ID = NCES_ID)

df <- left_join(df, pctiles, by=c("Year", "NCES_ID", "Test", "TestType", "Target_Grp"))
save(df, file="Data/tmp.Rdata")
