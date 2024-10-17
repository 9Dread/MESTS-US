IPR <- function() {
  data <- read_csv("Data/IPR/IPR_2016.csv") |> 
    rename(NCES_ID = NCESSCH) |> 
    mutate(Year = "2016") |> 
    select(!IPR_SE)
  years <- c(2017, 2018, 2019, 2020, 2021)
  for(year in years) {
    data <- rbind(data, read_csv(paste0("Data/IPR/IPR_",year,".csv")) |> 
                    rename(NCES_ID = NCESSCH) |> 
                    mutate(Year = as.character(year)) |> 
                    select(!IPR_SE)) 
  }
  return(data)
}