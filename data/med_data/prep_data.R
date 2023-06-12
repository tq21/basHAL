
library(data.table)
library(tidyverse)

cleveland <- fread("data/med_data/processed.cleveland.data")
cleveland[V14 != 0, V14 := 1]
cleveland <- drop_na(cleveland)
write.csv(cleveland, file = "data/med_data/cleveland.csv")


# ablone, target: last col
ablone <- fread("data/med_data/abalone/abalone.data")
ablone[, V1 := ifelse(V1 == "F", 1, 0)]
ablone <- as.data.frame(drop_na(ablone))
write.csv(ablone, file = "data/med_data/ablone.csv")

# credit, target: last col, binomial
credit <- fread("data/med_data/statlog+german+credit+data/german.data-numeric")
credit <- drop_na(credit)
credit <- as.data.frame(credit)
write.csv(credit, file = "data/med_data/credit.csv")
