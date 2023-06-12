library(data.table)
library(tidyverse)

# glass, target: last col, multinomial
glass <- fread("data/small_data/glass+identification/glass.data")
glass <- as.data.frame(glass)
write.csv(glass, "data/small_data/glass.csv")

# spam, target: last col, binomial
spam <- fread("data/med_data/spambase/spambase.data")
spam <- as.data.frame(drop_na(spam))
write.csv(spam, "data/med_data/spam.csv")


# ionosphere
iono <- fread("data/small_data/ionosphere/ionosphere.data")
iono[, V35 := ifelse(V35 == "g", 1, 0)]
iono <- as.data.frame(drop_na(iono))
write.csv(iono, "data/small_data/iono.csv")
