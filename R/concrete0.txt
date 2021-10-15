###
###
###  Concrete - Prelim  ver 0.0.1
###
###
####################################################


#-------------------------------------------------
###--- 0. Preliminary
library(tidyverse)
Concrete <- read_csv("https://nmimoto.github.io/datasets/concrete.csv")
Concrete

print(Concrete, n=100)  # if you want to see more rows

  # Cement           (component 1)                    -- quantitative -- kg in a m3 mixture
  # Slag             (component 2) Blast Furnace Slag -- quantitative -- kg in a m3 mixture
  # Fly              (component 3) Fly Ash            -- quantitative -- kg in a m3 mixture
  # Water            (component 4)                    -- quantitative -- kg in a m3 mixture
  # Superplasticizer (component 5)                    -- quantitative -- kg in a m3 mixture
  # Coarse           (component 6) Coarse Aggregate   -- quantitative -- kg in a m3 mixture
  # Fine             (component 7) Fine Aggregate     -- quantitative -- kg in a m3 mixture
  # Age                                               -- quantitative -- Day (1~365)
  # CCS              Concrete compressive strength    -- quantitative -- MPa -- response variable

#- Histogram of the respoonse variable
hist(Concrete$CCS)

# Rename medv column as resp.
Concrete2 <- Concrete %>% rename(resp=CCS) %>% relocate(resp)
Concrete2
