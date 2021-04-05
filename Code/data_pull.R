# Retrieve data

library(tidyverse)
library(haven)

# load experimental group data
nsw <- 
  # import
  haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta") %>%
  # drop if treet == 0
  filter(treat != 0)

# merge CPS controls (Dehejia and Wahba 2002)
cps <-
  haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/cps_mixtape.dta")

nsw_cps <- 
  nsw %>%
  bind_rows(cps)

# remove from envir
rm(cps)
rm(nsw)

# new var
nsw_cps <- 
  nsw_cps %>%
  mutate(
    agesq = age^2,
    agecb = age^3,
    educsq = educ^2, # note: cunningam codes this as educ*edu
    re74sq = re74^2,
    re75sq = re75^2,
    u74 = case_when(
      # 1 when re74 == 0
      re74 == 0 ~ 1,
      # 0 when not a missing val (read haven source code)
      !is.nan(re74) == TRUE ~ 0
    ),
    u75 = case_when(
      re75 == 0 ~ 1,
      !is.nan(re75) == TRUE ~ 0
    ),
    # interaction1
    educ_re74 = educ * re74,
    # interaction2
    u74_hisp = u74 * hisp
  )

# write merged df to csv
write_csv(nsw_cps, path = "Data/nsw_cps.csv")
