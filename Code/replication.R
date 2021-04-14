library(tidyverse)
library(glue)
library(ggthemes)
library(knitr)
library(kableExtra)
options(scipen = 999)



# Question 1 --------------------------------------------------------------

# load data from `data_pull.R`
nsw_cps <- read_csv("Data/nsw_cps.csv")

## Part A ------------------------------------------------------------------

# LPM - Quadratic
lpm <- lm(treat ~ age + agesq + educ + educsq + marr + nodegree + 
            black + hisp + re75 + u75,
          data = nsw_cps)

# Logit - Cubic
logit <- glm(treat ~ age + agesq + agecb + educ + educsq + marr + nodegree + 
               black + hisp + re75 + u75, 
             family = binomial(link = "logit"), data = nsw_cps)

## Part B ------------------------------------------------------------------

# LPM -- Propensity Scores
prs_lpm_df <- 
  tibble(
    pr_score = predict(lpm, type = "response"),
    treat = lpm$model$treat
  )

# Logit -- Propensity Score
prs_logit_df <- 
  tibble(
    pr_score = predict(logit, type = "response"),
    treat = logit$model$treat
  )

## Part C ------------------------------------------------------------------

tx_ctrl_labs <- c("Control", "Treatment")
names(tx_ctrl_labs) <- c("0", "1")

# LPM -- Histogram
prs_lpm_df %>%
  ggplot() +
  geom_histogram(aes(x = pr_score), fill = 'midnightblue', color = 'black', alpha = 0.8) + 
  labs(x = "Propensity Score", y = "Density") +
  theme_clean() + 
  facet_grid(. ~ treat, labeller = labeller(treat = tx_ctrl_labs)) 

# LPM -- max/min
prs_lpm_df %>%
  group_by(treat) %>%
  summarise(Max = round(max(pr_score), 4), Min = round(min(pr_score), 4)) %>%
  mutate(treat = if_else(treat == 0, "Control", "Treatment")) %>%
  rename(Treat = treat) %>%
  kbl("pipe")
  

# Logit -- Histogram
prs_logit_df %>%
  ggplot() +
  geom_histogram(aes(x = pr_score), fill = 'midnightblue', color = 'black', alpha = 0.8) + 
  labs(x = "Propensity Score", y = "Density") +
  theme_clean() + 
  facet_grid(. ~ treat, labeller = labeller(treat = tx_ctrl_labs)) 

# Logit -- max/min
prs_logit_df %>%
  group_by(treat) %>%
  summarise(Max = round(max(pr_score), 4), Min = round(min(pr_score), 4)) %>%
  mutate(treat = if_else(treat == 0, "Control", "Treatment")) %>%
  rename(Treat = treat) %>%
  kbl("pipe")

## Part D ------------------------------------------------------------------

# LPM -- Histogram // filtered 
prs_lpm_df %>%
  filter(between(pr_score, 0.1, 0.9)) %>%
  ggplot() +
  geom_histogram(aes(x = pr_score), fill = 'midnightblue', color = 'black', alpha = 0.8) + 
  labs(x = "Propensity Score", y = "Density") +
  theme_clean() + 
  facet_grid(. ~ treat, labeller = labeller(treat = tx_ctrl_labs)) 

# LPM -- max/min // filtered
prs_lpm_df %>%
  filter(between(pr_score, 0.1, 0.9)) %>%
  group_by(treat) %>%
  summarise(Max = round(max(pr_score), 4), Min = round(min(pr_score), 4)) %>%
  mutate(treat = if_else(treat == 0, "Control", "Treatment")) %>%
  rename(Treat = treat) %>%
  kbl("pipe")


# Logit -- Histogram // filtered
prs_logit_df %>%
  filter(between(pr_score, 0.1, 0.9)) %>%
  ggplot() +
  geom_histogram(aes(x = pr_score), fill = 'midnightblue', color = 'black', alpha = 0.8) + 
  labs(x = "Propensity Score", y = "Density") +
  theme_clean() + 
  facet_grid(. ~ treat, labeller = labeller(treat = tx_ctrl_labs)) 

# Logit -- max/min // filtered
prs_logit_df %>%
  filter(between(pr_score, 0.1, 0.9)) %>%
  group_by(treat) %>%
  summarise(Max = round(max(pr_score), 4), Min = round(min(pr_score), 4)) %>%
  mutate(treat = if_else(treat == 0, "Control", "Treatment")) %>%
  rename(Treat = treat) %>%
  kbl("pipe")

# Question 2 --------------------------------------------------------------

# LPM -- First Difference

lpm_ey1 <- prs_lpm_df %>% 
  filter(treat == 1) %>%
  pull(pr_score) %>% 
  mean()

lpm_ey0 <- prs_lpm_df %>% 
  filter(treat == 0) %>%
  pull(pr_score) %>% 
  mean()

lpm_sdo <- round(lpm_ey1 - lpm_ey0, 4)
glue('The simple difference for the LPM model is {lpm_sdo}.')

# Logit -- First Difference
logit_ey1 <- prs_logit_df %>% 
  filter(treat == 1) %>%
  pull(pr_score) %>% 
  mean() 

logit_ey0 <- prs_logit_df %>% 
  filter(treat == 0) %>%
  pull(pr_score) %>% 
  mean() 

logit_sdo <- round(logit_ey1 - logit_ey0, 4)
glue('The simple difference for the logit model is {logit_sdo}.')

# Question 3 --------------------------------------------------------------


# Dehejia and Wahba (2002) -- `see ipw.do``
nsw_dw_cpscontrol <- nsw_cps %>% cbind(pscore = prs_logit_df$pr_score)

N <- nrow(nsw_dw_cpscontrol)

#- Manual with non-normalized weights using all data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1 - treat)/(1 - pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)


nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1 - treat) * re78/(1 - pscore),
         ht = y1 - y0)

#- Manual with normalized weights
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1 - treat)*re78/(1 - pscore))/(s0/N),
         norm = y1 - y0)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()

#-- trimming propensity score
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  select(-d1, -d0, -y1, -y0, -ht, -norm) %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

N <- nrow(nsw_dw_cpscontrol)

#- Manual with non-normalized weights using trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1 - treat)/(1 - pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1 - treat) * re78/(1 - pscore),
         ht = y1 - y0)

#- Manual with normalized weights with trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1 - treat)*re78/(1 - pscore))/(s0/N),
         norm = y1 - y0)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()

# Callway and Sant'Anna
p_dt1 <- mean(nsw_dw_cpscontrol$pscore)

nsw_dw_cpscontrol %>%
  mutate(estimator = (re78 - re75)/p_dt1 * (treat - pscore) / (1 - pscore)) %>%
  summarise(mean(estimator),
            sd(estimator)/sqrt(n()))

#' When I estimate the treatment effect using inverse probability weighting using the non-normalized weighting procedure described earlier, we find an estimated ATT of 
#' \$11,564. Using the normalization of the weights, I get \$6,182.
#' 
#' Then I repeat the analysis having trimmed the propensity score, keeping only values whose scores are between 0.1 and 0.9. 
#' Now I find $218 using the non-normalized weights and $755 using the normalized weights. And we can see that the normalized weights are even closer.
#' 
#' Using Callaway & Sant'Anna, I find an ATT of \$1282.
