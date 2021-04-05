## Question 1

nsw_cps <- read_csv("Data/nsw_cps.csv")


# LPM ---------------------------------------------------------------------

# Model
lpm <- lm(treat ~ age + agesq + agecb + educ + educsq + marr + nodegree + 
            black + hisp + re74 + re75 + u74 + u75 + educ_re74,
            data = nsw_cps)

# Propensity Score
prs_lpm_df <- 
  tibble(
    pr_score = predict(lpm, type = "response"),
    treat = lpm$model$treat
  )


# Logit -------------------------------------------------------------------

# Model
logit <- glm(treat ~ age + agesq + agecb + educ + educsq + marr + nodegree + 
               black + hisp + re74 + re75 + u74 + u75 + educ_re74, 
             family = binomial(link = "logit"), data = nsw_cps)

# Propensity Score
prs_logit_df <- 
  tibble(
    pr_score = predict(logit, type = "response"),
    treat = logit$model$treat
  )
