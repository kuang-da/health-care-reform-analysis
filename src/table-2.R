library(dplyr)
library(ggplot2)
library(tidyverse)
library(foreign)

hcg <-  read.table("data/w.data")
colnames(hcg) = c("id","year", "doctco", "age", 
                  "male","educ","married","hsize", 
                  "sport", "goodh", "badh", "sozh", 
                  "loginc", "ft", "pt", "unemp", 
                  "winter", "spring", "fall")
emp_vec <- c()
for (idx in seq(dim(hcg)[1])){
  if(hcg[[idx, 'ft']] == 1)
    emp_vec <- c(emp_vec, 'full-time')
  else if (hcg[[idx, 'pt']] == 1)
    emp_vec <- c(emp_vec, 'part-time')
  else if (hcg[[idx, 'unemp']] == 1)
    emp_vec <- c(emp_vec, 'unemployed')
  else
    emp_vec <- c(emp_vec, 'self-employed')
}

data_df <- hcg %>% 
  tibble() %>%
  mutate(age2=age^2) %>%
  mutate(married=factor(married)) %>%
  mutate(sport=factor(sport)) %>%
  mutate(goodh=factor(goodh)) %>%
  mutate(badh=factor(badh)) %>%
  mutate(emp=factor(emp_vec)) %>%
  mutate(year=factor(year)) %>%
  select(doctco, age, age2, male, educ, married, hsize, sport, 
         goodh, badh, emp, loginc, year)

model_fit <- glm(doctco~age+age2+male+educ+married+hsize
    +sport+goodh+badh+emp+loginc+year, family = "poisson", data = data_df)

summary(model_fit)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -4.3728  -1.6481  -0.6017   0.4965  16.9887  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       7.447e-01  8.611e-02   8.648  < 2e-16 ***
#   age              -1.038e-02  2.606e-03  -3.983 6.80e-05 ***
#   age2              1.563e-04  3.132e-05   4.990 6.02e-07 ***
#   male             -2.087e-01  7.955e-03 -26.234  < 2e-16 ***
#   educ             -5.853e-03  1.635e-03  -3.579 0.000345 ***
#   married1          7.607e-02  8.971e-03   8.479  < 2e-16 ***
#   hsize            -5.178e-02  3.068e-03 -16.877  < 2e-16 ***
#   sport1            4.646e-02  8.507e-03   5.462 4.72e-08 ***
#   goodh1           -6.112e-01  8.916e-03 -68.546  < 2e-16 ***
#   badh1             8.134e-01  8.674e-03  93.774  < 2e-16 ***
#   emppart-time     -1.572e-02  1.270e-02  -1.237 0.215973    
# empself-employed  2.396e-01  9.257e-03  25.881  < 2e-16 ***
#   empunemployed     7.793e-02  1.360e-02   5.732 9.93e-09 ***
#   loginc            8.470e-02  9.100e-03   9.308  < 2e-16 ***
#   year96           -7.774e-04  1.061e-02  -0.073 0.941580    
# year97           -3.233e-02  1.075e-02  -3.007 0.002641 ** 
#   year98           -1.065e-01  1.086e-02  -9.805  < 2e-16 ***
#   year99           -9.961e-02  1.107e-02  -8.996  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 145325  on 32836  degrees of freedom
# Residual deviance: 111631  on 32819  degrees of freedom
# AIC: 173196
# 
# Number of Fisher Scoring iterations: 6