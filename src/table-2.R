library(dplyr)
library(ggplot2)
library(tidyverse)
library(foreign)
library(sandwich)

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
  mutate(emp=factor(emp_vec)) %>%
  select(doctco, age, age2, male, educ, married, hsize, sport, 
         goodh, badh, emp, sozh, loginc, year, winter, spring, fall)

model_fit <- glm(
  doctco~age+age2+factor(male)+educ+factor(married)+hsize+
    factor(sport)+factor(goodh)+factor(badh)+factor(emp)+factor(sozh)+
    loginc+factor(year)+factor(winter)+factor(spring)+factor(fall), 
  family = "poisson", data = data_df)

summary(model_fit)

cov.m1 <- vcovHC(model_fit, type = "HC0")
std.err <- sqrt(diag(cov.m1))
std.err
q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(model_fit)
  , "Robust SE" = std.err
  , z = (coef(model_fit)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(model_fit)/std.err), lower.tail = FALSE)
  , LL = coef(model_fit) - q.val  * std.err
  , UL = coef(model_fit) + q.val  * std.err
)

r.est
