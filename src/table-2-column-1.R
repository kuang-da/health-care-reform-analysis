# GLM with fixed effects
library(pglm)
library(tidyverse)

hcg <-  read.table("data/w.data")
colnames(hcg) = c("id","year", "doctco", "age", 
                  "male","educ","married","hsize", 
                  "sport", "goodh", "badh", "sozh", 
                  "loginc", "ft", "pt", "unemp", 
                  "winter", "spring", "fall")

emp_vec <- c()
for (idx in seq(dim(hcg)[1])){
  if(hcg[[idx, 'ft']] == 1)
    emp_vec <- c(emp_vec, 'full.time')
  else if (hcg[[idx, 'pt']] == 1)
    emp_vec <- c(emp_vec, 'part.time')
  else if (hcg[[idx, 'unemp']] == 1)
    emp_vec <- c(emp_vec, 'unemployed')
  else
    emp_vec <- c(emp_vec, 'self.employed')
}

hcg %>% 
  tibble() %>%
  mutate(age2=age^2) %>%
  mutate(emp=factor(emp_vec)) %>%
  select(id, doctco, age, age2, male, educ, married, hsize, sport, 
         goodh, badh, emp, sozh, loginc, year, winter, spring, fall) %>%
  pglm(doctco~age+age2+factor(male)+educ+factor(married)+hsize+
         factor(sport)+factor(goodh)+factor(badh)+factor(emp)+factor(sozh)+
         loginc+
         factor(year)+
         factor(winter)+factor(spring)+factor(fall), 
      data = .,
      model = "pooling",
      family = "poisson",
      index=c("id")
      ) %>% 
  summary()
