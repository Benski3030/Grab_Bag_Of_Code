library(tidyverse)
library(lubridate)

baby_data <- read_csv(
    "sf_gestationdata.csv",
    col_types = cols(
      motherbirthdate = col_double(),
      duedate = col_double(),
      birthdate = col_double())) %>%
  select(motherbirthyear, 
         duedate,
         birthdate,
         country, 
         multiples, 
         previousbirths, 
         cesarean,
         sex,
         pounds,
         ounces) %>%
  mutate(duedate = as_datetime(duedate),
         birthdate = as_datetime(birthdate)) %>% 
  filter(country == "Canada") %>% 
  filter(multiples == 1) %>% 
  filter(previousbirths == 0) %>% 
  filter(cesarean == 0) %>% 
  mutate(monther_age = 2020-motherbirthyear,
         miss_date = as.double(difftime(birthdate,duedate,unit="days")),
         weight_oz = (pounds * 16) + ounces,
         sex = as.factor(sex)) %>% 
  drop_na() %>% 
  filter(monther_age %in% (30:45)) %>% 
  filter(miss_date %in% (-20:20)) %>% 
  filter(sex != "NULL") %>%
  filter(weight_oz > 1) %>% 
  select(birthdate, sex, miss_date, pounds, ounces, weight_oz) 


prop.table(table(as.factor(gen$sex)))
  
ggplot(gen, aes(x = miss_date, fill = sex)) +
  geom_histogram(binwidth = 5)

ggplot(gen, aes(x = weight_oz, fill = sex)) +
  geom_histogram(binwidth = 5)

gen %>% 
  group_by(sex) %>% 
  summarize(mean = mean(weight_oz))

# The Stan model as a string.
model_string <- "
data {
  int n1;
  vector[n1] y1;
}

parameters {
  real mu1;
  real<lower=0> sigma1;
}

model {  
  mu1 ~ uniform(-40, 40);
  sigma1 ~ uniform(0, 1000);
  y1 ~ normal(mu1, sigma1);
}

generated quantities {
}
"
data_list <- list(y1 = baby_data$miss_date,
                  n1 = length(baby_data$miss_date))

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data_list)


s <- as.data.frame(stan_samples)
hist(s$mu1)
mean(s$mu1)
mean(mu_diff > 0)

