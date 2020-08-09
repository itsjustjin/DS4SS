#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(GGally)
library(broom)




# Data Import and Cleaning
replace_over_4 <- function(x) replace(x, x > 4, NA)
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>% 
  select(mh1 = H4MH3, mh2 = H4MH4, mh3 = H4MH5, mh4 = H4MH6,
       jailage = H4CJ20, gender = BIO_SEX4) %>% 
  mutate_at(vars(mh1:mh4), replace_over_4) %>% 
  mutate(mh1 = 4 - mh1,
         mh4 = 4 - mh4) %>% 
  rowwise() %>% 
  mutate(mh = mean(c(mh1, mh2, mh3, mh4), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(gender = factor(gender, levels = c(1,2), labels = c("Male", "Female"))) %>% 
  filter(jailage < 97)






# Visualization
select(health_tbl, jailage:mh) %>% ggpairs()
select(health_tbl, -gender) %>% cor(use = "pairwise")

# Analysis: Mental Health on Age of First Jailing and Gender
model1 <- lm(mh ~ jailage + gender, data = health_tbl)
par(mfrow = c(2,2))
plot(model1)
par(mfrow = c(1,1))
#No influential cases in residuals vs. leverage, no collinearity issues
summary(model1)
model1_augment <- augment(model1)
ggplot(model1_augment, aes(x = jailage, y = mh, color = gender)) +
  geom_line(aes(y = .fitted))
model2 <- lm(mh ~ jailage + gender + jailage:gender, data = health_tbl)
par(mfrow = c(2,2))
plot(model2)
par(mfrow = c(1,1))
#not much collinearity
summary(model2)
ggplot(health_tbl, aes(x = jailage, y = mh, color = gender, group = gender)) +
  geom_smooth(method = "lm", se = 0)
anova(model1, model2)
summary(model2)$r.squared - summary(model1)$r.squared
#the increase in R^2 associated with the addition of the interaction between jailage and gender was small (delta is 0.002)
# and statistically insignificant (F(1,383) = 0.947, p = 0.331), so the more complex model does not add sufficient
# variance in mh to justify reduced parsimony. Therefore, you should retain the less complex model.