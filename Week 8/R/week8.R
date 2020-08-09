#R Studio API Code
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Libraries
library(tidyverse)
library(GGally)
library(car)
library(yarrr)
library(lsmeans)
library(broom)
library(multcomp)
library(apatables)
library(lsr)
library(apaTables)



# Data Import and Cleaning
health_tbl <- read_tsv("../data/w4inhome_dvn.tab") %>% 
  transmute(admin_month = IMONTH4,
            gender = factor(BIO_SEX4, levels = c(1,2), labels = c("male", "female")),
            living_mother = factor(H4WP1,levels = c(0, 1, 8), labels = c("no", "yes", "don't know")),
            fiw = replace(H4LM29, H4LM29 >=6, NA))

# Visualization
ggpairs(health_tbl)

# Analysis: FIW on Gender and Living Mother
options(contrasts = c("contr.sum", "contr.poly"))
linear_model <- lm(fiw ~ admin_month + living_mother * gender, data = health_tbl)
anova_model <- Anova(linear_model, type = 3)
anova_model
plot(linear_model)
pirateplot(formula = "fiw ~ living_mother + gender", data=health_tbl)
mm_df <- tidy(lsmeans(linear_model, "gender", by = "living_mother"))
ggplot(mm_df,
       aes(x = living_mother,
           y = estimate,
           color = gender,
           group = gender)) +
  geom_line()
health_tbl <- health_tbl %>% mutate(cond = interaction(gender, living_mother, sep = " x "))
posthoc_model <- lm(fiw ~ admin_month + cond, data = health_tbl)
posthocs <- glht(posthoc_model, linfct = mcp(cond = "Tukey"))
summary(posthocs)
etaSquared(linear_model, type = 3, anova = T)
apa.aov.table(linear_model, conf.level=.95, type = 3, "../output/analysis.doc")
linear_model_lsm <- lsmeans(linear_model, "living_mother")
contrast(linear_model_lsm, list(knowledge=c(-.5, -.5, 1)))
#Neither H was supported, no statistically significant interaction (F(2,5018) = 0.88, p = 0.414) and
#no effect in the contrast ( t(5018) = 0.657, p - 0.512)