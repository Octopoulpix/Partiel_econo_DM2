library(haven)
library(tidyverse)
library(plm)
library(fixest)
library(marginaleffects)

df <- read_dta("DM_Subject_2_Data.dta")

# Correction de l'énonce
df$salhoraire = df$salmensuel / (df$heuretra * 4.33)

df$exp_muted = df$age - 18
df$exp_muted_carred = df$exp_muted ^ 2

# Question 1
# a)
df_q1 <- df %>%
  filter(t == 6 & acteu == 1 & !is.na(salhoraire))

# Convertir 'origine' en un factor et mettre '1' (France) comme groupe de réf
df_q1$origine <- factor(df_q1$origine)
df_q1$origine <- relevel(df_q1$origine, ref = "01")


# Regression 1: Level 
reg_level <- lm(salhoraire ~ origine + homme + exp_muted + exp_muted_carred
                + factor(tuu_r), 
                data = df_q1)

# Regression 2: Log
reg_log <- lm(logsalhoraire ~ origine + homme + exp_muted + exp_muted_carred + factor(tuu_r) , 
              data = df_q1)

summary(reg_level)
summary(reg_log)

# question c

# Question 2
# a
reg_level_2 <- lm(salhoraire ~ origine + homme + exp_muted + exp_muted_carred
                + factor(tuu_r) + as.factor(education), 
                data = df_q1)
summary(reg_level_2)
