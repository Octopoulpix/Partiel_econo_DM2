library(haven)
library(tidyverse)
library(plm)
library(fixest)
library(marginaleffects)
library(estimatr)
library(AER)
library(sandwich)
library(lmtest)

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
# a : probleme d'endogénéité : E[Xepsilon] != 0, mauvaise variable de controle, conséquence pas cause 
reg_level_2 <- lm(salhoraire ~ origine + homme + exp_muted + exp_muted_carred
                + factor(tuu_r) + as.factor(education), 
                data = df_q1)
summary(reg_level_2)


# b : D = education, Z = csp_mere, csp_pere, Cov(Z, D) !=0 : csp du père et de la mère sont fortement corr à l'educ de la personne
# Cov(Z, Y(d0=10)) = 0 : le salaire qu'aurait la personne avec 10 ans d'éducation n'est pas correl avec la csp des parents
# Eventuellement : tester la cond relevance (1) par la reg de education sur csp_pere, csp_mere


# c : Une fois l’éducation incluse dans la régression, l’effet de l’origine est estimé “toutes 
# choses égales par ailleurs”, c’est-à-dire à niveau d’éducation constant. Cela réduit les biais 
# d’omission, car une partie de la variation initialement attribuée à l’origine reflétait en réalité des différences d’éducation. 
# En conséquence, les coefficients associés à l’origine peuvent diminuer en magnitude et devenir moins significatifs.
reg_2SLS <- ivreg(
  salhoraire ~ as.factor(education) + as.factor(origine) + homme + exp_muted + exp_muted_carred + as.factor(tuu_r)
  | csp_pere + csp_mere + origine + homme + exp_muted + exp_muted_carred + as.factor(tuu_r),
  data = df_q1
)
summary(reg_2SLS)

# Question 3:
# dataframe pour q3

df_q3 <- df %>%
  filter((t == 6 | t == 1) & acteu == 1 & !is.na(salhoraire))

names(df)
# a
lm_q3 <- lm(salhoraire ~ origine + homme + exp_muted + exp_muted_carred
            + factor(tuu_r), data = df_q3)
results_clustered <- coeftest(lm_q3, vcov = vcovCL, cluster = ~id_individu)
print(results_clustered)

# b : oui possible, et ne résout rien (cf slide 5)

# c: non car si on suppose ça, on doit exclure tous les regresseurs invariants dans le temps (origine=constante) ((cf slide 5))


# Question 4: 
# dataframe pour q4 (on ne filtre pas sur T et on garde les inactifs et on garde employé + unemployed)

df_q4 <- df %>%
  filter(acteu %in% c(1, 2))

# a : on reste en linéaire d'après l'énonce (on suppose exog), donc seules les variables changent
lm_q4 <- lm(chomeur ~ origine + homme + exp_muted + exp_muted_carred
            + factor(tuu_r), data = df_q4)
results_clustered_q4 <- coeftest(lm_q4, vcov = vcovCL, cluster = ~id_individu)
print(results_clustered_q4)

# b : comme q3)c)

# c :

