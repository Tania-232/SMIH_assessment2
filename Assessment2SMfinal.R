# Loading the dataset using tidyverse
library(tidyverse)
heffpox <- read_rds("Documents/heffpox_2425.RData")
# Loading more required packages
library(mice)
library(survival)
library(survminer)
library(ggplot2)
library(cowplot)

#### EDA ####
head(heffpox)
view(heffpox)
## Checking for missing values
sum(is.na(heffpox)) # has 3315 missing values
summary(heffpox)
# Missing values are for bmi and for smoking. BMI has the most missing values (2915)

# heffpox only- data of interest for our analysis
filtered_df <- heffpox %>% filter(heffpox == 1)
head(filtered_df)
view(filtered_df)
summary(filtered_df)

## TEVENT analysis
# the important columns for this are TEVENT and Status. Neither have missing values.

TEVENT_fit <- survfit(Surv(filtered_df$TEVENT, filtered_df$Status)~ 1, conf.type= "log-log")
summary(TEVENT_fit)
ggsurvplot(TEVENT_fit, data= filtered_df, conf.int= TRUE, risk.table= TRUE)

# by treatment
KM_heffpox <- survfit(Surv(filtered_df$TEVENT, filtered_df$Status)~ filtered_df$milnepan, conf.type= "log-log")
ggsurvplot(KM_heffpox, data= filtered_df, conf.int= TRUE, risk.table= TRUE)

# log rank test
survdiff(Surv(TEVENT, Status)~ milnepan, data= filtered_df)

# Distributions of potential confounders
table(filtered_df$diabetes, filtered_df$milnepan)
ggplot(filtered_df, aes(x= as.factor(diabetes),
                        fill = as.factor(milnepan))) +
  geom_bar(position = "dodge")  +
  xlab("Diabetes Status") +
  labs(fill = "Milnepan") +
  scale_fill_manual(values = c("grey", "darkblue")) 

# also calculate odds and risks ratios for milnepan and death7day.
# is there initially an association between milnepan and death7day in this dataset- before looking at confounders.
table(filtered_df$milnepan, filtered_df$death7day)
treat_death <- ggplot(filtered_df, aes(x=as.factor(milnepan),
                      fill= as.factor(death7day))) +
  geom_bar(position = "dodge") +
  xlab("Milnepan (Prescribed = 1)") +
  labs(fill = "Death7day (Yes = 1)") +
  scale_fill_manual(values = c("#6b8ea4", "#cccccc"))

treat_death

# other variables to plot
icu_and_diabetes <- ggplot(filtered_df, aes(x= as.factor(icu),
                        fill = as.factor(diabetes))) +
  geom_bar(position = "dodge")  +
  xlab("ICU") +
  labs(fill = "Milnepan") +
  scale_fill_manual(values = c("grey", "darkblue")) 

# Among those with diabetes, how many were given milnepan?
diabetes_milnepan <-  ggplot(filtered_df, aes(x=as.factor(diabetes),
                                              fill= as.factor(milnepan))) +
  geom_bar(position = "dodge") +
  xlab("Diabetic (Yes = 1)") +
  labs(fill = "Milnepan (Prescribed = 1)") +
  scale_fill_manual(values = c("#6b8ea4", "#e48646"))

diabetes_milnepan

# How many patients were given milnepan for males and for females?
sex_milnepan <- ggplot(filtered_df, aes(x= as.factor(sex),
                                            fill = as.factor(milnepan))) +
  geom_bar(position = "dodge")  +
  xlab("Sex (Male = 1, Female = 0)") +
  labs(fill = "Milnepan (Prescribed = 1)") +
  scale_fill_manual(values = c("#6b8ea4", "#e48646")) 

sex_milnepan

# Among Smokers, how many were given milnepan? Also some counts for NA values.
smoking_milnepan <- ggplot(filtered_df, aes(x= as.factor(smoking),
                                            fill = as.factor(milnepan))) +
  geom_bar(position = "dodge")  +
  xlab("Smoking") +
  labs(fill = "Milnepan (Prescribed = 1)") +
  scale_fill_manual(values = c("#6b8ea4", "#e48646")) 

smoking_milnepan

# How does Age distribution vary for treated vs untreated patients?
library("hrbrthemes")

age_milnepan <- ggplot(filtered_df, aes(x= age,
                                        group = as.factor(milnepan),
                                        fill = as.factor(milnepan))) +
  geom_density(adjust = 1.5, alpha =0.4)  +
  xlab("Age") +
  labs(fill = "Milnepan (Prescribed = 1)") +
  scale_fill_manual(values = c("#6b8ea4", "#e48646")) 

age_milnepan

# How many patients in the ICU had taken milnepan first?

icu_milnepan <- ggplot(filtered_df, aes(x= as.factor(icu),
                                       fill = as.factor(milnepan))) +
  geom_bar(position = "dodge")+
  xlab("ICU (Yes = 1)") +
  labs(fill = "Milnepan (Prescribed = 1)") +
  scale_fill_manual(values = c("#6b8ea4","#e48646")) 

icu_milnepan

# putting the 5 EDA plots together
figure1 <- ggarrange(diabetes_milnepan, sex_milnepan, smoking_milnepan, 
                     icu_milnepan, age_milnepan, 
                     labels = c("A", "B", "C", "D", "E"),
                     ncol = 3, nrow = 2)

figure1

# Handling missing data

# Quantifying the missingness
colMeans(is.na(filtered_df))*100 # 33% of the data missing for BMI

# Looking further at the pattern of missingness 
table(is.na(filtered_df$smoking),is.na(filtered_df$bmi))
md.pattern(filtered_df[, c("smoking", "bmi")])

# doesn't seem to be a missingness pattern between the two variables

# Now lets look at patterns of missingness with OTHER variables
# missing smoking
glm_missing_smoking <- glm(is.na(smoking) ~ age + sex + diabetes + icu, family= "binomial",data = filtered_df)
summary(glm_missing_smoking)

#missing bmi
glm_missing_bmi <- glm(is.na(bmi) ~ age + sex + diabetes + icu, family= "binomial",data = filtered_df)
summary(glm_missing_bmi)

# imputing the missing values- imputation model

data_imputed <- mice(filtered_df, seed= 1, m=10, method = "pmm")

# diagnostics to check plausibility of imputation- for bmi 
stripplot(data_imputed,bmi ~ .imp, pch = 20, cex = 2)

# Extracted datasets

all_imputed <- complete(data_imputed, action= "all")

imputed_1 <-
  complete(data_imputed, action = 1)
imputed_2 <-
  complete(data_imputed, action = 2)
imputed_3 <-
  complete(data_imputed, action = 3)
imputed_4 <-
  complete(data_imputed, action = 4)
imputed_5 <-
  complete(data_imputed, action = 5)
imputed_6 <-
  complete(data_imputed, action = 6)
imputed_7 <-
  complete(data_imputed, action = 7)
imputed_8 <-
  complete(data_imputed, action = 8)
imputed_9 <-
  complete(data_imputed, action = 9)
imputed_10 <-
  complete(data_imputed, action = 10)

smoking_mean1 <- mean(imputed_1$smoking)
smoking_mean2 <- mean(imputed_2$smoking)
smoking_mean3 <- mean(imputed_3$smoking)
smoking_mean4 <- mean(imputed_4$smoking)
smoking_mean5 <- mean(imputed_5$smoking)
smoking_mean6 <- mean(imputed_6$smoking)
smoking_mean7 <- mean(imputed_7$smoking)
smoking_mean8 <- mean(imputed_8$smoking)
smoking_mean9 <- mean(imputed_9$smoking)
smoking_mean10 <- mean(imputed_10$smoking)

table(smoking_mean1, smoking_mean2, smoking_mean3, smoking_mean4, smoking_mean5,
      smoking_mean6, smoking_mean7, smoking_mean8, smoking_mean9, smoking_mean10)


# The analysis stage- part 1) adjusting for confounders
# according to our DAG the potential confounders we have in our data are diabetes, age.
age_milnepan <- ggplot(filtered_df, aes(x=age, color=as.factor(milnepan)))+ geom_density()

# model to check which are actually confounders
summary(glm(milnepan ~ age + diabetes,
            family = "binomial", data = imputed_1)) 

summary(glm(death7day ~ age + diabetes,
            family = "binomial", data = imputed_1))

# Yes, both age and diabetes are significantly associated with both death and milnepan- we need to adjust for them.


# based on this Age and diabetes are all potential confounders to adjust for causal inference.
# Model without accounting for confounders- for comparison
no_confounders <- with(data_imputed, glm(death7day ~ milnepan, family = "binomial"))
pooled_no_confounders <- pool(no_confounders)
summary(pooled_no_confounders)

### adjusting for confounders method 1- REGRESSION
# In this method, since diabetes is a confounder, we have held it constant by including it in the model.

model1 <- with(data_imputed, glm(death7day ~ milnepan + diabetes + age, family = "binomial"))
pooled_regression <- pool(model1)
summary(pooled_regression) 

# according to this death and milnepan association is significant for both models with and without confounders
# likelihood ratio test to select the best model


### adjusting for confounders method 2- Propensity Score Matching
library(MatchThem)
library(mitools)
library(cobalt)

psm <- matchthem(milnepan ~ diabetes + age, data= data_imputed, method= "nearest",
                 caliper = 0.1)
summary(psm)

imp_with_ps <- complete(psm, action = "all")

bal.tab(psm, m.threshold = 0.1)

bal.plot(psm, which= "both")

bal.plot(psm, "age", which= "both")

love.plot(bal.tab(psm, m.threshold = 0.1))

bal.plot(psm, "diabetes", which= "both")

# now answering the causal question with the matched data

model2 <- with(psm, glm(death7day ~ milnepan, family = "binomial"))

pooled_psm <- MatchThem::pool(model2)

summary(pooled_psm)


