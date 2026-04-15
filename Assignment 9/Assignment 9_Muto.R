#Assignment 9
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#===============================================================================
#SET UP
#setting working directory
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 9")
#loading libraries
library(carData)
library(vars)
library(MASS)
library(tidyverse)
library(dplyr)
library(nnet)
library(marginaleffects)
library(pscl)
library(AER)
library(survival)
library(broom)
library(ggplot2)
#datasets
data(bioChemists)
data(BEPS)
#===============================================================================
#PART 1
#===============================================================================
#_______________________________________________________________________________
#1.1. Ordered logit: perceptions of the national economy
#a)
table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)
#For good practice. We also check that levels are ordered.
#The most common category is 3 ("stayed the same"). The distribution is concentrated in the middle categories (2,3,and 4) while very few are at the extremes (1 and 5).
#OLS treats the distance between each unit as the same, while the Linkert survey assigns a descriptive quality to each unit. In this particular case, the distance between  “got much worse” (1) and “got a little worse” (2) is considerably larger than the one between “stayed the same” (3) and “got a little better” (4). 
#We apply ordered logit, which estimates the threshold parameters that let determine the spacing of the latent scale. 

#b)
m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)
summary(m_ologit)
#The raw coefficient in Europe  -0.122693, and its sign is negative. Considering this, higher support for European integration predict less optimistic views of the national economy. 
#Additional note from class about t values significance= t value must be above 1,64 for+, 1,96*, 1,99**)

#c)
avg_slopes(m_ologit)
#note: avg_slopes as it is is hard to read and kinda usefeñs
#d)
predictions(m_ologit, 
     newdata = datagrid(gender = c("female", "male")))

preds=tidy(predictions(m_ologit, by="gender"))
preds
#Overall, it seems that gender is not a strong predictor of economic perceptions, as both males and females seem to have similar distributions (higher concentration of neutral views "3", and slightly skewing right towards "5")
#Create female groups
female1=preds %>% 
  filter(group==1 & gender=="female") %>%
  pull(estimate)
female5=preds %>% 
  filter(group==5 & gender=="female") %>%
  pull(estimate)
female1-female5 #Calculate differences between female group 1 and female group 5
#Create male groups
male1=preds %>% 
  filter(group==1 & gender=="male") %>%
  pull(estimate)
male5=preds %>% 
  filter(group==5 & gender=="male") %>%
  pull(estimate)
male1-male5 #Calculate differences between male group 1 and male group 5
#When we calculate the difference, we observe that the difference between males that have a very positive outlook of the economy compared to the ones with a very low outlook is greater than within females.
# ____________________________________________________________________________
#1.2 Multinomial logit: vote choic
#a)
BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
#Class note: use relevel to change reference category
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)
#Conservative is reference category. Is it is positive means it is more likely to vote for the other party. 
#In the Labour vs. Conservative equation, the coefficient is positive for Blair. This implies that a person who has positive feelings towards Tony Blair is more likely to vote Labour. 

#b)
avg_slopes(m_mlogit)
#The AME of Blair voting Labour is  0.1156  and is statiscatally significant. 
#This means that, when holding all other variables constant, a one unit increase in Tony Blair's approval increases the likelihood of voting Labour by 11.5%
#c)
#c) The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two
#alternatives (e.g., Labour vs. Conservative) is unaffected by the presence or characteristics of the third alternative
#(Liberal Democrats). In the red bus / blue bus analogy, IIA fails because two alternatives are near-perfect substitutes
#and removing one simply shifts its probability to the other rather than distributing it proportionally. For British
#party choice, IIA is a moderate concern: Labour and the Liberal Democrats are both centre-left parties, sharing
#some ideological space, so some voters may treat them as partial substitutes in a way IIA cannot accommodate. The
#Conservatives, however, occupy a clearly distinct ideological position (right-wing), so the three-party menu is not
#as degenerate as two buses of different colours. Overall, IIA is plausible for Conservative vs. the others but is a
#more legitimate worry for the Labour/Liberal Democrat distinction
#_______________________________________________________________________________
#1.3 Poisson regression: publication counts
#a)
summary(bioChemists$art)
table(bioChemists$art)
var(bioChemists$art)
#the mean of art is 1.693 and the variance of art is 3.709742. In other words, the variance is almost as twice as the mean.
# As we can see, this is a case of overdispersion and violates the Poisson assumption. This means that it may underestimate uncertanty and produce anti-conservative se.
# We also graph the dispersion
graph1=ggplot(bioChemists, aes(x = art)) +
     geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
     theme_minimal() +
     labs(
          title = "Publications in last 3 years of PhD",
           x = "Number of articles", 
           y = "Count")
graph1
ggsave("graph1_assignment9.pdf", plot = graph1,
       width = 7, height = 5, units = "in")
#b)
m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois) # The coefficient on ment is 0.025543
exp(coef(m_pois)["ment"]) #  and when we expontiate it is 1.025872 (which is the Incidence Rate Ratio- IRR)
#This means that each additional article publiched by the mentor is linked with a multiplicative increase in expected articles published by the student when everything else is hold constant. It is important to note that the effect doesn't seem to be large. 

#c)
dispersiontest(m_pois)
#_______________________________________________________________________________
#1.4 Negative binomial regression
#a)
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment, data = bioChemists)
summary(m_nb)
#b)
AIC(m_pois, m_nb)
#AIC and #BIC are better when lower. Could be used for several models

#c)
predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))
#===============================================================================
#PART 2
#===============================================================================
#_______________________________________________________________________________
#2.1. Kaplan-Meier survival curves
nrow(lung)
lung$dead=lung$status - 1 #change status to binomial
#a)
n_total = nrow(lung)
n_events = sum(lung$dead == 1, na.rm = TRUE)
n_censored = sum(lung$dead == 0, na.rm = TRUE)
prop_cens = n_censored / n_total
n_total
n_events
n_censored
prop_cens
#There are 228 total observations, 165 events (deaths), 63 censored cases. 
# #Around 28% of patients are censored. It is a considerable percentages of cases. C
# Considering that we can't observe when they are going to die, ignoring them could affect the overall analysi downward. 

#b)
km_overall = survfit(Surv(time, dead) ~ 1, data = lung)
summary(km_overall)   
print(km_overall)     
#This means that one half of the patients died the first 310 days. The other half survived longer or were still alive at the time of the analysis (censored cases).
#c)
km_sex = survfit(Surv(time, dead) ~ sex, data = lung)
km_sex_df = broom::tidy(km_sex)
km_sex_df$sex_label = factor(
  km_sex_df$strata,
  levels = c("sex=1", "sex=2"),
  labels = c("Male", "Female")
)
graph2 = ggplot(km_sex_df, aes(x = time, y = estimate,
                             color = sex_label, fill = sex_label)) +
  geom_step(linewidth = 0.9) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.15, color = NA) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1)) +
  scale_color_manual(values = c(Male = "#4472C4", Female = "#E05C5C")) +
  scale_fill_manual(values  = c(Male = "#4472C4", Female = "#E05C5C")) +
  labs(
    title    = "Kaplan–Meier Survival Curves by Sex",
    subtitle = "Lung cancer patients — NCCTG study",
    x        = "Time (days)",
    y        = "Survival probability",
    color    = "Sex",
    fill     = "Sex"
  ) +
  theme_classic(base_size = 13) +
  theme(legend.position = c(0.85, 0.85))

print(p)
ggsave("km_curves_by_sex.pdf", plot = graph2,
       width = 7, height = 5, units = "in")
graph2
#we can observe that the red curve lies above the blu curve. This means that females have a higher survival probability than males at every point in time. 
#The confidence intervals overlap slightly in the first periods and around 600 days.
logrank_test = survdiff(Surv(time, dead) ~ sex, data = lung)
print(logrank_test)
# The log-rank test is used to check if the null hypothesis where the curves are identical (in this case, meaning that sex has no effect on survival).
# The p= 0.001, therefore we reject the null. This means that there is strong statistical evidence that females tend to survive for longer periods than males in this case. 
# ______________________________________________________________________________
#2.2 Cox proportional hazards model
#a)
cox_model = coxph(Surv(time, dead) ~ age + sex + ph.ecog,
                    data = lung)
summary(cox_model)
exp(coef(cox_model))
exp(confint(cox_model))
#The Hazard ratio for sex is 0.57 and it is statistically significant. It means that females have a 42.5 lower possibility of death than males. This results aligns with the KM.
#b)
hr_ecog = exp(coef(cox_model)["ph.ecog"])
pct_change = (round(hr_ecog, 3) - 1) * 100
cat("HR for ph.ecog: ", round(hr_ecog, 3), "\n")
cat("% change in hazard per 1-unit increase: ", pct_change, "%\n")
#The hazard ratio is 1.59. This means that one unit increase in ECOG score is related to a 59% higher hazard of death.

#c)
ph_test = cox.zph(cox_model)
print(ph_test) 
plot(ph_test)
#All p-values are above 0.05, including the global test. This means that they don't violate the proportional hzard assumption. This means that the Cox model is appropiate. 

#d)
# The Kaplan- Meier analysis shows that female patients are more likely to survive longer periods than their male counterparts. 
#The Cox proportional hazard model suggest that both sex and ECOG are statistically significant predictors, compared to age which wasn't. 
#Overall, these results suggest that biological sex and ECOG are strong predictors for the survival in cases of advance lung cancer.