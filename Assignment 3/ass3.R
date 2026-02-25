#Assignment 3
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#Set up and preparation

library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(gt)

setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 3")

#------------------

#PART 1
#1.1. Set up
raw=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")

df <- raw %>%
  transmute(
    voted = ifelse(V202109x < 0, NA, V202109x),
    age = ifelse(V201507x < 0, NA, V201507x),
    female = case_when(V201600 == 2 ~ 1, V201600 == 1 ~ 0, TRUE ~ NA_real_),
    education = case_when(
      V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14,
      V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_),
    income = ifelse(V201617x < 0, NA, V201617x),
    party_id = ifelse(V201231x < 0, NA, V201231x)
  )


df<-na.omit(df) #b)to drop missing values
nrow(df)

mean(df$voted) #c) Overall turnout rate and summary statistics
summary(df)  
#-----
#1.2. Exploratory visualization
##a) Bar chart of turnout bu education level
turnout_by_edu<-df %>% 
  group_by(education) %>% 
  summarise(turnout=mean(voted))
turnout_by_edu  
ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) +
  geom_col() +
  labs(x = "Years of education", y = "Turnout rate")
##b) This data suggests that turnout increases with education. In other words,  respondents with more years of education are more likely to report voting.

#-----
#1.3. Linear probability model
#a-b) Estimate the LMP
lmp<-lm(voted~age+education+income+female,data=df)
tidy(lmp)

#c) The coefficient of education is 0.0193 and is statistically significant. It means the estimated change in voting for each extra year of education. The coefficient on education represents the estimated change in the probability of voting for each additional year of education,keeping the other variables constant. 
#d) Check predicted probabilities:
  
pred_lmp<-predict(lmp)
sum(pred_lmp<0) #0 predicted probabilities are below 0
sum(pred_lmp>1) # 802 predicted probabilities are above 1
range (pred_lmp)
#-----
#1.4. Logistic regression
#a–b) Estimate the logit model:
logit<-glm(voted~age+education+income+female,family=binomial,data=df)
tidy(logit)
#c) Odds ratios
exp(coef(logit))
#The odds ratio for education indicates the multiplicative change in the odds of voting for each additional year of education. An odds ratio above 1 means more education is associated with higher odds of voting.
#d) Verify all predicted probabilities are bounded:
pred_logit<-predict(logit, type = "response")
sum(pred_logit<0)  
sum(pred_logit>1) 
range (pred_logit)

#-----

#1.5. Comparing LPM and logit
#a) Average marginal effects:
avg_slopes(logit)
#b) the logit AME are similar to the coefficients of the LPM. Therefore, both approaches tell a similar relationship between the variables.
#c) table:Summary table

modelsummary(list("LPM" = lmp, "Logit" = logit),
             vcov = list("robust", NULL), output = "gt") %>% 
  gtsave("part1-table-ass3.png")
#-----
#1.6. Predicted probabilities
#a) Predicted probability across education:
p1_1 = plot_predictions(logit, condition = "education")
p1_1
ggsave("part1_plotprobedu_ass3.png", p1_1, width = 6, height = 4)

#b) Predicted probabilities by age and gender:
p2_1 = plot_predictions(logit, condition = c("age", "female"))
p2_1
ggsave("part1_plotagegender_ass3.png", p2_1, width = 6, height = 4)
#c) Education shows a clear positive relationship with turnout. Age also has a positive effect. The plot by gender shows that both men and women follow similar age-turnout patterns, with any gender gap being modest relative to the age effect.

#-----
#1.7. Presenting results
#a–b) Coefficient plot:
p3_1 = modelplot(list("LPM" = lmp, "Logit" = logit),
               vcov = list("robust", NULL))
p3_1
ggsave("part1_coefplot_lpm_logit_ass3.png", p3_1, width = 6, height = 4)

#c) For this dataset, the LPM and logit lead to similar substantive conclusions: age, education, and income are all positively associated with turnout, and gender has a modest or negligible effect. The differences between LPM and logit matter more when predicted probabilities are close to the boundaries (0 or 1). In this sample, turnout is relatively common, so the linear approximation works reasonably well.

#------------------

#PART 2
#-----
#2.1. Set up and data preparation
##upload dataset
raw2=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

#a)Name variables
df2 <- raw2 %>%
  mutate(
    classtype = factor(
      classtype,
      levels = c(1, 2, 3),
      labels = c("Small", "Regular", "Regular+Aide") ),
    race=factor(
      race,
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("White", "Black", "Asian",
                 "Hispanic", "Native American", "Other")))
#b) create binary variable
df2 <- df2 %>%
  mutate(
    small = ifelse(classtype=="Small",1,0))
#c) Drop NAs for hsgrad  
df2 <- df2 %>%
  filter(!is.na(hsgrad))
nrow(df2) # There are  3047 remaining observations
#d) Compue...
##High school graduation rate over all
df2 %>% 
  summarise(
    over_all_grad_rate=mean((hsgrad))
  )
##Rates by class type. 
df2 %>% 
  group_by(classtype) %>% 
  summarise(
    grad_rate=mean(hsgrad),
    n = n()
  )
## The rates of high school graduation rates are very similar across different class types, with "Regular+Aide" being slightly higher than the rest.
#-----
#2.2. LPM and Logit
#a) LPM
lpm1_p2<-lm(hsgrad~small, data=df2)
tidy(lpm1_p2)
#b) Logit
logit1_p2<- glm(hsgrad~small, family = binomial,data=df2)
tidy(logit1_p2)%>%
  dplyr::mutate(
    stars = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )
#c)LPM coefficients measure change in probability with one unit change in the predictor. In this case it means that students in small classes are 0.38% more likely to graduate than students in other class types. It is important to note that the results are not statistically significant. 
#d)Compute AME for logit
avg_slopes(logit1_p2)
##The AME coefficient predicts an increase in the probability of graduating by 0.375% for small classes, holding everything else constant. This is similar to the results from de LPM.
#-----
#2.3. Running LPM and Logit with controls
#a) 
lpm2_p2 = lm(hsgrad ~ small + race + yearssmall, data = df2) #LPM with control
logit2_p2 = glm(hsgrad ~ small + race + yearssmall,#Logit with controls
             family = binomial, data = df2)
##Visualizing data using tidy and modelsummary
tidy(lpm2_p2)
tidy(logit2_p2) 
modelsummary(list(
  "LPM Bivariate"=lpm1_p2,
  "Logit  Bivariate"=logit1_p2,
  "LPM with controls" = lpm2_p2,
  "Logit with controls" = logit2_p2),
             vcov = "robust", output = "html")

#b) Both coefficients on small in the bivariate models suggest a very tiny positive effect and were not statistically significant. When adding the controls, the coefficients become statistically significant, larger, and change direction (negative). That means that, when controlling for years in a small classroom and race, being in a small classroom seems to have a negative effect on the probabilities of graduating high school. 
#c)Coefficent yearssmall
logit_data <- as.data.frame(df2)
logit2_p3 <- glm(hsgrad ~ small + yearssmall + race,
                 data = logit_data,
                 family = binomial)
avg_slopes(logit2_p3, variables = "yearssmall")

##The coefficient shows shows a positive relationship. It suggest that each addtional year a student spends on a small class increases their probability of graduating high school by 2.8. This could seem to contradict the previous result, though this difference in direction could be explained by the close relationship between the variables small and yearssmall. 

#-----
#2.4 Predicted probabilities

#a)Compute graduation probabilities for 2 different groups
newdata<- data.frame(
  classtype = c("Small", "Regular"),                   # 1 = Small class, 0 = Regular
  small = c(1, 0),
  yearssmall = c(3, 0),
  race = c("White", "Black")
)
newdata$race <- factor(newdata$race, levels = levels(df2$race))
pred <- predict(logit2_p2, newdata = newdata, type = "response", se.fit = TRUE)
fit_pred<-pred$fit
se_pred<-pred$se.fit
lower_int<-fit_pred-1.96*se_pred
upper_int<-fit_pred+1.96*se_pred
results <- data.frame(
  scenario = c("White, Small, 3", "Black, Regular, 0"),
  predicted_prob = fit_pred,
  lower_CI = lower_int,
  upper_CI = upper_int
)

results 
#Plot predicted graduation probabilities for small classes vs non-small classes.
logit4_p2 = glm(hsgrad ~ small + yearssmall,
                family = binomial, data = df2)

plot1_2<-plot_predictions(logit4_p2, condition = c("yearssmall", "small"))
ggsave("part2_plothsgradyearsmall_ass3.png", plot1_2, width = 6, height = 4)
#-----

#2.5. Interactions
#a)Estimate the interaction small*race with logit
logit3_p2 = glm(hsgrad ~ small * race + yearssmall,
             family = binomial, data = df2)
logit3_p2
#b)compute marginal effects effects using avg_slopes
avg_slopes(logit3_p2, variables = "small", by = "race")
#C) These results suggests that the effects of small classes differ by race, as it seems to have a negative and statistically significant effect on White and Black students, while having a positive (though not a statistically significant)effect on Asian students. In other words, the effects of being in a small class seem to differ considerably when considering the students background. 
#------
#2.6 Presenting results and discussion
#a)Create table
modelsummary(list(
  "LPM Bivariate"=lpm1_p2,
  "Logit  Bivariate"=logit1_p2,
  "LPM with controls" = lpm2_p2,
  "Logit with controls" = logit2_p2),
  vcov = list("robust",NULL,"robust",NULL), output = "gt") %>% 
  gtsave("part2-table-ass3.png")
#b) Create plot (I've separate LPM and Logits to make logit results easier to read. )
plot2_2<-modelplot(
  list("LPM Bivariate" = lpm1_p2, "LPM with controls" = lpm2_p2),
  vcov = "robust",
  coef_keep = c("small", "yearssmall"),
  coef_omit = "Intercept"
) 
plot2_2
ggsave("part2_coefplott1_ass3.png", plot2_2, width = 6, height = 4)

plot3_2<-modelplot(
  list("Logit Bivariate" = logit1_p2, "Logit with controls" = logit2_p2),
  vcov = NULL,
  coef_keep = c("small", "yearssmall"),
  coef_omit = "Intercept"
) 

plot3_2
ggsave("part2_coefplott2_ass3.png", plot3_2, width = 6, height = 4)

#C) The STAR data suggests that the effect of small classes on high school graduation is likely to either be minimal or non-existent. First, it appears that the rates of high school graduation are high overall, regardless of class type. Additionally, adding the controls and analyzing the interaction seem to suggest that students' background (using race as a proxy) seem to explain the effect of graduation more than class size. 
## When comparing LPM and Logit results, we can identify similar tendencies. This could suggest that the the marginal effects captured by both models point to the same underlying relationship between the explanatory variables and the probability of the outcome.
## This experimental evidence is more credible than an observational study as it allow us to examine the effects of class size on a randomized sample. These allow us to identify the real effect of class size, especially when controlling by other factors such as race. 

