#Assignment 5
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#--------
#Library set-up
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 5")
library(dplyr)
library(ggplot2)
library(fixest)
library(modelsummary)
library(gt)
library(haven)
library(plm)
#--------
#PART 1
#---------

# 1. Setup and data exploration
#a)Load the dataset. How many unique states and years are in the data? Use length(unique())
#or n distinct() to check. Is the panel balanced (i.e., does every state appear the same number of times)?

df=read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv") #Upload dataset
summary(df)

#Check unique states and years 

length(unique(df$Year))
length(unique(df$State))

table(table(df$State))
#The panel is not balanced.
#b) Compute summary statistics for PresApprov and UnemPct using summary() or 
#modelsummary::dat.Then plot PresApprov over Year for a few selected states 
#(e.g., California, Texas, New York) to visualize the panel structure:

#Use summary()to check variables
summary(df$PresApprov)
summary(df$UnemPct)

#Plot PresApprov over Year for Californa, Texas, and New York
df_sub=df %>% 
  filter(State %in%c("California","Texas","NewYork"))#creating dataframe with only those 3 States
plot1_p1=ggplot(df_sub,aes(x=Year,y=PresApprov,colour = State))+
  geom_line()+
  theme(legend.position = "none")+
  theme_minimal()+
  labs(
    x="Year",
    y="Presidential Approval (%)",
    color="State"
  ) #Creating plot and naming it
plot1_p1 #visualizing plot
ggsave("plot1_part1_ass5.png", plot1_p1, width = 6, height = 4) #saving plot
##The three states seem to have similar movements. This would suggest that approval 
#is affected by national factors mainly (for example, these could be economic 
#cycles or foreign policy). This results would suggest that state level differences are relatively stable.  

#c) Create a scatter plot of PresApprov (y-axis) against UnemPct (x-axis) across all state-year
#observations. Add a regression line with geom smooth(method = "lm"). In a comment,
#describe the cross-sectional relationship: does higher unemployment seem to be associated with lower or higher approval ratings?
  
plot2_p1=ggplot(df,aes(x=UnemPct,y=PresApprov,colour = State))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(
    x="State Unemployment Rate",
    y="Presidential Approval (%)",
    color="State"
  )#Creating plot and naming it
plot2_p1 #visualizing plot
ggsave("plot2_part1_ass5.png", plot2_p1, width = 6, height = 4)#saving plot

#Extra: let's create another plot national level

plot2_p1_extra=ggplot(df,aes(x=UnemPct,y=PresApprov))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "lm")+
  theme_minimal()+
  theme(legend.position = "none")+
  labs(
    x="State Unemployment Rate",
    y="Presidential Approval (%)"
  )#Creating plot and naming it
plot2_p1_extra #visualizing plot
ggsave("plot2_extra_part1_ass5.png", plot2_p1_extra, width = 6, height = 4)#saving plot

#---------
#2. Pooled OLS

#a)Estimate a pooled OLS model regressing presidential approval on unemployment:
#m pooled = lm(PresApprov ~ UnemPct, data = df). Report the results using summary()
#or modelsummary(). In a comment, interpret the coefficient on UnemPct: what does it
#say about the relationship between unemployment and approval?

m_pooled_p1=lm(PresApprov~UnemPct, data = df)
summary(m_pooled_p1)

#This model suggests that there is a negative relationship between presidential 
#approval and unemployment, as 1 percentage point decrease in unemployment is 
#associated with 0.14 decrease in presidential approval on average. Nonetheless, 
#these results are not statistically significant (p-value=0.671), therefore is 
#not recommended to generalize them. It is likely that this is because OLS cannot 
#separate across state variation and within state variation over time. 

#b)Add South as a control:m pooled2 = lm(PresApprov ~ UnemPct + South, data = df).
#Does controlling for whether a state is in the South change the coefficient on UnemPct? In a comment, explain why or why not.

m_pooled2_p1=lm(PresApprov~UnemPct+South, data=df)
tablesum1_p1_extra=modelsummary(list("Pooled OLS"=m_pooled_p1,"Pooled OLS (controls for South)"=m_pooled2_p1),
             stars = TRUE,output = "gt") #Create a table to compare both models 
tablesum1_p1_extra
gtsave(tablesum1_p1_extra,
       "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 5/tablesum1_p1_extra.png") #saving the table
#When controlling for "South" we can see that the effect of Unemployment increases 
#considerably, though the coefficient is still not statistically significant (also,
#R2 adjusted suggest that the explanatory power of both models is low.

#Extra: Calculate OLS factoring states
m_pooled_extrafactor=lm(PresApprov~UnemPct+factor(State), data = df) #Normal OLS using factors

summary(m_pooled_extrafactor)
# c)In a comment, reflect on the limitations of pooled OLS for this type of data. What kinds
#of unobserved, time-invariant differences across states might bias the estimate of the
#unemployment effect? Give two or three concrete examples.

#As we mentioned before, the R2 adjusted for both models suggest that both models have very limited explanatory power. 
#This would suggest that these models do not account within state variance across time nor difference across states. 
#These obsevered bias could be caused by states that historically have had higher unemployment rates, states more workers unionized, or even states with stronger partisan identification.

#--------
#3. Entity fixed effects

#a) Estimate a model with state fixed effects using fixest:

m_fe=feols(PresApprov~UnemPct | State, data=df)# PAnel with fixed effects

tablesum2_p1=modelsummary(list("Pooled OLS"=m_pooled_p1,"Pooled OLS (controls for South)"=m_pooled2_p1, "FE State"=m_fe),
             stars = TRUE,
             vcov = ~State,
             gof_map = c("r.squared", "nobs"),
             output = "gt")#Creating table
tablesum2_p1
gtsave(tablesum2_p1,
       "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 5/tablesum2_p1.png") #saving the table

#Here we can observe that the effect of unemployment increases, though the results 
#of the p-values suggest it is not statistically significant and the R2 suggest that all models have low explanatory power. 
#Overall, this would suggest that there are addiotanl unobserved variables that we are not considering. 

#b) In a comment, explain what the state fixed effects are absorbing. Note that the South
#variable drops out of the model — why can’t it be estimated when state fixed effects
#are included? What does this imply about any variable that does not vary within a
#state over time?

#Here Alabama is the reference category 
summary(OLS)
summary(m_fe)
newdf=df %>% 
  group_by(State) %>% 
  mutate(PA= PresApprov -mean (PresApprov),
         UnemPct= UnemPct -mean(UnemPct)) %>% ungroup()
ols2=lm(PresApprov~UnemPct, data = newdf)
summary(ols2)
Ols3=summary(lm(PresApprov~UnemPct+factor(State)+factor(Year), data = df)) 
Ols3
modelsummary(
  list("Pooled OLS"= m1_p1,"Pooled OLS +Control"=m2_p1, "OLS new data"=ols2,"State FE"=m_fe,"OLS"=OLS),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs")
)


#Model 1, without controls, seems to suggests that effect is close to 0. When controling for FE State in panel data it effet becomes larger.

#-----------------
#4 Two-way fixed effects
#a)a) Add year fixed effects to absorb common time shocks (e.g., national economic conditions, wars, presidential scandals) that affect all states simultaneously:
m_twfe=feols(PresApprov~UnemPct | State+Year, 
             data=df)
#b) Compare all three models in a single modelsummary() table with standard errors clustered by state:
  
tablesum3_p1=modelsummary(
  list("Pooled OLS"= m_pooled_p1,"Pooled OLS +Control"=m_pooled2_p1, "State FE"=m_fe, "State and Year FE"=m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "gt"
)
tablesum3_p1
gtsave(tablesum3_p1,
       "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 5/tablesum3_p1.png") #saving the table

#variation in R increases considerably in the TWFE models, this means that that the variation is mostly explained by time-place effects. Nonotheless, it doesn't mean that it explains more.

#--------
#PART 2
#---------
#1. Data exploration

df_p2=read_stata("C:/Users/pc/Downloads/teaching_evals.dta") #Load new data set

#a) How many unique instructors and courses are in the data? Use n distinct(df$InstrID)
#and n distinct(df$CourseID). What is the average number of observations (courseyear pairs) per instructor? In a comment, note whether this looks like a short or long
#panel.

n_distinct(df_p2$InstrID)
n_distinct(df_p2$CourseID)
n_distinct(df_p2$CourseID,df_p2$Year) #To identify distinct course-year pairs.
nrow(df_p2)#number of observations do not match course-year pairs. Therefore we will compute for both. 
avg_nrowinsT= (nrow(df_p2)/n_distinct(df_p2$InstrID))
avg_nrowinsT
avg_couryearinsT= (n_distinct(df_p2$CourseID,df_p2$Year)/n_distinct(df_p2$InstrID))
avg_couryearinsT
#Considering that there are 48 instructors and the average  the average observation course year per instructor 
#is approximately 15.17 (around 17.52 for total observations), this would be considered a short panel data, as there number of
#cross-sectional units (instructors) is larger than the time dimension for each unit.

#b)Create a scatter plot of Eval (y-axis) against Apct (x-axis). Add a regression line with
#geom smooth(method = "lm"). In a comment, describe the cross-sectional relationship
#between grading generosity and evaluations: is it positive or negative? Does this pattern surprise you?

plot1_p2=ggplot(df_p2,aes(x=Apct,y=Eval))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(
    x="% ofstudents receiving an A or A- in the course",
    y="Average Course Evaluation",
  ) #Creating plot and naming it
plot1_p2 #visualizing plot
ggsave("plot1_part2_ass5.png", plot1_p2, width = 6, height = 4) #saving plot

#This graph suggests that there is a positive relationship between grading generosity and
#evaluations, though it is interesting to note that the predicted course evaluation for 
#for courses with lower averages is still above 4. Though it is not surprising that courses with higher average grades
#tend to have higher evaluations, I find it surprising that even courses with lower averages present high evaluations. 

#--------
#2. Pooled OLS baseline
#a)Estimate a pooled OLS model with all three regressors:
#m1 = lm(Eval ~ Apct + Enrollment + Required, data = df). Report the results using summary() or modelsummary(). In a comment, interpret the coefficient on Apct: a
#one-percentage-point increase in the share of A grades is associated with how much of
#a change in evaluation scores?
  
m1_p2 = lm(Eval ~ Apct + Enrollment + Required, data = df_p2)
m1_p2
modelsummary(list("Pooled OLS"=m1_p2), stars = TRUE)
#This model suggests that an increase of 1% on As assigned means that it is expected to have and increase of 0.0036% in the evaluation score. Though the result is statistically significant, it could be argued that size of the effect is too small. 

#b) In a comment, explain why the OLS estimate of Apct might be biased. What unobserved characteristics of instructors could simultaneously drive both grading generosity and evaluation scores? Give at least two concrete examples. Is the expected bias upward or downward?

#There could be several reasons why the results are biased. From an institutional standpoint, I could be that the the district changed the evaluation process. In the instructor level, biases could be explained by a variaty of reasons such as the instructor personality or availability for office hours. Consider this, the bias is expected to move upwards. 

#----------
#3) Fixed effects models
#a)Estimate a model with instructor fixed effects, and a two-way model adding year fixed effects
m_instr_p2 = feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df_p2)
m_twfe_p2 = feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df_p2)
m_instr_p2
#b) Compare all three models (m1, m instr, m twfe) in a single table with standard errors clustered by instructor:

modelsummary(
  list("Pooled OLS" = m1_p2, "Instructor FE" = m_instr_p2, "Two-Way FE" = m_twfe_p2),
  vcov = ~InstrID,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"))
#c) Interpret the coefficient on Apct in the instructor-FE model (m instr). In a comment,
#explain what the instructor fixed effect is controlling for. Is the FE coefficient on Apct
#larger or smaller than in the pooled OLS? What does this tell us about the direction of
#omitted variable bias in the pooled OLS estimate — are more lenient graders systematically better or worse evaluators in terms of their unobserved characteristics?

#This results suggest that within the same instructor, an increase of 1% of students recieving an A is associated with a 0.0031 point increase in the evaluation score.These result imply a very small effect but are statistically significant. 
#The FE compares the results of the same instructor over time, therefore it controls variables such as personality, charisma, teaching ability, among other factors not considered. As the results of the FE are smaller than in the OLS, we can infer that the pooled model has un upward mobility caused by omitted variable bias. 

#-----
#4. Random effects and the Hausman test
#a) Estimate a random effects model using plm. The random effects model assumes that
#the unobserved instructor-level heterogeneity is uncorrelated with the regressors:
 
#Find duplicates 
table(df_p2$InstrID, df_p2$CourseID)

#Use Year so it runs

library(plm)

pdata = pdata.frame(df, index = c("InstrID", "CourseID"))

m_re = plm(Eval ~ Apct + Enrollment + Required,
           data = pdata,
           model = "random")
m_fe_plm = plm(Eval ~ Apct + Enrollment + Required,
               data = pdata, model = "within")
phtest(m_fe_plm, m_re)

#c)In a comment, interpret the Hausman test result. What is the null hypothesis? Is it
#rejected? Based on the test and on the substantive reasoning from the previous sub6
#section, should you prefer the fixed effects or the random effects estimator for this
#dataset? Explain in 3–5 sentences.

# The null hypothesis of the Hausman test is that the random effects estimator
# is consistent and efficient, meaning the unobserved instructor effects are
# uncorrelated with the regressors. Statistically, this suggests the random effects model
# could be used. However, based on the earlier reasoning that instructor traits
# (e.g., teaching ability or friendliness) may affect both grading and
# evaluations, the fixed effects estimator may still be preferable.