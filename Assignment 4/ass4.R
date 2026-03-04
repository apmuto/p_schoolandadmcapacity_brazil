#Assignment 4
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#------
#Library set up
library (haven)
library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
library(readstata13)
library(broom)
library(ggplot2)
library(gt)
library(moments)
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 4")
#-------
#Part 1
#----
#1.1 Setup and data exploration
##a)Load data
df = read.dta13("C:/Users/pc/Downloads/corruption.dta")
## b) Drop observations with missing values on key variables 
df = df %>%
  filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)#there are 170 countries remaining
## c) summary statistics : 
#For ti_cpi
summary(df$ti_cpi)
sd(df$ti_cpi)
#For undp_gdp
summary(df$undp_gdp)
sd(df$undp_gdp)
# The standard deviation for GDP is much larger than the median and it is also 
# larger than the mean, this indicates  a skew to the right. 
#----
# 1.2 Exploratory visualization 
## a) Create a scatter plot and add geom_smooth.
plot1_part1<-ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Table 1",
       x = "GDP per capita (PPP)",
       y = "Corruption Perceptions Index")+
  theme_minimal()
plot1_part1
ggsave("part1_plot1_ass4.png", plot1_part1, width = 6, height = 4)
## b) Here we observe a positive relationship between GDP and corruption perception.
#It is important to note that is hard to establish if it is a linear relationship as most cases skew to the left and Heteroskedasticity increases. 

## c) second scatter plot with log(undp_gdp) on x- axis
plot2_part1<-ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title="Table 2",
    x = "log(GDP per capita)", 
    y = "Corruption Perceptions Index")+
  theme_minimal()
plot2_part1
ggsave("part1_plot2_ass4.png", plot2_part1, width = 6, height = 4)

# The log transformation of the GDP helps to spread the data and observe better the relationship. It also creates a more linear relationship that can be read better in the scatter plot. 
#----
#1.3 Bivariate regression 
##a)Estimate a bivariate regression of corruption
m1= lm(ti_cpi~undp_gdp, data=df)
##b)Print results and comment
summary(m1)
coef(m1)["undp_gdp"]*10000 ###There would be an expected increase of 1.729782 in the corruption index for an increase of 10 000 USD in GDP.

## c) compute 25th and 75th percent. Then use predictions.
q25=quantile(df$undp_gdp,0.25)
q75=quantile(df$undp_gdp,0.75)
c(q25,q75)
predictions(m1,
            newdata = datagrid(undp_gdp=c(q25,q75)))
#This model predicts in general that richer countries tend to be less corrupt. In this case, a country in the 25% of GDP (1974.25 UDS  per capita) is predicted to have a corruption score of 2.84 while a 75% country (10,862UDS  per capita)is predicted to have a 4.38 corruption score (taking into account that higher corruption scores are linked with less corrupt governments).
#----
#1.4 Non-linear specifications
##a)) Estimate a model using the log of GDP per capita
m2=lm(ti_cpi~log(undp_gdp), data=df)
tidy(m2)
##b) Interpret the coefficient on log(undp gdp). You can infer this from the mathematical properties of level-log models, but the most efficient way is to create a prediction plot.

#This model represents what happens when you log the GDP. This means that 1% increase in GDP is associated with 0.0143. 
## c)Estimate a model with a quadratic GDP term:
m3=lm(ti_cpi~undp_gdp+I(undp_gdp^2), data=df)
tidy(m3)
##d) Compare the R2 of all three models. Which specification fits the data best? In a comment, explain why a non-linear specification might be appropriate for this relationship.
tablesum_part1=modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
tablesum_part1
#Comparing the R2s of the three models, model 3 has the highest score. This suggests that the relationship probably is not linear. Nonetheless, the differences between R2s is not large. 
#----
#1.5 Marginal effects
##a) For the log model (m2), compute the average marginal effect of GDP 
avg_slopes(m2, variables = "undp_gdp")
##b) In a comment, explain why the AME differs from the raw coefficient on log(undp gdp).What does the AME tell you in substantive terms?

#The difference between the the coefficient and the AME is because the AME is measuring the change in corruption index per the increase of 1 USD GDP per capita. To make the AME result more sustantial, we can multiply these results (eg. by 1000).

##c) For the quadratic model (m3), compute marginal effects at specific GDP 
slopes(m3, variables = "undp_gdp",
       newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))

#The ME of  the corruption index for a country with a 2000 USD GDP per capita would be 0.000254, while for a country with 10 000 USD GDP per capita would be 0.000214 and for a country with a 30 000 USD GDP per capita would be 0.000114. As the ME explains how the effect changes at different levels, this model predicts that the effect of an increase in GDP per capita in poorer countries has a stronger effect on the corruption index, but this effect reduces as GDP per capita increases, suggesting that GDP per capita has diminishing results. 

#----

#1.6 Prediction plots

##a) Create a prediction plot for the log model
plot3_part1 = plot_predictions(m2, condition = "undp_gdp")
plot3_part1
ggsave("part1_plot3_ass4.png", plot3_part1, width = 6, height = 4)
##b) Create a prediction plot for the quadratic model (m3) on the same variable
plot4_part1=plot_predictions(m3, condition = "undp_gdp")
plot4_part1
ggsave("part1_plot4_ass4.png", plot4_part1, width = 6, height = 4)
##c)In a comment, compare the two plots. Do the models tell a similar story about the corruption–wealth relationship? Where do they diverge?

# Both plots represent a similar relationship between GDP and corruption index but we can see that they tend to diverge as GDP per capita increases, which would be expected as there are few cases with very high GDP per capita and variance increases as GDP increases. 
#----
#1.7 Residual diagnostics
##a) Use broom::augment(m1) to get residuals and fitted values from the level-level model.
##Create a scatter plot of residuals (.resid) vs. fitted values (.fitted). Does the plot suggest non-linearity or heteroskedasticity?
m1_aug = augment(m1)
plot5_part1=ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")+
  theme_minimal()
plot5_part1
ggsave("part1_plot5_ass4.png", plot5_part1, width = 6, height = 4)
#This plot presents a clear curve, suggesting a non linear pattern. Also, as the dispersion of dots in the right suggests heteroskedasticity.

##b) Now do the same for the log model (m2). Does the log transformation improve the residual pattern?
m2_aug = augment(m2)
plot6_part1=ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")
plot6_part1
ggsave("part1_plot6_ass4.png", plot6_part1, width = 6, height = 4)
# This graph suggest that model 2 presents less  heteroskedasticity than the previous model. 
##c) Identify influential observations using Cook’s distance. Use plot(m2, which = 4) or
#compute Cook’s distance manually with cooks.distance(m2). Which countries (if
##any) have Cook’s distance above 4/n? Look up their names.

n = nrow(df)
threshold = 4 / n
cooks_d = cooks.distance(m2)
influential = which(cooks_d > threshold)
df$cname[influential]
plot7_part1=plot(m2, which = 4)
png("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 4/part1_plot7_ass4.png",
    width = 800, height = 600)

plot(m2, which = 4)

dev.off()

##d) Influential observations should not be removed automatically. They may represent genuine cases (e.g., very wealthy or very corrupt countries) rather than data errors. A recommended robustness check would be to reestimate the model excluding these observations and compare the coefficients. If the results are similar, the original
##estimates are robust.
#----
#1.8 Publication-quality table
##a) Create a regression table comparing all three models using:
tablesum_part1=modelsummary(
  list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
gtsave( tablesum_part1,
        "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 4/part1_tablesum1_ass4.png"
       )
#-------
#Part 2
#----
#2.1 Setup and data exploration
##a) Load the dataset and print summary statistics for all variables. 
df2 = read.dta13("C:/Users/pc/Downloads/infantmortality.dta")
summary(df2)
##b) Create a histogram of infant and a histogram of income. 
#For infants
plot1_part2=ggplot(df2, aes(x = infant)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(title = "Histogram of Infant Mortality",
       x = "Infant Mortality",
       y = "Count") +
  theme_minimal()
plot1_part2
ggsave("part2_plot1_ass4.png", plot1_part2, width = 6, height = 4)
#For income
plot2_part2=ggplot(df2, aes(x = income)) +
  geom_histogram(fill = "lightblue", color = "white", bins = 30) +
  labs(title = "Histogram of Infant Mortality",
       x = "Infant Mortality",
       y = "Count") +
  theme_minimal()
plot2_part2
ggsave("part2_plot2_ass4.png", plot2_part2, width = 6, height = 4)
##c) Create a scatter plot of infant (y-axis) against income (x-axis), coloring points by region. Describe the relationship in a comment
plot3_part2=ggplot(df2, aes(x = income, y = infant,color=region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Income",
       y = " Infant Mortality Rate (per 1,000 live births)",
       color="Region")
plot3_part2
ggsave("part2_plot3_ass4.png", plot3_part2, width = 6, height = 4)
#This scatter plot helps us observe the dispersion of income and infant mortality rate according to the region. This format allows us to identify quickly regions that both have high infant mortality rates and low incomes (eg. Africa) and regions with high incomes with low infant mortality rates (eg. Europe). Overall, the graph suggest that higher the income the expected infant mortality rate would decrease, tough the shape of the relationship could vary significantly by region. Also, this graph could suggest that some outlines could influence significantly the shape of the relationship, as in the case of Africa. 

##d) Create the same scatter plot but using log(income) on the x-axis and log(infant) on the y-axis. Does the log-log relationship look more linear?
plot4_part2=ggplot(df2, aes(x = log(income), y = log(infant),color=region)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Income",
       y = " Infant Mortality Rate (per 1,000 live births)",
       color="Region")

plot4_part2
ggsave("part2_plot4_ass4.png", plot4_part2, width = 6, height = 4)
#Yes, by logging both income and infant mortality we can observe a more linear relationship. It also seems to reduce heteroskedasticity.
#----
#2.2 Comparing specifications
##a) Estimate a level-level model:
m1_p2 = lm(infant ~ income, data = df2)
m1_p2
tidy(m1_p2)
##b) Estimate a log-log model
m2_p2 = lm(log(infant) ~ log(income), data = df2)
m2_p2
tidy(m2_p2)
tablesum1_part2=modelsummary(
  list("Level-Level" = m1_p2, "Log-Log" = m2_p2),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
tablesum1_part2
##c) Interpret the coefficient on income in each model
coef(m1_p2)["income"] * 1000
coef(m2_p2)["log(income)"]
#In thelevel-level model, the results suggests that if there increase of 1000 USD in income we would expect around 21 less infant death by 1000. On the other hand, in log-log model, means that a 1% increase in income is associated with approximately a 0.51% decrease in infant mortality. 
##better residual pattern? Discuss in a comment.

m1p2_aug <- augment(m1_p2)
m2p2_aug <- augment(m2_p2)
#For level-level
plot5_part2=ggplot(m1p2_aug, aes(x = .fitted, y = .resid)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted — Level-Level Model",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()
plot5_part2
ggsave("part2_plot5_ass4.png", plot5_part2, width = 6, height = 4)
#For log-log
plot6_part2=ggplot(m2p2_aug, aes(x = .fitted, y = .resid)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted — Level-Level Model",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()
plot6_part2
ggsave("part2_plot6_ass4.png", plot6_part2, width = 6, height = 4)
##The residuals-fitted graph for the level-level model suggest that the relationship is not entirely lineal and there is high heteroskedasticity,as a considerable amount of data skews right. 
#On the other hand,the graph of the log-log model suggests and overall better fit, as data is more dispersed and centered to the 0 line. 

#----
#2.3 Multiple regression with controls
##a) Estimate a log-log model with controls for region and oil-exporting status
m3_p2 = lm(log(infant) ~ log(income) + region + oil, data = df2)
#b) Print the results. In a comment, interpret the coefficient on log(income): does controlling for region and oil status change the income effect?
tidy(m3_p2) 
tablesum2_part2=modelsummary(
  list("Level-Level" = m1_p2, "Log-Log" = m2_p2, "Log-Log with Controls"=m3_p2),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
tablesum2_part2
#Let's change reference region
df2$region <- factor(df2$region, 
                     levels = c("Africa", "Asia", "Europe", "Americas"))
df2$region <- relevel(df2$region, ref = "Europe")
m3_p2_new <- lm(infant ~ income + oil + region, data = df2)
summary(m3_p2_new)
tablesum3_part2=modelsummary(
  list("Level-Level" = m1_p2, "Log-Log" = m2_p2, "Log-Log with Controls"=m3_p2_new),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
tablesum3_part2
#When controlling for both oil and region, the effect of income is reduced significantly. Nonetheless, in this model the coefficient of income is not statistically significant and the R2 is much lower that in the previous models, suggesting that the explanatory power of the log-log model with controls is lower than previous ones explored.

#c) Interpret the coefficient on the Africa region indicator (relative to the reference category). What does it tell you about infant mortality in Africa, controlling for income?

# In this model we decided to use Europe as the reference category. According to this model, the effect of Africa as a region would be 101.486244 and would be statistically significant, suggesting that infant mortality in Africa country would be considerably higher compared to Europe even controlling for income.

#d) Compute average marginal effects using avg slopes(m3). Focus on the AME of income and report it in a comment.
avg_slopes(m3_p2_new, variables = "income")

#This results suggest that on average a $1 increase in income is associated with a 0.005 decrease in infant mortality (or, in other words, we could expect 5 less infant death by 1000 when an increase in 1000 USD in income occurs). Nonetheless, the results are not statistically significant and we should consider the variance within data and explore other explanations. 
#----
#2.4 Interaction: oil status and income
##a) Estimate a model with an interaction between oil status and log income
m4_p2 = lm(log(infant) ~ log(income) * oil + region, data = df2)
tidy(m4_p2)
##b) Use avg slopes(m4, variables = "income", by = "oil") to compute the marginal effect of income separately for oil-exporting and non-oil countries.
avg_slopes(m4_p2, variables = "income", by = "oil")
##c) In a comment, discuss: does the relationship between income and infant mortality differ for oil-exporting countries? What might explain this?

#In this model, the results for non-oil countries suggest that higher income reduces infant mortality whereas the relationship doesnt't maintain for oil countries (where higher incomes are linked with higher mortality rates). It is important to note that the statistical importance of the latter result is is not as high as in non-oil countries. 

##d) Plot how the marginal effect of income varies by oil status: plot slopes(m4, variables = "income", condition = "oil"). Save the plot
plot7_part2 = plot_slopes(m4_p2, variables = "income", condition = "oil")
plot7_part2
ggsave("part2_plot7_ass4.png", plot7_part2, width = 6, height = 4)
#----
#2.5.Predicted values for specific scenarios
##a) Using model m3 (without interaction), compute predicted infant mortality rates 
pred1=predictions(m3_p2,
            newdata = datagrid(
              income = c(1000, 20000, 10000),
              region = c("Africa", "Europe", "Americas"),
              oil = c("no", "no", "yes")))
pred1$infant_pred <- exp(pred1$estimate)
pred1
plot_preds <- data.frame(
  scenario = c("Africa, $1k, non-oil", "Europe, $20k, non-oil", "Americas, $10k, oil"),
  infant_pred = pred1$infant_pred
)
plot_preds
plot10_part2<-ggplot(plot_preds, aes(x = scenario, y = infant_pred, fill = scenario)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(infant_pred,1)), vjust = -0.5) +
  labs(title = "Predicted Infant Mortality by Scenario",
       x = "",
       y = "Predicted Infant Mortality (per 1,000 live births)") +
  theme_minimal(base_size = 14) +
  scale_fill_brewer(palette = "Set2")
ggsave("part2_plot10_ass4.png", plot10_part2, width = 6, height = 4)

#----
#2.6 Publication-quality visualization
##a) Create a prediction plot showing predicted infant mortality across income levels, separately by region:
  
plot8_part2=plot_predictions(m3_p2, 
                 condition = c("income", "region"))+
  labs(
    title = "Predicted Infant Mortality Across Income Levels by Region",
    x="Income (USD)",
    y="Predicted Infant Mortality (per 1,000 live births)"
  )+
  theme_minimal(base_size = 14)
plot8_part2
ggsave("part2_plot8_ass4.png", plot8_part2, width = 6, height = 4)

#b) In a comment (5–10 sentences), discuss: what does this plot tell a general audience
#about the relationship between wealth and infant mortality? What role does geography play? What are the main limitations of this analysis (e.g., omitted variables,
#----
#2.7 Diagnostics and robust inference                                                                                                                                        reverse causality, ecological fallacy)?
#a) Create a residuals vs. fitted values plot for m3. Does the plot suggest heteroskedasticity?
m3p2_aug <- augment(m3_p2) 
plot9_part2=ggplot(m3p2_aug, aes(x = .fitted, y = .resid)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuals vs Fitted — Log-Log Model with Controls",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal()
plot9_part2
ggsave("part2_plot9_ass4.png", plot9_part2, width = 6, height = 4)
#This plot suggests there could the a slight heteroskedasticity, as most dots are located to the center-right of the graph. It is important to note that there are some dots considerably far away from the line. 
#b) Create a regression table comparing all four models with robust standard errors:
tablesum4_part2=modelsummary(
  list("Level" = m1_p2, "Log-Log" = m2_p2,
       "Controls" = m3_p2, "Interaction" = m4_p2),
  vcov = "robust",
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output="gt")
gtsave( tablesum4_part2,
        "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 4/part2_tablesum4_ass4.png"
)
tablesum4_part2
#c)In a comment, discuss the predicted values. Are they plausible? How large is the gap between the African and European scenarios?
  
tablesum_m3 <- modelsummary(
  list(
    "Controls (default SE)" = m3_p2,
    "Controls (robust SE)" = m3_p2
  ),
  stars = TRUE,
  vcov = list(
    NULL,           # first column: default SEs
    "robust"        # second column: robust SEs
  ),
  gof_map = c("r.squared", "nobs"),
  output = "gt"
)
tablesum_m3
gtsave(tablesum_m3,
       "C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 4/m3_comparison_SE.png")

#In this case we can observe that the coefficient do not change but the standard errors change considerably. It is preferably to use robust SE as they adjust for heteroskedasticity. 
