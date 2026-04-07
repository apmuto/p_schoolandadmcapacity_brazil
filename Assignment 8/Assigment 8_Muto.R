#Assignment 8
#Applied Quantitative Methods for the Social Sciences
#Ana Paula Muto

#===============================================================================
#SET UP
library(sf)
library(spData)
library(parameters)
library(spatialreg)
library(spdep)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(modelsummary)
setwd("C:/Users/pc/Documents/R-Projects/QSS/aqmss2/Assignment 8")

#===============================================================================
#PART 1
data(world)
#===============================================================================
#_______________________________________________________________________________
#1.1. Setup and OLS baseline

#a)
nrow(world) #we are starting with 177 rows
world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
nrow(world) #now he have 160 rows
world$log_gdp = log(world$gdpPercap)
#As there is great variance across GDP and distributions are skewed because of outliers (rich countries) , we log the results to compress the upper tail and make the relationship between variables more linear.

#b)
ols_fit = lm(lifeExp ~ log_gdp, data = world)
modelsummary(list(ols_fit),
             stars = TRUE,
             gof_map = c("nobs", "r.squared"))
#The estimated coefficient on log_gdp is 5.540 and it is statistically significant, as the p<0.001. 
#The coefficient means that one unit increase in log gdp -which means doubling the gdp- is associated with an increase in life expectancy by approximately 5.540 years.
#R2 is a measure that explains how good is the explanatory power of the model. R2 ranges from 0 to 1, with 1 usually meaning that the explanatory power is very high. 

#c)
world$ols_resid = residuals(ols_fit)
ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")
ggsave("Assignment8_map1_p1.pdf", width = 10, height = 5)
#In the map we can observe there are clusters of negative residuals that are concentrated mainly in Africa and around Russia. On the other hand, we can see clusters that tend to have positive residuals in areas such as Western Europe and East Asia. 
#This would mean that the areas where there is a cluster of positive residuals have higher life expectancy than what the model predicts, while the ones with negative residuals have lower life expectancy. 

#_______________________________________________________________________________
#1.2 Spatial weights matrix

#a)
nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)
#According to this, 16 countries have no neighbors. The most likely reason that could explain this is that some countries (Australia, Japan, New Zeland, etc.) and they border with the sea. 
#Queen contiguity means that it defines territories as neighbors if they share a border or a corner. 
#. The zero.policy = TRUE argument allows these units to remain in the analysis despite having no neighbors.

#b)
moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)
# The Moran's I Statistic is 0.437486921 and the p-vañue is 8.054e-12. As the Moran's I is considerably above 0, we can say that there is positive spatial clustering. As the p- value is <0.001, we can also hay that the results are statistically significant.
#Considering both results, we can say that there is significant spatial correlation. In other words, residuals are not randomly distributed in space. This violates the OLS assumption of independence of errors.



#Example done in class 
sample=world %>% 
  filter(name_long%in% c("Morocco","Spain","Portugal"))
nb2=poly2nb(sample,queen = TRUE)
lists = nb2listw(nb2, style = "W", zero.policy = TRUE)
sample$name_long
centroidsam=st_centroid(sample)
st_distance(sample)
distmat=st_distance(sample)
nb2=dnearneigh(centroids,d1=0. d2=100)
nbdist=mat2listw(distmat)
#Options to solve this is using distances or buffer zones
#_______________________________________________________________________________
#1.3 Lagrange Multiplier tests

lm_tests = lm.RStests(
  ols_fit,
  listw = listw,
  test = c("RSerr", "RSlag", "adjRSerr", "adjRSlag"),
  zero.policy = TRUE
)
summary(lm_tests)
#a)

#LMerr (RSerr )tests for spatial dependence in the error term (λ ̸= 0). In this case, the statistic is 52.17 and the p-value is 5.089e-13.
#LMlag tests for a spatially lagged dependent variable (ρ ̸= 0). In this case, the statistic is 0.062 and the p-value is 0.8040. 
#Considering these results, only LMerr is significant, suggesting that we should focus on the spatial dependence in the error term. 

#b)
#Both robust test control for the presence of other types of dependence. We can observe that the results are significant for RLMerr (p< 0.001) but not for RLMlag. 
#Based on these results, the Spatial Error Model (SEM) is prefered over the Spatial Lag Model (SLM).

#_______________________________________________________________________________
#1.4 Spatial Error Model (SEM)

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)
#Now we compare both models 
modelsummary(list(ols_fit,sem_fit),
             stars = TRUE,
             gof_map = c("nobs","r.squared"))

#a)
# The estimated coefficient on log_gdp from the SEM is 3.958 and is statistically significant (p<0.001). 
#Comparing this to the original OLS model, we can observe that the coefficient is considerably smaller (in the OLS was 5.540), and also that the intercepts have changed (from 20.266 in the OLS to 36.767 in the SEM). 
# The λ (lambda) parameter is 0.763 and its p-value is below 0.001, which means that is statistically significant.

#b)
#In the SEM, errors in one country depend on the errors of its neighbors (u = λWu + ε). so the  λ (lambda) parameter showcasses the spatial correlation in the error term. 
#A positive and significant  λ (lambda) parameter suggests that there are unobserved factors that are spatially clustered. In this case, we could say that there are unobserved factors in our original model that affect life expectancy more in some areas than in others; these factors could be related with health care systems, environment, or political institutions. 

#c)
world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)
#We can observe that in the SEM the Moran'I is −0.0860 and the p is 0.8843. In other words, in the SEM the spatial correlation is no longer statistically significant.
#Considering this, the SEM has reduced the spatial dependence in the residuals, therefore the model is a better fit. 

#_______________________________________________________________________________
#1.5 Distance-based weights: an alternative neighborhood
#a)
coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)
summary(nb_dist)
# In the queen contiguity there were 16 countries with no neighbors while in this model the amount increases to 114 countries. 
# This could be because this model calcultes neighbors from the centroid of the territorry (using 300km as a threshold). This would affect particularly large territories such as Brazil, United States, and Russia.

#b)

listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)
sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)
summary(sem_dist)

#c)
moran.test(world$sem_resid, listw = listw_dist, zero.policy = TRUE)
#Yes, this model also succeds as the p-value is quite high (0.4973), suggesting that the vale of the Moran'S I ( -0.02114705) is not significant, therefore that that the spatial correlation in the error term is no longer significant. 

#===============================================================================
#PART 2
#===============================================================================
#_______________________________________________________________________________
#2.1.Spatial Lag Model (SLM)
sml_fit = lagsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sml_fit)
modelsummary(list("OLS"=ols_fit,"SEM"=sem_fit,"SML"=sml_fit),
             stars = TRUE,
             gof_map = c("nobs","r.squared"),
             output = "tablemodels_ass8.png")
#a)
# The estimated  ρ(rho) parameter is-0.0042561 and it is not statistically significant as the p-value is 0.805

#b)
#SLM captures spatial diffusion, where the outcome of one country is determined by its neighbors outcomes. If ρ > 0 it means that there would be a direct relationship between a country's life expectancy and its neighbors' life expectancy. 
#In this case, the estimated  ρ (rho) is very close to 0 and is not statistically significant, which would suggest that a country's life expectancy is not driven by its neighbors' life expectancy. 

#c)
#Considering that solving y = ρWy+Xβ+ε for y gives y = ((I − ρW)^−1)(Xβ + ε). In other words,(Xβ + ε) term would be multiplied by (I − ρW)^−1). 
#While in the OLS and SEM models the coeffcient reflects the direct effect, in the SLM is only one part of the analysis, as the whole model calculates the spillover effects, which can have feedback loops. 

#_______________________________________________________________________________
#2.2 Direct and Indirect Effects
#NOTES: foe 2,2, they are asking us to calculate errors from simulations. Use set_seed to help randomization.
#a)
set.seed(123) #Setting seed at an arbitrary number
impacts_sml = impacts(sml_fit, listw = listw, R = 500)
summary(impacts_sml, zstats = TRUE)
#It calculates the impact od the direct effects of the log_gdp at 5.548223 (and being statistically significant at < 2.22e-16), the indirect effects at -0.02353896 ( and not being statistically significant), and the overall effects at 5.524684 (being statistically significant at 2.22e-16.
#The direct effect is slightly higher than the coefficients for the raw log_gdp coefficient from the OLS (5.548)and the same as the SML (5.548)

#b) 
#The indirect effect is linked with the spillover effect. That means that how a change in one unit of log GDP affects life expectancy of neighboring countries throughout spatial diffusion. 
# In this case, the indirect effects are negative, close to zero, and have little statistical significance. This suggests that there is little to none spatial diffusion. 

#c)
#In our case, the direct effects are slightly higher than the total. This happens because the total effects are offset slightly by the indirect effects, as they are negative.


#_______________________________________________________________________________
#2.3 Model Comparison
#a)
AIC(ols_fit, sem_fit, sml_fit)

#The OLS has an AIC of  965.9880, the SEM of 894.7021, and the SML of 967.9270. Considering that a lower AIC indicates a better fit, penalized for model complexity, the SEM has the lowest AIC. This fits with the results obtained in section 1.3., where we found that the OLS didn't take into account spatial dependence of the error term and also found that there was little spill over effect.

#b)
#In this assignment we have explored the relationship between log_gdp and life expectancy, considering regions. First, we calculated the relationship using an OLS and obtained a coefficient of 5.540, which was statistically significant. Nonetheless, the R2 suggested that the fit of the model could be improved. When we run the Moran's I test in this model we obtained a statistic of  0.437486921 and the p-vañue is 8.054e-12. This suggests that the OLS model exhibited strong spatial correlation in the residuals.
#To solve this problem, we first used  Lagrange Multiplier tests, which include "LMerr", "LMlag", "RLMerr", "RLMlag". High and statistically significant results in "LMerr" and  "RLMerr" would recommend to use a SEM model while high and significant results in "LMlag" and "RLMlag" would suggest to used SML. In our case, our results suggested that we only required to use a SEM model.
#Having calculated the OLS, SEM, and SLM models, we can observe that the coefficients for both OLS and SML are quite similar, while the SEM coefficiente is considerably lower (and also presents a considerably higher intercept). Considering the AIC results of these models, the SEM model is the best fit of the three options. 
#The results obtained from the SLM are still useful, as they prove that the spillover effects are either marginal or non-significant.
#One of the main limitations of using queen contiguity weights is that it focuses on geographical adjacency while it ignores other forms of connectivity. For instance, this format is not taking into consideration countries that have no geographical neighbors, which is the case of Australia, Japan, and New Zealand (which are comprised of islands). Additionally, it doesn't capture non geographical links between countries, which could be defined by trade treaties,beign part of the same international organizations and/or colonial legacies. 

