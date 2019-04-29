library(tibble)

n = 10000
disc_df <- tibble( woman = rbinom( n , 1 , 0.5 ) , 
                   discrimination = woman , 
                   ability = rnorm( n , 0 , 3 ) , 
                   occupation = 1 + 1*ability + 0*woman - 2*discrimination + rnorm( n , 0 , 1) , 
                   wage <- 1  - 1*discrimination + 1*occupation + 2*ability + rnorm( n , 0 , 1) 
)

fem_gap <- wage ~ woman 
wage_gap_biased <- wage ~ woman + occupation 
wage_gap <- wage ~ woman + occupation + ability 

reg_fem_gap <- lm(fem_gap)
reg_wage_gap_biased <- lm(wage_gap_biased)
reg_wage_gap <- lm(wage_gap)

#summary(reg_fem_gap)
#summary(reg_wage_gap_biased)
#summary(reg_wage_gap)

data <- data.frame(ability , woman, occupation, wage, discrimination)

## Plots

library(ggplot2)
# Ability and occupation

ggplot(data = data, aes(data$ability, data$occupation)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

# Wages and occupation

ggplot(data = data, aes(data$wage, data$occupation)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

#Ability and wages

ggplot(data = data, aes(data$ability, data$wage)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

# Gender and wages

ggplot(data = data, aes(data$woman, data$wage)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

# Gender and ability

ggplot(data = data, aes(data$woman, data$ability)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

# Gender and occupation

ggplot(data = data, aes(data$woman, data$occupation)) + 
  geom_point() + 
  geom_smooth(method="lm", color='red')

