library(pacman)
p_load(tidyverse ,
       dplyr ,
       haven , 
       broom ,
       ggplot2 , 
       estimatr)

nsw_df <- read_dta("D:/Economics/Class Materials, Assignments, Papers/UO/Econometrics III (Econ. 525)/nsw.dta")
psid_df <- read_dta("D:/Economics/Class Materials, Assignments, Papers/UO/Econometrics III (Econ. 525)/psid_controls.dta")

## HW Number 2

model_n2 <- re75 ~ treat
reg_n2 <- lm(data = nsw_df , formula = model_n2)
reg_n2 %>% tidy # Note: Not significant! Tells us that there was likely not significant selection into treatment based on 1975 income.

## HW Number 3

model_n3 <- re78 ~ treat
reg_n3 <- lm(data = nsw_df , formula = model_n3)
reg_n3 %>% tidy # We have a larger and marginally significant (p < 0.1) effect of treatment on 1978 earnings

## HW Number 5

model_n5 <- re78 ~ treat + age + education + black + hispanic
reg_n5 <- lm(data  = nsw_df , formula = model_n5)
reg_n5 %>% tidy # Adding race variables *reduced* precision of treatment and somewhat reduced the treatment effect estimate. 
                # This probably happens because having race and education both in our regression is a case of "bad controls"

## HW Number 6
# Bad controls are-- broadly speaking-- controls which "re-introduce" selection bias in a model which otherwise would not have such problems.
# Adding education is probably a bad control because we took race (which is indeed exogenous) and we've added education which is probably endogeneous with race-- see Neal and Johnson (1996).

## HW Number 7
# You didn't ask us to regress nodegree

## HW Number 8

means_nsw_tbl <- nsw_df %>% group_by(treat) %>% summarise( means = mean(re78)) 
means_nsw_tbl[2,2] - means_nsw_tbl[1,2] # The same as our treatment estimate from regression in number 3!

## Number 9
mixed_tbl <- bind_rows( nsw_df , psid_df ) 

## Number 10
means_mix_tbl <- mixed_tbl %>% group_by(treat) %>% summarise( means = mean( re78 ) ) # Table with means of treat/untreat
means_mix_tbl[2,2] - means_mix_tbl[1,2]

model_n8 <- re78 ~ treat
reg_n8_test <- lm( data = mixed_tbl , formula = model_n8 )
reg_n8_test %>% tidy

## Number 11
model_n11 <- treat ~ re78 + age + black + hispanic
logit <- glm(data = mixed_tbl , formula = model_n11 , family = binomial)
pro_scores <- enframe(logit$fitted.values)  %>% bind_cols(mixed_tbl) %>% rename(p.score = value)

ps_treat <- pro_scores %>% filter( treat==1 )
min_ps_treat <- min( ps_treat$p.score )
overlap_treat <- pro_scores %>% filter( p.score > min_ps_treat & treat==0 )
ps_untreat <- pro_scores %>% filter( treat==0 )
max_ps_untreat <- max( ps_untreat$p.score )
new_df <- pro_scores %>% filter( p.score < max_ps_untreat & treat==1 ) %>% bind_rows(overlap_treat)

ggplot(data = new_df , aes( x = p.score , fill = as.factor(treat) , color = as.factor(treat) )) +
  geom_histogram( binwidth = 0.06 , alpha = 0.6)


# NUmber 15

bias <- tidy(c(rep(1,1)))
unbias <- tidy(c(rep(0,1)))
sim_function <- function( i , n=50 ) {

  i_sim_model <- tibble( 
    e = rnorm( n , 0 , 3 ) , 
    u = rnorm( n , 0 , 2 ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + 2*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
}


set.seed(414)
sim_list <- map(1:1e3 , sim_function)
sim_df <- bind_rows(sim_list) %>% rename(bias = x)

ggplot(data = sim_df , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  ggtitle("Omitting Ability from Wage Equation") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank())
