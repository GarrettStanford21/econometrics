library(pacman)
p_load(tidyverse ,
       dplyr ,
       haven , 
       broom ,
       ggplot2 , 
       estimatr)

## HW Number 1 
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
##  HW Number 4
# Discuss

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

## HW Number 9
mixed_tbl <- bind_rows( nsw_df , psid_df ) 

## HW Number 10
means_mix_tbl <- mixed_tbl %>% group_by(treat) %>% summarise( means = mean( re78 ) ) # Table with means of treat/untreat
means_mix_tbl[2,2] - means_mix_tbl[1,2]

model_n8 <- re78 ~ treat
reg_n8_test <- lm( data = mixed_tbl , formula = model_n8 )
reg_n8_test %>% tidy

## HW Number 11
# Discuss

## HW Number 12

covariates_treat <- mixed_tbl %>% filter(treat==1) %>% select(age , education , black , hispanic , married, nodegree)
covariates_ntreat <-  mixed_tbl %>% filter(treat==0) %>% select(age , education , black , hispanic , married, nodegree)

## HW Number 13
model_n11 <- treat ~ age + education + black + hispanic + married + nodegree
logit <- glm(data = mixed_tbl , formula = model_n11 , family = binomial) # Logit regression for propensity scores
pro_scores <- enframe(logit$fitted.values)  %>% bind_cols(mixed_tbl) %>% rename(p.score = value) # Propensity scores

ps_treat <- pro_scores %>% filter( treat==1 ) # Want to get the lowest propensity score that any of the treated have
min_ps_treat <- min( ps_treat$p.score )
overlap_treat <- pro_scores %>% filter( p.score > min_ps_treat & treat==0 ) # Only look at untreated with at least that p-score
ps_untreat <- pro_scores %>% filter( treat==0 ) # Similarly get the highest propensity score that a nontreated has
max_ps_untreat <- max( ps_untreat$p.score )

##
## IMPORTANT! If you want to mess with enforcing overlap using P scores as Ed did, this next line is where to adjust!
## 

new_df <- pro_scores %>% filter( p.score > 0.05 & p.score < 0.45 ) 

ggplot(data = new_df , aes( x = p.score , fill = as.factor(treat) , color = as.factor(treat) )) +
  geom_histogram( binwidth = 0.05 , alpha = 0.6)

lm_robust(data = new_df , 
          formula = re78 ~ p.score + treat) %>% tidy

lm_robust(data = new_df , 
          formula = re78 ~ p.score*treat + treat + p.score) %>% tidy

new_df <- new_df %>% mutate( block = case_when(
                               p.score > 0 & p.score<= (1/20)*0.5 ~ 1 ,
                               p.score > (1/20)*0.5 & p.score<= (2/20)*0.5 ~ 2 ,
                               p.score > (2/20)*0.5 & p.score<= (3/20)*0.5 ~ 3 ,
                               p.score > (3/20)*0.5 & p.score<= (4/20)*0.5 ~ 4 ,
                               p.score > (4/20)*0.5 & p.score<= (5/20)*0.5 ~ 5 ,
                               p.score > (5/20)*0.5 & p.score<= (6/20)*0.5 ~ 6 ,
                               p.score > (6/20)*0.5 & p.score<= (7/20)*0.5 ~ 7 ,
                               p.score > (7/20)*0.5 & p.score<= (8/20)*0.5 ~ 8 ,
                               p.score > (8/20)*0.5 & p.score<= (9/20)*0.5 ~ 9 ,
                               p.score > (9/20)*0.5 & p.score<= (10/20)*0.5 ~ 10 ,
                               p.score > (10/20)*0.5 & p.score<= (11/20)*0.5 ~ 11 ,
                               p.score > (11/20)*0.5 & p.score<= (12/20)*0.5 ~ 12,
                               p.score > (12/20)*0.5 & p.score<= (13/20)*0.5 ~ 13,
                               p.score > (13/20)*0.5 & p.score<= (14/20)*0.5 ~ 14,
                               p.score > (14/20)*0.5 & p.score<= (15/20)*0.5 ~ 15,
                               p.score > (15/20)*0.5 & p.score<= (16/20)*0.5 ~ 16,
                               p.score > (16/20)*0.5 & p.score<= (17/20)*0.5 ~ 17,
                               p.score > (17/20)*0.5 & p.score<= (18/20)*0.5 ~ 18,
                               p.score > (18/20)*0.5 & p.score<= (19/20)*0.5 ~ 19,
                               p.score > (19/20)*0.5 & p.score<= 0.5 ~ 20
                             ) 
                            ) # Generates 20 equal size blocks. (Max P score is ~0.5) 
N_df <- new_df %>% summarise(data_n = n()) %>% as.numeric
block_n <- new_df %>% group_by(block) %>% summarise( grp_size = n() ) %>% rename( N_blk = grp_size) # Creates table of group sizes by block
block_treated  <- new_df %>% group_by(block) %>% filter(treat==1) %>% 
  summarise( grp_size = n() ) %>% select( block, grp_size ) %>% rename(N_treat  = grp_size) # Going to break down by treated/not treated
block_ntreated <- new_df %>% group_by(block) %>% filter(treat==0) %>% 
  summarise( grp_size = n() ) %>% select( block, grp_size ) %>% rename(N_ntreat = grp_size)

block_n <- block_n %>% bind_cols(block_treated) %>% bind_cols(block_ntreated) %>% 
  select( -c( block1, block2) ) %>% mutate(block_wgt = N_blk/N_df) # Dataframe with the group sizes and the weights we will need

inc_means_block  <- new_df %>% group_by(block, treat) %>% summarise(mean = mean(re78)) # Getting treatment means in blocks
treat_inc_block  <- inc_means_block %>% filter(treat==1)  %>% rename(treat_re78  = mean) 
ntreat_inc_block <- inc_means_block %>% filter(treat==0)  %>% rename(ntreat_re78 = mean)

blocking_df <- block_n %>% bind_cols(treat_inc_block , ntreat_inc_block) %>% 
  select( -c(block1 , treat , block2 , treat1)) %>% 
  mutate(diff_means = treat_re78 - ntreat_re78 , tau_block = diff_means*block_wgt) # Everything ready for estimating
tau <- blocking_df %>% summarise(tau_hat = sum(tau_block)) %>% as.numeric # Estimates
tau

# Double robustness

block_1 <- new_df %>% filter( block==1 )
lm_robust()

## HW Number 14

# Discuss


## HW Number 15

bias <- tidy(c(rep(1,1))) # Identifier for biased regression
unbias <- tidy(c(rep(0,1))) # Identifier for unbiased regression

# Functions
sim_function_v4 <- function( i , n=50 , v_u = 4 , sd_e = 3) {

  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + 2*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 4
sim_function_v6 <- function( i , n=50 , v_u = 6 , sd_e = 3) {
  
  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + 2*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 6
sim_function_v2 <- function( i , n=50 , v_u = 2 , sd_e = 3) {
  
  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + 2*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 2

# Simulations

set.seed(414) 

sim_list_v4 <- map(1:1e3 , sim_function_v4) # Simulates 1k times
sim_list_v6 <- map(1:1e3 , sim_function_v6) # Simulates 1k times
sim_list_v2 <- map(1:1e3 , sim_function_v2) # Simulates 1k times

sim_df_v4 <- bind_rows(sim_list_v4) %>% rename(bias = x)
sim_df_v6 <- bind_rows(sim_list_v6) %>% rename(bias = x)
sim_df_v2 <- bind_rows(sim_list_v2) %>% rename(bias = x)

# Graphs

var_4_graph <- ggplot(data = sim_df_v4 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  ggtitle("Omitting Ability from Wage Equation") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 

var_6_graph <- ggplot(data = sim_df_v6 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  ggtitle("Omitting Ability from Wage Equation") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 


var_2_graph <- ggplot(data = sim_df_v2 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  ggtitle("Omitting Ability from Wage Equation") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 

# Plots of estimates with different educ.  resid. variances. 
# Higher resid. variance on educ. corresponds to less correlation w ability since education has higher variation indep-
# endent of ability. Lower resid. variance means more of educ. variation is driven by variation in ability.

var_4_graph # When residual variance on education is 4. 
var_2_graph # When residual variance on education is 2.
var_6_graph # When residual variance on education is 6.
