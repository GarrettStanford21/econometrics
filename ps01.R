library(pacman)
p_load(tidyverse ,
       dplyr ,
       haven , 
       broom ,
       ggplot2 , 
       estimatr)

## HW Number 1 
nsw_df <- read_dta("http://users.nber.org/~rdehejia/data/nsw.dta")
psid_df <- read_dta("http://users.nber.org/~rdehejia/data/psid_controls.dta")

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
nsw_treat_df <- nsw_df %>% filter(treat==1) # Ed wants us to take out the untreated from NSW and bind with PSID.
mixed_tbl <- bind_rows( nsw_treat_df , psid_df ) 

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
model_n13 <- treat ~ age + education + black + hispanic + married + nodegree # Six covariates Ed mentions in 11 that I assume he wants us using
logit <- glm(data = mixed_tbl , formula = model_n13 , family = binomial()) # Logit regression for propensity scores
pro_scores <- enframe(logit$fitted.values)  %>% bind_cols(mixed_tbl) %>% rename(p.score = value) # Propensity scores

ps_treat <- pro_scores %>% filter( treat==1 ) # Want to get the lowest propensity score that any of the treated have
min_ps_treat <- min( ps_treat$p.score )
ps_untreat <- pro_scores %>% filter( treat==0 ) # Similarly get the highest propensity score that a nontreated has
max_ps_untreat <- max( ps_untreat$p.score )

##
## IMPORTANT! If you want to mess with enforcing overlap using P scores as Ed did, this next line is where to adjust!
## 

new_df <- pro_scores %>% filter( p.score >= min_ps_treat & treat==0 | p.score <= max_ps_untreat & treat==1 ) 

ggplot(data = new_df , aes( x = p.score , fill = as.factor(treat) , color = as.factor(treat) )) +
  geom_histogram( binwidth = 0.01 , alpha = 0.6)

lm_robust(data = new_df , 
          formula = re78 ~ p.score + treat) %>% tidy

lm_robust(data = new_df , 
          formula = re78 ~ p.score*treat + treat + p.score) %>% tidy
##
## If you choose your own threshold for p scores to enforce overlap, use sequence below to easily create blocks!!
## 

block_min <- min_ps_treat
block_max <- max_ps_untreat
block_size <- (block_max - block_min) / 20
block_bins <- seq(from = block_min , to = block_max , by = block_size)

new_df <- new_df %>% mutate( block = case_when(
                               p.score >= block_bins[1]  & p.score<= block_bins[2]  ~ 1  ,
                               p.score >  block_bins[2]  & p.score<= block_bins[3]  ~ 2  , 
                               p.score >  block_bins[3]  & p.score<= block_bins[4]  ~ 3  , 
                               p.score >  block_bins[4]  & p.score<= block_bins[5]  ~ 4  , 
                               p.score >  block_bins[5]  & p.score<= block_bins[6]  ~ 5  ,
                               p.score >  block_bins[6]  & p.score<= block_bins[7]  ~ 6  ,
                               p.score >  block_bins[7]  & p.score<= block_bins[8]  ~ 7  ,
                               p.score >  block_bins[8]  & p.score<= block_bins[9]  ~ 8  ,
                               p.score >  block_bins[9]  & p.score<= block_bins[10] ~ 9  ,
                               p.score >  block_bins[10] & p.score<= block_bins[11] ~ 10 ,
                               p.score >  block_bins[11] & p.score<= block_bins[12] ~ 11 ,
                               p.score >  block_bins[12] & p.score<= block_bins[13] ~ 12 ,
                               p.score >  block_bins[13] & p.score<= block_bins[14] ~ 13 ,
                               p.score >  block_bins[14] & p.score<= block_bins[15] ~ 14 ,
                               p.score >  block_bins[15] & p.score<= block_bins[16] ~ 15 ,
                               p.score >  block_bins[16] & p.score<= block_bins[17] ~ 16 ,
                               p.score >  block_bins[17] & p.score<= block_bins[18] ~ 17 ,
                               p.score >  block_bins[18] & p.score<= block_bins[19] ~ 18 , 
                               p.score >  block_bins[19] & p.score<= block_bins[20] ~ 19 ,
                               p.score >  block_bins[20] & p.score<= block_bins[21] ~ 20
  
                             ) 
                            ) # Generates 20 equal size blocks based on block_min & block_max & step size.
N_df <- new_df %>% summarise(data_n = n()) %>% as.numeric
block_n <- new_df %>% group_by(block) %>% summarise( grp_size = n() ) %>% rename( N_blk = grp_size) # Creates table of group sizes by block
block_treated  <- new_df %>% group_by(block) %>% filter(treat==1) %>% 
  summarise( grp_size = n() ) %>% select( block, grp_size ) %>% rename(N_treat  = grp_size) # Going to break down by treated/not treated
block_ntreated <- new_df %>% group_by(block) %>% filter(treat==0) %>% 
  summarise( grp_size = n() ) %>% select( block, grp_size ) %>% rename(N_ntreat = grp_size)

block_n <- block_n %>% bind_cols(block_treated) %>% bind_cols(block_ntreated) %>% 
  select( -c( block1, block2) ) %>% mutate(block_wgt = N_blk/N_df) # Dataframe with the group sizes and the weights we will need

blocking_df <- new_df %>% group_by(block, treat) %>% summarise( mean = mean(re78)) %>% summarise(diff = diff(mean)) %>% 
  bind_cols(block_n) # Creating the block treatment mean diffs and binding with group sizes and constructed weights.

tau <- blocking_df %>% mutate(tau_wgt = diff*block_wgt) %>% summarise(tau = sum(tau_wgt)) %>% as.numeric()
tau # PS block estimate

# Double robustness

model_n13f <- re78 ~ age + education + black + hispanic + married + nodegree + treat # "Six covariates" Ed refers us to use
double_robust_coefs <- c(rep(0,20))

for( i in 1:20 ) {
  
block_i <- new_df %>% filter( block == i )
reg_block_i <- lm_robust( data = block_i , formula = model_n13f )
double_robust_coefs[i] <- coef(reg_block_i)[8]
double_robust_coefs[i] <- double_robust_coefs[[i]]

} # Loop filters each block and runs the regression we'd like on it, then puts the treatment coefficient in a vector

t_k <- enframe(double_robust_coefs) %>% rename(block = name , tau_k = value) %>% bind_cols(block_n) %>% 
  mutate(weighted_effect = tau_k*block_wgt) %>% select(-c(block1)) # Includes vector of block effects & weights

t_block <- t_k %>% summarise(tau = sum(weighted_effect)) %>% as.numeric
t_block # Doubly robust estimate

## HW Number 14

# Discuss


## HW Number 15

bias <- tidy(c(rep(1,1))) # Identifier for biased regression
unbias <- tidy(c(rep(0,1))) # Identifier for unbiased regression

# Functions
sim_function_b1 <- function( i , n=50 , x_beta = 1 , v_u = 2 , sd_e = 3) {
  
  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + x_beta*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 6
sim_function_b2 <- function( i , n=50 , x_beta = 2 , v_u = 2 , sd_e = 3) {

  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + x_beta*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 4
sim_function_b4 <- function( i , n=50 , x_beta = 4 , v_u = 2 , sd_e = 3) {
  
  i_sim_model <- tibble( 
    e = rnorm( n , 0 , sd_e ) , 
    u = rnorm( n , 0 , sqrt(v_u) ) ,
    ability = rnorm(n , 0, sqrt(10)) ,
    education = 10 + x_beta*ability + u, 
    wage = 10 + 5*education + 5*ability + e
  )
  
  model_ub <- lm_robust( data = i_sim_model , formula = wage ~ education + ability ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(unbias)
  model_b <- lm_robust( data = i_sim_model , formula = wage ~ education ) %>% tidy %>% filter(term == 'education' ) %>% select(estimate) %>% bind_cols(bias)
  bind_rows(model_ub , model_b)                   
} # Create + estimate data for residual variance of education == 2

# Simulations

set.seed(414) 

sim_list_b1 <- map(1:1e3 , sim_function_b1) # Simulates 1k times
sim_list_b2 <- map(1:1e3 , sim_function_b2) # Simulates 1k times
sim_list_b4 <- map(1:1e3 , sim_function_b4) # Simulates 1k times

sim_df_b1 <- bind_rows(sim_list_b1) %>% rename(bias = x)
sim_df_b2 <- bind_rows(sim_list_b2) %>% rename(bias = x)
sim_df_b4 <- bind_rows(sim_list_b4) %>% rename(bias = x)

mean_bias_b1 <- sim_df_b1 %>% filter(bias==1) %>% summarise(mean = mean(estimate)) 
# Graphs

b1_graph <- ggplot(data = sim_df_b1 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  geom_vline(xintercept = 9.164869 , color = 'red' , size = 1 , linetype = 2) + 
  ggtitle("Omitting Ability from Wage Equation (Blue is Biased Model)") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 

b2_graph <- ggplot(data = sim_df_b2 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  geom_vline(xintercept = 9.164869 , color = 'red' , size = 1 , linetype = 2) + 
  ggtitle("Omitting Ability from Wage Equation (Blue is Biased Model)") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 


b4_graph <- ggplot(data = sim_df_b4 , aes( x = estimate , fill = as.factor(bias) , color = as.factor(bias))) + 
  geom_density( stat = 'density' , alpha = 0.6 ) +
  geom_vline(xintercept = 5 , color = 'red' , size = 1) + 
  geom_vline(xintercept = 9.164869 , color = 'red' , size = 1 , linetype = 2) + 
  ggtitle("Omitting Ability from Wage Equation (Blue is Biased Model)") +
  scale_x_continuous(name = 'Estimate of Education on Wage') +
  scale_y_continuous(name = 'Density') + 
  theme( legend.title = element_blank()) 


b1_graph 
b2_graph 
b4_graph 

