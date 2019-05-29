library(pacman)
p_load(tidyverse , 
       wooldridge , 
       estimatr ,
       MASS)


data("bwght")

# Number 1

bwght_ols_model <-lbwght ~ as.factor(male) + parity + lfaminc + packs
bwght_iv_model <- log(bwght) ~ packs + as.factor(male) + parity + lfaminc | cigprice + as.factor(male) + parity + lfaminc
bwght_ols <- lm_robust( formula = bwght_ols_model ,
                        data = bwght )
bwght_iv <- iv_robust(  formula = bwght_iv_model , 
                        data = bwght , 
                        diagnostics = TRUE)

bwght_iv %>% tidy %>% select(1:5)
bwght_ols %>% tidy %>% select(1:5)

## Reduced form

lm_robust(lbwght ~ as.factor(male) + parity + lfaminc + cigprice , data = bwght) %>% tidy %>% select(1:5)

# Number 3

N = 50
mu_vec  <- c( 5 , 0 , 0 , 0 ) # vector of means for the variables I wish to make
x_vcov  <- c( 5 , 0 , 0 , 0 ) # variance-covariance column for x
e_vcov  <- c( 0 , 1 , 0 , 0 ) # " " for e
u_vcov  <- c( 0 , 0 , 3 , 0 ) # " " for u
nu_vcov <- c( 0 , 0 , 0 , 7 ) # " " for nu

vcov <- matrix( c(x_vcov , e_vcov , u_vcov , nu_vcov ) , ncol = 4 ) # combining into variance covariance matrix

# models for the problem
w1_model <- y ~ w1
w2_model <- y ~ w2
w1_iv_model <- y ~ w1 | w2
w2_iv_model <- y ~ w2 | w1
multi_model <- y ~ w1 + w2 

it_count <- 1000
sim_function <- function( i ){
  sys_df <- mvrnorm( n = N , mu = mu_vec , Sigma = vcov ) # building linear system from vcov
  colnames(sys_df) <- c("x" , "e" , "u" , "nu") #this is the right way to name columns in a matrix
  sys_df <- sys_df %>% as_tibble # now I can make the matrix into a tibble
  
  sim_model_i = tibble( x = sys_df$x ,
                        u = sys_df$u ,
                        e = sys_df$e , 
                        nu = sys_df$nu , 
                        w1 = x + e , 
                        w2 = x + nu  ,
                        y = 3 + 7*x + u ) # specifying model DGP

w1_reg <-  lm_robust( data = sim_model_i , formula = w1_model ) %>% tidy %>% dplyr::select(1:4) %>% 
  filter(term == "w1") %>% bind_cols(model = rep( 1 , 1 ))
w2_reg <-  lm_robust( data = sim_model_i , formula = w2_model ) %>% tidy %>% dplyr::select(1:4) %>% 
  filter(term == "w2") %>% bind_cols(model = rep( 2 , 1 ))
multi_reg <-  lm_robust( data = sim_model_i , formula = multi_model ) %>% tidy %>% dplyr::select(1:4) %>% 
  filter(term == "w1" | term== "w2") %>% bind_cols(model = rep( 3 , 2 ))
w1_iv <-  iv_robust( data = sim_model_i , formula = w1_iv_model ) %>% tidy %>% dplyr::select(1:4) %>% 
  filter(term == "w1") %>% bind_cols(model = rep( 4 , 1 ))
w2_iv <-  iv_robust( data = sim_model_i , formula = w2_iv_model ) %>% tidy %>% dplyr::select(1:4) %>% 
  filter(term == "w2") %>% bind_cols(model = rep( 5 , 1 ))
  
bind_rows( w1_reg , w2_reg , multi_reg , w1_iv , w2_iv )
}

set.seed(414)
sim_df <- map_dfr(1:it_count , sim_function) %>% arrange(model)

sim_df$model <- factor( sim_df$model , 
                        levels = c(1 , 2 , 3 , 4 , 5) ,
                        labels = c("OLS (w1)" , "OLS (w2)" , "OLS (w1,w2)" , "IV (w1|w2)" , "IV(w2|w1)"))

# w1 Estimate Plot

ggplot( ) + 
  geom_density ( data = sim_df %>% filter(term=="w1") , 
                 stat = 'density' , 
                 aes(x = estimate , 
                     group = as.factor(model) , 
                     fill = as.factor(model) , 
                     color=as.factor(model) ) ,
                 alpha=0.5
                 ) +
  geom_vline( xintercept = 7 , 
              linetype = 2 , 
              color='red') + 
  scale_x_continuous(name='w1 Estimate') + 
  scale_y_continuous((name = 'Density'))

# w2 Estimate Plot

ggplot( ) + 
  geom_density ( data = sim_df %>% filter(term=="w2") , 
                 stat = 'density' , 
                 aes(x = estimate , 
                     group = as.factor(model) , 
                     fill = as.factor(model) , 
                     color=as.factor(model) ) ,
                 alpha=0.5
  ) +
  geom_vline( xintercept = 7 , 
              linetype = 2 , 
              color='red') + 
  scale_x_continuous(name='w2 Estimate') + 
  scale_y_continuous((name = 'Density'))

# introducing 'selection bias' into X

x_vcov_bias  <- c( 5 , 1 , 0 , -2 ) # variance-covariance column for x
e_vcov_bias  <- c( 1 , 1 , 0 , 0 ) # " " for e
u_vcov_bias  <- c( 0 , 0 , 3 , 0 ) # " " for u
nu_vcov_bias <- c( -2 , 0 , 0 , 7 ) # " " for nu
vcov_bias <- matrix( c(x_vcov_bias , e_vcov_bias , u_vcov_bias , nu_vcov_bias ) , ncol = 4 ) # combining into variance covariance matrix

sim_function_bias <- function( i ){
  sys_df <- mvrnorm( n = N , mu = mu_vec , Sigma = vcov_bias ) # building linear system from vcov
  colnames(sys_df) <- c("x" , "e" , "u" , "nu") #this is the right way to name columns in a matrix
  sys_df <- sys_df %>% as_tibble # now I can make the matrix into a tibble
  
  sim_model_i = tibble( x = sys_df$x ,
                        u = sys_df$u ,
                        e = sys_df$e , 
                        nu = sys_df$nu , 
                        w1 = x + e , 
                        w2 = x + nu  ,
                        y = 3 + 7*x + u ) # specifying model DGP
  
  w1_reg <-  lm_robust( data = sim_model_i , formula = w1_model ) %>% tidy %>% dplyr::select(1:4) %>% 
    filter(term == "w1") %>% bind_cols(model = rep( 1 , 1 ))
  w2_reg <-  lm_robust( data = sim_model_i , formula = w2_model ) %>% tidy %>% dplyr::select(1:4) %>% 
    filter(term == "w2") %>% bind_cols(model = rep( 2 , 1 ))
  multi_reg <-  lm_robust( data = sim_model_i , formula = multi_model ) %>% tidy %>% dplyr::select(1:4) %>% 
    filter(term == "w1" | term== "w2") %>% bind_cols(model = rep( 3 , 2 ))
  w1_iv <-  iv_robust( data = sim_model_i , formula = w1_iv_model ) %>% tidy %>% dplyr::select(1:4) %>% 
    filter(term == "w1") %>% bind_cols(model = rep( 4 , 1 ))
  w2_iv <-  iv_robust( data = sim_model_i , formula = w2_iv_model ) %>% tidy %>% dplyr::select(1:4) %>% 
    filter(term == "w2") %>% bind_cols(model = rep( 5 , 1 ))
  
  bind_rows( w1_reg , w2_reg , multi_reg , w1_iv , w2_iv )
}

set.seed(414)
sim_bias_df <- map_dfr(1:it_count , sim_function_bias) %>% arrange(model)

sim_bias_df$model <- factor( sim_bias_df$model , 
                        levels = c(1 , 2 , 3 , 4 , 5) ,
                        labels = c("OLS (w1)" , "OLS (w2)" , "OLS (w1,w2)" , "IV (w1|w2)" , "IV(w2|w1)"))


# w1 Estimate Plot

ggplot( ) + 
  geom_density ( data = sim_bias_df %>% filter(term=="w1") , 
                 stat = 'density' , 
                 aes(x = estimate , 
                     group = as.factor(model) , 
                     fill = as.factor(model) , 
                     color=as.factor(model) ) ,
                 alpha=0.5
  ) +
  geom_vline( xintercept = 7 , 
              linetype = 2 , 
              color='red') + 
  scale_x_continuous(name='w1 Estimate') + 
  scale_y_continuous((name = 'Density'))

# w2 Estimate Plot

ggplot( ) + 
  geom_density ( data = sim_bias_df %>% filter(term=="w2") , 
                 stat = 'density' , 
                 aes(x = estimate , 
                     group = as.factor(model) , 
                     fill = as.factor(model) , 
                     color=as.factor(model) ) ,
                 alpha=0.5
  ) +
  geom_vline( xintercept = 7 , 
              linetype = 2 , 
              color='red') + 
  scale_x_continuous(name='w2 Estimate') + 
  scale_y_continuous((name = 'Density'))

