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
sys_df <- mvrnorm( n = N , mu = mu_vec , Sigma = vcov ) # building linear system from vcov
colnames(sys_df) <- c("x" , "e" , "u" , "nu") #this is the right way to name columns in a matrix
sys_df <- sys_df %>% as_tibble # now I can make the matrix into a tibble

# models for the problem
w1_model <- y ~ w1
w2_model <- y ~ w2
w1_iv_model <- y ~ w1 | w2
w2_iv_model <- y ~ w2 | w1
multi_model <- y ~ w1 + w2 

it_count <- 1000
sim_function <- function( i ){
  sim_model_i = tibble( x = sys_df$x ,
                        u = sys_df$u ,
                        e = sys_df$e , 
                        nu = sys_df$nu , 
                        w1 = x + e , 
                        w2 = x + nu  ,
                        y = 3 + 7*x + u )

w1_reg <-  lm_robust( data = sim_model_i , formula = w1_model ) %>% tidy %>% select(1:4) %>% 
  filter(term = "w1") %>% bind_cols(model = rep( 1 , 2 ))
w2_reg <-  lm_robust( data = sim_model_i , formula = w2_model ) %>% tidy %>% select(1:4) %>% 
  filter(term = "w1") %>% bind_cols(model = rep( 2 , 2 ))
multi_reg <-  lm_robust( data = sim_model_i , formula = multi_model ) %>% tidy %>% select(1:4) %>% 
  filter(term = "w1") %>% bind_cols(model = rep( 3 , 3 ))
w1_iv <-  iv_robust( data = sim_model_i , formula = w1_iv_model ) %>% tidy %>% select(1:4) %>% 
  filter(term = "w1") %>% bind_cols(model = rep( 4 , 2 ))
w2_iv <-  iv_robust( data = sim_model_i , formula = w2_iv_model ) %>% tidy %>% select(1:4) %>% 
  filter(term = "w1") %>% bind_cols(model = rep( 5 , 2 ))
  
}

sim_df <- map_dfr(1:it_count , sim_function) %>% arrange(model)



ggplot( data = sim_df , 
        aes( x = estimate ,
             color = model ))

