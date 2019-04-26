library(pacman)
library(dplyr)

## Generate the variables and DGP
# Trying tibble
library(tibble)

n <- 1000
dgp_df <- tibble( 
  e = rnorm(n , 0 , 15 ) , 
  x = runif( n , 0 , 10 ) , 
  y = 1 + exp(0.5*x) + e 
  )


# Let's see what this graphs like (if we an make it look like ed's)
library(ggplot2)

ggplot( 
  data = dgp_df , 
  aes( x , y )
) + 
  geom_point( color = 'gray' )   + 
  geom_smooth( method = 'gam' ,
               formula = y ~ exp(0.5*x) ,
               color = 'blue')  +
  geom_smooth( method = 'lm' , 
               color = 'red')


## Regression models and simulations

library(pacman)
library(purrr)


sim_function <- function(iter , n = 1000 ){
  # Generate DGP like we did before
  iter_df <- tibble(
    e = runif( n, 0 , 15 ) , 
    x = runif( n , 0 , 10 ) , 
    y = 1 + exp(0.5*x) + e
  )
  # Doing the regression models
  model1 <- lm_robust( y ~ x , data = sim_df , se_type = "classical" )
  model2 <- lm_robust( y ~ x , data = sim_df , se_type = "HC2" )
  # Binding and I need to figure out what select does 
  bind_rows( tidy(model1) , tidy(model2) ) %>% 
    select(1:5) %>% filter( term == 'x' ) %>%  
    mutate( se_type = c("classical","HC2") , i = iter)
}

set.seed(12345)
sim_list <- map(1:1e4, sim_function)
sim_df <- bind_rows(sim_list)

ggplot(data = sim_df, aes(sim_df$e)) +
  geom_histogram()
