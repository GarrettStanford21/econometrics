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
library(estimatr)

# Generate DGP like we did before

sim_function <- function(iter , n = 30 ){
  iter_df <- tibble(
    e = rnorm( n, 0 , 15 ) , 
    x = runif( n , 0 , 10 ) , 
    y = 1 + exp(0.5*x) + e
  )
# Doing the regression models
  model1 <- lm_robust( y ~ x , data = iter_df , se_type = "classical" )
  model2 <- lm_robust( y ~ x , data = iter_df , se_type = "HC2" )
  bind_rows( tidy(model1) , tidy(model2) ) %>% 
    select(1:5) %>% filter( term == 'x' ) %>%  
    mutate( se_type = c("classical","HC2") , i = iter)
}

set.seed(12345)
sim_list <- map(1:1e4, sim_function)
sim_df <- bind_rows(sim_list)

# Error density plots
ggplot( data = sim_df , 
        aes(x = sim_df$std.error , 
            group = sim_df$se_type , fill=sim_df$se_type)) + 
  geom_density( stat = "density" , 
                color = "NA" ,
                alpha = 0.6 ) + 
  scale_x_continuous(name = "Standard error") +
  scale_y_continuous(name = "Density")  +
  ggtitle('Classical vs. heteroscedastic-consistent errors') +
  theme(legend.title = element_blank() ,
        panel.grid.major = element_line(colour = 'grey' , linetype = 2) ,
        panel.background = element_blank()
        )

# t-statistic density plots

ggplot( data = sim_df , 
        aes(x = sim_df$statistic , 
            group = sim_df$se_type , fill=sim_df$se_type)) + 
  geom_density( stat = "density" , 
                color = "NA" ,
                alpha = 0.6 ) + 
  scale_x_continuous(name = "t-statistic") +
  scale_y_continuous(name = "Density")  +
  ggtitle('Classical vs. heteroscedastic-consistent errors') +
  theme(legend.title = element_blank() ,
        panel.grid.major = element_line(colour = 'grey' , linetype = 2) ,
        panel.background = element_blank()
  )


## Updating to the correct null hypothesis

# DGP function

null_function <- function(iter , g = 0 , s = 1 , n = 30 ){
  iter_df <- tibble(
    e = rnorm( n, 0 , 15^s ) , 
    x = runif( n , 0 , 10 ) , 
    y = 1 + exp(g*x) + e
  )
  # Doing the regression models
  model1 <- lm_robust( y ~ x , data = iter_df , se_type = "classical" )
  model2 <- lm_robust( y ~ x , data = iter_df , se_type = "HC2" )
  bind_rows( tidy(model1) , tidy(model2) ) %>% 
    select(1:5) %>% filter( term == 'x' ) %>%  
    mutate( se_type = c("classical","HC2") , i = iter)
}

set.seed(123)
null_df <- map(1:1e4, null_function) %>% bind_rows()

# Plots

# Error density plots
ggplot( data = null_df , 
        aes(x = null_df$std.error , 
            group = null_df$se_type , fill=null_df$se_type)) + 
  geom_density( stat = "density" , 
                color = "NA" ,
                alpha = 0.6 ) + 
  scale_x_continuous(name = "Standard error") +
  scale_y_continuous(name = "Density")  +
  ggtitle('Classical vs. heteroscedastic-consistent errors') +
  theme(legend.title = element_blank() ,
        panel.grid.major = element_line(colour = 'grey' , linetype = 2) ,
        panel.background = element_blank()
  )

# t-statistic density plots

ggplot( data = null_df , 
        aes(x = null_df$statistic , 
            group = null_df$se_type , fill=null_df$se_type)) + 
  geom_density( stat = "density" , 
                color = "NA" ,
                alpha = 0.6 ) + 
  scale_x_continuous(name = "t-statistic") +
  scale_y_continuous(name = "Density")  +
  ggtitle('Classical vs. heteroscedastic-consistent errors') +
  theme(legend.title = element_blank() ,
        panel.grid.major = element_line(colour = 'grey' , linetype = 2) ,
        panel.background = element_blank()
  )

# p-value density plots

ggplot( data = null_df , 
        aes(x = null_df$p.value , 
            group = null_df$se_type , fill=null_df$se_type)) + 
  geom_density( stat = "density" , 
                color = "NA" ,
                alpha = 0.6 ) + 
  scale_x_continuous(name = "t-statistic") +
  scale_y_continuous(name = "Density")  +
  ggtitle('Classical vs. heteroscedastic-consistent errors') +
  theme(legend.title = element_blank() ,
        panel.grid.major = element_line(colour = 'grey' , linetype = 2) ,
        panel.background = element_blank()
  )
