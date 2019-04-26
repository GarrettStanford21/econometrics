## Generate the variables and DGP
# Trying tibble
library(tibble)

n <- 1000
dgp_df <- tibble( 
  e = rnorm(n , 0 , 15 ) , 
  x = runif( n , 0 , 10 ) , 
  y = 1 + exp(0.5*x) + e )
)

# Let's see what this graphs like (if we an make it look like ed's)
library(ggplot2)

ggplot( 
  data = dgp_df , 
  aes( x , y )
) + 
  geom_point( color = 'gray' ) + 
  geom_smooth( method = 'gam' ,
               formula = y ~ exp(0.5*x) ,
               color = 'green') +
  geom_smooth( method = 'lm' , 
               color = 'red')