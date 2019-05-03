library(pacman)
p_load( haven ,
        dplyr)

lalonde_df <- read_dta("http://users.nber.org/~rdehejia/data/nsw.dta")

est_basic <- lm( re78 ~ treat , data = lalonde_df) # No heterogenous treatment effects
est_het <- lm( re78 ~ treat*re75 , data = lalonde_df ) # Heterogenous treatment based on prev. income

# Results
est_basic %>% summary()
est_het %>% summary()
