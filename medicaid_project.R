library(pacman)
p_load( ipumsr, 
        broom ,
        haven ,
        tidyverse ,
        estimatr ,
        stargazer)

mcinerney_st_df <- read_dta("D:/Economics/Data/Medicaid Insurance and SUicide/mcinerney-state_level_controls_13.dta") %>% arrange(STATEFIP, YEAR)

suicide_df <- read_dta("D:/Economics/Data/Cause of Death Suicide (99-17)/R-ready-suicide-data.dta") %>% arrange(STATEFIP, YEAR)
cps_df <- read.table("D:/Economics/Data/CPS Data/asec_st") %>% group_by(STATEFIP, YEAR) %>% arrange(STATEFIP, YEAR)
data_df <- cps_df %>% filter(YEAR > 1998 & YEAR < 2018) %>% bind_cols(suicide_df) %>% select( -c(YEAR1, STATEFIP1))


data_df <- filter( data_df , YEAR > 2000 & YEAR < 2010) %>% bind_cols(mcinerney_st_df) %>% select( -c(
                                                                                                     MMR_PC_03, 
                                                                                                     prct_MMC_comp, 
                                                                                                     aunemr_nsl, 
                                                                                                     se1_v4w_lag,
                                                                                                     se1_v4w_lag2, 
                                                                                                     se1_v4w_lag3, 
                                                                                                     se1_v4w_lead, 
                                                                                                     se1_v4w_lead2
)
)


data_df <- data_df %>% mutate( suicide_rate_total = suicide_total*(100000/population) , # creating suicide-per-100,000 variables using CPS population data
                    suicide_rate_guns = suicide_guns*(100000/population) , 
                    suicide_rate_other = suicide_other*(100000/population) ,
                    medicaid_rate = medicaid_rate*100 , # the Medicaid eligibility measure uses %*100 format so I also want my rates to look that way, too
                    private_rate = private_rate*100 ,
                    prime_rate = prime_rate*100 ,
                    medicare_rate = medicare_rate*100 ,
                    militcare_rate = militcare_rate*100 ,
                    fem_rate = fem_rate*100 )

# model specifications: first stage then IV
model1s_basic <- log(medicaid) ~ se1_v4w + log(private) + prime_rate + fem_rate
model_iv_basic <-  log(suicide_total) ~ log(medicaid) + log(private) + prime_rate + fem_rate | se1_v4w + log(private) + prime_rate + fem_rate 

model1s_vets <- log(medicaid) ~ se1_v4w + log(private) + prime_rate + fem_rate + combat_vets + log(militcare)
model_iv_vets <- log(suicide_total) ~ log(medicaid) + log(private) + prime_rate + fem_rate + combat_vets + log(militcare) | se1_v4w + log(private) + prime_rate + fem_rate + combat_vets + log(militcare) 

# regressions: same ordering as above

s1_reg_basic <- lm_robust(data = data_df ,
                    formula = model1s_basic, 
                    se_type = "stata" ,
                    fixed_effects = ~STATEFIP + YEAR
)
iv_reg_basic <- iv_robust(data = data_df , 
                    formula = model_iv_basic ,
                    se_type = "stata" , 
                    fixed_effects = ~ STATEFIP + YEAR ,
                    clusters = STATEFIP
)


s1_reg_vets <- lm_robust(data = data_df ,
                          formula = model1s_vets, 
                          se_type = "stata" ,
                          fixed_effects = ~STATEFIP + YEAR
)
iv_reg_vets <- iv_robust(data = data_df , 
                          formula = model_iv_vets ,
                          se_type = "stata" , 
                          fixed_effects = ~ STATEFIP + YEAR ,
                          clusters = STATEFIP
)
                          

tidy(s1_reg_basic)
tidy(iv_reg_basic)

tidy(s1_reg_vets)
tidy(iv_reg_vets)

# suicide rate plot for proposal
y <- suicide_df %>% group_by(YEAR) %>% summarise( suicide = sum(suicide_total))
p <- cps_df %>% group_by(YEAR) %>% summarise(population = sum(population)) %>% filter( YEAR > 1998 & YEAR < 2018) %>% bind_cols(y)
p <- p %>% mutate( suicide_rate = suicide*(100000/population))

suiciderate_plot <- ggplot(data = p , aes(x = YEAR , y = suicide_rate )) + 
  geom_line( colour = 'red' , size = 1) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Suicide Rate (per 100,000 persons)") +
  ggtitle("Suicide Rate of the United States 1999-2017") +
  theme( panel.background = element_blank( ) ,
         panel.grid.major = element_line( color = 'grey' , linetype = 2) ,
         panel.grid.minor = element_line( color = 'grey' , linetype = 2)
  )

suiciderate_plot
save(suiciderate_plot, file="suicide_rate.RData")
