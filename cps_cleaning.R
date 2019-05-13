library(pacman)
p_load( ipumsr, 
        broom ,
        haven ,
        tidyverse ,
        estimatr ,
        stargazer)

# Data Gathering and Cleaning

ddi <- read_ipums_ddi("D:/Economics/Data/CPS Data/cps_00024.xml")
data <- read_ipums_micro(ddi)


## March Basic data
#mar_df  <- data  %>% filter( ASECFLAG == 2 ) %>% select( -c(ASECWTH, ASECWT))

# Annual Social and Economic Supplement (March Supplement)

asec_df <- data  %>% mutate(HFLAG = replace_na(HFLAG, 1)) %>% filter( ASECFLAG == 1 & HFLAG ==1 ) # See https://cps.ipums.org/cps/three_eighths.shtml

## Insurance, Demographic, and employment Variables

asec_df <- asec_df %>% mutate( woman = ifelse(SEX==2 , 1 , 0) ,
                               prime_age = ifelse(AGE > 24 & AGE < 66, 1, 0) ,
                               m_older_age = ifelse(AGE > 44 & SEX==1, 1, 0) ,
                               f_mid_age = ifelse(AGE > 44 & age < 56 & SEX==2, 1, 0) ,
                               private = ifelse(COVERPI==2, 1, 0) , 
                               public = ifelse(HIMCAID==2 | HIMCARE==2 | HICHAMP==2, 1, 0), 
                               medicaid = ifelse(HIMCAID==2, 1, 0) ,
                               medicare = ifelse(HIMCARE==2, 1, 0) ,
                               militcare = ifelse(HICHAMP==2, 1, 0) ,
                               unemployed = ifelse(EMPSTAT==20|EMPSTAT==21|EMPSTAT==22, 1, 0) , 
                               combat_vet = ifelse(VETSTAT==2, 1, 0) , 
                               ilf = ifelse(LABFORCE==2, 1, 0) )

asec_df <- asec_df %>% mutate( woman_pop = woman*ASECWT ,
                               prime_age_pop = prime_age*ASECWT ,
                               m_older_age_pop = older_age*ASECWT , 
                               f_mid_age_pop = f_mid_age*ASECWT ,
                               private_pop = private*ASECWT , 
                               public_pop = public*ASECWT ,
                               medicaid_pop = medicaid*ASECWT ,
                               medicare_pop = medicare*ASECWT ,
                               militcare_pop = militcare*ASECWT ,
                               unemployed_pop = unemployed*ASECWT ,
                               vet_pop = combat_vet*ASECWT ,
                               lfp = ilf*ASECWT )

## Collapsing by state/year

asec_st <- asec_df %>% group_by( STATEFIP ,  YEAR ) %>% summarise( population = sum(ASECWT) ,
                                                                   women = sum(woman_pop) , 
                                                                   prime_age = sum(prime_age_pop) , 
                                                                   m_older_age = sum(m_older_age_pop) , 
                                                                   f_mid_age = sum(f_mid_age_pop) , 
                                                                   private = sum(private_pop) , 
                                                                   medicare=sum(medicare_pop) ,  
                                                                   medicaid=sum(medicaid_pop) , 
                                                                   militcare = sum(militcare_pop) , 
                                                                   unemployed = sum(unemployed_pop) , 
                                                                   combat_vets = sum(vet_pop) , 
                                                                   lfp = sum(lfp) ) %>% arrange(YEAR)

asec_st <- asec_st %>% mutate( fem_rate = women / population , 
                               prime_rate = prime_age / population ,
                               elder_rate = older_age / population , 
                               private_rate = private/ population ,
                               medicaid_rate = medicaid / population , 
                               medicare_rate = medicare / population , 
                               militcare_rate = militcare / population ,
                               vet_rate = combat_vets / population ,
                               lfpr = lfp / population ,
                               ue_rate = unemployed / lfp )


write.table(x = asec_st, 
            file = "D:/Economics/Data/CPS Data/asec_st")

asec_st$STATEFIP <- factor(asec_st$STATEFIP ,
                           levels = c( 
                             1	,  2	,   4	,   5	,   6	,   8	,   9	,   10	,
                             11	,   12	,   13	,   15	,   16	,   17	,   18	,
                             19	,   20	,   21	,   22	,   23	,   24	,   25	,   
                             26	,   27	,   28	,   29	,   30	,   31	,   32	,   
                             33	,   34	,   35	,   36	,   37	,   38	,   39	,
                             40	,   41	,   42	,   44	,   45	,   46	,   47  ,   
                             48	,   49	,   50	,   51	,   53	,   54	,   55	,
                             56  
                           ),
                           labels = c(
                             "Alabama"	,"Alaska"	,"Arizona"	,"Arkansas"	,"California"	,"Colorado"	,"Connecticut"	,"Delaware"	,
                             "District of Columbia"	,"Florida"	,"Georgia"	,"Hawaii"	,"Idaho"	,"Illinois"	,"Indiana"	,"Iowa"	,
                             "Kansas"	,"Kentucky"	,"Louisiana"	,"Maine"	,"Maryland"	,"Massachusetts"	,"Michigan"	,"Minnesota"	,
                             "Mississippi"	,"Missouri"	,"Montana"	,"Nebraska"	,"Nevada"	,"New Hampshire"	,"New Jersey"	,"New Mexico"	,
                             "New York"	,"North Carolina"	,"North Dakota"	,"Ohio" ,"Oklahoma"	,"Oregon"	,"Pennsylvania"	,"Rhode Island"	,
                             "South Carolina"	,"South Dakota"	,"Tennessee"	,"Texas"	,"Utah"	,"Vermont"	,"Virginia"	,"Washington"	,
                             "West Virginia"	,"Wisconsin"	,"Wyoming" 
                           )
)

upper_midwest <- asec_st %>% filter( STATEFIP == "Michigan" | STATEFIP == "Minnesota" | STATEFIP == "Wisconsin" ) %>% select(YEAR, STATEFIP, population , medicaid_rate , medicaid )


ggplot( data = upper_midwest , aes( x = YEAR , y = population , color=STATEFIP) ) + 
  geom_line(aes( group= STATEFIP ) , 
            size=2 ) +
  scale_x_continuous( name = 'Year' ) +
  scale_y_continuous( name = 'Population') +
  ggtitle("Population Over Time in the Upper Midwest") +
  theme( panel.background = element_blank() ,
         panel.grid.major = element_line(color = 'grey' , linetype = 2) , 
         panel.grid.minor = element_line(color = 'grey' , linetype = 2) ,
         legend.title = element_blank()
  )
ggplot( data = upper_midwest , aes( x = YEAR , y = medicaid , color=STATEFIP) ) + 
  geom_line(aes( group= STATEFIP ) ,
            size=2) +
  scale_x_continuous( name = 'Year' ) +
  scale_y_continuous( name = 'Population Insured by Medicaid') +
  ggtitle("Medicaid-Covered Population Over Time in the Upper Midwest") +
  theme( panel.background = element_blank() ,
         panel.grid.major = element_line(color = 'grey' , linetype = 2) , 
         panel.grid.minor = element_line(color = 'grey' , linetype = 2) ,
         legend.title = element_blank()
  )
ggplot( data = upper_midwest , aes( x = YEAR , y = medicaid_rate , color=STATEFIP) ) + 
  geom_line(aes( group= STATEFIP ) ,
            size=2) +
  scale_x_continuous( name = 'Year' ) +
  scale_y_continuous( name = 'Medicaid Coverage Rate') +
  ggtitle("Medicaid Coverage Rates Over Time in the Upper Midwest") +
  theme( panel.background = element_blank() ,
         panel.grid.major = element_line(color = 'grey' , linetype = 2) , 
         panel.grid.minor = element_line(color = 'grey' , linetype = 2) ,
         legend.title = element_blank()
  )
