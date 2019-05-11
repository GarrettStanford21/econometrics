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

## Insurance and Demographic Variables

asec_df <- asec_df %>% mutate( woman = ifelse(SEX==2 , 1 , 0) ,
                               prime_age = ifelse(AGE > 24 & AGE < 66, 1, 0) ,
                               private = ifelse(COVERPI==2, 1, 0) , 
                               public = ifelse(HIMCAID==2 | HIMCARE==2 | HICHAMP==2, 1, 0), 
                               medicaid = ifelse(HIMCAID==2, 1, 0) ,
                               medicare = ifelse(HIMCARE==2, 1, 0) ,
                               militcare = ifelse(HICHAMP==2, 1, 0) )

asec_df <- asec_df %>% mutate( woman_pop = woman*ASECWT ,
                               prime_age_pop = prime_age*ASECWT ,
                               private_pop = private*ASECWT , 
                               public_pop = public*ASECWT ,
                               medicaid_pop = medicaid*ASECWT ,
                               medicare_pop = medicare*ASECWT ,
                               militcare_pop = militcare*ASECWT)

## Collapsing by state/year

asec_st <- asec_df %>% group_by( STATEFIP ,  YEAR ) %>% summarise( population = sum(ASECWT) , women = sum(woman_pop) , prime_age = sum(prime_age_pop) , private = sum(private_pop) , medicare=sum(medicare_pop) ,  medicaid=sum(medicaid_pop) , militcare = sum(militcare_pop) ) %>% arrange(YEAR)
asec_st <- asec_st %>% mutate( fem_rate = women / population , 
                               prime_rate = prime_age / population , 
                               private_rate = private/ population ,
                               medicaid_rate = medicaid / population , 
                               medicare_rate = medicare / population , 
                               militcare_rate = militcare / population)

##Code Test : Plot Wisconsin medicaid
wisconsin <- asec_st %>% filter(STATEFIP == 55) %>% select(YEAR, STATEFIP, population , medicaid_rate , medicaid)
ggplot() + geom_line( data = wisconsin , mapping = aes( x = YEAR , y = medicaid ) )

write.table(x = asec_st, 
            file = "D:/Economics/Data/CPS Data/asec_st")
