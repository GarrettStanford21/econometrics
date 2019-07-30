library(pacman)
p_load( ipumsr, 
        broom ,
        haven ,
        dplyr , 
        furrr ,
        purrr , 
        tidyverse ,
        estimatr ,
        tictoc )

# Data Gathering and Cleaning

setwd("D:/Economics/Data/CPS")
ddi <- read_ipums_ddi("cps_00024.xml")
data <- read_ipums_micro(ddi)


## March Basic data
#mar_df  <- data  %>% filter( ASECFLAG == 2 ) %>% select( -c(ASECWTH, ASECWT))

# Annual Social and Economic Supplement (March Supplement)

asec_df = data %>% mutate(HFLAG = replace_na(HFLAG, 1)) %>% filter( ASECFLAG == 1 & HFLAG ==1 ) # See https://cps.ipums.org/cps/three_eighths.shtml

## Insurance, Demographic, and employment Variables

asec_df <- asec_df %>% mutate( i_woman = ifelse(SEX==2 , 1 , 0) ,
                               i_prime_age = ifelse(AGE > 24 & AGE < 66, 1, 0) ,
                               i_m_older_age = ifelse(AGE > 44 & SEX==1, 1, 0) ,
                               i_f_mid_age = ifelse(AGE > 44 & AGE < 56 & SEX==2, 1, 0) ,
                               i_private = ifelse(COVERPI==2, 1, 0) , 
                               i_public = ifelse(HIMCAID==2 | HIMCARE==2 | HICHAMP==2, 1, 0), 
                               i_medicaid = ifelse(HIMCAID==2, 1, 0) ,
                               i_medicare = ifelse(HIMCARE==2, 1, 0) ,
                               i_militcare = ifelse(HICHAMP==2, 1, 0) , 
                               i_combat_vet = ifelse(VETSTAT==2, 1, 0) , 
                               i_unemployed = ifelse(EMPSTAT==20|EMPSTAT==21|EMPSTAT==22, 1, 0) ,
                               i_ilf = ifelse(LABFORCE==2, 1, 0) ,
                               i_emp = ifelse(LABFORCE>0 , 1 , 0) ,
                               i_vet = ifelse(VETSTAT>0 , 1 , 0) ,
                               i_mcare = ifelse(HIMCARE>0 , 1 , 0) )


## Collapsing by state/year

collapse_function_pay = function( i ){
  pay_df_i = asec_df %>% filter(YEAR == i , INCWAGE < 9999998 ) %>% group_by(STATEFIP , YEAR ) %>% summarise( pay_avg = weighted.mean(INCWAGE , w = ASECWT) )
}
collapse_function_hrs = function( i ){
  hrs_df_i = asec_df %>% filter(YEAR == i , UHRSWORKLY < 999 ) %>% group_by(STATEFIP , YEAR ) %>% summarise( hrs_avg = weighted.mean(UHRSWORKLY , w = ASECWT) )
  }
collapse_function_lf1 = function(i){
  asec_lf1_i = asec_df %>% filter(AGE>13 & YEAR==i) %>% group_by( STATEFIP , YEAR ) %>% summarise( ilf = weighted.mean(LABFORCE==2 , w = ASECWT ),
                                                                                                   unemployment = weighted.mean(EMPSTAT==20|EMPSTAT==21|EMPSTAT==22 , w = ASECWT )
  )
}
collapse_function_lf = function(i){
  asec_lf_i = asec_df %>% filter(AGE>14 & YEAR==i) %>% group_by( STATEFIP , YEAR ) %>% summarise( ilf = weighted.mean(LABFORCE==2 , w = ASECWT ),
                                                                                                  unemployment = weighted.mean(EMPSTAT==20|EMPSTAT==21|EMPSTAT==22 , w = ASECWT )
  ) 
}
collapse_function_vet1 = function(i){
  asec_vet1_i = asec_df %>% filter(AGE>14 & YEAR==i) %>% group_by( STATEFIP , YEAR ) %>% summarise( vet = weighted.mean( VETSTAT==2 , w = ASECWT ))
  
}
collapse_function_vet = function(i){
  asec_vet_i = asec_df %>% filter(AGE>16 & YEAR==i) %>% group_by( STATEFIP , YEAR ) %>% summarise( vet = weighted.mean( VETSTAT==2 , w = ASECWT ))
  
}
collapse_function_care = function(i){
  asec_care_i = asec_df %>% filter(AGE>14 & YEAR==i) %>% group_by( STATEFIP , YEAR ) %>% summarise( medicare = weighted.mean(HIMCARE==2 , w = ASECWT ) )
  
}
collapse_function_pop = function(i){
  asec_pop_i = asec_df %>% filter(YEAR==i) %>% 
    group_by( STATEFIP ,  YEAR ) %>% summarise( population = sum(ASECWT) ,
                                                women = weighted.mean(SEX==2 , w = ASECWT) , 
                                                prime_age_pop = weighted.mean(AGE > 24 & AGE < 66 , w = ASECWT) , 
                                                m_older_age_pop = weighted.mean(AGE > 44 & SEX==1 , w = ASECWT) , 
                                                f_mid_age = weighted.mean(AGE > 44 & AGE < 56 & SEX==2 , w = ASECWT) , 
                                                private = weighted.mean(COVERPI==2 , w = ASECWT) ,
                                                medicaid = weighted.mean(HIMCAID==2, w = ASECWT)  ,
                                                militcare = weighted.mean(HICHAMP==2 , w = ASECWT ) 
    )
}

tic()
pay_df = map_dfr(1977:2018 , collapse_function_pay )
hrs_df = map_dfr(1977:2018 , collapse_function_hrs )
test_df = pay_df %>% bind_cols( hrs_df ) %>% select( -STATEFIP1 , -YEAR1)
asec_lf1 = map_dfr(1977:1989 , collapse_function_lf1 )
asec_lf = map_dfr(1990:2018 , collapse_function_lf) %>% bind_rows(asec_lf1) %>% arrange(YEAR , .by_group = TRUE )
asec_vet1 = map_dfr(1977:2005 , collapse_function_vet1)
asec_vet = map_dfr(2006:2018 , collapse_function_vet) %>% bind_rows(asec_vet1) %>% arrange(YEAR , .by_group = TRUE )
asec_care = map_dfr(1977:2018 , collapse_function_care)
asec_pop = map_dfr(1977:2018 , collapse_function_pop )
asec_st = asec_pop %>% bind_cols( asec_care , test_df , asec_vet , asec_lf ) %>% 
  select( -STATEFIP1 , -STATEFIP2 , -STATEFIP3 , -STATEFIP4 , 
          -YEAR1 , -YEAR2 , -YEAR3 , -YEAR4)
toc()

write.table(x = asec_st, 
            file = "D:/Economics/Data/CPS/asec_st")

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
