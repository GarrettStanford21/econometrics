library(pacman)
p_load( haven , 
        openxlsx ,
        dplyr , 
        tidyr ,
        reshape ,
        ggplot2
        )

setwd("D:/Economics/Data/Production Data/")

payments_xlsx = read.xlsx( xlsxFile = "valadded.xlsx" , 
                      sheet = "payments")

real_payments_xlsx  = read.xlsx( xlsxFile = "valadded.xlsx" , 
                             sheet = "real")


payments_df = reshape(payments_xlsx , 
                   direction = "long" ,
                   v.names = "spending" ,
                   timevar = "year" ,
                   varying = 2:22
) %>%
        separate( col = id ,
                  into = c( "category" ,
                            "factor"
                  )
        ) %>% 
        mutate( year = year + 1996 ,
                spending = as.numeric(spending)
        ) 

real_payments_df = reshape(real_payments_xlsx , 
                   direction = "long" ,
                   v.names = "real.spending" ,
                   timevar = "year" ,
                   varying = 2:23
) %>% 
        mutate( year = year + 1996 ,
                real.spending = as.numeric(real.spending)
        ) %>%
        dplyr::rename( category = id )

payments_df$category = factor( payments_df$category , 
                      levels = c(1:97) , 
                      labels = c(
                              "Gross domestic product" ,
                              "Private industries" ,
                              "Agriculture, forestry, fishing, and hunting" ,
                              "Farms" ,
                              "Forestry, fishing, and related activities" ,
                              "Mining" ,
                              "Oil and gas extraction" ,
                              "Mining, except oil and gas" ,
                              "Support activities for mining" ,
                              "Utilities" ,
                              "Construction" ,
                              "Manufacturing" ,
                              "Durable goods" ,
                              "Wood products" ,
                              "Nonmetallic mineral products" ,
                              "Primary metals" ,
                              "Fabricated metal products" ,
                              "Machinery" ,
                              "Computer and electronic products" ,
                              "Electrical equipment, appliances, and components" ,
                              "Motor vehicles, bodies and trailers, and parts" ,
                              "Other transportation equipment" ,
                              "Furniture and related products" ,
                              "Miscellaneous manufacturing" ,
                              "Nondurable goods" ,
                              "Food and beverage and tobacco products" ,
                              "Textile mills and textile product mills", 
                              "Apparel and leather and allied products",
                              "Paper products",
                              "Printing and related support activities",
                              "Petroleum and coal products",
                              "Chemical products",
                              "Plastics and rubber products",
                              "Wholesale trade",
                              "Retail trade",
                              "Motor vehicle and parts dealers",
                              "Food and beverage stores",
                              "General merchandise stores",
                              "Other retail",
                              "Transportation and warehousing",
                              "Air transportation",
                              "Rail transportation",
                              "Water transportation",
                              "Truck transportation",
                              "Transit and ground passenger transportation",
                              "Pipeline transportation",
                              "Other transportation and support activities",
                              "Warehousing and storage",
                              "Information",
                              "Publishing industries, except internet (includes software)",
                              "Motion picture and sound recording industries",
                              "Broadcasting and telecommunications",
                              "Data processing, internet publishing, and other information services",
                              "Finance, insurance, real estate, rental, and leasing",
                              "Finance and insurance",
                              "Federal Reserve banks, credit intermediation, and related activities",
                              "Securities, commodity contracts, and investments",
                              "Insurance carriers and related activities",
                              "Funds, trusts, and other financial vehicles",
                              "Real estate and rental and leasing",
                              "Real estate",
                              "Housing",
                              "Other real estate",
                              "Rental and leasing services and lessors of intangible assets",
                              "Professional and business services",
                              "Professional, scientific, and technical services",
                              "Legal services",
                              "Computer systems design and related services",
                              "Miscellaneous professional, scientific, and technical services",
                              "Management of companies and enterprises",
                              "Administrative and waste management services",
                              "Administrative and support services",
                              "Waste management and remediation services",
                              "Educational services, health care, and social assistance",
                              "Educational services",
                              "Health care and social assistance",
                              "Ambulatory health care services",
                              "Hospitals",
                              "Nursing and residential care facilities",
                              "Social assistance",
                              "Arts, entertainment, recreation, accommodation, and food services",
                              "Arts, entertainment, and recreation",
                              "Performing arts, spectator sports, museums, and related activities",
                              "Amusements, gambling, and recreation industries",
                              "Accommodation and food services" ,
                              "Accommodation" ,
                              "Food services and drinking places" ,
                              "Other services, except government" ,
                              "Government" ,
                              "Federal" ,
                              "General government" ,
                              "National defense" ,
                              "Nondefense" ,
                              "Government enterprises" ,
                              "State and local" ,
                              "General government" ,
                              "Government enterprises" 
                      )
                      )

real_payments_df$category = factor( real_payments_df$category , 
                            levels = c(1:97) , 
                            labels = c(
                                    "Gross domestic product" ,
                                    "Private industries" ,
                                    "Agriculture, forestry, fishing, and hunting" ,
                                    "Farms" ,
                                    "Forestry, fishing, and related activities" ,
                                    "Mining" ,
                                    "Oil and gas extraction" ,
                                    "Mining, except oil and gas" ,
                                    "Support activities for mining" ,
                                    "Utilities" ,
                                    "Construction" ,
                                    "Manufacturing" ,
                                    "Durable goods" ,
                                    "Wood products" ,
                                    "Nonmetallic mineral products" ,
                                    "Primary metals" ,
                                    "Fabricated metal products" ,
                                    "Machinery" ,
                                    "Computer and electronic products" ,
                                    "Electrical equipment, appliances, and components" ,
                                    "Motor vehicles, bodies and trailers, and parts" ,
                                    "Other transportation equipment" ,
                                    "Furniture and related products" ,
                                    "Miscellaneous manufacturing" ,
                                    "Nondurable goods" ,
                                    "Food and beverage and tobacco products" ,
                                    "Textile mills and textile product mills", 
                                    "Apparel and leather and allied products",
                                    "Paper products",
                                    "Printing and related support activities",
                                    "Petroleum and coal products",
                                    "Chemical products",
                                    "Plastics and rubber products",
                                    "Wholesale trade",
                                    "Retail trade",
                                    "Motor vehicle and parts dealers",
                                    "Food and beverage stores",
                                    "General merchandise stores",
                                    "Other retail",
                                    "Transportation and warehousing",
                                    "Air transportation",
                                    "Rail transportation",
                                    "Water transportation",
                                    "Truck transportation",
                                    "Transit and ground passenger transportation",
                                    "Pipeline transportation",
                                    "Other transportation and support activities",
                                    "Warehousing and storage",
                                    "Information",
                                    "Publishing industries, except internet (includes software)",
                                    "Motion picture and sound recording industries",
                                    "Broadcasting and telecommunications",
                                    "Data processing, internet publishing, and other information services",
                                    "Finance, insurance, real estate, rental, and leasing",
                                    "Finance and insurance",
                                    "Federal Reserve banks, credit intermediation, and related activities",
                                    "Securities, commodity contracts, and investments",
                                    "Insurance carriers and related activities",
                                    "Funds, trusts, and other financial vehicles",
                                    "Real estate and rental and leasing",
                                    "Real estate",
                                    "Housing",
                                    "Other real estate",
                                    "Rental and leasing services and lessors of intangible assets",
                                    "Professional and business services",
                                    "Professional, scientific, and technical services",
                                    "Legal services",
                                    "Computer systems design and related services",
                                    "Miscellaneous professional, scientific, and technical services",
                                    "Management of companies and enterprises",
                                    "Administrative and waste management services",
                                    "Administrative and support services",
                                    "Waste management and remediation services",
                                    "Educational services, health care, and social assistance",
                                    "Educational services",
                                    "Health care and social assistance",
                                    "Ambulatory health care services",
                                    "Hospitals",
                                    "Nursing and residential care facilities",
                                    "Social assistance",
                                    "Arts, entertainment, recreation, accommodation, and food services",
                                    "Arts, entertainment, and recreation",
                                    "Performing arts, spectator sports, museums, and related activities",
                                    "Amusements, gambling, and recreation industries",
                                    "Accommodation and food services" ,
                                    "Accommodation" ,
                                    "Food services and drinking places" ,
                                    "Other services, except government" ,
                                    "Government" ,
                                    "Federal" ,
                                    "General government" ,
                                    "National defense" ,
                                    "Nondefense" ,
                                    "Government enterprises" ,
                                    "State and local" ,
                                    "General government" ,
                                    "Government enterprises" 
                            )
)

payments_df$factor = factor( payments_df$factor , 
                      levels = c(1:4) , 
                      labels = c(
                              "Total" ,
                              "Compensation of employees" ,
                              "Taxes on production and imports less subsidies" ,
                              "Gross operating surplus" 
                      )
)

setwd("D:/Economics/Projects/econometrics")

saveRDS(payments_df , "payments.rds")
saveRDS(real_payments_df , "real_payments.rds")

total_payments_df = payments_df %>% 
        filter( factor == "Total" )
wage_payments_df = payments_df %>% 
        filter( factor == "Compensation of employees" )
profit_payments_df = payments_df %>% 
        filter( factor == "Gross operating surplus" )
tax_payments_df = payments_df %>%
        filter( factor == "Taxes on production and imports less subsidies")

payments_share_df = bind_cols( year = total_payments_df$year ,
                             category = total_payments_df$category ,
                             total.share = total_payments_df$spending / total_payments_df$spending ,
                             lab.share = wage_payments_df$spending / total_payments_df$spending ,
                             tax.share = tax_payments_df$spending / total_payments_df$spending ,
                             profit.share = profit_payments_df$spending / total_payments_df$spending) %>%
        reshape( varying = 3:6 ,
                 direction = "long" ,
                 v.names = "share" ,
                 timevar = "factor"
                 ) %>% select( -id ) %>% arrange( year , category , factor )

payments_share_df$factor = factor( payments_share_df$factor , 
                          levels = c(1:4) , 
                          labels = c(
                                  "Total" ,
                                  "Compensation of employees" ,
                                  "Taxes on production and imports less subsidies" ,
                                  "Gross operating surplus" 
                          )
)

payments_df  = payments_df %>% 
        bind_cols(payments_share_df %>% 
                          select(share))
real_payments_df = real_payments_df %>%
        filter( year < 2018 ) %>%
        bind_cols(payments_df %>% 
                          filter( factor == "Total" ) %>% select( spending ) )  %>%
        dplyr::rename( nominal.spending = spending ) %>%
        reshape( varying = 3:4 ,
                 direction = "long" , 
                 v.names = "payments" ,
                 timevar = "spending" 
        )

real_payments_df = real_payments_df %>% dplyr::rename( cpi = spending ) %>% select( -id )
real_payments_df$cpi = factor( real_payments_df$cpi , 
                               levels = c( 1,2 ) ,
                               labels = c( "Real" , 
                                           "Nominal" 
                                           )
                               )

saveRDS( payments_df , "payments_and_VAshares.rds")
saveRDS( real_payments_df , "real_payments.rds")

ggplot( data = payments_df %>% 
                filter( category == "Wholesale trade" ,
                        factor != "Total" ) ) +
        geom_line( aes( x = year , 
                        y = share ,
                        color = factor
                        )
        ) +
        scale_y_continuous(name= "Pct. of Value Added") +
        ggtitle("Wholesale trade")

ggplot( ) +
        geom_line( data = payments_df %>% 
                           filter( category == "Wholesale trade") ,
                   aes( x = year , 
                        y = spending ,
                        color = factor
        )
        ) +
        geom_line( data = real_payments_df %>% 
                           filter( category == "Wholesale trade" , 
                                   cpi == "Real" ) ,
                   aes( x = year , 
                        y = payments ,
                        color = cpi
                   )
        ) +
        ggtitle("Wholesale trade") +
        scale_y_continuous(name="$(Millions) spent")
