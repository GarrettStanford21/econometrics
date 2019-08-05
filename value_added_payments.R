library(pacman)
p_load( haven , 
        openxlsx ,
        dplyr , 
        tidyr ,
        reshape ,
        ggplot2
        )

setwd("D:/Economics/Data/Production Data/")
factor_pay = read.xlsx( xlsxFile = "valadded.xlsx" , 
                      sheet = "payments")

payments = reshape(factor_pay , 
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

payments$category = factor( payments$category , 
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
payments$factor = factor( payments$factor , 
                      levels = c(1:4) , 
                      labels = c(
                              "Total" ,
                              "Compensation of employees" ,
                              "Taxes on production and imports less subsidies" ,
                              "Gross operating surplus" 
                      )
)

setwd("D:/Economics/Projects/econometrics")
saveRDS(payments , "payments.rds")

payments_totals = payments %>% 
        filter( factor == "Total" )
payments_wages = payments %>% 
        filter( factor == "Compensation of employees" )
payments_profit = payments %>% 
        filter( factor == "Gross operating surplus" )
payments_tax = payments %>%
        filter( factor == "Taxes on production and imports less subsidies")

payments_shares = bind_cols( year = payments_totals$year ,
                             category = payments_totals$category ,
                             total.share = payments_totals$spending / payments_totals$spending ,
                             lab.share = payments_wages$spending / payments_totals$spending ,
                             tax.share = payments_tax$spending / payments_totals$spending ,
                             profit.share = payments_profit$spending / payments_totals$spending) %>%
        reshape( varying = 3:6 ,
                 direction = "long" ,
                 v.names = "share" ,
                 timevar = "factor"
                 ) %>% select( -id ) %>% arrange( year , category , factor )

payments_shares$factor = factor( payments_shares$factor , 
                          levels = c(1:4) , 
                          labels = c(
                                  "Total" ,
                                  "Compensation of employees" ,
                                  "Taxes on production and imports less subsidies" ,
                                  "Gross operating surplus" 
                          )
)

payment_df  = payments %>% bind_cols(payments_shares %>% select(share))

ggplot( data = payment_df %>% 
                filter( category == "Construction" ,
                        factor != "Total" ) ) +
        geom_line( aes( x = year , 
                        y = share ,
                        color = factor
                        )
        ) +
        scale_y_continuous(name= "Pct. of Value Added") +
        ggtitle("Construction")

ggplot( data = payment_df %>% 
                filter( category == "Construction") ) +
        geom_line( aes( x = year , 
                        y = spending ,
                        color = factor
        )
        ) +
        ggtitle("Construction") +
        scale_y_continuous(name="$ spent")
