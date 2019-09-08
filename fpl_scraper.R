library( pacman )
p_load( haven , 
        tidyverse , 
        dplyr , 
        rvest ,
        readr
        )

fpl_page = read_html("https://aspe.hhs.gov/prior-hhs-poverty-guidelines-and-federal-register-references")
fpl_tbl = fpl_page %>% 
  html_node("#content > div.content.clearfix.node-site-page > table") %>% 
  html_table( fill = TRUE) %>% 
  select(1:3) %>%
  slice( -39 ) %>% 
  rename( year = Year )

names(fpl_tbl)[2] <- "fpl_fp"
names(fpl_tbl)[3] <- "fpl_addp"
fpl_tbl$fpl_addp[4] = fpl_tbl$fpl_addp[5]

fpl_tbl = fpl_tbl %>%
  mutate( year = as.numeric(parse_number(year)) , 
          fpl_fpl = as.numeric(parse_number(fpl_fp)) , 
          fpl_addp = as.numeric(parse_number(fpl_addp)) 
          )

saveRDS(fpl_tbl , "fed_poverty_levels.rds")