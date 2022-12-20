## nmhss1 only has 3 age groups "OPAGETOT017", "OPAGETOT1864", "OPAGETOT65"
## brfss has 6, but only fall into 2 of nmhss1 groups. 
## So we'll only be looking at 18-65 vs 65+ :(

## nmhss1 has 50 states, DC, and "other"
## brfss has 50 states, DC,  plus PR and GUAM

## nmhss1 contains 6 race categories including unkown, counts hispanic as ethnicity
## brfss has 6 categories including unkown and hispanic
## gonna have to discount it :(

# age, state, and race

require(dplyr)
require(forcats)
require(readxl)
require(tidyr)
require(purrr)
require(rlang)

## important vignette https://dplyr.tidyverse.org/articles/programming.html
## http://jonthegeek.com/2018/06/04/writing-custom-tidyverse-functions/

####################################

# slicer

drillb <- function(...) {
    
    quos <- enquos(...)
        
    brfss %>%
      
        dplyr::select(!!!quos, state, X_LLCPWT) %>%
        
        dplyr::group_by(!!!quos, state) %>%
        
        dplyr::count(wt = X_LLCPWT, .drop = FALSE, name = "count")
}


drilln <- function(...) {
  
    quos <- enquos(...)
    
    nmhss %>%
      
        dplyr::select(!!!quos, state) %>%
        
        dplyr::group_by(!!!quos, state) %>%
        
        dplyr::count(.drop = FALSE, name = "count")
    }

# 
# b1 <- brfss %>%
#   dplyr::select(-date, -PHYSHLTH, -MENTHLTH, -POORHLTH, -health, -HTM4, -WTKG3) %>%
#   dplyr::mutate(age = fct_collapse(age, "18-64" = c("18-24", "25-34", "35-44", "45-54", "55-64"), 
#                                    "65+" = c("65+"))) %>%
#   na.omit()
#   #pivot_wider(across(is.factor), id_cols = name)
# 
# ################ nmhss
# # list of #yes# vars using purrr
# yvar <- nmhss1 %>%
#   dplyr::select(where(~ "yes" %in% levels(.))) %>%
#   names()
# 
# under <- function(x) {
#   str_extract(x, pattern = "[0-9]*")
# }
# 
# lvar <- nmhss1 %>%
#   dplyr::select(where(~ "1-10" %in% levels(.))) %>%
#   names()
# 
# ### for state
# n1 <- nmhss1 %>%
#   
#   #### yessing but needs to be done with 1 row per facility
#   group_by(name) %>%
#   mutate(across(all_of(yvar), .fns = list(count = ~sum(. == "yes"))),
#          TOT = n(),
#          across(all_of(lvar), .fns = list(lbound = ~sum(as.numeric(under(.x)), na.rm = TRUE)))) %>%
#   dplyr::select(-all_of(yvar), -all_of(lvar), -facnum, -OTHFAC)
# 
# fvars <- c("facilitytype", "ownership", "focus", "operator")
# 
# n2 <- tibble(name = statekey$name)
# 
# #for (i in fvars) {
# #  n2 <- n1 %>%
# #    group_by(name) %>%
# #    dplyr::count(.data{{i}}, .drop = TRUE) # some weird naming convention?
# #    pivot_wider(names_from = paste0("\"", {{i}}, "\""), values_from = n, id_cols = name)
# #    left_join(n2, by = "name")
# #}
# 
# factype <- n1 %>%
#   group_by(name) %>%
#   dplyr::count(facilitytype, .drop = TRUE) %>%
#   pivot_wider(names_from = facilitytype, values_from = n)
# 
# ownship <- n1 %>%
#   group_by(name) %>%
#   dplyr::count(ownership, .drop = TRUE) %>%
#   pivot_wider(names_from = ownership, values_from = n)
# 
# focus <- n1 %>%
#   group_by(name) %>%
#   dplyr::count(focus, .drop = TRUE) %>%
#   pivot_wider(names_from = focus, values_from = n)
# 
# operator <- n1 %>%
#   group_by(name) %>%
#   dplyr::count(operator, .drop = TRUE) %>%
#   pivot_wider(names_from = operator, values_from = n)
# 
# n2 <- left_join(n1, factype) %>%
#   left_join(ownship) %>%
#   left_join(focus) %>%
#   left_join(operator) %>%
#   dplyr::select(-all_of(fvars)) %>%
#   distinct()
