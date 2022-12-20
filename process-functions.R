# process-functions

statekey <- tibble(name = str_to_lower(state.name), abb = state.abb, num = c(1:8, 10:51)) %>%
    
    rows_insert(tibble(name = "district of columbia", abb = "DC", num = 9), by = "name") %>%
    
    arrange(name)

#==================================== functions ================================
`%!in%` <- Negate(`%in%`)

# 0 = no
# 1 = yes
# -1 = missing
# -2 = logical skip
basiclayer <- function(x, ...) {
  if (all(levels(as.factor(x)) == c("-2", "-1", "0", "1"))) { # 4
    factor(x, labels = c("-2" = "logical skip", "-1" = NA, "0" = "no", "1" = "yes"), ordered = TRUE) 
  } else if (all(levels(as.factor(x)) == c("-1", "0", "1"))) { # 3
    factor(x, labels = c("-1" = NA, "0" = "no", "1" = "yes"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("0", "1"))) { # 2
    factor(x, labels = c("0" = "no", "1" = "yes"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2", "0", "1"))) { # 3 without missing
    factor(x, labels = c("-2" = "logical skip", "0" = "no", "1" = "yes"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2", "0", "1", "2", "3", "4", "5", "6", 
                                           "7", "8", "9", "10", "11", "12"))) { # 14 level to 12
    factor(x, labels = c("-2" = "logical skip", "0" = "0", "1" = "1-10", "2" = "11-20", 
                         "3" = "21-30", "4" = "31-40", "5" = "41-50", "6" = "51-75", 
                         "7" = "76-100", "8" = "101-250", "9" = "251-500", 
                         "10" = "501-1000", "11" = "1001-1500", "12" = "1500+"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2","-1", "0", "1", "2", "3", "4", "5", "6", 
                                           "7", "8", "9", "10", "11", "12"))) { # 15 level
    factor(x, labels = c("-2" = "logical skip", "-1" = NA, "0" = "0", "1" = "1-10", 
                         "2" = "11-20", "3" = "21-30", "4" = "31-40", "5" = "41-50", 
                         "6" = "51-75", "7" = "76-100", "8" = "101-250", "9" = "251-500", 
                         "10" = "501-1000", "11" = "1001-1500", "12" = "1500+"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2","-1", "0", "1", "2", "3", "4", "5", "6", 
                                           "7", "8", "9", "10", "11"))) { # 14 level to  11
    factor(x, labels = c("-2" = "logical skip", "-1" = NA, "0" = "0", "1" = "1-10", 
                         "2" = "11-20", "3" = "21-30", "4" = "31-40", "5" = "41-50", 
                         "6" = "51-75", "7" = "76-100", "8" = "101-250", "9" = "251-500", 
                         "10" = "501-1000", "11" = "1001-1500"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2","-1", "0", "1", "2", "3", "4", "5", "6", 
                                           "7", "8", "9", "10"))) { # 13 level
    factor(x, labels = c("-2" = "logical skip", "-1" = NA, "0" = "0", "1" = "1-10", 
                         "2" = "11-20", "3" = "21-30", "4" = "31-40", "5" = "41-50", 
                         "6" = "51-75", "7" = "76-100", "8" = "101-250", "9" = "251-500", 
                         "10" = "501-1000"), ordered = TRUE)
  } else if (all(levels(as.factor(x)) == c("-2","-1", "0", "1", "2", "3", "4", "5", "6", 
                                           "7"))) { # pct levels
    
    factor(x, labels = c("-2" = "logical skip", "-1" = NA, "0" = "0", "1" = "1-10", 
                         "2" = "11-20", "3" = "21-30", "4" = "31-40", "5" = "41-50", 
                         "6" = "51-75", "7" = "76-100"), ordered = TRUE)
  }
  else {
    cat("Function not applicable for column", deparse(substitute(x)), "\n")
    return(x)
  }
}

clean_cdc <- function(data, goodvars) {
  
  names(data) <- sub("^_", "X_", names(data))
  
  temp <- data %>%
    
    dplyr::filter(IYEAR == 2020, X_STATE != "66" & X_STATE != "72") %>%
    
    dplyr::select(all_of(goodvars)) %>%
    
    dplyr::mutate(date = as.Date(IDATE, format = "%m%d%Y"),
                  qstlang = factor(QSTLANG, labels = c("1" = "english", "2" = "spanish", "3" = "other")),
                  sex = factor(X_SEX, labels = c("1" = "male", "2" = "female")),
                  edu = factor(X_EDUCAG, labels = c("1" = "Did not graduate High School",
                                                    "2" = "Graduated High School",
                                                    "3" = "Attended College or Technical School",
                                                    "4" = "Graduated from College or Technical School",
                                                    "9" = NA)),
                  metro = factor(X_METSTAT, labels = c("1" = "metropolitan", "2" = "non-metropolitan")),
                  urban = factor(X_URBSTAT, labels = c("1" = "urban", "2" = "rural")),
                  race = factor(X_IMPRACE, labels = c("1" = "white", "2" = "black", "3" = "asian",
                                                      "4" = "american indian", "5" = "hispanic", "6" = "other")),
                  health = factor(X_RFHLTH, labels = c("1" = "good", "2" = "poor", "9" = NA)),
                  phys14d = factor(X_PHYS14D, labels = c("1" = "0", "2" = "1-13", "3" = "14+", "9" = NA)),
                  ment14d = factor(X_MENT14D, labels = c("1" = "0", "2" = "1-13", "3" = "14+", "9" = NA)),
                  exercise = factor(EXERANY2, labels = c("1"= "yes", "2" = "no", "7" = NA, "9" = NA)),
                  genhealth = factor(GENHLTH, labels =  c("1" = "excellent", "2" = "very good", "3" = "good",
                                                          "4" = "fair", "5" = "poor", "7" = NA, "9" = NA)),
                  age = factor(X_AGE_G, labels = c("1" = "18-24", "2" = "25-34", "3" = "35-44",
                                                   "4" = "45-54", "5" = "55-64", "6" = "65+")),
                  income = factor(X_INCOMG, labels = c("1" = "<15000", "2" = "15000-24999",
                                                       "3" = "25000-34999", "4" = "35000-49999",
                                                       "5" = "50000+", "9" = NA), ordered = TRUE),
                  employed = factor(EMPLOY1, labels = c("1" = "Employed", "2" = "Self-employed",
                                                        "3" = "Out of work 1+ years", "4" = "Out of work <1 year",
                                                        "5" = "Homemaker", "6" = "Student", "7" = "Retired",
                                                        "8" = "Unable to work", "9" = NA))) %>% # add these vars to calc
    
    dplyr::select(-all_of(calc))
  
  brfss <- tibble(oldstate = unique(as.numeric(temp$X_STATE)), newstate = 1:51) %>%
    
    right_join(temp, by = c("oldstate" = "X_STATE")) %>%
    
    right_join(statekey, by = c("newstate" = "num")) %>%
    
    dplyr::select(-abb, -oldstate, -newstate) %>%
    
    dplyr::mutate(name = as.factor(name))
  
  return(brfss)
}

get_cdc <- function(fn, vars = goodvars, path = "./data/brfss.csv") {
    
    if (file.exists(path)) {
        
        data <- read_csv(path)
        
    } else {
        
        temp <- tempfile()
    
        download.file("https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip", temp)
        
        data <- read_xpt(temp)
        
        file.remove(temp)
        
        data <- 
        
        return(fn(data, vars))
        
        rm(temp, data)
    }
}
