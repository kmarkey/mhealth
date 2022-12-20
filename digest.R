# cleaning and eda for cdc

require(readr)
require(dplyr)
require(stringr)
require(here)
library(rio)
library(haven)
library(here)

here()

# setwd("LocalRStudio/mhealth352")

`%!in%` <- Negate(`%in%`)

#=========================== population and states =============================

statekey <- tibble(state = str_to_lower(state.name), abb = state.abb, num = c(1:8, 10:51)) %>%
  
  rows_insert(tibble(state = "district of columbia", abb = "DC", num = 9), by = "state") %>%
  
  arrange(state)

pop <- read_csv("./data/census2020pop.csv") %>%
    
    dplyr::mutate(state = tolower(NAME)) %>%
    
    right_join(statekey, by = "state") %>%
    
    summarise(state, abb, num, pop = ESTIMATESBASE2020, pct = pop / sum(ESTIMATESBASE2020))

#===========================  read in CDC  data ================================

# FNS

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
        
    } else {
        
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
                        mscode = factor(MSCODE, labels = c("1" = "Center of an MSA", "2" = "Outside city, inside county",
                                                           "3" = "Inside suburban county", "5" = "Not in MSA")),
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
                                                              "8" = "Unable to work", "9" = NA)),
                        children = ifelse(CHILDREN == "88", 0, ifelse(as.numeric(CHILDREN) > 88, 0, as.numeric(CHILDREN))),
                        marital = factor(MARITAL, labels = c("1" = "Married", "2" = "Divorced", "3" = "Widowed",
                                                             "4" = "Separated", "5" = "Never Married", 
                                                             "6" = "Unmarried couple", "9" = NA)),
                        checkup = factor(CHECKUP1, labels = c("1" = "<1", "2" = "1-2", "3" = "2-5", "4" = "5+", 
                                                              "7" = NA, "8" = "inf", "9" = NA)),
                        medcost = factor(MEDCOST, labels = c("1" = "yes", "2" = "no", "7" = NA, "9" = NA)),
                        persdoc2 = factor(PERSDOC2, labels = c("1" = "Only one", "2" = "More than one", "3" = "No",
                                                               "7" = NA, "9" = NA)),
                        hlthplan = factor(HLTHPLN1, labels = c("1" = "yes", "2" = "no", "7" = NA, "9" = NA))) %>% # add these vars to calc
        
        dplyr::select(-all_of(calc))
    
    brfss <- tibble(oldstate = unique(as.numeric(temp$X_STATE)), newstate = 1:51) %>%
        
        right_join(temp, by = c("oldstate" = "X_STATE")) %>%
        
        right_join(statekey, by = c("newstate" = "num")) %>%
        
        dplyr::select(-abb, -oldstate, -newstate) %>%
        
        dplyr::mutate(state = as.factor(state))
        
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
        
        data <- fn(data, vars)
        
        write_csv(data, path)
        
        return(data)
        
        rm(temp, data)
    }
}

# data dictionary
# https://www.cdc.gov/brfss/annual_data/2020/pdf/codebook20_llcp-v2-508.pdf
# large df downloadable from https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip

# filter out redundant vars
# remove bad vars
# decode some vars
# report decisions

#  important vars

idvar <- c("X_STATE", "IDATE", "QSTLANG", "X_METSTAT",
           "X_URBSTAT", "X_IMPRACE", "X_RFHLTH",
           "X_PHYS14D", "X_MENT14D", "X_SEX", "X_AGE_G",
           "HTM4", "WTKG3", "X_BMI5", "X_EDUCAG", "X_INCOMG", "X_LLCPWT")

cdchealthvars <- c("GENHLTH", "PHYSHLTH", "MENTHLTH", "POORHLTH", "EMPLOY1", "EXERANY2")

othervars <- c("HLTHPLN1", 'PERSDOC2', "MEDCOST", "CHECKUP1", "MARITAL", "CHILDREN", "MENTHLTH",
               "PHYSHLTH", "GENHLTH", "MSCODE")

# not included

calc <- c("IDATE", "QSTLANG", "X_SEX", "X_EDUCAG", "X_URBSTAT", "X_METSTAT", "X_IMPRACE",
          "X_RFHLTH", "X_PHYS14D", "X_MENT14D", "EXERANY2", "GENHLTH", "X_AGE_G", "X_INCOMG", "EMPLOY1", 
          "CHILDREN", "MARITAL", "CHECKUP1", "MEDCOST", "PERSDOC2", "HLTHPLN1", "MSCODE")

goodvars <- c(idvar, cdchealthvars, othervars)

brfss <- get_cdc(clean_cdc, goodvars)

#============================== nmhss ==========================================

# dictionary: https://www.datafiles.samhsa.gov/sites/default/files/field-uploads-protected/studies/N-MHSS-2020/N-MHSS-2020-datasets/N-MHSS-2020-DS0001/N-MHSS-2020-DS0001-info/N-MHSS-2020-DS0001-info-codebook.pdf

nmhss <- read_csv("./data/nmhss-puf-2020-csv.csv")

# Group some vars

# Important vars

ndem <- c("LST", "MHINTAKE", "MHDIAGEVAL", "TREATMT", "FACILITYTYPE", "FOCUS", "OWNERSHP", 
         "PUBLICAGENCY", "RELIG", "NOTREAT", "ANTIPSYCH", "MHCASEMGMT", "MHCHRONIC", "PRIMARYCARE",
         "DIETEXERCOUNSEL", "MHEDUCATION", "MHNOSVCS", "SRVC31", "NOSPECGRP", "LANG", "ADOPTSECLUSION",
         "PAYASST", "LICENMH", "OTHFAC", "FACNUM", "IPSERV", "IPTOTAL")

# Taken in only April 2020

apr2020 <- c("OPSERV", "OPTOTAL", "OPSEXTOTM", "OPSEXTOTF", "OPAGETOT017", "OPAGETOT1864", "OPAGETOT65",
             "OPETHTOTHISP", "OPETHTOTNONHISP", "OPETHTOTUNK", "OPRACETOTINDIAN", "OPRACETOTASIAN", 
             "OPRACETOTINDIAN", "OPRACETOTBLK", "OPRACETOTHAWPAC", "OPRACETOTWHIT", "OPRACETOTUNK",
             "OPLEGALTOTVOL", "OPLEGALTOTNONFOREN", "OPLEGALTOTFOREN", "COD_PCT", "TOTADMIS", 
             "PERCENTVA")

# About treatment types and options

treat <- c("TREATMT", "TREATPSYCHOTHRPY", "TREATFAMTHRPY", "TREATGRPTHRPY", "TREATCOGTHRPY", 
           "TREATDIALTHRPY", "TREATCOGREM", "TREATBEHAVMOD", "TREATDUALMHSA", "TREATTRAUMATHRPY",
           "TREATACTVTYTHRPY","TREATELECTRO", "TREATTMS", "TREATKIT", "TREATEMDR", "TREATTELEMEDINCE",
           "TREATOTH", "ASSERTCOMM", "SED", "TAYOUNGADULTS", "SPMI", "SRVC63", "ALZHDEMENTIA", 
           "SRVC31", "SPECGRPEATING", "FIRSTEPPSYCH", "SRVC122", "POSTTRAUM", "SRVC116", 
           "TRAUMATICBRAIN", "SRVC113", "SRVC114", "SRVC115", "SRVC62", "SRVC61", "SRVC32", "SRVC35",
           "NOSPECGRP")

# Facility drug treatment

drug <- c("ANTIPSYCH", "CHLOR_NO", "CHLOR_ORAL", "CHLOR_INJ", "CHLOR_REC", "DROPE_NO", 
          "DROPE_INJ", "FLUPH_NO", "FLUPH_ORAL", "FLUPH_INJ", "FLUPH_INJ", "HALOP_NO", 
          "HALOP_ORAL", "HALOP_INJ", "HALOP_LAI", "LOXAP_NO", "LOXAP_ORAL", "LOXAP_INJ", 
          "PERPH_NO", "PERPH_NO", "PERPH_ORAL", "PERPH_INJ", "PIMOZ_NO", "PIMOZ_ORAL", 
          "PIMOZ_TOP", "PROCH_NO", "PROCH_ORAL", "PROCH_INJ", "PROCH_REC", "THIOT_NO", 
          "THIOT_ORAL", "THIOT_INJ", "THIOR_NO", "THIOR_ORAL", "TRIFL_NO", "TRIFL_ORAL",
          "TRIFL_INJ", "FSTGENOTH_NO", "FSTGENOTH_ORAL", "FSTGENOTH_INJ", "FSTGENOTH_LAI",
          "FSTGENOTH_REC", "FSTGENOTH_TOP", "ARIPI_NO", "ARIPI_ORAL", "ARIPI_INJ", "ARIPI_LAI",
          "ASENA_NO", "ASENA_ORAL", "ASENA_INJ", "BREXP_NO", "BREXP_ORAL", "CARIP_NO",
          "CARIP_ORAL", "CLOZA_NO", "CLOZA_ORAL", "ILOPE_NO", "ILOPE_ORAL", "LURAS_NO", 
          "LURAS_ORAL", "OLANZ_NO", "OLANZ_ORAL", "OLANZ_INJ", "OLANZ_LAI", "OLANZFLU_NO",
          "OLANZFLU_ORAL", "PALIP_NO", "PALIP_ORAL", "PALIP_INJ", "PALIP_LAI", "QUETI_NO",
          "QUETI_ORAL", "RISPE_NO", "RISPE_ORAL", "RISPE_INJ", "RISPE_LAI", "ZIPRA_NO", 
          "ZIPRA_ORAL", "ZIPRA_INJ", "SECGEN1OTH_NO", "SECGEN1OTH_ORAL", "SECGEN1OTH_INJ",
          "SECGEN1OTH_LAI", "SECGEN1OTH_REC", "SECGEN1OTH_TOP", "SECGEN2OTH_NO", "SECGEN2OTH_ORAL",
          "SECGEN2OTH_INJ", "SECGEN2OTH_LAI", "SECGEN2OTH_REC", "SECGEN2OTH_TOP", "SECGEN3OTH_NO",
          "SECGEN3OTH_ORAL", "SECGEN3OTH_INJ", "SECGEN3OTH_LAI", "SECGEN3OTH_REC", "SECGEN3OTH_TOP")


management <- c("MHINTCASEMGMT", "MHCASEMGMT", "MHCOURTORDERED", "MHAOT", "MHCHRONIC", 
                "ILLNESSMGMT", "PRIMARYCARE", "DIETEXERCOUNSEL", "FAMPSYCHED", "MHEDUCATION",
                "MHHOUSING", "SUPPHOUSING",  "MHPSYCHREHAB", "MHVOCREHAB", "SUPPEMPLOY", "FOSTERCARE",
                "MHLEGAL", "MHSUICIDE", "MHCONSUMER", "MHHBV", "MHHCV", "MHHIV", "MHSTD", "MHTB",
                "MHTOBACCOUSE", "MHTOBACCOCESS", "MHNICOTINEREP", "SMOKINGCESSATION", "MHOTH",
                "MHNOSVCS", "YNGCHLD", "CHILDREN", "ADOLES", "YOUNGADULTS", "ADULT", "SENIORS", 
                "CRISISTEAM2", "PSYCHON", "PSYCHOFF","SIGNLANG", "LANG", "LANGPROV", "CONTED", 
                "CASEREV", "QUALREV", "OUTFUP","CQIP", "SATSUR", "CPPR", "RCA", "SMOKINGPOLICY",
                "USEDSECLUSION", "ADOPTSECLUSION", "FEESCALE", "PAYASST","REVCHK1", "REVCHK2",
                "REVCHK8", "REVCHK5", "REVCHK10", "FUNDSMHA", "FUNDSTATEWELFARE","FUNDSTATEJUV",
                "FUNDSTATEEDUC", "FUNDOTHSTATE", "FUNDLOCALGOV", "FUNDCSBG", "FUNDCMHG", "FUNDFEDGRANT",
                "REVCHK15", "FUNDVA", "REVCHK17", "FUNDPRIVCOMM", "REVCHK2A", "LICENMH", "LICENSED",
                "LICENPH", "LICENSEDFCS", "LICENHOS", "JCAHO", "CARF", "COA", "CMS", "OTHSTATE",
                "OTHFAC", "FACNUM", "IPSERV", "IPTOTAL", "IPAGETOT017", "IPAGETOT1864", "IPAGETOT65")

# Language support vars

lang <- c("LANG", "LANGPROV", "LANG16", "LANG_B", "LANG1", "LANG2", "LANG3", "LANG21", "LANG4", "LANG5",
           "LANG6", "LANG7", "LANG8", "LANG24", "LANG9", "LANG10", "LANG22", "LANG25", "LANG26", "LANG11",  
           "LANG19", "LANG23", "LANG12", "LANG13", "LANG14", "LANG15", "LANG20", "LANG17", "LANG18")

# Calculated vars

calc <- c("LANGPROV", "TOTADMIS", "OPLEGALTOTFOREN", "OPLEGALTOTNONFOREN", "OPLEGALTOTVOL", "OPRACETOTUNK",
          "OPRACETOTMR", "OPRACETOTWHIT", "OPRACETOTHAWPAC", "OPRACETOTBLK", "OPRACETOTASIAN", "OPRACETOTINDIAN",
          "OPETHTOTUNK", "OPETHTOTNONHISP", "OPETHTOTHISP", "OPAGETOT65", "OPAGETOT1864", "OPAGETOT017")

nmhss <- nmhss %>%
    
    dplyr::transmute(across(all_of(c(apr2020, ndem)), basiclayer),
                  facilitytype = factor(FACILITYTYPE, labels = c("1" = "Psychiatric hospital",
                                                                 "2" = "Separate inpatient psychiatric unit of a general hospital",
                                                                 "3" = "Residential treatment center for children",
                                                                 "4" = "Residential treatment center for adults",
                                                                 "5" = "Other residential treatment facility",
                                                                 "6" = "Veterans Administration Medical Center (VAMC)",
                                                                 "7" = "Community Mental Health Center (CMHC)",
                                                                 "8" = "Certified Community Behavioral Health Clinic (CCBHC)",
                                                                 "9" = "Partial hospitalization/day treatment facility",
                                                                 "10" = "Outpatient MHF",
                                                                 "11" = "Multi-setting MHF",
                                                                 "12" = "Other")),
                  focus = factor(FOCUS, labels = c("1" = "Mental health treatment",
                                                   "3" = "Mental health/substance abuse treatment",
                                                   "4" = "General health care",
                                                   "5" = "Other")),
                  ownership = factor(OWNERSHP, labels = c("1" = "Private for-profit organization",
                                                          "2" = "Private non-profit organization",
                                                          "3" = "Public agency or department")),
                  operator = factor(PUBLICAGENCY, labels = c("-2" = "logical skip",
                                                             "1" = "State mental health authority (SMHA)",
                                                             "2" = "Other state government agency or department",
                                                             "3" = "Regional authority orgovernment",
                                                             "4" = "Tribal government",
                                                             "5" = "Indian Health Service",
                                                             "6" = "Department of Veterans Affairs",
                                                             "7" = "Other")),
                  facnum = factor(FACNUM, labels = c("-2" = "1",
                                                     "1" = "2-5",
                                                     "2" = "6-10",
                                                     "3" = "11-30",
                                                     "4" = "30+"))) %>%
    
    right_join(statekey, by = c("LST" = "abb")) %>%
    
    dplyr::select(-LST, -num)


# fix states, remove PR and other

rm(basiclayer, clean_cdc, get_cdc)
