
############################# data loaders #####################################

# process-functions


statekey <- tibble(state = state.name, abb = state.abb, fips = as.numeric(fips(state))) %>%
  
  rows_insert(tibble(state = "District of Columbia", abb = "DC", fips = 11), by = "state") %>%
  
  arrange(state)

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

# FNS

load_cdc <- function(fn, vars = goodvars, path = "data/brfss.csv") {
  
  if (file.exists(path)) {
    
    data <- read_csv(path)
    
    return(data)
    
  } else {
    
    temp <- tempfile()
    
    download.file("https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip", temp)
    
    data <- read_xpt(temp)
    
    file.remove(temp)
    
    data <- fn(data, vars)
    
    # write out
    write_csv(data, path)
    
    return(data)
    
    rm(temp, data)
  }
}

clean_cdc <- function(data, goodvars) {
  
  #  make data resemble data dict
  names(data) <- sub("^_", "X_", names(data))
  
  brfss <- data %>%
    
    dplyr::select(all_of(goodvars)) %>%
    
    # for each var, label and as factor
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
  
  # brfss <- tibble(oldstate = unique(as.numeric(temp$X_STATE)), newstate = 1:53) %>%
  #   
  #   right_join(temp, by = c("oldstate" = "X_STATE")) %>%
  #   
  #   right_join(statekey, by = c("newstate" = "num")) %>%
  #   
  #   dplyr::select(-abb, -oldstate, -newstate) %>%
  #   
  #   dplyr::mutate(state = as.factor(state))
  
  return(brfss)
}

# slicers
drillb <- function(...) {
  
  quos <- enquos(...)
  
  brfss %>%
    
    dplyr::select(!!!quos, state, X_LLCPWT) %>%
    
    dplyr::group_by(!!!quos, state) %>%
    
    dplyr::count(wt = X_LLCPWT, .drop = FALSE, name = "count") %>%
    
    group_by(state) %>%
    
    mutate(statecount = sum(count),
           state = str_to_title(state))
}

drilln <- function(...) {
  
  quos <- enquos(...)
  
  nmhss %>%
    
    dplyr::select(!!!quos, state) %>%
    
    dplyr::group_by(!!!quos, state) %>%
    
    dplyr::count(.drop = FALSE, name = "count") %>%
    
    group_by(state) %>%
    
    mutate(statecount = sum(count),
           state = str_to_title(state))
}

########################### plotting ###########################################

pal <- c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#CA8D8F", "#ae2012", "#9b2226", "#61252F")

factype <- c("#264653", "#287271", "#2a9d8f", "#8ab17d", "#babb74", "#e9c46a", "#efb366", "#f4a261", "#ee8959", "#e76f51", "#DA5231", "#B85146")

operator <- c("#582f0e", "#734821", "#7f4f24", "#936639", "#a68a64", "#b6ad90", "#c2c5aa", "#a4ac86", "#A4A181", "#656d4a", "#414833", "#333d29")

cvi_palettes = function(palette, n, type = c("discrete", "continuous")) {
  
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = "pal", class = "palette")
}

fill_pal_discrete <- function(palette) {
  
  ggplot2::scale_fill_manual(values = cvi_palettes(palette,
                                                   type = "discrete"))
}

fill_pal_continuous <- function(palette) {
  
  ggplot2::scale_fill_manual(values = cvi_palettes(palette,
                                                   type = "continuous"))
}

# main plotly fun
plotly_chloropleth <- function(data, z, tick_breaks = c(0.12, 0.14, 0.16, 0.18, 0.20), title = "", colors = "Oranges", symbol = "", digits = 1) {
  
  # helper fun
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  # creating scale breaks and color palette 
  all_scaled_breaks <- range01(c(min(data[[z]], na.rm = TRUE), tick_breaks, max(data[[z]], na.rm = TRUE)))

  z_colors <- brewer.pal(length(all_scaled_breaks) - 1, colors)
  
  # list of z breaks
  z_list <- embed(all_scaled_breaks, 2)[, 2:1] |>
    as.vector() |>
    sort() %>%
    data.frame(z = .)
  
  # final color scale df
  colorScale <- bind_cols(z_list, cols = rep(z_colors, each = 2))
  
  
  # chloropleth specs
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = FALSE
  )
  
  if (symbol == "%") {
    
    # plot with 1 decimal pt and * 100
    fig <- plot_geo(data, 
                    locationmode = 'USA-states',
                    colorscale = colorScale,
                    hovertemplate = paste0(format(round(data[[z]] * 100, digits = digits), nsmall = digits), symbol, "<br>",
                                           data$state, "<extra></extra>")) %>% 
      add_trace(
        z = ~.data[[z]], 
        text = ~state, 
        locations = ~abbr,
        color = ~.data[[z]], 
        colorbar = list(ypad = 1, 
                        tickmode = "array",
                        ticktext = paste0(format(round(tick_breaks * 100, digits = digits), nsmall = digits), symbol),
                        tickvals = tick_breaks
        )
      ) %>%
      
      colorbar(title = "") %>%
      
      layout(
        title = list(text = title, y = 0.9),
        geo = g
      )
    
  } else {
    
    # plot with 0 decimal pts
    fig <- plot_geo(data, 
                    locationmode = 'USA-states',
                    colorscale = colorScale,
                    hovertemplate = paste0(format(round(data[[z]], digits = 0), nsmall = 0), symbol, "<br>",
                                           data$state, "<extra></extra>")) %>% 
      add_trace(
        z = ~.data[[z]], 
        text = ~state, 
        locations = ~abbr,
        color = ~.data[[z]], 
        colorbar = list(ypad = 1, 
                        tickmode = "array",
                        ticktext = paste0(format(round(tick_breaks, digits = 0), nsmall = 0), symbol),
                        tickvals = tick_breaks
        )
      ) %>%
      
      colorbar(title = "") %>%
      
      layout(
        title = list(text = title, y = 0.9),
        geo = g
      )
    
  }
  
  fig
  
}

# add annotation to plotly plots
plotly_anno <- function(p, text = "") {
  
  p  %>%
    
    layout(
      annotations =
        list(
          x = 1,
          y = 0,
          text = text,
          showarrow = F,
          xref = 'paper',
          yref = 'paper',
          xanchor = 'right',
          yanchor = 'bottom',
          xshift = 0,
          yshift = 0,
          font = list(size = 10)
        )
    )
}

# thematizes maps
map_theme <- function() {
  theme(legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_line(color = "transparent"),
        plot.background = element_rect(color = "transparent"))
}