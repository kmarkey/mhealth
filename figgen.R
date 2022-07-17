################ fig generation

library(ggplot2)
library(dplyr)
library(comma)
library(scales)
library(cowplot)
library(ggrepel)

################# theme
#Set universal theme + palette for figures

pal <- c("#f6aa1c","#08415c","#6b818c","#eee5e9","#ba7ba1","#c28cae","#a52a2a")

blue <- "#114482"
lightblue <- "#146ff8"
llightblue <- "#AFCFFF"
red <- "#a52a2a"
white <- "#FBFFF1"
yellow <- "#F6AA1C"

ljtheme <- function(){
  theme_minimal() %+replace%
    theme(
      panel.grid.major = element_line(linetype = "solid", color = llightblue, size = 0.1),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = white), #light
      panel.border = element_rect(color = lightblue, fill = NA, linetype = "solid", size = 2),
      legend.background = element_rect(fill = white, color = lightblue, size = 1), # legend
      legend.text = element_text(color = blue),
      legend.title = element_text(face = "bold.italic", color = blue),
      legend.position = "bottom",
      legend.key = element_rect(fill = white),
      text = element_text(color = white),
      axis.title = element_text(face = "italic", size = 11, color = white), 
      axis.text = element_text(color = white, size = 9), 
      axis.ticks = element_line(color = white, size = .5, lineend = "butt"), 
      axis.ticks.length = unit(.1, "cm"),
      plot.title = element_text(face = "bold", # labels
                                color = white, size = 14, hjust = 0, vjust = 1.5),
      plot.subtitle = element_text(color = white, hjust = 0, vjust = 1.5, face = "italic"),
      plot.caption = element_text(color = white, face = "bold", hjust = 1),
      plot.background = element_rect(fill = blue),
      strip.background = element_blank(),
      strip.text = element_text())}

theme_set(ljtheme())
#################

source("digest.R")
source("joins.R")

### extra data
#population stats

pop <- read_csv("data/census2020pop.csv") %>%
  dplyr::mutate(name = tolower(NAME)) %>%
  right_join(statekey, by = "name") %>%
  summarise(name, abb, num, pop = ESTIMATESBASE2020, per = pop / sum(ESTIMATESBASE2020))


### Physical/Mental health days
p <- brfss %>% 
  ggplot() +
  geom_bar(aes(x = phys14d, fill = phys14d)) +
  scale_y_continuous(limits = c(0, 300000), labels = comma) +
  labs(x = "Phsyical Health", y = "Count") +
  ljtheme() +
  guides(fill = "none")

m <- brfss %>%
  ggplot() +
  geom_bar(aes(x = ment14d, fill = ment14d)) +
  scale_y_continuous(limits = c(0, 300000), labels = comma) +
  labs(x = "Mental Health", y = "Count") +
  ljtheme() +
  guides(fill = "none")

title <- ggdraw() + 
  draw_label(
    "Number of Days per Month Respondants Had a \"Bad Day\"",
    fontface = 'bold',
    x = 0,
    hjust = -.1,
    color = blue
  )

plot_grid(title, plot_grid(p, m, align = "vh"), nrow = 2, rel_heights = c(0.1, 1))

### Physical/Mental scatter
brfss %>%
  dplyr::filter(MENTHLTH < 31, PHYSHLTH < 31) %>%
  ggplot() +
  geom_point(aes(x = MENTHLTH, y = PHYSHLTH), alpha = 0.05) +
  labs(title = "Number of days ____ health was not good")

### Other vars
#### medical cost
brfss %>%
  dplyr::filter(MEDCOST %in% c(1, 2)) %>%
  ggplot() + geom_bar(aes(x = MEDCOST)) + scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("Yes", "No")) +
  labs(caption = "BRFSS",
       x = "Was there a time in the past 12 months when you needed to see a doctor \nbut could not because of cost?")

#### checkup
brfss %>%
  dplyr::filter(CHECKUP1 %in% c(1, 2, 3, 4)) %>%
  ggplot() + geom_bar(aes(x = CHECKUP1)) + scale_y_continuous(labels = comma) +
  scale_x_discrete(limits = c("< 1 year", "< 2 years", "< 5 years", "5+ years")) +
  labs(caption = "BRFSS",
       x = "About how long has it been since you last visited a doctor for a routine checkup?")

### number of patients per facility
nmhss1 %>%
  dplyr::filter(IPTOTAL != "logical skip") %>%
  ggplot() +
  geom_bar(aes(x = IPTOTAL), fill = pal[7]) +
  labs(x = "Total number of patients receiving 24-hour hospital inpatient \nmental health treatment",
       caption = "N-MHSS",
       title = "Inpatient Count for each Facility",
       subtitle = "On April 30th, 2020") +
  theme(axis.text.x = element_text(angle = 10, vjust = .90))

### mental health days 14+ per state

#### top 10 mhd 14+
mhd14top <- b1 %>%
  group_by(name) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0))/n(), rank = 1) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  head(10)

#### bottom 10 mhd14+
mhd14bottom <- b1 %>%
  group_by(name) %>%
  summarise( per = sum(ifelse(ment14d == "14+", 1, 0))/n(), rank = 2) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  tail(10)

bind_rows(mhd14top, mhd14bottom) %>%
  ggplot() + geom_bar(aes(x = reorder(name, -per), y = per * 100, fill = rank), stat = "identity") +
  guides(fill = "none") +
  labs(x = "", y = "Percent", title = "Percentage of Respondents Who Had 14+ Bad Mental Health Days",
       subtitle = "By State, Top 10 and Bottom 10", caption = "BRFSS") +
  theme(axis.text.x = element_text(angle = 20))

## chloropleths

us_states <- map_data("state")

#### % who had > 13 bad MHD
data <- b1 %>%
  group_by(name) %>%
  summarise( per = sum(ifelse(ment14d != "0", 1, 0))/n()) %>%
  distinct() %>%
  right_join(us_states, by = c("name" = "region"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = per * 100)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Percent", title = "Percentage of Respondents Who Had \nat Least 1 Bad Mental Health Days",
                     caption = "Data provided by BRFSS") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

### number of facilities

data <- n1 %>%
  summarise(TOT, name) %>%
  distinct() %>%
  right_join(us_states, by = c("name" = "region"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = TOT)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Percent", title = "Total Number of Mental Health Facilities",
                     caption = "Data provided by SAMSA.gov") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

#### Number of MHF per cap
data <- n1 %>%
  dplyr::select(name, TOT) %>%
  distinct() %>%
  left_join(us_states, by = c("name" = "region")) %>%
  left_join(pop, by = c("name"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = TOT/pop * 100,000)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Rate per 100,000", title = "Number of Mental Health Facilities per Capita",
                     caption = "Data provided by SAHMSA.gov") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

### Mental health treatment specialty
pcor <- n1 %>%
  dplyr::filter(focus == "Mental health treatment") %>%
  group_by(name) %>%
  dplyr::summarise(TOT = n()) %>%
  distinct() %>%
  na.omit()

mcor <- b1 %>%
  group_by(name) %>%
  summarise( per = sum(ifelse(ment14d != "0", 1, 0))/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "name")

cat("Number of MHF vs. At Least 1 Bad Mental Health Day: \n", cor(data$TOT, data$per))

ggplot(data) +
  geom_text_repel(aes(x = per, y = TOT, label = name), size = 3) +
  labs(x = "Percent of Respondents who had at Least 1 Bad Mental Health Day",
       y = "Number of MHF", 
       title = "Mental Health Need and Availability",
       subtitle = "Facilities with MHT focus")

#### per capita?

pcor <- n1 %>%
  dplyr::select(name, TOT) %>%
  distinct() %>%
  left_join(pop, by = c("name")) %>%
  dplyr::summarise(name, pcap = TOT/pop) %>%
  na.omit()

mcor <- b1 %>%
  group_by(name) %>%
  summarise(per = sum(ifelse(ment14d != "0", 1, 0))/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "name")

cat("MHF per capita vs. At Least 1 Bad Mental Health Day: \n", cor(data$pcap, data$per))

ggplot(data) +
  geom_text_repel(aes(x = per, y = pcap, label = name), size = 3) +
  labs(x = "Percent of Respondents who had at Least 1 Bad Mental Health Day",
       y = "Number of MHF per capita", 
       title = "Mental Health Need and Availability",
       subtitle = "Facilities with MHT focus")

#### cutoff at 14+ days instead?
pcor <- n1 %>%
  dplyr::select(name, TOT) %>%
  distinct() %>%
  left_join(pop, by = c("name")) %>%
  dplyr::summarise(name, pcap = TOT/pop) %>%
  na.omit()

mcor <- b1 %>%
  group_by(name) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0))/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "name")

cat("Number of Facilities Per Capita vs. 14+ Bad Mental Health Days: \n", cor(data$pcap, data$per))

ggplot(data, aes(x = per, y = pcap)) +
  geom_point() +
  geom_text_repel(aes(label = name), size = 3) +
  labs(x = "Percent of Respondents who had at 14+ Bad Mental Health Days",
       y = "Number of MHF per capita", 
       title = "Mental Health Need and Availability",
       subtitle = "Facilities with MHT focus") +
  geom_smooth(formula = y ~ x, method = "lm", lty = 2, se = FALSE)

## facet by facility type??
