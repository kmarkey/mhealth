################ fig generation

library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)
library(ggrepel)
library(stringr)

psave <- function(path, save = FALSE) {
    
    if (save == TRUE) {
        ggsave(path, height = unit(6, "in"), width = unit(8, "in"))
        print("File saved")
    } else {
        print("Save == FALSE")
    }
}

################# theme
# Set universal theme + palette for figures

pal <- c("#001219", "#005f73", "#0a9396", "#94d2bd", "#e9d8a6", "#ee9b00", "#ca6702", "#bb3e03", "#CA8D8F", "#ae2012", "#9b2226", "#61252F")

pal2 <- c("#264653", "#287271", "#2a9d8f", "#8ab17d", "#babb74", "#e9c46a", "#efb366", "#f4a261", "#ee8959", "#e76f51", "#DA5231", "#B85146")

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

cvi_palettes(pal, 3)

scale_fill_pal = function(palette) {
  ggplot2::scale_fill_manual(values = cvi_palettes(palette,
                                                   type = "discrete"))
}

#################

source("digest.R")
source("joins.R")

### Physical/Mental health days

p <- brfss %>% 
  ggplot() +
  geom_bar(aes(x = phys14d), fill = "#e76f51", color = "#2B2D42") +
  scale_y_continuous(limits = c(0, 300000), labels = comma) +
  labs(x = "Phsyical Health", y = "Frequency") +
  guides(fill = "none")

m <- brfss %>%
  ggplot() +
  geom_bar(aes(x = ment14d), fill = "#588157", color = "#2B2D42") +
  scale_y_continuous(limits = c(0, 300000), labels = comma) +
  labs(x = "Mental Health") +
  theme(axis.title.y = element_blank()) +
  guides(fill = "none")

title <- ggdraw() + 
  draw_label(
    "Number of Days per Month Respondants \"Had a Bad ____ Day\"",
    fontface = 'bold',
    x = -0.08,
    hjust = -.1,
  )

plot_grid(title, plot_grid(p, m, align = "vh"), nrow = 2, rel_heights = c(0.1, 1))

psave("./figs/plot1.png")

### Physical/Mental scatter

brfss %>%
  dplyr::filter(MENTHLTH < 31, PHYSHLTH < 31) %>%
  ggplot() +
  geom_density2d_filled(aes(x = MENTHLTH, y = PHYSHLTH)) +
  labs(title = "Number of Days ____ Health Was Bad")

psave("./figs/plot2.png")

### Other vars
#### medical cost

brfss %>%
  dplyr::filter(medcost %in% c("yes", "no")) %>%
  ggplot() + geom_bar(aes(x = medcost), color = "#2B2D42") + 
  scale_y_continuous(labels = comma) +
  labs(caption = "BRFSS",
       x = "Was there a time in the past 12 months when you needed to see a doctor \nbut could not because of cost?",
       y = "Frequency")

psave("./figs/plot3.png")

#### checkup
brfss %>%
  ggplot() + geom_bar(aes(x = checkup), color = "#2B2D42") + scale_y_continuous(labels = comma) +
  # scale_x_discrete(limits = c("< 1 year", "< 2 years", "< 5 years", "5+ years")) +
  labs(caption = "BRFSS",
       x = "About how long has it been since you last visited a doctor for a routine checkup?",
       y = "Frequency")

psave("./figs/plot4.png")

### number of patients per facility
nmhss %>%
  dplyr::filter(IPTOTAL != "logical skip") %>%
  ggplot() +
  geom_bar(aes(x = IPTOTAL), fill = pal2[7]) +
  labs(x = "Total number of patients receiving 24-hour hospital inpatient \nmental health treatment",
       y = "Count",
       caption = "N-MHSS",
       title = "Inpatient Count for each Facility",
       subtitle = "On April 30th, 2020") +
  theme(axis.text.x = element_text(angle = 10, vjust = .90))

psave("./figs/plot5.png")

### mental health days 14+ per state

#### top 10 mhd 14+

mhd14top <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE)/n(), rank = 1) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  head(5)

#### bottom 10 mhd14+
mhd14bottom <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE)/n(), rank = 2) %>%
  distinct() %>%
  arrange(desc(per)) %>%
  tail(5)

bind_rows(mhd14top, mhd14bottom) %>%
  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), -per), y = per * 100, fill = rank), stat = "identity") +
  guides(fill = "none") +
  labs(x = "", y = "Percent", title = "Percentage of Respondents Who Had 14+ Bad Mental Health Days",
       subtitle = "By State, Top 10 and Bottom 10", caption = "BRFSS") +
  theme(axis.text.x = element_text(angle = 20))

psave("./figs/plot6.png")

## chloropleths

us_states <- map_data("state")

#### % who had > 13 bad MHD
data <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE)/n()) %>%
  distinct() %>%
  right_join(us_states, by = c("state" = "region"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = per * 100)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Percent", title = "Percentage of Respondents Who Had \n14+ Bad Mental Health Days",
                     caption = "BRFSS") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

psave("./figs/plot7.png")

#### % woh had >0 bad MHD
data <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d != "0", 1, 0), na.rm = TRUE)/n()) %>%
  distinct() %>%
  right_join(us_states, by = c("state" = "region"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = per * 100)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Percent", title = "Percentage of Respondents Who Had \nat Least 1 Bad Mental Health Day",
                     caption = "BRFSS") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

psave("./figs/plot8.png")

## facilities
### number of facilities

data <- nmhss %>%
  group_by(state) %>%
  summarise(state, n = n()) %>%
  distinct() %>%
  right_join(us_states, by = c("state" = "region"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = n)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Count", title = "Total Number of Mental Health Facilities",
                     caption = "N-MHSS") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

psave("./figs/plot9.png")

#### Number of MHF per cap
data <- nmhss %>%
  group_by(state) %>%
  dplyr::summarise(state, n = n()) %>%
  distinct() %>%
  left_join(us_states, by = c("state" = "region")) %>%
  left_join(pop, by = c("state"))

ggplot(data, aes(x = long, y = lat,
                 group = group, fill = n/pop * 100,000)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  geom_polygon(color = "#8d99ae", size = 0.1) +
  theme_map() + labs(fill = "Rate per 100,000", title = "Number of Mental Health Facilities per Capita",
                     caption = "N-MHSS") + 
  scale_fill_distiller(palette = "Spectral") +
  theme(text = element_text(size = 10))

psave("./figs/plot10.png")

### Mental health treatment specialty correlation
pcor <- nmhss %>%
 # dplyr::filter(focus == "Mental health treatment") %>%
  group_by(state) %>%
  dplyr::summarise(TOT = n()) %>%
  distinct() %>%
  na.omit()

mcor <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d != "0", 1, 0), na.rm = TRUE)/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "state")

cat("Number of MHF vs. At Least 1 Bad Mental Health Day Correlation: \n", cor(data$TOT, data$per))

ggplot(data) +
  geom_text_repel(aes(x = per, y = TOT, label = str_to_title(state)), size = 2) +
  labs(x = "Percent of Respondents who had at Least 1 Bad Mental Health Day",
       y = "Number of MHF", 
       title = "Mental Health Need and Availability",
       subtitle = "All Facilities")

psave("./figs/plot11.png")

#### per capita?

pcor <- nmhss %>%
  group_by(state) %>%
  dplyr::summarise(state, TOT = n()) %>%
  distinct() %>%
  left_join(pop, by = c("state")) %>%
  dplyr::summarise(state, pcap = TOT/pop) %>%
  na.omit()

mcor <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d != "0", 1, 0), na.rm = TRUE)/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "state")

cat("MHF per capita vs. At Least 1 Bad Mental Health Day: \n", cor(data$pcap, data$per))

ggplot(data) +
  geom_text_repel(aes(x = per, y = pcap, label = str_to_title(state)), size = 2) +
  labs(x = "Percent of Respondents who had at Least 1 Bad Mental Health Day",
       y = "Number of MHF per capita", 
       title = "Mental Health Need and Availability",
       subtitle = "All Facilities")

psave("./figs/plot12.png")

#### cutoff at 14+ days instead?
pcor <- nmhss %>%
  group_by(state) %>%
  dplyr::summarise(state, TOT = n()) %>%
  distinct() %>%
  left_join(pop, by = c("state")) %>%
  dplyr::summarise(state, pcap = TOT/pop) %>%
  na.omit()

mcor <- brfss %>%
  group_by(state) %>%
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE)/n()) %>%
  distinct()

data <- left_join(pcor, mcor, by = "state")

cat("Number of Facilities Per Capita vs. 14+ Bad Mental Health Days: \n", cor(data$pcap, data$per))

ggplot(data, aes(x = per, y = pcap)) +
  geom_point() +
  geom_text_repel(aes(label = state), size = 2) +
  labs(x = "Percent of Respondents who had at 14+ Bad Mental Health Days",
       y = "Number of MHF per capita", 
       title = "Mental Health Need and Availability",
       subtitle = "All Facilities") +
  geom_smooth(formula = y ~ x, method = "lm", lty = 2, se = FALSE)

psave("./figs/plot13.png")

## by facility type
drilln(facilitytype) %>%
  ggplot() + geom_bar(aes(x = reorder(state, count), y = count, fill = facilitytype), 
                      stat = "identity", position = "stack") +  
  theme_classic() +
  guides(fill = guide_legend(nrow  = 6)) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45),
        axis.title.x = element_blank()) +
  scale_fill_pal(pal2)

psave("./figs/plot14.png")
