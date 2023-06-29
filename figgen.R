################ fig generation

library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)
library(ggrepel)
library(stringr)
library(plotly)
library(here)

setwd("~/LocalRStudio/mhealth/")
psave <- function(path, save = TRUE) {
    
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

#################
source("digest.R")
source("joins.R")

### Physical/Mental health days
  
p <- drillb(phys14d) %>%
  group_by(phys14d) %>%
  summarise(phys14d, tot = sum(count)) %>%
  unique() %>%
  ggplot() +
  geom_bar(aes(x = phys14d, y = tot/1000000), stat = "identity", fill = "#e76f51", color = "#2B2D42") +
  scale_y_continuous(limits = c(0, 200), labels = comma) +
  scale_x_discrete(position = "top") +
  labs(x = "Phsyical Health", y = "Frequency (in millions)") +
  theme(axis.title.x = element_text(size = 14, face = "bold")) +
  guides(fill = "none")

m <- drillb(ment14d) %>%
  group_by(ment14d) %>%
  summarise(ment14d, tot = sum(count)) %>%
  unique() %>%
  ggplot() +
  geom_bar(aes(x = ment14d, y = tot/1000000), stat = "identity", fill = "#588157", color = "#2B2D42") +
  scale_y_continuous(limits = c(0, 200), labels = comma) +
  scale_x_discrete(position = "top") +
  labs(x = "Mental Health") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, face = "bold")) +
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

rm(p, m)

### Physical/Mental density

brfss %>%
  dplyr::filter(MENTHLTH < 31, PHYSHLTH < 31) %>%
  ggplot() +
  geom_density2d_filled(aes(x = MENTHLTH, y = PHYSHLTH)) +
  labs(title = "Number of Days ____ Health Was Bad")

psave("./figs/plot2.png")

### Other vars
#### medical cost

a <- brfss %>%
  dplyr::filter(medcost %in% c("yes", "no")) %>%
  ggplot() + geom_bar(aes(x = medcost), color = "#2B2D42") + 
  scale_y_continuous(labels = comma) +
  labs(caption = "BRFSS",
       x = "Was there a time in the past 12 months when you needed to see a doctor \nbut could not because of cost?",
       y = "Frequency")

ggplotly(a)

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
  geom_bar(aes(x = IPTOTAL), fill = factype[7]) +
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
  
  summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE) / n(), rank = 2) %>%
  
  distinct() %>%
  
  arrange(desc(per)) %>%
  
  tail(5)

p <- bind_rows(mhd14top, mhd14bottom) %>%
  
  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), -per), y = per, fill = rank,
                          text = paste0(round(per * 100, 2), "%")), stat = "identity") +
  
  guides(fill = "none") +
  
  scale_y_continuous(labels = percent) +
  
  labs(x = "", y = "Percentage of Responses", title = "Respondents Who Had 14+ Bad Mental Health Days",
       subtitle = "By State, Top 10 and Bottom 10", caption = "BRFSS") +
  
  theme(axis.text.x = element_text(angle = 20))

ggplotly(p, tooltip = "text")

psave("./figs/plot6.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot6.html")


## weighted percentage of residents

mhd14top <-
  drillb(ment14d) %>% 
  group_by(state) %>%
  summarise(per = ifelse(ment14d == "14+", count, 0)/sum(count), rank = 1) %>%
  dplyr::filter(per > 0) %>%
  arrange(desc(per)) %>%
  head(5)

mhd14bottom <-
  drillb(ment14d) %>% 
  group_by(state) %>%
  summarise(per = ifelse(ment14d == "14+", count, 0)/sum(count), rank = 2) %>%
  dplyr::filter(per > 0) %>%
  arrange(desc(per)) %>%
  tail(5)

p <- bind_rows(mhd14top, mhd14bottom) %>%
  
  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), -per), y = per, fill = rank,
                          text = paste0(round(per * 100, 2), "%")), stat = "identity") +
  
  guides(fill = "none") +
  
  scale_y_continuous(labels = percent) +
  
  labs(x = "", y = "Weighted Percentage of Respondents", title = "Percentage Who Had 14+ Bad Mental Health Days",
       subtitle = "By State, Top 10 and Bottom 10", caption = "BRFSS") +
  
  theme(axis.text.x = element_text(angle = 20))

ggplotly(p, tooltip = "text")

psave("./figs/plot6.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot6.html")  # group_by(state) %>%
  # summarise(per = sum(ifelse(ment14d == "14+", 1, 0), na.rm = TRUE)/n(), rank = 1) %>%
  # distinct() %>%
  # arrange(desc(per)) %>%
  # head(5)

## chloropleths

us_states <- usmap::us_map(regions = "states") %>%
  group_by(group) %>%
  mutate(color = round(runif(1)))

#### % who had > 13 bad MHD

data <- drillb(ment14d) %>% 
  
  group_by(state) %>%
  
  summarise(per = `count`/sum(`count`), ment14d, n = sum(`count`)) %>%
  
  dplyr::filter(ment14d == "14+") %>%
  
  right_join(us_states, by = c("state" = "full"))


p <- data %>%
  
  ggplot() + geom_polygon(aes(x = x, 
                              y = y, 
                              group = group, 
                              fill = per, 
                              text = paste0(round(per * 100, 2), "%")), 
                          color = "black", 
                          linewidth = 0.2) +
  
  coord_sf() +
  
  scale_fill_fermenter(palette = 3, 
                       guide = "colorbar", 
                       direction = 1, 
                       labels = c("10%", "12%", "14%", "16%")
                       ) +
  
  labs(title = "Weighted Percentage of Residents Who Had\n14+ Bad Mental Health Days per Month",
       caption = "BRFSS 2020\ncdc.gov",
       fill = "") +

  theme(legend.position = "right",
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid = element_line(color = "transparent"), 
        plot.background = element_rect(color = "transparent"))

ggplotly(p, tooltip = "text")

psave("./figs/plot7.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot7.html")

#### % who had >0 bad MHD
data <- drillb(ment14d) %>% 
  
  group_by(state) %>%
  
  dplyr::summarise(n = sum(ifelse(ment14d == "1-13" | ment14d == "14+", `count`, 0), na.rm = TRUE),
                   per = n/statecount,
                   ment14d) %>%
  dplyr::filter(ment14d == "0") %>%
  
  right_join(us_states, by = c("state" = "full"))

p <- data %>% 
  
  ggplot() + geom_polygon(aes(
    x = x,
    y = y,
    group = group,
    fill = per,
    text = paste0(round(per * 100, 2), "%\nn = ", comma(n)), 
  ),
  color = "black",
  size = 0.2) +
  
  coord_sf() +
  
  scale_fill_fermenter(palette = 8, guide = "colorbar",
                       labels = c("30%", "35%", "40%", "45%"), direction = 1) +

  labs(fill = "Percent", title = "Weighted Percentage of Residents Who Had \nAt Least 1 Bad Mental Health Day per Month",
       caption = "BRFSS") +
  
  theme(legend.position = 'right',
        
        legend.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.grid = element_line(color = "transparent"), 
        plot.background = element_rect(color = "transparent"))

ggplotly(p, tooltip = "text")

psave("./figs/plot8.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot8.html")

## facilities
### number of facilities

p <- drilln() %>%
  right_join(us_states, by = c("state" = "full")) %>%
  
  ggplot() + geom_polygon(
    aes(
      x = x,
      y = y,
      group = group,
      fill = count,
      text = count
    ),
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_sf() +
  
  scale_fill_fermenter(guide = "colorbar",
                       direction = 1,
                       breaks = c(200, 400, 600, 800)) +
  labs(fill = "Count",
       title = "Larger States Have More Facilities",
       caption = "NMHSS") +
  theme(
    legend.position = 'left',
    
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_line(color = "transparent"),
    plot.background = element_rect(color = "transparent")
  )


ggplotly(p, tooltip = "text")

psave("./figs/plot9.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot9.html")

#### Number of MHF per cap
p <- drilln() %>%
  left_join(pop, by = c("state")) %>%
  dplyr::mutate(per = count / pop * 100000) %>%
  
  right_join(us_states, by = c("state" = "full")) %>%
  
  ggplot() + geom_polygon(
    aes(
      x = x,
      y = y,
      group = group,
      fill = per,
      text = round(per, 1)
    ),
    color = "black",
    linewidth = 0.2
  ) +
  
  coord_sf() +
  
  scale_fill_fermenter(guide = "colorbar",
                       direction = 1,
                       breaks = c(3, 6, 9, 12)) +
  labs(
    fill = "Count",
    title = "Facilties Per 100,000 Residents",
    subtitle = "Maine and Alaska lead the nation",
    caption = "NMHSS") +
  
  theme(
    legend.position = 'top',
    
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_line(color = "transparent"),
    plot.background = element_rect(color = "transparent")
  )

ggplotly(p, tooltip = "text")

psave("./figs/plot10.png")

htmlwidgets::saveWidget(last_plot(), "./figs/plot10.html")

### Mental health treatment specialty correlation ##############################
#### per capita?

pcor <- drilln() %>%
  left_join(pop, by = c("state")) %>%
  dplyr::summarise(state, pcap = count/pop * 100000) %>% # For scale
  na.omit()

mcor <- drillb(ment14d) %>%
  dplyr::mutate(per = count/statecount) %>%
  dplyr::filter(ment14d == "1-13")

data <- left_join(pcor, mcor, by = "state")

cat("MHF per capita vs. Weighted Proportion of Residents with 1-13 Bad Mental Health Days: \n", cor(data$pcap, data$per))

ggplot(data) +
  geom_text_repel(aes(x = pcap, y = per, label = str_to_title(state)), size = 2) +
  labs(y = "Weighted Percent of Residents who had 1-13 Bad Mental Health Days",
       x = "Number of MHF per 100,000 Residents", 
       title = "Mental Health Need and Availability")

psave("./figs/plot12.png")

#### cutoff at 14+ days instead
# More severe

mcor <- drillb(ment14d) %>%
  dplyr::mutate(per = count/statecount) %>%
  dplyr::filter(ment14d == "14+")

data <- left_join(pcor, mcor, by = "state")

cat("Number of Facilities Per Capita vs. 14+ Bad Mental Health Days: \n", cor(data$pcap, data$per))

ggplot(data, aes(x = pcap, y = per)) +
  geom_point() +
  geom_text_repel(aes(label = state), size = 2) +
  labs(y = "Weighted Percent of Residents who had at 14+ Bad Mental Health Days",
       x = "Number of MHF per capita", 
       title = "Mental Health Need and Availability",
       subtitle = "All Facilities") +
  geom_smooth(formula = y ~ x, method = "lm", lty = 2, se = FALSE) +
  theme_bw()

psave("./figs/plot13.png")

## by facility type
# corrected for population

p <- drilln(facilitytype) %>%
    
    inner_join(pop, by = "state") %>%
    
    ggplot() + geom_bar(aes(x = reorder(str_to_title(state), pop), 
                            y = count/pop * 100000, fill = facilitytype,
                            text = paste0(round(count/pop * 100000, 2), "\n", facilitytype)), 
                        stat = "identity", position = "stack") +
  fill_pal_discrete(factype) +
  
    
    theme_classic() +
  
    guides(fill = guide_legend(nrow  = 6)) +
    
    theme(legend.position = "top", 
          axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1),
          axis.title.x = element_blank(),
          legend.title = element_blank()) +
    
    
    labs(title = "Outpatient and Community Centers Account for State Diffferences",
       subtitle = "Number of facilities, by state and type, proportional to population",
       y = "Facilities per 100,000 residents") +
    
    geom_segment(aes(x = 1, y = 15, xend = 31, yend = 15), 
                 arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +

    annotate("text", x = 24, y = 14, label = "More residents", size = 3.3, hjust = 0)

p

ggplotly(p, tooltip = "text")

psave("./figs/plot15.png")

# larger states have less facilities per person. They might have larger facilities (more beds, staff), but this also means
# its likely harder to find a facility near you

# by owner

drilln(ownership) %>%
    
  inner_join(pop, by = "state") %>%

  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), statecount/pop), y = count, fill = ownership), 
                      stat = "identity", position = "fill") +
  
  theme_classic() +
  
  guides(fill = guide_legend(ncow = 3)) +
  
  # coord_cartesian(xlim = c(1, 51), ylim = c(0, 1), clip = "off") +
  
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1),
        axis.title.x = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.box.background = element_blank()) +
  
  scale_y_continuous(breaks = c(0.25, 0.5, 0.75, 1.0)) +
    
  fill_pal_discrete(factype) +
  
  labs(title = "Most MH Facilities Are Owned by Private Non-Profit Organizations",
       subtitle = "Proportion of facilities, by state and facility ownership",
       y = "Proportion of Facilities") +
  
  annotate("segment", x = 1, y = 1.03, xend = 31, yend = 1.03,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  
  annotate("text", x = 23, y = 1.07, label = "More facilities per capita", size = 3.3, hjust = 0)

psave("./figs/plot16.png")

# by operator

drilln(operator) %>%
    
  filter(operator != "logical skip") %>%
  
  inner_join(pop, by = "state") %>%
  
  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), statecount/pop), y = count, fill = operator), 
                      stat = "identity", position = "fill") +
  
  coord_cartesian(xlim = c(1, 51), ylim = c(0, 1), clip = "off") +
  
  theme_classic() +
  
  guides(fill = guide_legend(nrow  = 3, ncol = 3)) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1),
        axis.title.x = element_blank(),
        legend.position =  "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()) +
  
  fill_pal_discrete(factype) +
  
  labs(title = "State & Regional Involvement Varies",
       subtitle = "Publicly owned facilities, by state and operating body",
       y = "Proportion of Public Facilities") +
  
  annotate("segment", x = 1, y = 1.03, xend = 31, yend = 1.03,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  
  annotate("text", x = 23, y = 1.07, label = "More facilities per capita", size = 3.3, hjust = 0)

psave("./figs/plot17.png")


# By focus
drilln(focus) %>%
  
  filter(focus != "logical skip") %>%
  
  inner_join(pop, by = "state") %>%
  
  ggplot() + geom_bar(aes(x = reorder(str_to_title(state), statecount/pop), y = count, fill = focus), 
                      stat = "identity", position = "fill") +
  
  coord_cartesian(xlim = c(1, 51), ylim = c(0, 1), clip = "off") +
  
  theme_classic() +
  
  guides(fill = guide_legend(nrow  = 2, ncol = 2)) +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1),
        axis.title.x = element_blank(),
        legend.position =  "top",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_blank()) +
  
  fill_pal_discrete(factype) +
  
  labs(title = "Most Facilities Primary Focus is Mental Health",
       subtitle = "Facilities by state and treatment focus",
       y = "Proportion of Facilities") +
  
  annotate("segment", x = 1, y = 1.03, xend = 31, yend = 1.03,
           arrow = arrow(type = "closed", length = unit(0.02, "npc"))) +
  
  annotate("text", x = 23, y = 1.07, label = "More facilities per capita", size = 3.3, hjust = 0)

