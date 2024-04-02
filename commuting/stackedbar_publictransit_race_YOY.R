
library(tidyverse)
library(tidycensus)
library(plotly)
library(RColorBrewer)

options(scipen=999) #scientific notation setup

census_api_key("e40cc68dcd205c78498b9c938801139a9b8f6362", #personal key
               overwrite = T,
               install = T)
readRenviron("~/.Renviron") 

#### Importing ACS 1-year estimates ####

# https://www.socialexplorer.com/data/ACS2022/metadata/?ds=SE
var22 <- load_variables(year=2022,dataset = "acs1")

vars_race <- c(
  tot_hispanic = 'B08105I_001',
  drovealone_hispanic = "B08105I_002",
  carpooled_hispanic = "B08105I_003",
  pubtrans_hispanic = "B08105I_004",
  walked_hispanic = 'B08105I_005',
  other_hispanic = 'B08105I_006',
  home_hispanic = "B08105I_007",
  tot_black = 'B08105B_001',
  drovealone_black = "B08105B_002",
  carpooled_black = "B08105B_003",
  pubtrans_black = "B08105B_004",
  walked_black= 'B08105B_005',
  other_black = 'B08105B_006',
  home_black = "B08105B_007",
  tot_white = 'B08105H_001',
  drovealone_white = "B08105H_002",
  carpooled_white = "B08105H_003",
  pubtrans_white = "B08105H_004",
  walked_white = 'B08105H_005',
  other_white = 'B08105H_006',
  home_white = "B08105H_007",
  tot_asian = 'B08105D_001',
  drovealone_asian = "B08105D_002",
  carpooled_asian = "B08105D_003",
  pubtrans_asian = "B08105D_004",
  walked_asian = 'B08105D_005',
  other_asian = 'B08105D_006',
  home_asian = "B08105D_007"
)

# 2022 data
comm2022 <- get_acs(
  survey = "acs1",
  year = 2022,
  geography = "county",
  county = "Philadelphia",
  state = "PA",
  output = "wide",
  variables = vars_race
)

# 2017 data
comm2017 <- get_acs(
  survey = "acs1",
  year = 2017,
  geography = "county",
  county = "Philadelphia",
  state = "PA",
  output = "wide",
  variables = vars_race
)

#### Calculate variables by group ####

commute_race_22 <- comm2022 %>% 
  transmute(perchisp_drove = drovealone_hispanicE/tot_hispanicE,
         perchisp_carp = carpooled_hispanicE/tot_hispanicE,
         perchisp_pubtrans = pubtrans_hispanicE/tot_hispanicE,
         perchisp_walked = walked_hispanicE/tot_hispanicE,
         perchisp_other = other_hispanicE/tot_hispanicE,
         perchisp_home = home_hispanicE/tot_hispanicE,
         percblack_drove = drovealone_blackE/tot_blackE,
         percblack_carp = carpooled_blackE/tot_blackE,
         percblack_pubtrans = pubtrans_blackE/tot_blackE,
         percblack_walked = walked_blackE/tot_blackE,
         percblack_other = other_blackE/tot_blackE,
         percblack_home = home_blackE/tot_blackE,
         percwhite_drove = drovealone_whiteE/tot_whiteE,
         percwhite_carp = carpooled_whiteE/tot_whiteE,
         percwhite_pubtrans = pubtrans_whiteE/tot_whiteE,
         percwhite_walked = walked_whiteE/tot_whiteE,
         percwhite_other = other_whiteE/tot_whiteE,
         percwhite_home = home_whiteE/tot_whiteE,
         percasian_drove = drovealone_asianE/tot_asianE,
         percasian_carp = carpooled_asianE/tot_asianE,
         percasian_pubtrans = pubtrans_asianE/tot_asianE,
         percasian_walked = walked_asianE/tot_asianE,
         percasian_other = other_asianE/tot_asianE,
         percasian_home = home_asianE/tot_asianE)

commute_race_17 <- comm2017 %>% 
  transmute(perchisp_drove = drovealone_hispanicE/tot_hispanicE,
            perchisp_carp = carpooled_hispanicE/tot_hispanicE,
            perchisp_pubtrans = pubtrans_hispanicE/tot_hispanicE,
            perchisp_walked = walked_hispanicE/tot_hispanicE,
            perchisp_other = other_hispanicE/tot_hispanicE,
            perchisp_home = home_hispanicE/tot_hispanicE,
            percblack_drove = drovealone_blackE/tot_blackE,
            percblack_carp = carpooled_blackE/tot_blackE,
            percblack_pubtrans = pubtrans_blackE/tot_blackE,
            percblack_walked = walked_blackE/tot_blackE,
            percblack_other = other_blackE/tot_blackE,
            percblack_home = home_blackE/tot_blackE,
            percwhite_drove = drovealone_whiteE/tot_whiteE,
            percwhite_carp = carpooled_whiteE/tot_whiteE,
            percwhite_pubtrans = pubtrans_whiteE/tot_whiteE,
            percwhite_walked = walked_whiteE/tot_whiteE,
            percwhite_other = other_whiteE/tot_whiteE,
            percwhite_home = home_whiteE/tot_whiteE,
            percasian_drove = drovealone_asianE/tot_asianE,
            percasian_carp = carpooled_asianE/tot_asianE,
            percasian_pubtrans = pubtrans_asianE/tot_asianE,
            percasian_walked = walked_asianE/tot_asianE,
            percasian_other = other_asianE/tot_asianE,
            percasian_home = home_asianE/tot_asianE)

# export dataframes and manually merge in csv
write.csv(commute_race_17, "commute_race_17.csv", row.names=TRUE)
write.csv(commute_race_22, "commute_race_22.csv", row.names=TRUE)

perc_commute = read.csv('commute_race_YOY.csv')
head(perc_commute) 

# create separate dataframes for each group
hisp <- perc_commute %>% 
  filter (Race == 'Hispanic') %>% 
  group_by(Year) %>% 
  mutate(Commute = factor(Commute, levels = Commute[order(Proportion)]))
hisp$Commute <- fct_rev(hisp$Commute)

black <- perc_commute %>% 
  filter (Race == 'Black') %>% 
  group_by(Year) %>% 
  mutate(Commute = factor(Commute, levels = Commute[order(Proportion)]))
black$Commute <- fct_rev(black$Commute)

white <- perc_commute %>% 
  filter (Race == 'White') %>% 
  group_by(Year) %>% 
  mutate(Commute = factor(Commute, levels = Commute[order(Proportion)]))
white$Commute <- fct_rev(white$Commute)

asian <- perc_commute %>% 
  filter (Race == 'Asian') %>% 
  group_by(Year) %>% 
  mutate(Commute = factor(Commute, levels = Commute[order(Proportion)]))
asian$Commute <- fct_rev(asian$Commute)


#### Stacked bar graph ####

# define colors
mycolors <- colorRampPalette(brewer.pal(6, "Set2"))(6)
mymodes <- unique(asian$Commute)
names(mycolors) <- mymodes

# create a stacked bar chart for each group
fig_hisp <- plot_ly(hisp, 
                   x = ~Year, 
                   y = ~Proportion, 
                   type = 'bar',
                   color = ~Commute, 
                   colors = mycolors,
                   text = ~Commute,
                   textposition = "none",
                   hovertemplate=paste("<i>%{text}:</i><br>%{y}")) %>% 
  layout(barmode = 'stack',
         xaxis = list(tickvals = list(2017, 2022)
        ))

fig_black <- plot_ly(black, 
                    x = ~Year, 
                    y = ~Proportion, 
                    type = 'bar',
                    color = ~Commute, 
                    colors = mycolors,
                    text = ~Commute,
                    textposition = "none",
                    hovertemplate=paste("<i>%{text}:</i><br>%{y}")) %>% 
  layout(barmode = 'stack',
         xaxis = list(tickvals = list(2017, 2022)
         ))


fig_white <- plot_ly(white, 
                     x = ~Year, 
                     y = ~Proportion, 
                     type = 'bar',
                     color = ~Commute, 
                     colors = mycolors,
                     text = ~Commute,
                     textposition = "none",
                     hovertemplate=paste("<i>%{text}:</i><br>%{y}")) %>% 
  layout(barmode = 'stack',
         xaxis = list(tickvals = list(2017, 2022)
         ))


fig_asian <- plot_ly(asian, 
                     x = ~Year, 
                     y = ~Proportion, 
                     type = 'bar',
                     color = ~Commute, 
                     colors = mycolors,
                     text = ~Commute,
                     textposition = "none",
                     hovertemplate=paste("<i>%{text}:</i><br>%{y}")) %>% 
  layout(barmode = 'stack',
         xaxis = list(tickvals = list(2017, 2022)
         ))

# combine subplots
fig <- subplot(fig_asian, 
               style(fig_black, showlegend = F), 
               style(fig_hisp, showlegend = F),
               style(fig_white, showlegend = F),
               titleX = FALSE, shareY = T) %>% 
  layout(barmode = 'stack',
         title = list(text = "<br>Means of Transportation to Work by Race or Ethnicity"),
         yaxis = list(title = 'Proportion of Workers', tickformat = ".0%"), 
         xaxis = list(title = ""),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         bargap = 0.5,
         plot_bgcolor = 'grey60', paper_bgcolor = 'grey60',
         margin = list(l=50,
                       r=75,
                       t=75,
                       b=90,
                       pad=20),
         annotations = list(
           list(x = 0.09,
                y = -0.11,
                font = list(color = "black",size = 14),
                text = "Asian",
                xref = "paper",
                yref = "paper",
                showarrow = F),
           list(
             x = 0.37,
             y = -0.11,
             font = list(color = "black",size = 14),
             text = "Black",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE),
           list(
             x = 0.62,
             y = -0.11,
             font = list(color = "black",size = 14),
             text = "Hispanic",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE),
           list(
             x = 0.91,
             y = -0.11,
             font = list(color = "black",size = 14),
             text = "White",
             xref = "paper",
             yref = "paper",
             showarrow = FALSE)))
fig
