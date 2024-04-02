
library(readr)
library(dplyr)
library(tidyverse)
library(plotly)
library(RColorBrewer)

#### Import data ####
dem <- read_csv("21_22_EnrollmentDemographics.csv")
schools <- read_csv("21_22_SchoolList.csv")
zip <- read_csv("ACS2021.csv")
poverty <- read_csv("ACS_poverty.csv")
homes <- read_csv("MedianHome.csv")
scores <- read_csv('2122_School_Metric_Scores.csv')

unique(scores$metric)

### school-level data
# filter scores data to desired metrics
scores <- scores %>% 
  filter(metric != 'Admission Type',
         !(metric_score %in% c('Ineligible by Report Type', 'Insufficient Sample', 
                               'Data Unavailable', 'Not Applicable', 'Ineligible by Grades Served')),
         metric %in% c("% of Students Attending at Least 90% of Instructional Days",
                       "% of Teachers Attending at Least 90% of Instructional Days",
                       "Year-to-Year Teacher Retention",
                       '% of Students Who Are Economically Disadvantaged',
                       '% of Suspensions Given to Each Racial/Ethnic Subgroup Compared to % of Students in Each Group, African American/Black',
                       '% of Suspensions Given to Each Racial/Ethnic Subgroup Compared to % of Students in Each Group, White')
  ) %>% 
  mutate(across('metric_score', as.numeric)) 

# pivot scores and take mean for each school 
scores_wider <- scores %>% 
  select(school_id, report_type, metric, metric_score) %>% 
  pivot_wider(names_from = metric,
              values_from = metric_score)
colnames(scores_wider) = gsub(" ", "_", colnames(scores_wider))
colnames(scores_wider) = gsub("[\\%,]", "Perc", colnames(scores_wider))
colnames(scores_wider) = gsub("-", "", colnames(scores_wider))
colnames(scores_wider) = gsub("/", "or", colnames(scores_wider))

scores_wider <- scores_wider %>% 
  group_by(school_id) %>% 
  summarise(st_att = mean(Perc_of_Students_Attending_at_Least_90Perc_of_Instructional_Days, na.rm = T),
            t_att = mean(Perc_of_Teachers_Attending_at_Least_90Perc_of_Instructional_Days, na.rm = T),
            t_ret = mean(YeartoYear_Teacher_Retention, na.rm = T),
            econ = mean(Perc_of_Students_Who_Are_Economically_Disadvantaged, na.rom = T),
            sus_b = mean(Perc_of_Suspensions_Given_to_Each_RacialorEthnic_Subgroup_Compared_to_Perc_of_Students_in_Each_GroupPerc_African_AmericanorBlack, na.rm = T),
            sus_w = mean(Perc_of_Suspensions_Given_to_Each_RacialorEthnic_Subgroup_Compared_to_Perc_of_Students_in_Each_GroupPerc_White, na.rm = T))

# filter school demographics to select school-wide metrics
dem <- dem %>% 
  filter(GradeLevel == 'All Grades')

# join school information to demographics
schools <- schools %>% 
  mutate(across('ZipCode', as.character),
         ZipCode = substr(schools$ZipCode, 0, 5)) %>%
  left_join(dem, by = 'ULCSCode') %>%
  left_join(scores_wider, join_by(ULCSCode == school_id)) %>%
  select(PublicationName, SchoolLevel, AdmissionType, SchoolReportingCategory, ZipCode, GPSLocation,
         Sector, StudentEnrollment, ELPCT, IEPPCT, MalePCT, FemalePCT, 
         AmericanIndianPCT, AsianPCT, BlackAfricanAmericanPCT, HispanicPCT,
         MultiRacePCT, PacificIslanderPCT, WhitePCT, CEPEconomicallyDisadvantagedRate,
         st_att, t_att, t_ret, sus_b, sus_w, econ) %>% 
  separate(GPSLocation, into = c('lat', 'lon'), sep=",")

### zipcode-level data
# select median home by zipcode
homes <- homes %>%
  mutate(across('NAME', str_replace, "ZCTA5 ", ""),
         median_home = as.numeric(B25077_001E)) %>%
  rename(zipcode = NAME) %>% 
  select(zipcode, median_home)

# select poverty level by zipcode
poverty <- poverty %>% 
  mutate(across('NAME', str_replace, "ZCTA5 ", ""),
         pov = 100*(as.numeric(B17001_002E)/as.numeric(B17001_001E))) %>%
  rename(zipcode = NAME) %>% 
  select(zipcode, pov)
 
# join all zip-level data
zip <- zip %>%
  mutate(across('NAME', str_replace, "ZCTA5 ", "")) %>% 
  mutate_if(is.character, as.numeric, na.rm = TRUE) %>% 
  mutate(across('NAME', as.character)) %>% 
  rename(zipcode = NAME,
         white = B03002_003E,
         black = B03002_004E,
         amerind = B03002_005E,
         asian = B03002_006E,
         aapi = B03002_007E,
         other = B03002_008E,
         multi = B03002_009E,
         hisp = B03002_012E) %>% 
  select(zipcode, white, black, amerind, asian, aapi, other, multi, hisp) %>% 
  mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
  mutate_if(is.numeric, list(~ round(./total*100, 2))) %>% 
  left_join(homes, join_by(zipcode == zipcode)) %>% 
  left_join(poverty, join_by(zipcode == zipcode))

# join all data sets
df <- left_join(schools, zip, join_by(ZipCode == zipcode)) %>% 
  filter(SchoolReportingCategory != 'SDP Special Education',
         SchoolReportingCategory != 'Virtual',
         SchoolReportingCategory != 'Alternate Schools')

#### EDA ####
summary(df)

table(df$AdmissionType)
# Citywide, citywide with criteria, neighborhood, special admit
table(df$Sector)
# 214 SDP schools, 85 charter

print(df %>% count(Sector, SchoolLevel, AdmissionType), n = 40)

diverse <- df %>% 
  filter(WhitePCT >= 20, 
         BlackAfricanAmericanPCT >= 20)

length(df %>% 
  filter(WhitePCT >= 20, 
         BlackAfricanAmericanPCT >= 20,
         HispanicPCT >= 10))

  
#### Figure 1 ####
## Boxplot of school demographics, by charter vs. district ##

col1 <- '#929591'
col2 <- '#FFCB05'
col3 <- '#0088C7'
col4 <- '#F7931E'
col5 <- '#1E8449'
col6 <- '#A02974'

hline <- function(y = 0, x0 = 0, x1 = 0, color = color) {
  list(
    type = "line", 
    x0 = x0, 
    x1 = x1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = color, dash="dot")
  )
}


hline <- function(y = 0, x0 = 0, x1 = 0, color = color) {
  list(
    type = "line", 
    x0 = x0, 
    x1 = x1, 
    xref = "paper",
    y0 = y, 
    y1 = y, 
    line = list(color = 'black')
  )
}

fig <- df %>% 
  group_by(Sector) %>% 
  plot_ly(type="box",
          x= ~Sector,
          y = ~econ,
          name = 'Economically Disadvantaged',
          color = I(col6), hoverinfo = 'y') %>% 
  add_trace(y= ~BlackAfricanAmericanPCT,
            name = 'Percent Black', 
            color = I(col5), 
            hoverinfo = 'y') %>% 
  add_trace(y = ~HispanicPCT, name = 'Percent Hispanic',
            color = I(col3),  hoverinfo = 'y') %>% 
  add_trace(y = ~WhitePCT, name = 'Percent White',
            color = I(col2),  hoverinfo = 'y') %>% 
  add_trace(y = ~AsianPCT, name = 'Percent Asian',
            color = I(col4),  hoverinfo = 'y') %>%
  layout(boxmode = 'group',
         title="<b>Student Demographics by School Type</b>",
         yaxis=list(title=""),
         xaxis=list(title=""),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20)) %>% 
  layout(shapes = list(hline(23, .08, .15),
                       hline(40, .15, .22),
                       hline(15, .22, .29),
                       hline(34, .29, .36),
                       hline(7, .36, .42),
                       hline(23, .57, .64),
                       hline(40, .64, .71),
                       hline(15, .71, .78),
                       hline(34, .78, .85),
                       hline(7, .85, .92))) %>% 
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

fig
  
#### Figure 2 ####
## Bar graph of school vs zipcode demographics ##
col1 <- '#929591'
col2 <- '#FFCB05'
col3 <- '#0088C7'
col4 <- '#F7931E'
col5 <- '#1E8449'

median(df$pov - df$econ, na.rm = T)

df %>% 
  group_by(Sector) %>% 
  summarise(black_med_dif = median(BlackAfricanAmericanPCT-black), 
            hisp_med_dif = median(HispanicPCT-hisp), 
            white_med_dif = median(WhitePCT-white), 
            asian_med_dif = median(AsianPCT-asian),
            other_dif = median(AmericanIndianPCT+PacificIslanderPCT+MultiRacePCT - amerind - aapi-multi),
            amerind_med_dif = median(AmericanIndianPCT-amerind),
            aapi_med_dif = median(PacificIslanderPCT-aapi),
            multi_med_dif = median(MultiRacePCT-multi),
            pov_med_dif = median(econ-pov, na.rm = T)
  ) %>% 
  plot_ly(type="bar",
          x= ~black_med_dif,
          y= ~Sector,
          name = 'Percent Black', 
          color = I(col5)) %>% 
  add_trace(x = ~hisp_med_dif, name = 'Percent Hispanic', color = I(col3)) %>% 
  add_trace(x = ~white_med_dif, name = 'Percent White', color = I(col2)) %>% 
  add_trace(x = ~asian_med_dif, name = 'Percent Asian', color = I(col4)) %>% 
  add_trace(x = ~other_dif, name = 'Percent Other', color = I(col1)) %>%
  layout(title="<b>Median Difference between School Demographics<br> and Surrounding Zipcodes</b>",
         yaxis=list(title=""),
         xaxis=list(title="Percentage Points Difference"),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20))


#### Figure 3 ####
## Boxplot comparing schools in neighborhoods below and above median home value ##
city_med <- 184100
df$homes <- ifelse(df$median_home < city_med, 'Zipcode Median Home Value < City Median', 
                   'Zipcode Median Home Value >= City Median')

col1 <- '#0340A8'
col2 <- '#929591'
col3 <- '#0088C7'

df %>% group_by(homes) %>%
  filter(Sector == 'District') %>% 
  plot_ly(x= ~st_att,
          y= ~homes,
          type = 'box',
          name = 'Students Meeting Attendance Metric',
          hoverinfo = 'x',
          color = I(col1)) %>% 
  add_trace(x = ~t_att, name = 'Teachers Meeting Attendance Metric',
            hoverinfo = 'x', color = I(col2)) %>% 
  add_trace(x = ~t_ret, name = 'Teacher Retention',
            hoverinfo = 'x', color = I(col3)) %>% 
  layout(boxmode = 'group',
         title="<b>School Metrics by Neighborhood:</b><br> District Schools Only",
         yaxis=list(title=""),
         xaxis=list(title=""),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20))


#### Supplementary visuals ####

## Demographics of schools in wealthier/poorer neighborhoods ##
df %>% group_by(homes) %>% 
  plot_ly(type="box",
          x= ~BlackAfricanAmericanPCT,
          y= ~homes,
          name = 'Black') %>%
  add_trace(x = ~WhitePCT,
            name = 'White') %>% 
  add_trace(x = ~HispanicPCT,
            name = 'Hispanic') %>% 
  layout(boxmode = 'group',
         title="<b>School Demographics</b>",
         yaxis=list(title=""),
         xaxis=list(title=""),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20))


## Boxplot of charter vs. district, school metrics ##
fig <- df %>% 
  group_by(Sector) %>% 
  plot_ly(type="box",
          x= ~Sector,
          y= ~st_att,
          name = 'Student Attendance') %>% 
  add_trace(y = ~sus_b, name = 'Black Student Suspensions out of Total ') %>%
  layout(boxmode = 'group',
         title="<b>School Metrics</b>",
         yaxis=list(title=""),
         xaxis=list(title=""),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20))
fig

## Boxplot of school demographics, by admission type ##
fig <- df %>% 
  group_by(AdmissionType) %>% 
  plot_ly(type="box",
          x= ~AdmissionType,
          y= ~BlackAfricanAmericanPCT-black,
          name = 'Percent Black') %>% 
  add_trace(y = ~HispanicPCT-hisp, name = 'Percent Hispanic') %>% 
  add_trace(y = ~WhitePCT-white, name = 'Percent White') %>% 
  add_trace(y = ~AsianPCT-asian, name = 'Percent Asian') %>% 
  layout(boxmode = 'group',
         title="<b>School Demographics</b>",
         yaxis=list(title=""),
         xaxis=list(title=""),
         showlegend = TRUE,
         margin=list(l=50,
                     r=10,
                     t=80,
                     b=10,
                     pad=20))
fig

## Scatter plot of median home vs. school metrics ##
fig <- df %>%  plot_ly(
  type = 'scatter',
  x = ~st_att,
  y = ~median_home,
  name = 'Student Attendance') %>% 
  add_trace(x = ~t_att, name = 'Teacher Attendance') %>% 
  add_trace(x = ~t_ret, name = 'Teacher Retention')
fig

## scatter plot of percent Black vs. school metrics ##
fig <- df %>%  plot_ly(
  type = 'scatter',
  x = ~st_att,
  y = ~BlackAfricanAmericanPCT,
  name = 'Student Attendance') %>% 
  add_trace(x = ~t_att, name = 'Teacher Attendance') %>% 
  add_trace(x = ~t_ret, name = 'Teacher Retention')
fig

