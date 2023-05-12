# Title: An Introduction to R Functions

# Purpose : This script is contains the syntax for an introductory workshop on 
#           functions.
#           Sources:
#               -https://swcarpentry.github.io/r-novice-inflammation/02-func-R/
#               -https://r4ds.had.co.nz/functions.html
#               -https://hbctraining.github.io/Intro-to-R/lessons/03_introR-functions-and-arguments.html

# Author: A. Trinidad
# Contact details: ATrinidad@nscr.nl // alexander.trinidad@protonmail.com

# Date script created: Wed Feb 15 09:05:44 2023 
# Date script last modified: Thu Feb 21 10:05:00 2023 

# package dependencies

library(crimedata)
library(here)
library(tidyverse)
library(tidytuesdayR)


# Get source code of a generic function. Using S3 method
# methods(mean)
# getAnywhere(mean.default)


# Function structure
# 
# name_of_function <- function(argument1, argument2) {
#   statements or code that does something
#   return(something)
# }


# Function without arguments
test_function <- function(){
  
  x <- 4 + 5 
  
  return(x)
  
}

# Calculates crime rates per 100000
crime_rates <- function(crime_count, popu_risk){
  crime_rate <- (crime_count / popu_risk) *100000
  return(crime_rate) 
}

# Test the function
crime_rates(400, 3000)

# Min-Max Normalization function

minmax <- function(crime) {
  
  crim_minmax <- (crime - min(crime)) / (max(crime) - min(crime))
  
  return(crim_minmax)
}

# Composing functions

minmax_rates <- function(crime_count, popu_risk){
  my_crime_rates <- crime_rates(crime_count, popu_risk)
  minmax_transf <- minmax(my_crime_rates)
  return(minmax_transf)
}


# Testing our Functions ---------------------------------------------------


# Creates crime counts per imaginary block group for testing purposes 
set.seed(123)
mycrimecount <- rpois(500,  lambda = 2)
myblockgroupid <- seq(1:500)
mypopu <- round(runif(500, 600, 3000))

mycrimdata <- tibble(block_group_id = myblockgroupid,
                     crime_count = mycrimecount, 
                     pop_bg = mypopu)

# Testing if our function works
test <- mycrimdata %>% 
  mutate(c_rates = crime_rates(crime_count, pop_bg),
         minmaxrat = minmax(c_rates),
         minmaxnorm = minmax_rates(crime_count, pop_bg))

# Nesting function calls
minmax(crime_rates(mycrimdata$crime_count, mycrimdata$pop_bg))


# Handling Errors --------------------------------------------------------

# Add some NA values in our crime count variable
crimdataNA <- mycrimdata %>% 
  mutate(crime_count = if_else(crime_count == 3, NA_integer_, crime_count))

# Test our function
head(minmax_rates(crimdataNA[,2], crimdataNA[,3]), 10)

# Modify the functions to allow operating with NA values

crime_rates <- function(crime_count, popu_risk){
    
  crime_rate <- na.omit((crime_count / popu_risk)) * 100000

  return(crime_rate) 
}

minmax_rates(crimdataNA[, 2], crimdataNA[, 3])

crime_rates <- function(crime_count, popu_risk){
  
  crime_rate <- na.omit((crime_count / popu_risk)) * 100000
  
  return(crime_rate) 
}

# Documenting your function. Your future you will thank you... and the rest
# of the community as well. 

crime_rates <- function(crime_count, popu_risk){
  # returns a new numeric vector containing the crime rates per 100000 inhabitants
  
  crime_rate <- na.omit((crime_count / popu_risk)) * 100000
  
  return(crime_rate) 
}



# Defining defaults -------------------------------------------------------

# Not defined
crime_rates <- function(crime_count, popu_risk, inhabitants){
  # returns a new numeric vector contain the crime rates per num of inhabitant
  
  crime_rate <- na.omit((crime_count / popu_risk)) * inhabitants
  
  return(crime_rate) 
}

head(crime_rates(mycrimdata[,2], mycrimdata[,3], 100000), 10)

# Defining inhabitants
crime_rates <- function(crime_count, popu_risk, inhabitants = 100000){
  # returns a new numeric vector contain the crime rates per num of inhabitant
  
  crime_rate <- na.omit((crime_count / popu_risk)) * inhabitants
  
  return(crime_rate) 
}

head(crime_rates(mycrimdata[,2], mycrimdata[,3]), 10)



# Let's get some crime data before ----------------------------------------

# We need to access some Tidy Tuesdays 
tuesdata <- tidytuesdayR::tt_load('2022-12-20')
weather <- tuesdata$weather_forecasts

# Now is your turn
# Activity 1: Create your own function. To transform observed temperature to 
# Celsius


# Crime data
crimesUS <- get_crime_data(years = 2021, 
                           cities = c("Chicago", "Detroit", "Seattle", "Nashville"))
# Want to explore the data
glimpse(weather)
glimpse(crimesUS)


# Capitalizing the city name in homicide data to match it with weather city name
crimes <- crimesUS %>%
  filter(city_name %in% c("Chicago", "Detroit", "Seattle", "Nashville")) %>% 
  mutate(new_date = strftime(date_single, format = "%Y-%m-%d"),
         city_name = str_to_upper(city_name))
  
# Vector with city names from homicides data to match with weather data
crimecity_name <- names(table(crimes[,2]))

# Filtering cities in weather data matching with cities in homicides data
# and filtering the year
weather2021 <- weather %>% 
  mutate(year = format(date, format = "%Y")) %>%  
  filter(city %in% crimecity_name,
         year == "2021") 

# Separate cities data sets

chicrimes <- filter(crimes, city_name == "CHICAGO") %>% 
  mutate(date = as.Date(date_single))
detrocrimes <- filter(crimes, city_name == "DETROIT") %>% 
  mutate(date = as.Date(date_single))
seatcrimes <- filter(crimes, city_name == "SEATTLE") %>% 
  mutate(date = as.Date(date_single))
nashcrimes <- filter(crimes, city_name == "NASHVILLE") %>% 
  mutate(date = as.Date(date_single))


chicweath <- filter(weather2021, city == "CHICAGO") 
detroweath <- filter(weather2021, city == "DETROIT")
seatweath <- filter(weather2021, city == "SEATTLE") 
nashweath <- filter(weather2021, city == "NASHVILLE")

# Dates need to be the same 

chicdates <- unique(chicweath[, 1])
detrodates <- unique(detroweath[, 1])
seadates <- unique(seatweath[, 1])
nashdates <- unique(nashweath[, 1])

all.equal(chicdates, nashdates)

# Unique temperature per day
chicmeantem <- chicweath %>% 
  group_by(date) %>% 
  summarise(meantemp = mean(observed_temp),
            meanprecip = mean(observed_precip))

# Mean temperature as function
meantemp <- function(weathtemp) {
  
  myweathmean <- weathtemp %>% 
    group_by(date) %>% 
    summarise(meantemp = mean(observed_temp),
              meanprecip = mean(observed_precip))
  
  return(myweathmean)
  
}

# Have unique dates with unique temp and precipitations
chicmeanweath <- meantemp(chicweath)
detrmeanweath <- meantemp(detroweath)
seatmeanweath <- meantemp(seatweath)
nashmeanweath <- meantemp(nashweath)


# Aggregating crime data to date and per type of offense
chicacount <- chicrimes %>% 
  group_by(date, offense_against) %>% 
  summarise(count = n()) %>% 
  filter(offense_against %in% c("persons", "property"))

# Function aggregate crime to date

aggre_crime_date <- function(crime_data){
  
  mycrimeselec <- crime_data %>% 
    group_by(date, offense_against) %>% 
    summarise(count = n()) %>% 
    filter(offense_against %in% c("persons", "property"))
  
  return(mycrimeselec)
} 

# Aggregating crime data using the function
chic_crim_dat <- aggre_crime_date(chicrimes)
detr_crim_dat <- aggre_crime_date(detrocrimes)
seat_crim_dat <- aggre_crime_date(seatcrimes)
nash_crim_dat <- aggre_crime_date(nashcrimes)

# Have the offenses and temperature in same data set
chicweath <- left_join(chicmeanweath, chic_crim_dat, by = "date") %>% 
  mutate(year = format(date, format = "%Y"), 
       month = format(date, format = "%m"), 
       day = format(date, format = "%d"))

detrweath <- left_join(detrmeanweath, detr_crim_dat, by = "date") %>% 
  mutate(year = format(date, format = "%Y"), 
         month = format(date, format = "%m"), 
         day = format(date, format = "%d"))

seatweath <- left_join(seatmeanweath, seat_crim_dat, by = "date") %>% 
  mutate(year = format(date, format = "%Y"), 
         month = format(date, format = "%m"), 
         day = format(date, format = "%d"))

nashweath <- left_join(nashmeanweath, seat_crim_dat, by = "date") %>% 
  mutate(year = format(date, format = "%Y"), 
         month = format(date, format = "%m"), 
         day = format(date, format = "%d"))


# Plotting some relations ##### HERE I STOP 

seatweath %>% 
  drop_na() %>% 
  mutate(prop_cri_per = proportions(count)) %>% 
  group_by(month, offense_against) %>% 
  summarise(mo_crim_pro = sum(prop_cri_per),
            mo_crim_cou = sum(count),
            meanmotemp = mean(meantemp)) %>% 
  ggplot() +
  geom_line(aes(x = month, y = mo_crim_cou, group = 1)) +
  facet_wrap(~ offense_against)


# Function for grouping by different groups
crime_prop <- function(crime_data, group1 = NULL, group2 = NULL) {
  
  mydata <- crime_data %>% 
    drop_na() %>% 
    mutate(prop_cri_per = proportions(count)) %>% 
    group_by({{group1}}, {{group2}}) %>% 
    summarise(mo_crim_pro = sum(prop_cri_per),
              mo_crim_cou = sum(count),
              meanmotemp = mean(meantemp)) 
  
  return(mydata)
}


chictest <- crime_prop(chicweath, group1 = month)


# Function generating line graph 
offenses_month <- function(crime_data, group1 = NULL, group2 = NULL, x, y, title = NULL){
  
  
    mydata <- crime_data %>% 
      drop_na() %>% 
      mutate(prop_cri_per = proportions(count)) %>% 
      group_by({{group1}}, {{group2}}) %>% 
      summarise(mo_crim_pro = sum(prop_cri_per),
                mo_crim_cou = sum(count),
                meanmotemp = mean(meantemp)) 
 
  
  mygraph <- ggplot(mydata) +
    geom_line(aes(x = {{x}}, y = {{y}}, group = 1, color = {{group2}}), lwd = 1) +
    facet_wrap(~offense_against) +
    theme(legend.position = "none") +
    ggtitle({{title}})
  
    
  return(mygraph)
  
}

offenses_month(seatweath, group1 = month, group2 = offense_against,
               x = month, y = mo_crim_cou, "Seattle Monthly Offenses Proportion")

offenses_month(chicweath, group1 = month, group2 = offense_against,
               x = month, y = mo_crim_cou, "Chicago Monthly Offenses Proportion")


offenses_month(detrweath, group1 = month, group2 = offense_against,
               x = month, y = mo_crim_cou, "Detroit Monthly Offenses Proportion")

offenses_month(nashweath, group1 = month, group2 = offense_against,
               x = month, y = mo_crim_cou, "Nashville Monthly Offenses Proportion")



# Function generating some correlation graph

chicweath %>% 
  drop_na() %>% 
  mutate(prop_cri_per = proportions(count)) %>% 
  group_by(month, offense_against) %>% 
  summarise(mo_crim_pro = sum(prop_cri_per),
            mo_crim_cou = sum(count),
            meanmotemp = mean(meantemp)) %>%
  ggplot(aes(meanmotemp, mo_crim_cou)) +
  geom_point(color="firebrick") +
  stat_smooth(method="loess", se=FALSE)




cor_off_weath <- function(crime_data, group1 = NULL, group2 = NULL, x, y, 
                           title = NULL, method = "lm", se = FALSE){
  
  
  mydata <- crime_data %>% 
    drop_na() %>% 
    mutate(prop_cri_per = proportions(count)) %>% 
    group_by({{group1}}, {{group2}}) %>% 
    summarise(mo_crim_pro = sum(prop_cri_per),
              mo_crim_cou = sum(count),
              meanmotemp = mean(meantemp),
              meanmoprecip = mean(meanprecip)) 
  
  
  mycorgraph <-  ggplot(data = mydata, aes({{x}}, {{y}})) +
    geom_point(color = "firebrick") +
    facet_wrap(~offense_against) +
    stat_smooth(method = method, se = se) +
    ggtitle({{title}})
  
  
  return(mycorgraph)
  
}


# Checking relationships 

cor_off_weath(chicweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Chicago Offenses and Temperature", 
              se = TRUE)


cor_off_weath(seatweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Seattle Offenses and Temperature", 
              se = TRUE)

cor_off_weath(detrweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Detroit Offenses and Temperature", 
              se = TRUE)

cor_off_weath(nashweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Nashville Offenses and Temperature", 
              se = TRUE)

# Checking a better curve to fit the association

cor_off_weath(chicweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Chicago Offenses and Temperature", method = "loess",
              se = TRUE)


cor_off_weath(seatweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Seattle Offenses and Temperature", method = "loess",
              se = TRUE)

cor_off_weath(detrweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Detroit Offenses and Temperature", method = "loess",
              se = TRUE)

cor_off_weath(nashweath, group1 = month, group2 = offense_against, 
              x = meanmotemp, y = mo_crim_cou, 
              title = "Nashville Offenses and Temperature", method = "loess",
              se = TRUE)

# Pipes from maggrittr are substituted to base R pipes
# formatR::tidy_pipe() 

sessionInfo()

# R version 4.2.2 (2022-10-31 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8   
# [3] LC_MONETARY=English_United Kingdom.utf8 LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.utf8    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] crimedata_0.3.1    tidytuesdayR_1.0.2 forcats_0.5.1      stringr_1.4.0     
# [5] dplyr_1.0.9        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
# [9] tibble_3.1.7       ggplot2_3.3.6      tidyverse_1.3.1    here_1.0.1        
# 
# loaded via a namespace (and not attached):
# [1] lubridate_1.8.0  lattice_0.20-45  assertthat_0.2.1 rprojroot_2.0.3 
# [5] digest_0.6.29    utf8_1.2.2       repr_1.1.6       R6_2.5.1        
# [9] cellranger_1.1.0 backports_1.4.1  reprex_2.0.1     httr_1.4.3      
# [13] pillar_1.7.0     rlang_1.0.2      curl_4.3.2       readxl_1.4.0    
# [17] rstudioapi_0.13  Matrix_1.4-1     labeling_0.4.2   splines_4.2.2   
# [21] selectr_0.4-2    bit_4.0.4        munsell_0.5.0    broom_0.8.0     
# [25] xfun_0.31        compiler_4.2.2   modelr_0.1.8     base64enc_0.1-3 
# [29] pkgconfig_2.0.3  mgcv_1.8-40      htmltools_0.5.4  tidyselect_1.1.2
# [33] fansi_1.0.3      crayon_1.5.1     tzdb_0.3.0       dbplyr_2.2.0    
# [37] withr_2.5.0      grid_4.2.2       nlme_3.1-158     jsonlite_1.8.0  
# [41] gtable_0.3.0     lifecycle_1.0.1  DBI_1.1.3        formatR_1.14    
# [45] magrittr_2.0.3   scales_1.2.0     cli_3.3.0        stringi_1.7.6   
# [49] vroom_1.5.7      farver_2.1.0     fs_1.5.2         skimr_2.1.5     
# [53] xml2_1.3.3       ellipsis_0.3.2   generics_0.1.2   vctrs_0.4.1     
# [57] tools_4.2.2      bit64_4.0.5      glue_1.6.2       hms_1.1.1       
# [61] fastmap_1.1.0    parallel_4.2.2   colorspace_2.0-3 rvest_1.0.2     
# [65] knitr_1.39       haven_2.5.0      usethis_2.1.6
#
#
# The materials used in this workshop are adapted from work that is Copyright
# © Data Carpentry (http://datacarpentry.org/). 
# All Data Carpentry instructional material is made available under
# the Creative Commons Attribution license (CC BY 4.0).
