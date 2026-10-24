################## BA Katharina Mair: data analysis###################################
#Author: Katharina Mair
#Date: Spring 2024
################## setup ################################
Sys.setenv(LANG = "EN")
Sys.setlocale("LC_TIME", "English")
Sys.getlocale(category = "LC_ALL")

if("stargazer" %in% rownames(installed.packages()) == FALSE) {install.packages("stargazer")}
if("readxl" %in% rownames(installed.packages()) == FALSE) {install.packages("readxl")}
if("lfe" %in% rownames(installed.packages()) == FALSE) {install.packages("lfe")}
if("plm" %in% rownames(installed.packages()) == FALSE) {install.packages("plm")}
if("fixest" %in% rownames(installed.packages()) == FALSE) {install.packages("fixest")}
#if("feglm" %in% rownames(installed.packages()) == FALSE) {install.packages("feglm")}
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
if("WikidataR" %in% rownames(installed.packages()) == FALSE) {install.packages("WikidataR")}
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("legislatoR" %in% rownames(installed.packages()) == FALSE) {install.packages("legislatoR")}
if("downloader" %in% rownames(installed.packages()) == FALSE) {install.packages("downloader")}
if("rvest" %in% rownames(installed.packages()) == FALSE) {install.packages("rvest")}
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
if("RCurl" %in% rownames(installed.packages()) == FALSE) {install.packages("RCurl")}
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
if("gtools" %in% rownames(installed.packages()) == FALSE) {install.packages("gtools")}
if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
if("gsubfn" %in% rownames(installed.packages()) == FALSE) {install.packages("gsubfn")}
if("proto" %in% rownames(installed.packages()) == FALSE) {install.packages("proto")}
if("RSQLite" %in% rownames(installed.packages()) == FALSE) {install.packages("RSQLite")}
if("sqldf" %in% rownames(installed.packages()) == FALSE) {install.packages("sqldf")}
if("htm2txt" %in% rownames(installed.packages()) == FALSE) {install.packages("htm2txt")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("pdftools" %in% rownames(installed.packages()) == FALSE) {install.packages("pdftools")}
if("mgsub" %in% rownames(installed.packages()) == FALSE) {install.packages("mgsub")}
if("Unicode" %in% rownames(installed.packages()) == FALSE) {install.packages("Unicode")} 
if("stringi" %in% rownames(installed.packages()) == FALSE) {install.packages("stringi")} 
if("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
if("RSelenium" %in% rownames(installed.packages()) == FALSE) {install.packages("RSelenium")}
if("openxlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("openxlsx")}
if("janitor" %in% rownames(installed.packages()) == FALSE) {install.packages("janitor")}
if("reshape2" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape2")}
if("broom" %in% rownames(installed.packages()) == FALSE) {install.packages("broom")}
if("forecast" %in% rownames(installed.packages()) == FALSE) {install.packages("forecast")}
# Load  packages
library(readxl)
library(stargazer)
library(broom)
library(gtools)
library(lfe)
library(fixest)
library(plm)
library(car)
library(stringr)
library(WikidataR)
library(sqldf)
library(RCurl)
library(XML)
library(rvest)
library(gtools)
library(tidyverse)
library(legislatoR)
library(downloader)
library(rvest)
library(XML)
library(RCurl)
library(XML)
library(gtools)
library(zoo)
library(gsubfn)
library(proto)
library(RSQLite)
library(forecast)
library(sqldf)
library(htm2txt)
library(dplyr)
library(pdftools)
library(tseries)
library(mgsub)
library(Unicode) 
library(stringi) 
#install.packages("rJava", repos="https://rforge.net/", type='source') #in case library(xlsx) gives error
library(xlsx)
library(openxlsx)
library(RSelenium)
library(stringi)
library(ggplot2)
library(mFilter)
library(quantmod)
library(reshape2)
theme_set(theme_classic())

setwd("C:/Users/kathi/OneDrive - Universität Wien/Desktop/Bachelorarbeit_POWI/data/data_analysis")

data_sm <- read_excel("data_SM.xlsx", col_names = TRUE)
data_eps <- read_excel("EPS_allcountries.xlsx", col_names = TRUE)
data_ccpi <- read_excel("CCPI_data_values.xlsx", col_names = TRUE)
################################################################################
####################Chapter 1: Final data cleaning##############################
data_sm$GDP[data_sm$GDP==".."] <- NA
data_sm$trade[data_sm$trade==".."] <- NA
data_sm$fem_parl[data_sm$fem_parl==".."] <- NA
data_sm$renew_use[data_sm$renew_use==".."] <- NA
data_sm$energy_use_GDP[data_sm$energy_use_GDP==".."] <- NA
data_sm <- subset(data_sm, data_sm$year<2021)
table(data_sm$fem_parl, exclude = NULL)
data_sm$year <- as.character(data_sm$year)

data_eps <- data_eps[data_eps$VAR == "EPS",]

#add EPS and CCPI-values to dataframe
data_sm <- sqldf("SELECT data_sm.*, data_eps.Value
                         FROM data_sm LEFT JOIN data_eps
                         ON
                         (data_sm.year = data_eps.Year
                         AND
                         data_sm.country = data_eps.COU)
                         ")
names(data_sm)[names(data_sm) == "Value"] <- "EPS"

data_sm <- sqldf("SELECT data_sm.*, data_ccpi.climate_policy
                         FROM data_sm LEFT JOIN data_ccpi
                         ON
                         (data_sm.year = data_ccpi.years
                         AND
                         data_sm.country = data_ccpi.country_code)
                         ")
data_sm <- sqldf("SELECT data_sm.*, data_ccpi.int_climate_policy
                         FROM data_sm LEFT JOIN data_ccpi
                         ON
                         (data_sm.year = data_ccpi.years
                         AND
                         data_sm.country = data_ccpi.country_code)
                         ")
data_sm <- sqldf("SELECT data_sm.*, data_ccpi.nat_climate_policy
                         FROM data_sm LEFT JOIN data_ccpi
                         ON
                         (data_sm.year = data_ccpi.years
                         AND
                         data_sm.country = data_ccpi.country_code)
                         ")
#change intervall 0 to 1 to percent
data_sm$av_gender_lag <- data_sm$av_gender_lag*100
data_sm$av_gender <- data_sm$av_gender*100
data_sm$nat_climate_policy <- data_sm$nat_climate_policy*100
data_sm$int_climate_policy <- data_sm$int_climate_policy*100
data_sm$GDP_growth <- as.numeric(data_sm$GDP_growth)
data_sm$trade <- as.numeric(data_sm$trade)
data_sm$trade_perc_GDP <- as.numeric(data_sm$trade_perc_GDP)
data_sm$energy_use_cap <- as.numeric(data_sm$energy_use_cap)
data_sm$energy_use_GDP <- as.numeric(data_sm$energy_use_GDP)
data_sm$CO2_cap_territorial <- as.numeric(data_sm$CO2_cap_territorial)
data_sm$natural_res <- as.numeric(data_sm$natural_res)
data_sm$renew_use <- as.numeric(data_sm$renew_use)
data_sm$FDI_net <- as.numeric(data_sm$FDI_net)
data_sm$fem_parl <- as.numeric(data_sm$fem_parl)
data_sm$EPS <- as.numeric(data_sm$EPS)
data_sm$GDP <- as.numeric(data_sm$GDP)
data_sm$CO2_cap_consum <- as.numeric(data_sm$CO2_cap_consum)
data_sm$executive_control <- as.numeric(data_sm$executive_control)
data_sm$federalism <- as.numeric(data_sm$federalism)
data_sm$pop_density <- as.numeric(data_sm$pop_density)
data_sm$cabinet_mean_lr_year <- as.numeric(data_sm$cabinet_mean_lr_year)
data_sm$greens_in_cabinet <- as.numeric(data_sm$greens_in_cabinet)
#manual data check
################################################################################
####################Chapter 2: Calculate averages###############################
#I have weights on each of the variables, added by "*". To calculate the mean, divide by the sum of all shares in yearshare
#if e.g. there is only a switch in the transport ministry and in none of the others, the weights should still sum up to the total number of relevant ministers
# e.g 1 for minister of environment, 1 for energy, 0.25 for transport, and 0,75 for transport = 3
for (i in 1: nrow(data_sm)) {
  data_sm$sum_yearshares[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$yearshare[i]), ";")), na.rm =TRUE)) #sum of weighs
}
#ISCED2011
for (i in 1: nrow(data_sm)) {
  data_sm$sum_highest_ISCED2011[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$highest_ISCED2011[i]), ";"))), na.rm = TRUE)
}
data_sm$av_highest_ISCED2011 <- data_sm$sum_highest_ISCED2011/data_sm$sum_yearshares #divide the sums

#edu in years
for (i in 1: nrow(data_sm)) {
  data_sm$sum_highest_years[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$highest_years[i]), ";"))), na.rm = TRUE)
}
data_sm$av_highest_years <- data_sm$sum_highest_years/data_sm$sum_yearshares #divide the sums

#Left right
for (i in 1: nrow(data_sm)) {
  data_sm$sum_weighed_lr[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$weighed_lr[i]), ";"))), na.rm = TRUE)
}
data_sm$av_weighed_lr <- data_sm$sum_weighed_lr/data_sm$sum_yearshares #divide the sums

#gender average: females (=1), male (=0)
for (i in 1: nrow(data_sm)) {
data_sm$sum_gender[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$gender[i]), ";"))), na.rm = TRUE)
}
data_sm$av_gender <- data_sm$sum_gender/data_sm$sum_yearshares #divide the sums

#environmental protection
for (i in 1: nrow(data_sm)) {
  data_sm$sum_weighed_environmental_protection[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$weighed_environmental_protection[i]), ";"))), na.rm = TRUE)
}
data_sm$av_environmental_protection <- data_sm$sum_weighed_environmental_protection/data_sm$sum_yearshares #divide the sums
data_sm$av_environmental_protection[is.na(data_sm$weighed_environmental_protection)] <- NA
#anti-growth
for (i in 1: nrow(data_sm)) {
  data_sm$sum_anti_growth[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$weighed_anti_growth[i]), ";"))), na.rm = TRUE)
}
data_sm$av_anti_growth <- data_sm$sum_anti_growth/data_sm$sum_yearshares #divide the sums
data_sm$av_anti_growth[is.na(data_sm$weighed_anti_growth)] <- NA
#free market
for (i in 1: nrow(data_sm)) {
  data_sm$sum_free_market[i] <- sum(as.numeric(unlist(strsplit(as.character(data_sm$weighed_free_market[i]), ";"))), na.rm = TRUE)
}
data_sm$av_free_market <- data_sm$sum_free_market/data_sm$sum_yearshares #divide the sums
data_sm$av_free_market[is.na(data_sm$weighed_free_market)] <- NA

data_sm$sum_free_market <- NULL
data_sm$sum_anti_growth <- NULL
data_sm$sum_weighed_environmental_protection <- NULL
data_sm$sum_gender <- NULL
data_sm$sum_weighed_lr<- NULL
data_sm$sum_highest_years<- NULL
data_sm$sum_highest_ISCED2011 <- NULL
table(as.character(data_sm$av_gender), exclude=NULL)
hist(data_sm$av_gender)
table(data_sm$av_edu_years, exclude=NULL)
table(data_sm$av_edu_isced, exclude=NULL)
################################################################################
####################Chapter 3: Check variation in variables######################
data_sm$year <- as.numeric(data_sm$year)
#EPS
#plot(data_sm$EPS[data_sm$country=="AUT"])
#plot(data_sm$EPS[data_sm$country=="DEU"])
#plot(data_sm$EPS[data_sm$country=="FIN"])
#plot(data_sm$EPS[data_sm$country=="FRA"])
#plot(data_sm$EPS[data_sm$country=="NLD"])
#plot(data_sm$EPS[data_sm$country=="CHE"])
#education
#plot(data_sm$av_highest_years[data_sm$country=="AUT"])
#plot(data_sm$av_highest_years[data_sm$country=="DEU"])
#plot(data_sm$av_highest_years[data_sm$country=="FIN"])
#plot(data_sm$av_highest_years[data_sm$country=="FRA"])
#plot(data_sm$av_highest_years[data_sm$country=="NLD"])
#plot(data_sm$av_highest_years[data_sm$country=="CHE"])
#summary(data_sm$EPS)
#hist(data_sm$GDP)
#hist(data_sm$trade)
#hist(data_sm$av_gender)
#hist(data_sm$fem_parl)
#hist(data_sm$CO2_cap_consum)

################################################################################
####################Chapter 4: Calculate lags###########################
#delete variables
data_sm$sum_gender <- NULL
data_sm$sum_highest_ISCED2011 <- NULL
data_sm$sum_highest_years <- NULL
data_sm$gender <- NULL
data_sm$highest_years <- NULL
data_sm$highest_ISCED2011 <- NULL
data_sm$prof_org_edu_type <- NULL
data_sm$yearshare <- NULL
data_sm$sum_yearshares <- NULL
#lag all variables:
#av_gender_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$av_gender_lag[i]=data_sm$av_gender[(i-1)]
  } 
  else {
    data_sm$av_gender_lag[i]=NA
  }
}

#av_highest_ISCED2011_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$av_highest_ISCED2011_lag[i]=data_sm$av_highest_ISCED2011[(i-1)]
  } 
  else {
    data_sm$av_highest_ISCED2011_lag[i]=NA
  }
}
#fem_parl_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$fem_parl_lag[i]=data_sm$fem_parl[(i-1)]
  } 
  else {
    data_sm$fem_parl_lag[i]=NA
  }
}
#CO2_cap_territorial_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$CO2_cap_territorial_lag[i]=data_sm$CO2_cap_territorial[(i-1)]
  } 
  else {
    data_sm$CO2_cap_territorial_lag[i]=NA
  }
}
#GDP_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$GDP_lag[i]=data_sm$GDP[(i-1)]
  } 
  else {
    data_sm$GDP_lag[i]=NA 
  }
}
#trade_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$trade_lag[i]=data_sm$trade[(i-1)]
  } 
  else {
    data_sm$trade_lag[i]=NA 
  }
}
#GDP_growth_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$GDP_growth_lag[i]=data_sm$GDP_growth[(i-1)]
  } 
  else {
    data_sm$GDP_growth_lag[i]=NA 
  }
}
#FDI_net_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$FDI_net_lag[i]=data_sm$FDI_net[(i-1)]
  } 
  else {
    data_sm$FDI_net_lag[i]=NA 
  }
}
#energy_use_cap_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$energy_use_cap_lag[i]=data_sm$energy_use_cap[(i-1)]
  } 
  else {
    data_sm$energy_use_cap_lag[i]=NA 
  }
}
#renew_use_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$renew_use_lag[i]=data_sm$renew_use[(i-1)]
  } 
  else {
    data_sm$renew_use_lag[i]=NA 
  }
}
#av_highest_years_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$av_highest_years_lag[i]=data_sm$av_highest_years[(i-1)]
  } 
  else {
    data_sm$av_highest_years_lag[i]=NA 
  }
}
#natural_res_lag
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$natural_res_lag[i]=data_sm$natural_res[(i-1)]
  } 
  else {
    data_sm$natural_res_lag[i]=NA 
  }
}
#left right
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$av_weighed_lr_lag[i]=data_sm$av_weighed_lr[(i-1)]
  } 
  else {
    data_sm$av_weighed_lr_lag[i]=NA 
  }
}
#cabinet_mean
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$cabinet_lr_lag[i]=data_sm$cabinet_mean_lr_year[(i-1)]
  } 
  else {
    data_sm$cabinet_lr_lag[i]=NA 
  }
}
#greens in cabinet
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$greens_cabinet_lag[i]=data_sm$greens_in_cabinet[(i-1)]
  } 
  else {
    data_sm$greens_cabinet_lag[i]=NA 
  }
}
#population density
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$pop_density_lag[i]=data_sm$pop_density[(i-1)]
  } 
  else {
    data_sm$pop_density_lag[i]=NA 
  }
}
#executive_control
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$executive_control_lag[i]=data_sm$executive_control[(i-1)]
  } 
  else {
    data_sm$executive_control_lag[i]=NA 
  }
}
#environmental_protection
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$environmental_protection_lag[i]=data_sm$av_environmental_protection[(i-1)]
  } 
  else {
    data_sm$environmental_protection_lag[i]=NA 
  }
}
#anti growth
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$anti_growth_lag[i]=data_sm$av_anti_growth[(i-1)]
  } 
  else {
    data_sm$anti_growth_lag[i]=NA 
  }
}
#free market
for(i in 2:nrow(data_sm)){
  if(data_sm$country[i]==data_sm$country[(i-1)]){
    data_sm$free_market_lag[i]=data_sm$av_free_market[(i-1)]
  } 
  else {
    data_sm$free_market_lag[i]=NA 
  }
}

###############################################################################
####################Chapter 5: Check for autokorrelation####################
#AUT
plot.ts(data_sm$EPS[data_sm$country =="AUT"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="AUT"]) # Empirical Autocorrelation #6-7 lags
Pacf(data_sm$EPS[data_sm$country =="AUT"]) # Partial Autocorrelation
#DEU
plot.ts(data_sm$EPS[data_sm$country =="DEU"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="DEU"]) # Empirical Autocorrelation #6 lags
Pacf(data_sm$EPS[data_sm$country =="DEU"]) # Partial Autocorrelation
#NLD
plot.ts(data_sm$EPS[data_sm$country =="NLD"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="NLD"]) # Empirical Autocorrelation #6 lags
Pacf(data_sm$EPS[data_sm$country =="NLD"]) # Partial Autocorrelation
#FRA
plot.ts(data_sm$EPS[data_sm$country =="FRA"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="FRA"]) # Empirical Autocorrelation #6-7 lags
Pacf(data_sm$EPS[data_sm$country =="FRA"]) # Partial Autocorrelation
#CHE
plot.ts(data_sm$EPS[data_sm$country =="CHE"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="CHE"]) # Empirical Autocorrelation #6 lags
Pacf(data_sm$EPS[data_sm$country =="CHE"]) # Partial Autocorrelation
#FIN
plot.ts(data_sm$EPS[data_sm$country =="FIN"]) # plot over time
Acf(data_sm$EPS[data_sm$country =="FIN"]) # Empirical Autocorrelation #6-7 lags
Pacf(data_sm$EPS[data_sm$country =="FIN"]) # Partial Autocorrelation

###############################################################################
####################Chapter 6: Descriptives##########################

data <- data.frame(year= data_sm$year, country = data_sm$country, EPS = data_sm$EPS,
                   GDP_lag = data_sm$GDP_lag,
                   av_gender_lag = data_sm$av_gender_lag,
                   av_highest_ISCED2011_lag = data_sm$av_highest_ISCED2011_lag,
                   av_highest_years_lag = data_sm$av_highest_years_lag,
                   trade_lag = data_sm$trade_lag,
                   fem_parl_lag = data_sm$fem_parl_lag,
                   CO2_cap_territorial_lag = data_sm$CO2_cap_territorial_lag,
                   GDP_growth_lag = data_sm$GDP_growth_lag,
                   FDI_net_lag = data_sm$FDI_net_lag,
                   energy_use_cap_lag = data_sm$energy_use_cap_lag,
                   renew_use_lag = data_sm$renew_use_lag,
                   natural_res_lag = data_sm$natural_res_lag,
                   av_weighed_lr_lag = data_sm$av_weighed_lr_lag,
                   cabinet_lr_lag = data_sm$cabinet_lr_lag,
                   greens_cabinet_lag = data_sm$greens_cabinet_lag,
                   pop_density_lag = data_sm$pop_density_lag,
                   environmental_protection_lag = data_sm$environmental_protection_lag,
                   anti_growth_lag = data_sm$anti_growth_lag,
                   free_market_lag = data_sm$free_market_lag,
                   federalism =data_sm$federalism,
                   executive_control_lag = data_sm$executive_control_lag
)
data <- data[data$year > 1989,]

#####PLOTS AV_GENDER
library(ggplot2)

##Mean average gender by year
mean_av_gender <- aggregate(av_gender ~ year, data = data_sm, FUN = mean)

plot_female_ministers <- ggplot(data = mean_av_gender, aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Average Proportion of Women holding Environmental-Policy Ministerial Portfolios in the Sample", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank())  # Centered title and removed x-axis label

#ggsave("plot_share_female_ministers.jpg", plot = plot_female_ministers, width = 10, height = 6, dpi = 300)

#AUT
ggplot(data = data_sm[data_sm$country == "AUT",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers Austria", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
#DEU
ggplot(data = data_sm[data_sm$country == "DEU",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers Germany", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
#CHE
ggplot(data = data_sm[data_sm$country == "CHE",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers Switzerland", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
data_che <- data_sm[data_sm$country == "CHE",]
table(data_che$av_gender)
#FIN
ggplot(data = data_sm[data_sm$country == "FIN",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers Finland", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
#FRA
ggplot(data = data_sm[data_sm$country == "FRA",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers France", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
#NLD
ggplot(data = data_sm[data_sm$country == "FRA",], aes(x = year, y = av_gender)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = function(x) abs(x)) +
  labs(y = "Share of women in %", title = "Share of Female Environmental-Policy Ministers Netherlands", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


#####PLOTS EDUCATION
##Mean average education by year
mean_av_education_years <- aggregate(av_highest_years ~ year, data = data_sm, FUN = mean)

plot_average_education_years <- ggplot(data = mean_av_education_years, aes(x = year, y = av_highest_years)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(15, 22, by = 0.25)) +
  labs(y = "Level of Education (min. years required)", title = "Average Educational Attainment of Environmental-Policy Ministers (in years)") +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 2),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 20)))  # Adjust the margin here

#ggsave("plot_average_education_years.jpg", plot = plot_average_education_years, width = 10, height = 6, dpi = 300)

##Mean average ISCED2011
mean_av_education_ISCED <- aggregate(av_highest_ISCED2011 ~ year, data = data_sm, FUN = mean)

plot_average_education_ISCED <- ggplot(data = mean_av_education_ISCED, aes(x = year, y = av_highest_ISCED2011)) +
  geom_line(color = "#526024", size = 1.25) +  # Line plot with pastel blue color and thicker line
  geom_point(color = "#526024", size = 2.5) +  # Add points with a lighter shade of blue and larger size
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 8, by = 0.25), labels = function(x) abs(x)) +
  labs(y = "Level of Education (ISCED 2011)", title = "Average Educational Attainment of Environmental-Policy Ministers (ISCED 2011 classification) in the Sample", size = 4) +  # Axis labels and title
  theme_minimal() +  # Minimal theme
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),)  # Centered title and removed x-axis label

#ggsave("plot_average_education_ISCED.jpg", plot = plot_average_education_ISCED, width = 10, height = 6, dpi = 300)
#descriptive summary

library(stargazer)
class(data$greens_cabinet_lag)
#baseline variables
summary_stats <- function(var_name, source) {
  c(
    "Min." = round(min(data[[var_name]], na.rm = TRUE), 2),
    "Max." = round(max(data[[var_name]], na.rm = TRUE), 2),
    "Mean" = round(mean(data[[var_name]], na.rm = TRUE), 2),
    "SD" = round(sd(data[[var_name]], na.rm = TRUE), 2),
    "Source" = source
  )
}

vars <- c("EPS", "av_gender_lag","av_highest_ISCED2011_lag", 
          "environmental_protection_lag",  "GDP_lag", "trade_lag", 
          "greens_cabinet_lag", "pop_density_lag" )

sources <- c("OECD, 2016", "Nyrup and Bramwell, 2020; Ennser-Jedenastik et al., 2024", "Ennser-Jedenastik et al., 2024", "Lehmann et al., 2023", 
             "IMF, 2023", "World Bank and OECD, 2024",
             "Lehmann et al., 2023", "FAO and World Bank, 2024")

summary_list <- Map(summary_stats, vars, sources)
summary_table <- do.call(rbind, summary_list)
rownames(summary_table) <- c("Environmental Policy Stringency", "Share of female environmental-policy ministers",
                             "Average educational attainment", "Party support for environmental protection",
                              "GDP per capita", "Trade", "Green party in cabinet",
                             "Population density")

#stargazer(summary_table, type = "text", title = "Descriptive Summary - Variables used in Baseline analysis", out = "descriptive_summary.html")

####PLOTS EPS
mean(data$EPS)
sd(data$EPS)
min(data$EPS)
max(data$EPS)
#plot(data$EPS[data$country == "AUT"])
#plot(data$EPS[data$country == "DEU"])
#plot(data$EPS[data$country == "FIN"])
#plot(data$EPS[data$country == "FRA"])
#plot(data$EPS[data$country == "NLD"])
#plot(data$EPS[data$country == "CHE"])

# Combine data for all countries
mean_EPS <- aggregate(EPS ~ year, data = data, FUN = mean)
mean_EPS <- as.data.frame(mean_EPS)
mean_EPS$country <- "Mean EPS"
data_EPS <- subset(data, select = c("year", "EPS", "country"))
combined_data <- rbind(data_EPS, mean_EPS)

group.colors <- c("AUT" = "#a0534f", "DEU" = "#bb7647", "NLD" = "#cca767", "FIN" = "#668fae", "FRA" = "#7c9e80", "CHE" = "#526024", "Mean EPS" = "#000000")

table(combined_data$country)
# Plot data for all countries and mean EPS
plot_EPS <- ggplot(data = combined_data, aes(x = year, y = EPS, color = country)) +
  geom_line(aes(group = country), size = 0.9) +  # thinner lines
  geom_point(aes(group = country), size = 1.5) +
 labs(y = "EPS-Index",title = "Environmental Policy Stringency Index per Year", size = 4) + 
  scale_x_continuous(breaks = seq(1990, 2020, by = 2)) +
  scale_y_continuous(breaks = seq(0, 6, by = 0.5), labels = function(x) abs(x)) +  # Turn y-axis labels to horizontal
  scale_color_manual(values = group.colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.y = element_text(angle = 0, hjust = 1),  # Horizontal y-axis labels
        panel.grid.major.y = element_line(color = "gray", size = 0.2),  # Horizontal grid lines
        panel.grid.minor = element_blank(),  # remove minor grid lines
        legend.title = element_blank())  # Remove legend title/header

#ggsave("plot_EPS.jpg", plot = plot_EPS, width = 10, height = 6, dpi = 300)

###############################################################################
####################Chapter 7: Panel Models###########################
data_panel <- data.frame(year= data_sm$year, country = data_sm$country, EPS = data_sm$EPS,
                   GDP_lag = data_sm$GDP_lag,
                   av_gender_lag = data_sm$av_gender_lag,
                   av_highest_ISCED2011_lag = data_sm$av_highest_ISCED2011_lag,
                   av_highest_years_lag = data_sm$av_highest_years_lag,
                   trade_lag = data_sm$trade_lag,
                   fem_parl_lag = data_sm$fem_parl_lag,
                   greens_cabinet_lag = data_sm$greens_cabinet_lag,
                   pop_density_lag = data_sm$pop_density_lag,
                   environmental_protection_lag = data_sm$environmental_protection_lag 
)

data_panel <- na.omit(data_panel)
#fixed effects panel model with Driscoll and Kraay standard errors and all independent variables lags by 1 year#
#add lags
fixed_dkse_no_lags <- plm(EPS ~ av_gender_lag+log(GDP_lag)+trade_lag+fem_parl_lag+
                        CO2_cap_territorial_lag+GDP_growth_lag+ FDI_net_lag+energy_use_cap_lag+renew_use_lag+
                        natural_res_lag+av_highest_ISCED2011_lag +environmental_protection_lag+free_market_lag+anti_growth_lag+ 
                        federalism+pop_density_lag+executive_control_lag+
                        greens_cabinet_lag+cabinet_lr_lag+av_weighed_lr_lag,
                      data = data,
                      model = "within",  # Modelltyp "fixed"
                      index = c("country", "year"),  # ID-Variablen für Panels und Zeitpunkte
                      vcov = "DK")  # Verwendung von DKSE

summary(fixed_dkse_no_lags)
pwartest(fixed_dkse_no_lags) #autocorrelation present without lags, acf function suggest 6 lags

fixed_dkse_all_lags <- plm(EPS ~ av_gender_lag+log(GDP_lag)+trade_lag+fem_parl_lag+
                             CO2_cap_territorial_lag+GDP_growth_lag+ FDI_net_lag+energy_use_cap_lag+renew_use_lag+
                             natural_res_lag+av_highest_ISCED2011_lag +environmental_protection_lag+free_market_lag+anti_growth_lag+ 
                             federalism+pop_density_lag+executive_control_lag+
                             greens_cabinet_lag+cabinet_lr_lag+av_weighed_lr_lag+lag(EPS, 1)+lag(EPS, 2)+lag(EPS, 3)+lag(EPS, 4)+
                             lag(EPS, 5)+lag(EPS, 6),
                           data = data,
                           model = "within",  # model fixed
                           index = c("country", "year"),  # id-variables
                           vcov = "DK")  #using DKSE

summary(fixed_dkse_all_lags)
pwartest(fixed_dkse_all_lags) #still autokorrelation with 6 lags (as suggest by acf graphs) at the 5 % level
#however, adding more lags does not lower the p-value (actually increases it), therefore 6 lags seem to be the best fit in this case
#####models 1 & 2
#model 1: av_gender
fixed_dkse_model1 <- plm(EPS ~ av_gender_lag+
                           environmental_protection_lag+greens_cabinet_lag+
                           log(GDP_lag)+trade_lag+
                           log(pop_density_lag)+
                           lag(EPS, 1L)+lag(EPS, 2L)+lag(EPS, 3L)+lag(EPS, 4L)+
                           lag(EPS, 5L)+lag(EPS, 6L),
                         data = data_panel,
                         model = "within",  
                         index = c("country", "year"),  
                         vcov = "DK")  
summary(fixed_dkse_model1)
pwartest(fixed_dkse_model1) #Similarly, autocorrelation remains an issue despite 
#including up to ten lags (p-value doesn't decrease by adding more lags)
#model 2: education 
fixed_dkse_model2 <- plm(EPS ~ av_highest_ISCED2011_lag+environmental_protection_lag+
                           greens_cabinet_lag+log(GDP_lag)+trade_lag+
                           log(pop_density_lag)+lag(EPS, 1L)+lag(EPS, 2L)+lag(EPS, 3L)+
                           lag(EPS, 4L)+lag(EPS, 5L)+lag(EPS, 6L),
                         data = data_panel,
                         model = "within",  
                         index = c("country", "year"),   
                         vcov = "DK") 

summary(fixed_dkse_model2)
pwartest(fixed_dkse_model2)#Similarly, autocorrelation remains an issue despite 
#including up to ten lags (p-value doesn't decrease by adding more lags)

#model 3: av_gender + education
fixed_dkse_model3 <- plm(EPS ~ av_gender_lag+av_highest_ISCED2011_lag+
                           environmental_protection_lag+
                           greens_cabinet_lag+log(GDP_lag)+trade_lag+
                           log(pop_density_lag)+lag(EPS, 1L)+lag(EPS, 2L)+lag(EPS, 3L)+
                           lag(EPS, 4L)+lag(EPS, 5L)+lag(EPS, 6L),
                         data = data_panel,
                         model = "within",  
                         index = c("country", "year"),  
                         vcov = "DK")  

summary(fixed_dkse_model3)
pwartest(fixed_dkse_model3)#Similarly, autocorrelation remains an issue despite 
#including up to ten lags (p-value doesn't decrease by adding more lags)

#check assumptions:
beta.hat.4 <- coef(fixed_dkse_model3)
e.hat_4 <- residuals(fixed_dkse_model3)
y.hat_4 <- fitted(fixed_dkse_model3)
X_4 <- model.matrix(fixed_dkse_model3)
model_frame <- model.frame(fixed_dkse_model3)
y_4 <- model_frame$EPS
y_4 <- as.character(y_4)
#FOC: linear unabhängig
crossprod(X_4,e.hat_4)#FOC fulfilled!
#mean of errors
sum(e.hat_4)
summary(e.hat_4) #checked
e.hat_4 <- as.character(e.hat_4)
y.hat_4 <- as.character(y.hat_4)
#heteroskedasticity - nonlinearity
plot(y.hat_4, e.hat_4) #checked
#model fit
plot(y.hat_4,y_4)
y_4 <- as.numeric(y_4)
y.hat_4 <- as.numeric(y.hat_4)
abline(lm(y_4~y.hat_4)) #checked
#distribution of errors
e.hat_4 <- as.numeric(e.hat_4)
hist(e.hat_4,breaks=30) #checked

#display all models
models_list <- list(fixed_dkse_model1, fixed_dkse_model2, fixed_dkse_model3)

table_html <- stargazer(models_list, type = "html", title = "Fixed Effects Linear Regression - Baseline Results",
                        covariate.labels = c("Women environmental-policy ministers",
                                             "Educational attainment environmental-policy ministers",
                                             "Party environmental protection",
                                             "Green party in cabinet",
                                             "GDP per capita (log)",
                                             "Trade",
                                             "Population density (log)",
                                             "EPS (Lag 1)","EPS (Lag 2)","EPS (Lag 3)","EPS (Lag 4)", "EPS (Lag 5)", "EPS (Lag 6)"),
                        dep.var.labels = "Environmental Policy Stringency Index (EPS)")
table_html <- gsub("<body>", "<body style='font-size: 10px;'>", table_html)
#writeLines(table_html, "table_regression_model_baseline.html")
###############################################################################
####################Chapter 8: Robustness tests#################################
data_panel$interaction_variable <- data_panel$av_gender_lag * data_panel$fem_parl_lag

fixed_dkse_model_fem_parl <- plm(EPS ~ av_gender_lag+av_highest_ISCED2011_lag+fem_parl_lag+
                           environmental_protection_lag+
                           greens_cabinet_lag+log(GDP_lag)+trade_lag+
                           log(pop_density_lag)+lag(EPS, 1L)+lag(EPS, 2L)+lag(EPS, 3L)+
                           lag(EPS, 4L)+lag(EPS, 5L)+lag(EPS, 6L),
                         data = data_panel,
                         model = "within", 
                         index = c("country", "year"),  
                         vcov = "DK") 

summary(fixed_dkse_model_fem_parl)
pwartest(fixed_dkse_model_fem_parl)

fixed_dkse_model_fem_parl_interaction <- plm(EPS ~ av_gender_lag+av_highest_ISCED2011_lag+fem_parl_lag+
                                    interaction_variable + environmental_protection_lag+
                                   greens_cabinet_lag+log(GDP_lag)+trade_lag+
                                   log(pop_density_lag)+lag(EPS, 1L)+lag(EPS, 2L)+lag(EPS, 3L)+
                                   lag(EPS, 4L)+lag(EPS, 5L)+lag(EPS, 6L),
                                 data = data_panel,
                                 model = "within",  
                                 index = c("country", "year"),  
                                 vcov = "DK")  

summary(fixed_dkse_model_fem_parl_interaction)

models_list <- list(fixed_dkse_model_fem_parl, fixed_dkse_model_fem_parl_interaction)

table_html <- stargazer(models_list, type = "html", title = "Fixed Effects Linear Regression - Women in Parliament",
                        covariate.labels = c("Women environmental-policy ministers",
                                             "Educational attainment environmental-policy ministers",
                                             "Women in parliament",
                                             "Women environmental-policy ministers x Women in parliament",
                                             "Party environmental protection",
                                             "Green party in cabinet",
                                             "GDP per capita (log)",
                                             "Trade",
                                             "Population density (log)",
                                             "EPS (Lag 1)","EPS (Lag 2)","EPS (Lag 3)","EPS (Lag 4)", "EPS (Lag 5)", "EPS (Lag 6)"),
                        dep.var.labels = "Environmental Policy Stringency Index (EPS)")
table_html <- gsub("<body>", "<body style='font-size: 10px;'>", table_html)
#writeLines(table_html, "table_regression_model_fem_parl_interaction.html")


#F test for the joint significance
linearHypothesis(fixed_dkse_model_fem_parl_interaction, c("av_gender_lag=0", "av_gender_lag:fem_parl_lag=0"), white.adjust = "hc1", test=c( "F"))
#we can reject the null hypothesis that H0= beta_av_gender_lag=av_gender_lag*fem_parl_lag=0 at a 1 % level-> jointly significant

#marginal effect of female environmental-policy ministers in dependence on the values of females in parliament
#where is marginal effect = 0?
#coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag"] + coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag:fem_parl_lag"] * data_panel$fem_parl_lag = 0
coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag"]/-coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag:fem_parl_lag"] #26.40699
#marginal effect at mean
mean(data$fem_parl_lag, na.rm = TRUE)#28.75206
coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag"] + coef(fixed_dkse_model_fem_parl_interaction)["av_gender_lag:fem_parl_lag"]*28.75206 #-0.0005658553

#using climate_policy, international_climate_policy, and national_climate_poicy
data_panel_CCPI <- data.frame(year= data_sm$year, country = data_sm$country, EPS = data_sm$EPS, climate_policy = data_sm$climate_policy,
                         int_climate_policy = data_sm$int_climate_policy,
                         nat_climate_policy=data_sm$nat_climate_policy,
                          GDP_lag = data_sm$GDP_lag,
                         av_gender_lag = data_sm$av_gender_lag,
                         av_highest_ISCED2011_lag = data_sm$av_highest_ISCED2011_lag,
                         av_highest_years_lag = data_sm$av_highest_years_lag,
                         trade_lag = data_sm$trade_lag,
                         fem_parl_lag = data_sm$fem_parl_lag,
                         greens_cabinet_lag = data_sm$greens_cabinet_lag,
                         pop_density_lag = data_sm$pop_density_lag,
                         environmental_protection_lag = data_sm$environmental_protection_lag 
)
data_panel_CCPI <- na.omit(data_panel_CCPI)
data_panel_CCPI$interaction_variable <- data_panel_CCPI$av_gender_lag * data_panel_CCPI$fem_parl_lag
cor(data_panel_CCPI$EPS, data_panel_CCPI$climate_policy) # 0.3403396 (overall)

#robustness tests, additional descriptives
summary_stats <- function(var_name, source) {
  c(
    "Min." = round(min(data_panel_CCPI[[var_name]], na.rm = TRUE), 2),
    "Max." = round(max(data_panel_CCPI[[var_name]], na.rm = TRUE), 2),
    "Mean" = round(mean(data_panel_CCPI[[var_name]], na.rm = TRUE), 2),
    "SD" = round(sd(data_panel_CCPI[[var_name]], na.rm = TRUE), 2),
    "Source" = source
  )
}

vars <- c("climate_policy","nat_climate_policy", "int_climate_policy" ,"fem_parl_lag")

sources <- c("Burck et al., 2019","Burck et al., 2019","Burck et al., 2019", "Inter-Parliamentary Union, 2024")

summary_list <- Map(summary_stats, vars, sources)
summary_table <- do.call(rbind, summary_list)
summary_table <- as.data.frame(summary_table)
rownames(summary_table) <- c("CCPI-climate policy", "CCPI-national climate policy", "CCPI-international climate policy", "Share of women in parliament")
#use femparl_from data_panel dataframe instead
summary_table$Min.[summary_table$Source =="Inter-Parliamentary Union, 2024"] <- round(min(data$fem_parl, na.rm = TRUE), 2)
summary_table$Max.[summary_table$Source =="Inter-Parliamentary Union, 2024"] <- round(max(data$fem_parl, na.rm = TRUE), 2)
summary_table$Mean[summary_table$Source =="Inter-Parliamentary Union, 2024"] <- round(mean(data$fem_parl, na.rm = TRUE), 2)
summary_table$SD[summary_table$Source =="Inter-Parliamentary Union, 2024"] <- round(sd(data$fem_parl, na.rm = TRUE), 2)
summary_table <- as.matrix(summary_table)
stargazer(summary_table, type = "text", title = "Descriptive Summary - Variables used for Robustness Tests", out = "descriptive_summary_robustness.html")

#AUT
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="AUT"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="AUT"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="AUT"]) # Empirical Autocorrelation #0 lags
#DEU
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="DEU"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="DEU"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="DEU"]) # Empirical Autocorrelation #0 lags
#NLD
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="NLD"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="NLD"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="NLD"]) # Empirical Autocorrelation #0 lags
#FRA
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="FRA"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="FRA"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="FRA"]) # Empirical Autocorrelation #0 lags
#CHE
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="CHE"]) # Empirical Autocorrelation #1 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="CHE"]) # Empirical Autocorrelation #1 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="CHE"]) # Empirical Autocorrelation #0 lags
#FIN
Acf(data_panel_CCPI$climate_policy[data_panel_CCPI$country =="FIN"]) # Empirical Autocorrelation #1 lags
Acf(data_panel_CCPI$nat_climate_policy[data_panel_CCPI$country =="FIN"]) # Empirical Autocorrelation #0 lags
Acf(data_panel_CCPI$int_climate_policy[data_panel_CCPI$country =="FIN"]) # Empirical Autocorrelation #0 lags

#model 3, and without interaction: 
#climate_policy
fixed_dkse_model3_climate_policy <- plm(climate_policy ~ av_gender_lag + av_highest_ISCED2011_lag +
                                          environmental_protection_lag + greens_cabinet_lag +
                                          log(GDP_lag) + trade_lag + log(pop_density_lag) +
                                          lag(climate_policy, 1L),
                                            data = data_panel_CCPI,
                                            model = "within",  
                                            index = c("country", "year"),  
                                            vcov = "DK")  

summary(fixed_dkse_model3_climate_policy)
pwartest(fixed_dkse_model3_climate_policy) 

fixed_dkse_fem_interaction_climate_policy <- plm(climate_policy ~ av_gender_lag+ 
                                          av_highest_ISCED2011_lag+
                                            fem_parl_lag +interaction_variable + 
                                          environmental_protection_lag + greens_cabinet_lag +
                                          log(GDP_lag) + trade_lag + log(pop_density_lag) +
                                          lag(climate_policy, 1L),
                                        data = data_panel_CCPI,
                                        model = "within",  
                                        index = c("country", "year"),  
                                        vcov = "DK")  

summary(fixed_dkse_fem_interaction_climate_policy)
pwartest(fixed_dkse_fem_interaction_climate_policy)#no serial correlation after adding 1 lag
#international_climate_policy
fixed_dkse_model3_int_climate_policy <- plm(int_climate_policy ~ av_gender_lag + av_highest_ISCED2011_lag +
                                              environmental_protection_lag + greens_cabinet_lag +
                                              log(GDP_lag) + trade_lag + log(pop_density_lag)+
                                              lag(int_climate_policy, 1L)+lag(int_climate_policy, 2L),
                         data = data_panel_CCPI,
                         model = "within",  
                         index = c("country", "year"),  
                         vcov = "DK")  

summary(fixed_dkse_model3_int_climate_policy)
pwartest(fixed_dkse_model3_int_climate_policy) #no serial correlation after adding 2 lags

fixed_dkse_fem_interaction_int_climate_policy <- plm(int_climate_policy ~ av_gender_lag+ 
                                              av_highest_ISCED2011_lag+
                                                fem_parl_lag +interaction_variable +
                                              environmental_protection_lag + greens_cabinet_lag +
                                              log(GDP_lag) + trade_lag + log(pop_density_lag)+
                                              lag(int_climate_policy, 1L)+lag(int_climate_policy, 2L),
                                            data = data_panel_CCPI,
                                            model = "within",  
                                            index = c("country", "year"),  
                                            vcov = "DK")  

summary(fixed_dkse_fem_interaction_int_climate_policy)
pwartest(fixed_dkse_fem_interaction_int_climate_policy) #no serial correlation after adding 2 lags

#nat_climate_policy
fixed_dkse_model3_nat_climate_policy <- plm(nat_climate_policy ~ av_gender_lag + av_highest_ISCED2011_lag +
                                              environmental_protection_lag + greens_cabinet_lag +
                                              log(GDP_lag) + trade_lag + log(pop_density_lag) +lag(nat_climate_policy, 1L),
                                            data = data_panel_CCPI,
                                            model = "within",  
                                            index = c("country", "year"),  
                                            vcov = "DK")  

summary(fixed_dkse_model3_nat_climate_policy)
pwartest(fixed_dkse_model3_nat_climate_policy) #no serial correlation after adding 1 lag
fixed_dkse_fem_interaction_nat_climate_policy <- plm(nat_climate_policy ~ av_gender_lag+
                                              av_highest_ISCED2011_lag+
                                                fem_parl_lag +interaction_variable + 
                                              environmental_protection_lag + greens_cabinet_lag +
                                              log(GDP_lag) + trade_lag + log(pop_density_lag) +lag(nat_climate_policy, 1L),
                                            data = data_panel_CCPI,
                                            model = "within",  
                                            index = c("country", "year"),  
                                            vcov = "DK")  

summary(fixed_dkse_fem_interaction_nat_climate_policy)
pwartest(fixed_dkse_fem_interaction_nat_climate_policy) #no serial correlation after adding 1 lag
#display all models 3 and without interaction
models_list_all <- list(fixed_dkse_model3_climate_policy, fixed_dkse_fem_interaction_climate_policy,
                    fixed_dkse_model3_nat_climate_policy, fixed_dkse_fem_interaction_nat_climate_policy,
                    fixed_dkse_model3_int_climate_policy, fixed_dkse_fem_interaction_int_climate_policy)

table_html <- stargazer(models_list_all, type = "html", title = "Fixed Effects Linear Regression - Alternative Dependent Variables",
                        covariate.labels = c("Women environmental-policy ministers",
                                             "Educational attainment environmental-policy ministers",
                                             "Women in parliament",
                                             "Women environmental-policy ministers x Women in parliament",
                                             "Party environmental protection",
                                             "Green party in cabinet",
                                             "GDP per capita (log)",
                                             "Trade",
                                             "Population density (log)",
                                             "CCPI-climate policy (Lag 1)", "CCPI-national climate policy (Lag 1)",
                                             "CCPI-international climate policy (Lag 1)", "CCPI-international climate policy (Lag 2)"),
                        dep.var.labels = c("CCPI-climate policy",  "CCPI-national climate policy", "CCPI-international climate policy"))

table_html <- gsub("<body>", "<body style='font-size: 10px;'>", table_html)
#writeLines(table_html, "table_regression_model_3_no_interaction_CCPI.html")

###############################################################################
####################Chapter 9: model 1-3: by country#####
data_che <- data_panel[data_panel$country == "CHE",]
data_fra <- data_panel[data_panel$country == "FRA",]
data_aut <- data_panel[data_panel$country == "AUT",]
data_deu <- data_panel[data_panel$country == "DEU",]
data_fin <- data_panel[data_panel$country == "FIN",]
data_nld <- data_panel[data_panel$country == "NLD",]

#model 4:
CHE_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag+
                  environmental_protection_lag +
                 greens_cabinet_lag + log(GDP_lag) + trade_lag +
                 log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
                 lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
                 lag(EPS, 6L),
               data = data_che)

summary(CHE_lm)

DEU_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag +
               environmental_protection_lag +
               greens_cabinet_lag + log(GDP_lag) + trade_lag +
               log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
               lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
               lag(EPS, 6L),
             data = data_deu)

summary(DEU_lm)

FRA_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag +
               environmental_protection_lag +
               greens_cabinet_lag + log(GDP_lag) + trade_lag +
               log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
               lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
               lag(EPS, 6L),
             data = data_fra)

summary(FRA_lm)

AUT_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag +
               environmental_protection_lag +
               greens_cabinet_lag + log(GDP_lag) + trade_lag +
               log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
               lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
               lag(EPS, 6L),
             data = data_aut)

summary(AUT_lm)

NLD_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag +
               environmental_protection_lag +
               greens_cabinet_lag + log(GDP_lag) + trade_lag +
               log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
               lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
               lag(EPS, 6L),
             data = data_nld)

summary(NLD_lm)

FIN_lm <- lm(EPS ~ av_gender_lag + av_highest_ISCED2011_lag +
                environmental_protection_lag +
               greens_cabinet_lag + log(GDP_lag) + trade_lag +
               log(pop_density_lag) + lag(EPS, 1L) + lag(EPS, 2L) +
               lag(EPS, 3L) + lag(EPS, 4L) + lag(EPS, 5L) +
               lag(EPS, 6L),
             data = data_fin)

summary(FIN_lm)


models_list_bycountry <- list(AUT_lm, DEU_lm, 
                              CHE_lm, FRA_lm, 
                              FIN_lm, NLD_lm)

table_textM1.2 <- stargazer(models_list_bycountry, type = "text", title = "Linear Regression Results (AUT (1), DEU(2), CHE(3), FRA(4), FIN(5), NLD(6))", out = "table_regression_model_by_country.html",
                            covariate.labels = c("Share of female environmental-policy ministers",
                                                 "Average educational attainment",
                                                 "Share of women in parliament",
                                                 "Average party support for environmental protection",
                                                 "Green party in cabinet",
                                                 "GDP per capita (log)",
                                                 "Trade",
                                                 "Population density (log)",
                                                 "EPS (Lag 1)","EPS (Lag 2)","EPS (Lag 3)","EPS (Lag 4)", "EPS (Lag 5)", "EPS (Lag 6)"),
                            dep.var.labels = "Environmental Policy Stringency")
