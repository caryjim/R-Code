#Codes are based on "R Statistics Bookbook" by Francisco Juretig 2019 with modification. 

# p.290 in chapter 7 
install.packages("tscount")
library(tscount)
install.packages("dummy")
library(dummy)
library(dplyr)


setwd("D:/Documents/R/Chapter07")
data = read.table("./E1.txt",sep="\t",header = TRUE) #Read text data
colnames(data)  # Read column names
class(data) #baseR code to check object class
str(data) #baseR utils function to display internal structure of an object 
data$home_away = ifelse(data$ha == "H", 1, 0) #create a new column by re-coding H or A into binary values as numeric
data$date = as.Date(data$date,format="%m/%d/%Y") #format date column from character string

#Calculate days difference from the date column. 
data = data %>% mutate(diff_days = as.numeric(date-lag(date))) #append a new column diff_days by subtracting the date difference
data[is.na(data)]  = 0  #Convert all na values in dataframe to zero 

#p.299 in Chapter 7

library(anomalize)
library(dplyr)
library(tibbletime)
# See business science in https://github.com/business-science/tibbletime

currency_sales = read.csv("./sold_usd_oilseeds.csv") #Load data from csv file 
str(currency_sales)

currency_sales$indice_tiempo = as.Date(currency_sales$indice_tiempo,"%Y-%m-%d") #Convert date column 

# Anomaly detection has several components 
#Transform the dataset into tibbletime object and assigned date as index 
results_anomalies = as_tbl_time(currency_sales,index = indice_tiempo) 
str(results_anomalies)
results_anomalies

#Decompose the time series into trend, seasonality, and remainder components 
results_anomalies %>% 
  time_decompose(promedio_diario, merge = TRUE) #time_decompose from anomalize library 
# It gives you an output of how many months in frequency and trend calculation
# Four columns are displayed for time tibble: observed, season, trend, and remainder 

#Detect the anomalies using the remainder
#use the anomalize function from the anomalize library 
#data has to be in a tibble or tibble time object

results_anomalies %>% anomalize(remainder, method = "iqr")
#this step, three columns are displayed: remainder_l1, remainder_l2, anomaly 

#putting the sequence together and assigned to dataframe 
#the anomaly detection algorithm runs on the series of residuals 
results_anomalies = 
  results_anomalies %>%
  time_decompose(promedio_diario, method = "stl", merge = TRUE) %>% #stl method remove the trend and seasonality, to obtain a remainder
  anomalize(remainder, method = "iqr")
#IQR method use an inner-quartile range of 25% and 75% to establish a baseline distribution around the median
#alpha is default to 0.05 in this method 
#decrease alpha will make it more diffcult to detect outlier
#increase alphe will make it easier to detect outlier

#Finally, join everything back together 
t2 <- results_anomalies%>% time_recompose() #Recompose the bands separating anomalies from normal observations
#Assigned to a new tibble named t2 for comparison of how columns changes 
#Two new columns added for recomposed_l1, recomposted_l2

#Plot the result 
results_anomalies %>% plot_anomaly_decomposition(ncol=3,alpha_dots = 0.3) 
#The red indicated an anomaly value in the plot 

#p.300-301 
# Set the parameters for the amplitude of what is normal to detect anomalies
anomalies_exp_2 = 
  as_tbl_time(currency_sales,index = indice_tiempo) %>% 
  time_decompose (promedio_diario, "stl", merge = TRUE) %>%
  anomalize(remainder,alpha=0.10,max_anoms=0.02) %>% 
  time_recompose()

#this example, determine the value outside of expected nromal
# alpha is reduced to 0.10, which lower the threshold for more anomalies to appear
# max_anoms = 0.02 meaning that at most 2% of the data could be considered anomalies. 

anomalies_exp_2 %>% plot_anomaly_decomposition(ncol=3,alpha_dots = 0.3) 

#Example 3 with Twitter trend method in time_decompose function 
anomalies_exp_3 = 
  as_tbl_time(currency_sales,index = indice_tiempo) %>% 
  time_decompose (promedio_diario, "twitter", merge = TRUE) %>%
  anomalize(remainder,alpha=0.10,max_anoms=0.02) %>% 
  time_recompose()

anomalies_exp_3 %>% plot_anomaly_decomposition(ncol=2, alpha_dots = 0.3)
#modify to 2 columns for the ease of view 

#By comparison the plots, there are 3 outliers in both methods, but one outliers of the the 3
# was different time point in example 2 and example 3. 

#it was in 2015-12-01 vs. 2015-11-01 between twitter method and stl method 
