# *------------------------------------------------------------------
# | PROGRAM NAME: Analysis of Uber Supply-Demand Gap
# | DATE: 09/06/2017
# | CREATED BY:  Ankur Shrivastava
# | PROJECT FILE: DDA----.R             
# *----------------------------------------------------------------
# | PURPOSE: The purpose of this file is to analyze the Demand               
# |          Supply gap faced by Uber leading to loss of revenue
# *------------------------------------------------------------------
# | COMMENTS:               
# |
# |  1: Initial Version 
# |*------------------------------------------------------------------
# | DATA USED:  Uber Request data             
# |
# |
# |*------------------------------------------------------------------

# The following packages are loaded assuming they are already installed in the 
# R environment. If not, they can be installed using the command 
# "install.packages("package-name", dependencies = TRUE)"

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

#-----------------Data cleaning/Transformation-----------------------------------
uber <- read.csv("Uber Request Data.csv", header = TRUE, stringsAsFactors = FALSE)
str(uber)
summary(uber)

#-------Coverting Pickup point, Status, and driver id to factors-------------
uber$Request.id <- as.factor(uber$Request.id)
uber$Pickup.point <- as.factor(uber$Pickup.point)
uber$Driver.id <- as.factor(uber$Driver.id)
uber$Status <- as.factor(uber$Status)

str(uber)
summary(uber)

#Remove any leading or trailing whitespaces
uber$Request.timestamp <- trimws(uber$Request.timestamp)

#Create Request and Drop date time objects from Request and drop timestamps respectively
#There are 2 formats in which timestamp has been recorded. 1) dd/mm/YYYY HH:MM and 2) dd-mm-YYYY HH:MM:SS.
#Datetime objects need to be created considering both the formats

uber$Reqdt <- parse_date_time(uber$Request.timestamp, orders = c("d/m/Y H:M","d-m-Y H:M:S"), tz = "Asia/Calcutta")
uber$Dropdt <- parse_date_time(uber$Drop.timestamp, orders = c("d/m/Y H:M","d-m-Y H:M:S"), tz = "Asia/Calcutta")
summary(uber)

#From the summary, it is clear that all the records are from 2016-07-11 to 2016-07-16
#So, year and month will not have any impact on data. To analyze, day, hour and minutes
#need to be extracted from the date columns

uber$Rday <- day(uber$Reqdt)
uber$Rhour <- hour(uber$Reqdt)
uber$Rmnt <- minute(uber$Reqdt)
summary(uber)

#Visulizing status
counts <- as.data.frame(count(uber, Status))
ggplot(counts, aes(x = Status, y = n, fill = Status)) + geom_bar(stat = "identity")

#Percentage of trips completed
puber <- uber %>% group_by(Status) %>% summarise(ptotal = n()/nrow(uber) *100)
ggplot(puber, aes(x = Status, y = ptotal, fill = Status)) +
      geom_bar(stat = "identity") +
      ylab("Percentage of Requests")

# As can be seen from the graph, only around 42% of the requests end in completed trips,
# Nearly 39% did not get scheduled due to 'No Cars Available' and nearly 19% got 
# cancelled by the drivers

#Percentage of trips completed by each day
duber <- uber %>% group_by(Rday, Status) %>% 
         tally() %>% group_by(Rday) %>%
         mutate(pct = n/sum(n) * 100)
  
ggplot(duber, aes( x = Rday, y = pct, fill = Status)) +
       geom_bar(stat = "identity", color = "black") +
       ylab("Percentage of Requests") +
       xlab("Day of July 2016")

# From the graph, there doesn't appear to be a significant difference between the 
# various categories of status on the 5 days.

#Percentage of trips completed by each hour
huber <- uber %>% group_by(Rhour, Status) %>% 
         tally() %>% group_by(Rhour) %>%
         mutate(hpct = n/sum(n) * 100)

ggplot(huber, aes( x = Rhour, y = hpct, fill = Status)) +
       geom_bar(stat = "identity", color = "black") +
       ylab("Percentage of Requests") +
       xlab("Hour of Day") 

# From the graph, it is clear that high number of cancellatins occur between 4 to 10
# a.m. and high number of No cars available between 5 to 9 p.m. and between 12a.m.to 
# 3 a.m.

# Below graph also confrms the same

ggplot(huber, aes( x = Rhour, y = hpct)) + geom_line() +
      facet_wrap(~ Status) +
      ylab("Percentage of Requests") +
      xlab("Hour of Day")                  
       
# Thus it is clear that there are certain timeslots where the cancelled requests
# No cars available scenario peak. So, it would be easier to analyze using 4 hour
# timeslots. These would be 1 a.m. - 5 a.m. -> Late Night (LN)
#                           5 a.m. - 9 a.m. -> Early Morning (EM)
#                           9 a.m. - 1 p.m. -> Late Morning (LM) 
#                           1 p.m. - 5 p.m. -> Afternoon (AN)
#                           5 p.m. - 9 p.m. -> Evening (EV)
#                           9 p.m. - 1 a.m. -> Early Night (EN)
# Creating new variable for time slots

uber$Tslot <- if_else(uber$Rhour >= 1 & uber$Rhour < 5, "LN",
                       if_else(uber$Rhour >= 5 & uber$Rhour < 9 ,"EM",
                               if_else(uber$Rhour >= 9 & uber$Rhour < 13, "LM",
                                       if_else(uber$Rhour >= 13 & uber$Rhour < 17, "AN",
                                               if_else(uber$Rhour >= 17 & uber$Rhour < 21, "EV","EN")))))

uber$Tslot <- factor(uber$Tslot, levels = c("LN","EM","LM","AN", "EV","EN"))

# Consolidating by timeslots and also by Pickup point
tsuber <- uber %>% group_by(Tslot, Status, Pickup.point) %>% 
                   tally() %>% group_by(Tslot) %>%
                   mutate(tspct = n/sum(n) * 100)

ggplot(tsuber, aes( x = Tslot, y = tspct, fill = Status)) +
       geom_bar(stat = "identity", color = "black", position = "fill") +
       ylab("Percentage of Requests") +
       xlab("Timeslot") +
       facet_wrap(~ Pickup.point)

# From the graph, it is clear that for Requests originating at the airport,
# there is a high percentage of 'No Cars Available' in the Evening and Early Night
# timeslots. For Requests orignating from the city, there are high percentage of 
# cancellations for requests originating from the city in EM and LM time slots.

# To find out Demand supply gap, the demand and supply need to be aggregated by
# each time slot

uber$condition <- if_else(uber$Status == "Trip Completed", TRUE, FALSE)

dsuber <- uber %>% group_by(Tslot, Pickup.point, condition)  %>% 
                   tally() %>% 
                   mutate(demand = sum(n)) %>%
                   filter(condition == TRUE) %>%
                   mutate(supply = n, gap = demand - supply)

ggplot(dsuber, aes( x = Tslot, y = gap, fill = Tslot)) + 
       geom_bar(stat = "identity", color = "black") +
       facet_wrap(~ Pickup.point) +
       xlab("Time Slot") +
       ylab("Demand-Supply Gap")
       
#From the graph, it can be clearly seen that the gap is highest in the evening
# and Early night for requests originating at the airport. Gap is highest in Early
# Mornings and late Mornings for requests originating in the city.

#uber_new <- uber[-c("")]
# Exporting data for tableau graphs
write.csv(uber, file = "uber-new.csv")
                   
                   
