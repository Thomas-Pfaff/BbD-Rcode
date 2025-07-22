######################################################################
### Title Electricity Cost by State, Month, and Renewable
### Produced by Thomas J Pfaff for Briefed by Data https://briefedbydata.substack.com/
### Data https://www.eia.gov/electricity/data/browser/
### Data https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index
### Date  July 2025
######################################################################

## Packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gganimate)
library(lubridate)
library(ggrepel)


## Colors

MyBlue <- "#437fca"
MyRed <- "#be4242"
MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc" 
MyPurple2 <- "#6b196b"
MyPurple3 <- "#7b327b"
MyPurple4 <- "#8c4c8c"
MyPurple5 <-  "#9c669c"

url <- "https://github.com/Thomas-Pfaff/BbD-Data/raw/refs/heads/main/Electricty-Cost-Renwables-State-2024.csv"
data <- read.csv(url)
str(data)

## Defined variables

CaptionData <- "Data: eia Electricity Data Browser"

## Wrangle

data <- data %>% filter( !State == "District Of Columbia")

data$StateAbb <- state.abb[match(data$State,state.name)]

data$PercentWind <- as.numeric(data$PercentWind)

data$PercentSolar <- as.numeric(data$PercentSolar)

data$TotalRenew <- data$PercentWind + data$PercentSolar

data$MonthN <- month(parse_date_time(data$Month, orders = "b"))

dataRes <- data %>% filter(Consumer == "residential")


## By Wind


g1 <- ggplot(dataRes, aes(x = PercentWind, y = Cost, color = Politics, 
                          label = Month)) +
      geom_label_repel(aes(label = StateAbb), fill = MyLightP, 
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_text(aes(x = 0.5, y = 40),  alpha = 0.2,  col = MyPurple3, size = 30) +
      theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.90),
        plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
      labs(title = "2024 Electricity Cost by Percent of Wind Energy",
           x ="Percent Wind Electricity Generation",
           y ="Cents per Kilowatthour", 
           caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous(labels = label_currency(prefix = "", suffix = "¢")) +
      scale_color_manual(values = c(MyBlue,MyRed), labels = c("Dem", "Rep")) +
      transition_states(MonthN, state_length = 1) + 
      ease_aes('linear') 


animate(g1,
        duration = 30,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 10)

## By Solar

g2 <- ggplot(dataRes, aes(x=PercentSolar, y = Cost, color = Politics, 
                          label = Month)) +
      geom_label_repel(aes(label = StateAbb), fill = MyLightP, 
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_text(aes(x = 0.25, y = 40),  alpha = 0.2,  col = MyPurple3, size = 30) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
        plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.9, 0.90),
        plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
      labs(title = "2024 Electricity Cost by Percent of Solar Energy",
           x ="Percent Solar Electricity Generation",
           y ="Cents per Kilowatthour", 
           caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous(labels = label_currency(prefix = "", suffix = "¢")) +
      scale_color_manual(values = c(MyBlue,MyRed), labels = c("Dem", "Rep")) +
      transition_states(MonthN, state_length = 1) + 
      ease_aes('linear') 

animate(g2,
        duration = 30,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 10)

## Total Solar and Wind

g3 <- ggplot(dataRes, aes(x=TotalRenew, y = Cost, color = Politics, 
                          label = Month)) +
      geom_label_repel(aes(label = StateAbb), fill = MyLightP, 
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_text(aes(x = 0.60, y = 40),  alpha = 0.2,  col = MyPurple3, size = 30) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            legend.background = element_rect(fill = MyLightP2),
            legend.position = "inside",
            legend.position.inside = c(0.9, 0.90),
            plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "2024 Electricity Cost by Percent of Solar & Wind Energy",
           x ="Percent Solar & Wind Electricity Generation",
           y ="Cents per Kilowatthour", 
           caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous(labels = label_currency(prefix = "", suffix = "¢")) +
      scale_color_manual(values = c(MyBlue,MyRed), labels = c("Dem", "Rep")) +
      transition_states(MonthN, state_length = 1) + 
      ease_aes('linear') 

animate(g3,
        duration = 30,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 10)

