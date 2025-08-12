######################################################################
### Title GDP vs Fertility Rate for Select Countries
### Produced by Thomas J Pfaff for Briefed by Data 
### https://briefedbydata.substack.com/
### Data GDP https://data.worldbank.org/indicator/NY.GDP.PCAP.CD
### Data Fertility https://data.worldbank.org/indicator/SP.DYN.TFRT.IN
### Date  Aug 2025
######################################################################

## Packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(gganimate)


## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"  
MyLightP3 <- "#fcfafc"  

## Load data

data<-read.csv("https://github.com/Thomas-Pfaff/BbD-Data/raw/refs/heads/main/GDPfert.csv",sep=",",check.names=FALSE)

## Define variables

CaptionData <- "Data: World Bank"

## Select Countries

Countries <- c( "Australia",  "Chile", "China",
                "Costa Rica", "Germany", "Ireland", "Israel",
                "Italy", "Japan", "Korea, Rep.", "Mexico",
                "Norway", "Poland", "Spain",
                "United Kingdom", "United States" )

## Removed countries

#  "Luxembourg", "Czechia", "Latvia", "Sweden",
#  "Iceland", "Greece", "Hungary", "Colombia",
#  "Estonia", "Switzerland", "Denmark", "Finland", "France", 
#  "Lithuania", "Netherlands", "Slovak Republic", "Slovenia",  
#  "Austria", "Belgium", "Canada", "Portugal", "Turkiye", 

data2 <- data %>% filter(CountryName %in% Countries)

## Create Graph 

g1 <- ggplot(data2, aes(x=FertilityRate, y = GDPperCap, 
                          label = as.character(Year))) +
      geom_label_repel(aes(label = CountryCode  ), fill = MyLightP, col = MyPurple,
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_text(aes(x =0.4, y = 100000),  alpha = 0.15,  col = MyPurple, size = 30) +
      xlim(0, 5)+
      ylim(0, 111000)+
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "GDP per Capita by Fertility Rate",
           x ="Fertility Rate",
           y ="GDP per Capita", 
           caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_y_continuous(labels = scales::dollar_format()) +
      transition_states(Year, state_length = 1) + 
      ease_aes('linear') +
      shadow_mark(exclude_layer = 2, alpha = 0.4) 
  
animate(g1, duration = 40,
            fps = 10, 
            width = 1456,
            height = 936,
            start_pause = 10, 
            end_pause = 10)
