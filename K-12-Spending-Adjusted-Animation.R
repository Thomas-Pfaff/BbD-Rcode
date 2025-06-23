######################################################################
### Title: K-12 Spending and results
### Produced by Thomas J Pfaff for Briefed by Data https://briefedbydata.substack.com/
### Data Spending: https://www.census.gov/data/tables/2023/econ/school-finances/secondary-education-finance.html
### Data Scores https://www.nationsreportcard.gov/profiles/stateprofile
### Data Politics https://en.wikipedia.org/wiki/Cook_Partisan_Voting_Index
### Data Adjusted Scores https://www.urban.org/research/publication/states-demographically-adjusted-performance-2024-national-assessment
### June 2025
######################################################################


## Packages

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(gganimate)


## Colors

MyBlue <- "#437fca"
MyRed <- "#be4242"
MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc" 

## Read data

data<-read.csv("https://github.com/Thomas-Pfaff/BbD-Data/raw/refs/heads/main/Spending-Scores-Adjusted-8th-Grade-2024.csv",sep=",",check.names=FALSE)

## Defined variables

CaptionData <- "Data: Census Bureau, Nations Report Card, Urban Institute"


## Wrangle as needed

data$type <- ifelse(data$ScoreType == "Raw", 0, 1)

dataRaw <- data %>% filter(ScoreType == "Raw")


USmeans <- dataRaw %>% summarize(Math = mean(Math),
                                 Reading = mean(Reading),
                                 Spending = mean(TotalSpending))

## Create math animation

g1 <- ggplot(data, aes(x = TotalSpending, y = Math, color = Politics)) +
      geom_vline(xintercept = USmeans[[ "Spending" ]], color = MyPurple, 
                linewidth = 1.25) +
      geom_hline(yintercept = USmeans[[ "Math" ]], color = MyPurple, 
                 linewidth = 1.25) +
      annotate("text", x = 22000, y = USmeans[[ "Math" ]] - 0.25, 
               label = "Raw Score Mean", color = MyPurple, vjust = 1, size = 5) +
      geom_label_repel(aes(label = State), fill = MyLightP,
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_point(size = 5, alpha = 0.6) +
      scale_color_manual(values = c(MyBlue, MyRed), labels = c("Dem", "Rep")) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            legend.background = element_rect(fill = MyLightP2),
            legend.position = "inside",
            legend.position.inside = c(0.88, 0.855),
            legend.box="horizontal",
            plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "Total K-12 Spending and 2024 8th Grade Math Results Raw to Adjusted",
           x ="Total Spending Per Student",
           y ="Mean Math Score", 
           caption = c( "Briefed by Data || Thomas J Pfaff", CaptionData )) +
      scale_x_continuous( labels = scales::dollar_format() ) +
      transition_time(type) + 
      ease_aes('linear')+
      shadow_mark(alpha = 0.4) 


animate(g1,
        duration = 20,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 30)


## Create reading animation

g2 <- ggplot(data, aes(x = TotalSpending, y = Reading, color = Politics)) +
      geom_vline(xintercept = USmeans[[ "Spending" ]], color = MyPurple, 
                 linewidth = 1.25) +
      geom_hline(yintercept = USmeans[[ "Reading" ]], color = MyPurple, 
                 linewidth = 1.25) +
      annotate("text", x = 22000, y = USmeans[[ "Reading" ]] - 0.25, 
               label = "Raw Score Mean", color = MyPurple, vjust = 1, size = 5) +
      geom_label_repel(aes(label = State), fill = MyLightP,
                       box.padding = 0.1, force=0.0, size = 6) +
      geom_point(size = 5, alpha = 0.6) +
      scale_color_manual(values = c(MyBlue, MyRed), labels = c("Dem", "Rep")) + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            legend.background = element_rect(fill = MyLightP2),
            legend.position = "inside",
            legend.position.inside = c(0.88, 0.855),
            legend.box="horizontal",
            plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "Total K-12 Spending and 2024 8th Grade Reading Results Raw to Adjusted",
           x ="Total Spending Per Student",
           y ="Mean Reading Score", 
           caption = c( "Briefed by Data || Thomas J Pfaff", CaptionData )) +
      scale_x_continuous( labels = scales::dollar_format() ) +
      transition_time(type) + 
      ease_aes('linear')+
      shadow_mark(alpha = 0.4) 


animate(g2,
        duration = 20,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 30)
