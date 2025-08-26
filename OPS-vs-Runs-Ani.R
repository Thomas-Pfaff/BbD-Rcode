######################################################################
### Title MLB OPS vs Runs
### Produced by Thomas J Pfaff for Briefed by Data 
### https://briefedbydata.substack.com/
### Data Lahman package in R
### Date  Aug 2025
######################################################################


library(dplyr)
library(ggplot2)
library(gganimate)
library(ggrepel)


## Colors


MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc"  

## Import data

data<-read.csv("https://github.com/Thomas-Pfaff/BbD-Data/raw/refs/heads/main/MLB-1994-2023-Team.csv",sep=",",check.names=FALSE)


## Defined variables

CaptionData <- "Data:  Lahman Package"

MyColors2 <-c("#f781bf", "#5B005B")


## Create Graph of AVE vs OPS


g1 <- ggplot(data, aes(x = AVE, y = OPS, label = as.character(yearID))) +
      geom_point(aes(color = lgID), size=6) +
      geom_text(aes(x =0.22, y = 0.866),  alpha = 0.15,  col = MyPurple, size = 30) +
      xlim(0.2, 0.3) +
      ylim(0.6, 0.9) +
      theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.position = "inside",
        legend.position.inside = c(0.1, 0.85),
        plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
      labs(title = "MLB OPS vs. AVE 1994 to 2023",
           x ="AVE",
           y ="OPS", 
           color = "Leage",
      caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_color_manual(values = MyColors2) +
      transition_states(yearID, state_length = 1) + 
      ease_aes('linear') +
      shadow_mark(exclude_layer = 2, alpha = 0.2, size = 3) 

animate(g1, duration = 30,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 10)

## Create Graph of Runs vs OPS

g2 <- ggplot(data, aes(x = OPS, y = R, color = lgID,
                      label = as.character(yearID))) +
      geom_label_repel(aes(label = teamID), fill = MyLightP, 
                      box.padding = 0.1, force = 0.0, size = 8) +
      geom_text(aes(x = 0.66, y = 925),  alpha = 0.15,  col = MyPurple, size = 30) +
      xlim(0.6, 0.9) +
      ylim(200, 1010) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 16),
            legend.position = "inside",
            legend.position.inside = c(0.1, 0.85),
            plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "MLB Team Runs vs. OPS 1994 to 2023",
           x ="OPS",
           y ="Runs", 
           color = "Leage",
      caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))  +
      scale_color_manual(values = MyColors2) +
      transition_states(yearID, state_length = 1) + 
      ease_aes('linear') +
      shadow_mark(exclude_layer = c(2), alpha = 0.2, size = 3) 

animate(g2, duration = 30,
        fps = 10, 
        width = 1456,
        height = 936,
        start_pause = 10, 
        end_pause = 10)
