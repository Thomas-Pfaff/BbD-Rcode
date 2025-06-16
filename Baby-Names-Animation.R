######################################################################
### Title: Baby Names Animation
### Produced by Thomas J Pfaff for Briefed by Data
### Data https://www.ssa.gov/oact/babynames/limits.html
### https://briefedbydata.substack.com/
### Date: June 2025
######################################################################

## Packages

library(dplyr)
library(ggplot2)
library(gganimate)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc"  
MyPurple5 <-  "#9c669c"

url <- "https://github.com/Thomas-Pfaff/BbD-Data/raw/refs/heads/main/Top30-NationalBabyNames-2024.csv"

data<-read.csv(url)

## Defined variables

CaptionData <- "Data: SSA"

## Select Males (M) or Females (F)
## Change title below to match

data2 <- data %>%  filter(Sex == "F") %>%
        select(Name, Rank, Year, Percent)

## Create vector of all unique names.

AllNames <- unique(data2$Name)

## Need to add missing names for each year.
## Otherwise the animation moved boxes that
## didn't need moving. I conjecture that it was
## a matching problem with names that needed to be moved.

for(y in 1880:2024){
  currentNames <- data2 %>% filter(Year == y) %>% select(Name) 
  j <- 0
  for(i in AllNames){
    if(!(i %in% as.vector(currentNames$Name))){
      temp <- data.frame(Name = i, Rank = 100 + j,Year = y, Percent = 20)
      data2 <- rbind(data2, temp)
      }
    }
}

## I found that years needed to be ordered by name not rank.
## When it was ordered by rank the names just switched instead
## of the bars gliding to a new spot.

data2 <- data2 %>%
  group_by(Year) %>% arrange(Name, .by_group=TRUE) %>% ungroup()

## Calculate the mean percent by rank.

RankMeans <- data2 %>% group_by(Rank) %>% summarize(Percent = mean(Percent))

## Make Graph
## Change title for if you filter for males above

g1 <- ggplot(data2) +
      aes(xmin = 0, xmax = Percent, ymin = Rank - 0.4, ymax = Rank + 0.4, y = Rank) +
      geom_rect(alpha = 0.7, fill = MyPurple5) +
      scale_x_continuous(limits = c(-1, 10), breaks = 1:10, 
                         labels = paste(1:10, "%" , sep="")) +
      geom_text(aes(label = Name), x = -0.1, color = MyPurple, hjust = "right", size = 10) +
      geom_text(aes(label = as.character(Year)), x = 7, y = -9, size = 30,
                color = MyPurple) +
      geom_point(data = RankMeans, aes(x = Percent, y = Rank), size = 6, color = MyPurple) +
      scale_y_reverse() + 
      theme(axis.text.x = element_text(size = 24),
	          axis.title.x = element_text(size = 36),
	          plot.title = element_text(size = 40),
	          panel.grid.major.y = element_blank(),
	          panel.grid.minor.y = element_blank(),
	          axis.text.y = element_blank(),
	          axis.line.y = element_blank(),
	          axis.ticks.y = element_blank(),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
	          plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                        color = c(MyPurple, "black"))) +
      labs(title = "Top 25 Female Baby Names by Year",
           x = "Percentage of All Names",
           y = NULL, 
	    caption = c("Briefed by Data || Thomas J Pfaff", CaptionData))  +
      transition_time(Year) + 
      ease_aes('linear')+
      view_follow(fixed_y = c(25.5, 0.5), fixed_x = c(-1.5, 10))

## Create animation
## Note this may take some time depending
## on your computer. You might test with
## duration = 10 first.

animate(g1,
        duration = 80,
        fps = 10, 
        width = 936,
        height = 1456 ,
        start_pause = 10, 
        end_pause = 20)
