############################################################################################
### Title Beveridge Curve
### By Thomas J. Pfaff
### For Briefed by Data: https://briefedbydata.substack.com/
### Data https://fredblog.stlouisfed.org/2025/01/the-unusual-shape-of-the-beveridge-curve/
############################################################################################

## Packages

library(ggplot2)
library(scales)
library(ggrepel)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc"  

## Import Data

url <- "https://github.com/Thomas-Pfaff/Stats-Course-Data/raw/refs/heads/main/Beveridge-Curve.csv"
Bcurve <-read.csv("Beveridge-Curve.csv")

CaptionData <- "Data: FRED"

## Main Graph

dev.off()
dev.new(width = 1456, height = 936, unit = "px")

ggplot(Bcurve, aes(x = UnemploymentRate, y = JobOpeningRate)) +
  geom_point(size=3, color=MyPurple)+
  theme(axis.text = element_text(size = 14),
	     axis.title = element_text(size = 16),
	     plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.5, 0.10),
	     plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
  labs(title ="The Beveridge Curve",
       x ="Unemployment Rate",
       y ="Job Opening Rate", 
	    caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))+
  scale_y_continuous(labels = label_percent(scale = 1)) + 
  scale_x_continuous(labels = label_percent(scale = 1))
 
## Color By Year

dev.off()
dev.new(width = 1456, height = 936, unit = "px")

ggplot(Bcurve, aes(x = UnemploymentRate, y = JobOpeningRate, color=as.factor(Year) )) +
  geom_point(size=3)+
  theme(axis.text = element_text(size = 14),
	     axis.title = element_text(size = 16),
	     plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.91, 0.70),
	     plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
  labs(title ="The Beveridge Curve",
       x ="Unemployment Rate",
       y ="Job Opening Rate", 
       color="Year",
	    caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))+
  scale_y_continuous(labels = label_percent(scale = 1)) + 
  scale_x_continuous(labels = label_percent(scale = 1))
 
## Add Year Text

dev.off()
dev.new(width = 1456, height = 936, unit = "px")

ggplot(Bcurve, aes(x = UnemploymentRate, y = JobOpeningRate, color=as.factor(Year) )) +
  geom_point(size=3)+
  theme(axis.text = element_text(size = 14),
	     axis.title = element_text(size = 16),
	     plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.91, 0.70),
	     plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
  labs(title ="The Beveridge Curve",
       x ="Unemployment Rate",
       y ="Job Opening Rate", 
       color="Year",
	    caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))+
  scale_y_continuous(labels = label_percent(scale = 1)) + 
  scale_x_continuous(labels = label_percent(scale = 1)) +
  geom_text_repel(aes(label = Year), size = 3)

### Color By Month

dev.off()
dev.new(width = 1456, height = 936, unit = "px")

ggplot(Bcurve, aes(x = UnemploymentRate, y = JobOpeningRate, color=as.factor(Month) )) +
  geom_point(size=3)+
  theme(axis.text = element_text(size = 14),
	     axis.title = element_text(size = 16),
	     plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill = MyLightP), 
        legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.91, 0.70),
	     plot.caption = element_text(hjust = c(1, 0), size = c(14, 14),
                                    color = c(MyPurple, "black"))) +
  labs(title ="The Beveridge Curve",
       x ="Unemployment Rate",
       y ="Job Opening Rate", 
       color="Month",
	    caption = c("Briefed by Data || Thomas J Pfaff",CaptionData))+
  scale_y_continuous(labels = label_percent(scale = 1)) + 
  scale_x_continuous(labels = label_percent(scale = 1))

