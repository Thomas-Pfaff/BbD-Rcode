######################################################################
### Title Fertilty rate population simulation
### For Breifed by Data 
### By Thomas J Pfaff 
### Date January 2025
######################################################################

## Packages

library(dplyr)
library(ggplot2)
library(scales)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf"  
MyLightP2 <-  "#f8f4f8"  
MyLightP3 <- "#fcfafc"  

## Function for data

Population <- function(r, df, n){
  for (i in 1:n){
   temp <- c(df[30, i + 1] * r / 2, df[1:79, i + 1])
   df <- df %>% mutate("Y.{i+1}" :=temp)
   }
  return(df) }


## Generate total population data

df <- data.frame("Age" = 1:80, "Y1" = rep(1000000, 80))
data <- data.frame(Year = 0:100, rate = rep(0.5, 101),
        Population = as.vector(colSums(Population(0.5, df, 100)))[2:102] )

for (r in c(0.75, 1, 1.25, 1.5, 1.75, 2)){
 df <- data.frame("Age" = 1:80, "Y1" = rep(1000000, 80))
 temp <- data.frame(Year = 0:100, rate = rep(r,101),
        Population = as.vector(colSums(Population(r, df, 100)))[2:102] )
 data <- rbind(data, temp) }

## Create Graph  16:9 is 1200 x 675 and 14:10 is 1456 x 1048 


MyColors7 <- c('black', '#ffff33', '#ff7f00', '#984ea3',  
               '#4daf4a', '#377eb8', '#e41a1c')

dev.off()
dev.new(width = 1456, height = 936, unit = "px")

ggplot(data, aes(x = Year, y = Population, color = as.factor(rate))) +
  geom_line(linewidth = 1.25) +
  theme(axis.text = element_text(size = 14),
	     axis.title = element_text(size = 16),
	     plot.title = element_text(size = 20),
        plot.background = element_rect(fill = MyLightP3),
        panel.background = element_rect(fill= MyLightP), 
	     legend.background = element_rect(fill = MyLightP2),
        legend.position = "inside",
        legend.position.inside = c(0.105, 0.53),
	     plot.caption = element_text(hjust = c(1), size = c(14),
                                    color = c(MyPurple))) +
  labs(title = "Simulated Population Size for Different Fertility Rates",
	    y = NULL, 
		 x = "Years into Future",
       color = "Fertility Rate",
		 caption=c("Briefed by Data || Thomas J Pfaff")) +
  scale_y_continuous(label = comma) +
  scale_color_manual(values = rev(MyColors7)) +
  guides(color = guide_legend(reverse = TRUE))


