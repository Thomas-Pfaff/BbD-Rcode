######################################################################
### Title: ANOVA Animation
### Date: May 2025
### Produced by Thomas J Pfaff for Briefed by Data
### https://briefedbydata.substack.com/
######################################################################

## Packages

library(dplyr)
library(ggplot2)
library(animation)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"   
MyLightP3 <- "#fcfafc"  
MyPurple3 <- "#7b327b"
MyPurple4 <- "#8c4c8c"

## Create Data

set.seed(43)
start <- rnorm(100, 20, 1)
data <- data.frame( results = rep( start, times = 3), 
                    group = rep( 1:3, each = 100))


## Create Animation  

steps <- 20
n1<-100
n <-300
k <- 3

Gstats <- data %>% group_by(group) %>%
          summarize(SD = round( sd(results), 2), 
                    Mean = round( mean(results), 2))

Tmean <- round(mean(data$results), 2)

SSG <- round(sum( 100 * (Gstats[1:3,3] - Tmean)^2), 2)
SSE <- round(sum( (100 - 1) * Gstats[1:3,2]^2), 2)


saveGIF(
{ ani.options(interval = 0.5, nmax = 50)

  g1 <- ggplot( data, aes(x = results, fill = as.factor(group)) ) +
        geom_histogram( position = "identity", bins = 30 ) +
        xlim( c(12, 28) ) +
        ylim(c( 0, 60) ) +
        scale_fill_manual( values = c(MyPurple, MyPurple4, MyPurple) ) +             
        annotate( "text", x = 12, y = 55, hjust = 0, size = 12,
                  label = bquote( SSG ~~ is ~~.(n1) * ( .(Gstats[[1,3]]) - .(Tmean) )^2 + 
                                    .(n1) * ( .(Gstats[[2,3]]) - .(Tmean) )^2 + 
                                    .(n1) * ( .(Gstats[[3,3]]) - .(Tmean) )^2 == .(SSG) 
                                )
                 ) +
        annotate( "text", x = 12, y = 46, hjust = 0, size = 12,
                  label = bquote( SSE ~~ is ~~ (.(n1)-1)* .(Gstats[[1,2]])^2 + 
                                    (.(n1)-1)* .(Gstats[[2,2]])^2 + 
                                    (.(n1)-1)* .(Gstats[[3,2]])^2 == .(SSE) 
                                )
                 )  +
        annotate( "text", x = 12, y = 37, hjust = 0, size = 12,
                  label = bquote( F ~~ is ~~ frac( SSG / (k-1), SSE /(n-k))
                                    == .( ( SSG/(k-1) ) / (SSE / (n-k)) ) 
                                )
                 ) +
        geom_segment(aes(x = Gstats[[1,3]], y = 0, xend = Gstats[[1,3]], yend = 29),
                     color = MyPurple3, linewidth = 1.5) +
        geom_segment(aes(x = Gstats[[2,3]], y = 0, xend = Gstats[[2,3]], yend = 29),
                     color = MyPurple3, linewidth = 1.5) +
        geom_segment(aes(x = Gstats[[3,3]], y = 0, xend = Gstats[[3,3]], yend = 29),
                     color = MyPurple3, linewidth = 1.5) +
        annotate("text", x = Gstats[[1,3]], y = 30, hjust = 1, vjust = 1,
                 label = Gstats[[1,3]], size = 10, color = MyPurple3) +
        annotate("text", x = Gstats[[2,3]], y = 30, hjust = 0.5, vjust = 0,
                 label = Gstats[[2,3]], size = 10, color = MyPurple3) + 
        annotate("text", x = Gstats[[3,3]], y = 30, hjust = 0, vjust = 1,
                 label = Gstats[[3,3]], size = 10, color = MyPurple3) +
        theme(axis.text = element_text(size = 14),
	           axis.title = element_text(size = 16),
	           plot.title = element_text(size = 30),
              plot.background = element_rect(fill = MyLightP3),
              panel.background = element_rect(fill = MyLightP), 
              legend.background = element_rect(fill = MyLightP2),
              legend.position = "none",
              legend.position.inside = c(0.5, 0.10),
	           plot.caption = element_text(hjust = 1, size = 25,
                                          color = MyPurple)) +
        labs(title = "ANOVA Animation",
             x =NULL,
             y =NULL, 
	          caption = c("Briefed by Data || Thomas J Pfaff"))
    print(g1)
    ani.pause()

    for(j in 1:4){
      print(g1)
      ani.pause() }

  for (i in 1:steps)
  {  
    dataT <- data %>% mutate( results = case_when(
                              group == 1 ~ results - 5*i/steps,
                              group == 2 ~ results,
                              group == 3 ~ results + 5*i/steps))
    Gstats <- dataT %>% group_by(group) %>%
              summarize( SD = round( sd(results), 2 ),
                        Mean = round( mean(results), 2) )

    Tmean <- round( mean(data$results), 2)

    SSG <- round( sum( 100 * (Gstats[1:3,3] - Tmean)^2 ), 2)
    SSE <- round( sum( (100 - 1) * Gstats[1:3,2]^2), 2)

    g2 <- ggplot(dataT, aes(x=results, fill=as.factor(group)))+
          geom_histogram( position = "identity", bins = 30 ) +
          xlim( c(12, 28) ) +
          ylim(c( 0, 60) ) +
          scale_fill_manual( values = c(MyPurple, MyPurple4, MyPurple) ) +             
          annotate( "text", x = 12, y = 55, hjust = 0, size = 12,
                    label = bquote( SSG ~~ is ~~.(n1) * ( .(Gstats[[1,3]]) - .(Tmean) )^2 + 
                                      .(n1) * ( .(Gstats[[2,3]]) - .(Tmean) )^2 + 
                                      .(n1) * ( .(Gstats[[3,3]]) - .(Tmean) )^2 == .(SSG) 
                                  )
                   ) +
          annotate( "text", x = 12, y = 46, hjust = 0, size = 12,
                    label = bquote( SSE ~~ is ~~ (.(n1)-1)* .(Gstats[[1,2]])^2 + 
                                      (.(n1)-1)* .(Gstats[[2,2]])^2 + 
                                      (.(n1)-1)* .(Gstats[[3,2]])^2 == .(SSE) 
                                  )
                   )  +
          annotate( "text", x = 12, y = 37, hjust = 0, size = 12,
                    label = bquote( F ~~ is ~~ frac( SSG/ (k-1), SSE / (n-k) )
                                      == .( ( SSG / (k-1) ) / ( SSE / (n-k) ) ) 
                                  )
                   ) +
          geom_segment(aes(x = Gstats[[1,3]], y = 0, xend = Gstats[[1,3]], yend = 29),
                       color = MyPurple3, linewidth = 1.5) +
          geom_segment(aes(x = Gstats[[2,3]], y = 0, xend = Gstats[[2,3]], yend = 29),
                       color = MyPurple3, linewidth = 1.5) +
          geom_segment(aes(x = Gstats[[3,3]], y = 0, xend = Gstats[[3,3]], yend = 29),
                       color = MyPurple3, linewidth = 1.5) +
          annotate("text", x = Gstats[[1,3]], y = 30, hjust = 1, vjust = 1,
                   label = Gstats[[1,3]], size = 10, color = MyPurple3) +
          annotate("text", x = Gstats[[2,3]], y = 30, hjust = 0.5, vjust = 0,
                   label = Gstats[[2,3]], size = 10, color = MyPurple3) + 
          annotate("text", x = Gstats[[3,3]], y = 30, hjust = 0, vjust = 1,
                   label = Gstats[[3,3]], size = 10, color = MyPurple3) +
          theme(axis.text = element_text(size = 14),
	             axis.title = element_text(size = 16),
	             plot.title = element_text(size = 30),
                plot.background = element_rect(fill = MyLightP3),
                panel.background = element_rect(fill = MyLightP), 
                legend.background = element_rect(fill = MyLightP2),
                legend.position = "none",
                legend.position.inside = c(0.5, 0.10),
	             plot.caption = element_text(hjust = 1, size = 25,
                                            color = MyPurple)) +
          labs(title = "ANOVA Animation",
               x =NULL,
               y =NULL, 
	            caption = c("Briefed by Data || Thomas J Pfaff"))
               
        print(g2)
        ani.pause()
  }

  for(j in 1:10){
      print(g2)
      ani.pause() }

}, movie.name = "ANOVA.gif", ani.width = 1456, ani.height = 936)


