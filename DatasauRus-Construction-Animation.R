######################################################################
### Title Construting DatasauRus type data
### Created by Thomas J. Pfaff 
### For Briefed by Data post Constructing a datasauRus data set
### https://briefedbydata.substack.com/ 
######################################################################

## Packages

library(ggplot2)
library(dplyr)
library(animation)

## Colors

MyPurple <- "#5B005B"
MyLightP <- "#dfdbdf" 
MyLightP2 <-  "#f8f4f8"  
MyLightP3 <- "#fcfafc"

## Define variables and data

n <- 75  
iterations <- 2500*n  
error <- 0.01

X <- rnorm(n)
Y <- rnorm(n)

A <- X
B <- Y
moves <- 0
graphs <- 0

Points <- data.frame( x=A, y=B)

Points <- Points %>% mutate(MeanX = format( round( mean(x), 2), nsmall = 2), 
                MeanY = format( round( mean(y), 2), nsmall = 2),
                SdX = format( round( sd(x), 2), nsmall = 2),
                SdY = format( round( sd(y), 2), nsmall =2 ),
                N = n(), 
                r = format( round( cor(x,y), 2), nsmall=2))

## Create Animation

saveGIF(
{
ani.options(interval = 0.2, nmax = 50)

g1 <- ggplot(Points, aes(x=x, y=y,  label = 
                 paste("Mean ( X, Y ) = ( ", MeanX, ", ", MeanY, " )\nSd ( X, Y ) = ( ",
                                      SdX , ", ", SdY, " )\nr = ", r, sep="" ))) + 
          geom_point(color = MyPurple, size = 4) +
          geom_text( aes(0, 2.75), size = 6, hjust = 0, vjust=0, color = MyPurple) +
          xlim(-4, 4) +
          ylim(-4,4) +
          theme(axis.text = element_text(size = 14),
	           plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            plot.caption = element_text(hjust = 1, size = 14,
                                        color = MyPurple)) +
          labs(title = "Constructing a Datasaurus Style Dataset",
               x = NULL, y = NULL, 
	            caption = c("Briefed by Data || Thomas J Pfaff"))

print(g1)
ani.pause()


for (i in 1:iterations){
  
  R <- rnorm(n,error,5*error)
  
  XD <- R*A*(1/sqrt(A^2+B^2)-1)
  YD <- R*B*(1/sqrt(A^2+B^2)-1)

  select <- sqrt(A^2 + B^2) > 1.1

  if(max(abs(mean(X)-mean(A+XD)),
         abs(mean(Y)-mean(B+YD)),
         abs(sd(X)-sd(A+XD)),
         abs(sd(Y)-sd(B+YD)),
         abs(cor(X,Y)-cor(A+XD,B+YD)) 
        ) < 0.01 &
     max( pmax( sqrt( A[select]^2 + B[select]^2 ) -
                sqrt( (A[select]+XD[select])^2 + (B[select]+YD[select])^2 ))) < 0.06
    ) {
    A <- A+XD
    B <- B+YD
    moves <- moves+1
  }

  if(graphs < moves){
    Points <- data.frame( x=A, y=B)
    Points <- Points %>% mutate(MeanX = format( round( mean(x), 2), nsmall = 2), 
                MeanY = format( round( mean(y), 2), nsmall = 2),
                SdX = format( round( sd(x), 2), nsmall = 2),
                SdY = format( round( sd(y), 2), nsmall =2 ),
                N = n(), 
                r = format( round( cor(x,y), 2), nsmall=2))
    g2 <- ggplot(Points, aes(x=x, y=y,  label = 
                 paste("Mean ( X, Y ) = ( ", MeanX, ", ", MeanY, " )\nSd ( X, Y ) = ( ",
                                      SdX , ", ", SdY, " )\nr = ", r, sep="" ))) + 
          geom_point(color = MyPurple, size = 4) +
          geom_text( aes(0, 2.75), size = 6, hjust = 0, vjust=0, color = MyPurple) +
          xlim(-4, 4) +
          ylim(-4,4) +
          theme(axis.text = element_text(size = 14),
	         plot.title = element_text(size = 20),
            plot.background = element_rect(fill = MyLightP3),
            panel.background = element_rect(fill = MyLightP), 
            plot.caption = element_text(hjust = 1, size = 14,
                                        color = MyPurple)) +
          labs(title = "Constructing a Datasaurus Style Dataset",
               x = NULL,y = NULL, 
	            caption = c("Briefed by Data || Thomas J Pfaff"))
      print(g2)
      ani.pause()
      graphs <- graphs + 5
    }
}

}, movie.name = "CirclePush3.gif",ani.width = 800, ani.height = 800)







print(moves)
