# function to geneate a platetools plot with concentration of libraries

library(ggplot2)
library(gridExtra)
library(platetools)

# CHANGE ME: point df to the alignment data csv

df = read.csv("")

sorter_platetools_qc_plot <- function(df){
  df$intended_hit_or_miss <- as.factor(df$intended_hit_or_miss)
  df$hit_or_miss <- as.factor(df$hit_or_miss)
  
  a <- raw_map(data=df$intended_hit_or_miss,
               well=df$sample,
               plate=96) +
    ggtitle("intended alignment") +
    theme(legend.position="none") +
    scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))
  
  b <- raw_map(data=df1$hit_or_miss,
               well=df1$sample,
               plate=96) +
    ggtitle("actual alignment") +
    theme(legend.position="none") +
    scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))
  
  grid.arrange(a,b,ncol=1)
}

sorter_platetools_qc_plot(df)
