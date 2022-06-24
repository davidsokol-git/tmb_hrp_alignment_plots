# plot data from flow validation done on 6/23/2022
# rmg-2 cell line used for alignment
# a hit is coded as 1 and a miss is coded as 0 in the data frames
# s6 used for 96 well plate
# astrios used for 384 well plate

library(ggplot2)
library(platetools)
library(gridExtra)

#
##
### 96 well plate alignment with beads
##
#

## function to do the platetools plots

sorter_platetools_qc_plot <- function(flow_sort_matrix_csv){
  df1 = read.csv(flow_sort_matrix_csv)
  df1$intended_hit_or_miss <- as.factor(df1$intended_hit_or_miss)
  df1$hit_or_miss <- as.factor(df1$hit_or_miss)
  
  a <- raw_map(data=df1$intended_hit_or_miss,
               well=df1$sample,
               plate=96) +
    ggtitle(paste("Intended cell alignment from\n",basename(flow_sort_matrix_csv))) +
    theme(legend.position="none") +
    scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))
  
  b <- raw_map(data=df1$hit_or_miss,
               well=df1$sample,
               plate=96) +
    ggtitle(paste("Actual cell alignment from\n",basename(flow_sort_matrix_csv))) +
    theme(legend.position="none") +
    scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))
  
  a # intended bead alignment
  b # actual bead alignment
  grid.arrange(a,b,ncol=1)
}


## run the function
# test
# sorter_platetools_qc_plot("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_match_a.csv")

# run the function for our plates

dir <- '~/Documents/20220616_flow_alignment_tests/data/'
files <- list.files(dir,pattern = '.csv')
files2 <- paste0(dir,files)
lapply(files2,FUN=sorter_platetools_qc_plot)

## manually done 6 times

df1 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_match_a.csv")
df1$intended_hit_or_miss <- as.factor(df1$intended_hit_or_miss)
df1$hit_or_miss <- as.factor(df1$hit_or_miss)

a <- raw_map(data=df1$intended_hit_or_miss,
             well=df1$sample,
             plate=96) +
  ggtitle("intended cell alignment (experiment replica A)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))

b <- raw_map(data=df1$hit_or_miss,
             well=df1$sample,
             plate=96) +
  ggtitle("actual cell alignment (experiment replica A)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))

a # intended bead alignment
b # actual bead alignment
grid.arrange(a,b,ncol=1)

#
##
### replicate A for 96 well plate with cells
##
#

df2 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_a.csv")
df2$intended_hit_or_miss <- as.factor(df2$intended_hit_or_miss)
df2$hit_or_miss <- as.factor(df2$hit_or_miss)

c <- raw_map(data=df2$intended_hit_or_miss,
             well=df2$sample,
             plate=96) +
  ggtitle("intended cell alignment (replicate A)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

d <- raw_map(data=df2$hit_or_miss,
             well=df2$sample,
             plate=96) +
  ggtitle("actual cell alignment (replicate A)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

c # intended cell alignment for replicate a
d # intended cell alignment for replicate b
grid.arrange(c,d,ncol=1)

#
##
### replicate B for 96 well plate with cells
##
#

df3 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_b.csv")
df3$intended_hit_or_miss <- as.factor(df3$intended_hit_or_miss)
df3$hit_or_miss <- as.factor(df3$hit_or_miss)

e <- raw_map(data=df3$intended_hit_or_miss,
             well=df3$sample,
             plate=96) +
  ggtitle("intended cell alignment (replicate B)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

f <- raw_map(data=df3$hit_or_miss,
             well=df3$sample,
             plate=96) +
  ggtitle("actual cell alignment (replicate B)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

e # intended alignment for replicate b
f # actual alignment for replicate b
grid.arrange(e,f,ncol=1)

#
##
### replicate C for 96 well plate with cells
##
#

df4 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_c.csv")
df4$intended_hit_or_miss <- as.factor(df4$intended_hit_or_miss)
df4$hit_or_miss <- as.factor(df4$hit_or_miss)

g <- raw_map(data=df4$intended_hit_or_miss,
             well=df4$sample,
             plate=96) +
  ggtitle("intended cell alignment (replicate C)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

h <- raw_map(data=df4$hit_or_miss,
             well=df4$sample,
             plate=96) +
  ggtitle("actual cell alignment (replicate C)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

g # intended alignment for replicate c
h # actual alignment for replicate c
grid.arrange(g,h,ncol=1)

#
##
### replicate D for 96 well plate with cells
##
#

df5 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_d.csv")
df5$intended_hit_or_miss <- as.factor(df5$intended_hit_or_miss)
df5$hit_or_miss <- as.factor(df5$hit_or_miss)

i <- raw_map(data=df5$intended_hit_or_miss,
             well=df5$sample,
             plate=96) +
  ggtitle("intended cell alignment (replicate D)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

j <- raw_map(data=df5$hit_or_miss,
             well=df5$sample,
             plate=96) +
  ggtitle("actual cell alignment (replicate D)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1"))

i # intended alignment for replicate d
j # actual alignment for replicate d
grid.arrange(i,j,ncol=1)

#
##
### x-tra replicate for 96 well plate
##
#

df6 = read.csv("~/Documents/20220616_flow_alignment_tests/data/flow_sort_cell_alignment_match_b.csv")
df6$intended_hit_or_miss <- as.factor(df6$intended_hit_or_miss)
df6$hit_or_miss <- as.factor(df6$hit_or_miss)

k <- raw_map(data=df6$intended_hit_or_miss,
             well=df6$sample,
             plate=96) +
  ggtitle("intended cell alignment (experiment replica B") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory","lightblue1", "royalblue1"))

l <- raw_map(data=df6$hit_or_miss,
             well=df6$sample,
             plate=96) +
  ggtitle("actual alignment (experiment replica B)") +
  theme(legend.position="none") +
  scale_fill_manual(values=c("ivory", "lightblue1", "royalblue1"))

k # intended alignment for replicate d
l # actual alignment for replicate d
grid.arrange(k,l,ncol=1)
