# data analysis for 2013 dat
# special collections, health sciences, law

library(ggplot2)
library(scales)
library(dplyr)
load("ARL2013data.Rda")

# just look at type == P and type == S
dat <- subset(dat, type %in% c("P","S"))

# variables of interest
vars <- c("totexp", "totstu", "phdawd", "phdfld", "fac", "vols", "gradstu")

