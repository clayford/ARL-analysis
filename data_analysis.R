#' ---
#' title: "ARL data exploration and analysis"
#' author: "Clay Ford"
#' date: "Feb 20, 2015"
#' output: pdf_document
#' ---

# notes from MPC:
# pairwise plots
# size of univ vs budget
# number of faculty
# simple linear regression

# do one year to start with

# remove canadian univ (type == C)
# type == P and type == S
# include all regions

# highlight outliers
# private/state and just state

library(ggplot2)
library(scales)
library(dplyr)
load("R.default.par.RData")

# Exploratory -------------------------------------------------------------


load("~/_clients/Tolson/ARLdata.Rda")
# NOTE: data contains only variables common to 2010 - 2014 data.

# just look at type == P and type == S
dat <- subset(dat, type %in% c("P","S"))

# variables of interest
vars <- c("totexp", "totstu", "phdawd", "phdfld", "fac", "vols", "gradstu")

# correlations
cor(dat[vars], use = "pairwise.complete.obs")

# top 10 plus uva in variables of interest

t10uva <- function(d, var, n=10){
  tmp <- d[c(var, "inam")]
  tmp <- tmp[order(tmp[var], decreasing = T),]  
  row.names(tmp) <- NULL
  tmp <- rbind(head(tmp, n=10),tmp[tmp$inam == "VIRGINIA",])
  if(var=="totexp") tmp[,1] <- dollar(tmp[,1]) else
    tmp[,1] <- comma(tmp[,1])
  tmp
} 

tmp <- split(x = dat, dat$year)
# lapply(tmp, t10uva, var="totexp")


for(i in vars){
  print(paste("####",i,"####"))
  print(lapply(tmp, t10uva, var=i))
}


# function for scatter plots by year
splots <- function(d, x, xlab, title){
  ggplot(subset(d, type %in% c("P","S")), aes_string(y="totexp", x=x)) + geom_point() + 
    geom_smooth(se=F) + geom_smooth(se=F, method="lm", color="grey") + 
    scale_y_continuous(labels=dollar) +
    labs(y="Total Expenditures",x=xlab) +
    facet_wrap(~year) + ggtitle(title) + 
    # make point for UVa
    geom_point(data = subset(d, instno=="8900"), mapping = aes_string(y="totexp", x=x), 
               color="red", size=3)
}

# UVa is red dot in plots

# 1
splots(dat, x="totstu", xlab="Total Full-time Student Enrollment", 
       title="Total Expenditures vs. Total Full-time Student Enrollment")
splots(dat, x="phdawd", xlab="Ph.Ds Awarded", 
       title="Total Expenditures vs. Ph.Ds Awarded")
splots(dat, x="phdfld", xlab="Ph.D. Fields",
       title="Total Expenditures vs. Ph.D. Fields")


# NOTE
# GEORGIA TECH likely has data entry error for 2014; phdawd and phdfld fields transposed
dat[dat$inam=="GEORGIA TECH",c("phdawd","phdfld")]
# CF: FIXING
dat[dat$inam=="GEORGIA TECH" & dat$year==2014,c("phdawd","phdfld")] <- c(488, 30)



splots(dat, x="fac", xlab="Instructional Faculty",
       title="Total Expenditures vs. Instructional Faculty")
# 2
splots(dat, x="gradstu", xlab="Total Graduate Student Enrollment",
       title="Total Expenditures vs. Total Graduate Student Enrollment")
splots(dat, x="vols", xlab="Volumes Held",
       title="Total Expenditures vs. Volumes Held")

# who has phdawd > 3000?
dat[dat$phdawd>3000,"inam"]


library(corrplot)
corrplot(cor(dat[vars],use = "pairwise.complete.obs"))

par(mfrow=c(2,3))
for(i in 2010:2014){
  corrplot(cor(dat[dat$year==i, vars],use = "pairwise.complete.obs"), title = i, mar=c(0,0,1,0))
}

par(par.defaults)


# Distribution of totexp; UVa is red dots

# totexp is skewed
with(dat, MASS::truehist(totexp))
x <- dat[dat$inam=="VIRGINIA","totexp"]
with(dat, points(x,rep(0,5), col="red",pch=19))

# log transformed
with(dat, MASS::truehist(log(totexp)))
with(dat, points(log(x),rep(0,5), col="red",pch=19))


# totexp over time; uva is red
ggplot(dat, aes(x=year, y=totexp, group=inam)) + geom_line() +
  scale_y_continuous(label=dollar) +
  geom_line(data = subset(dat, instno=="8900"), mapping = aes(y=totexp, x=year), color="red", size=2)



# some basic modeling -----------------------------------------------------
# all vars
mod.all <- lm(totexp ~ totstu + phdawd + phdfld + fac + vols + gradstu + year, data=dat, 
              subset= inam!="VIRGINIA")

summary(mod.all)
# drop year?
mod.2 <- lm(totexp ~ totstu + phdawd + phdfld + fac + vols + gradstu, data=dat, 
              subset= inam!="VIRGINIA")

anova(mod.2, mod.all)
summary(mod.2)
# plot(mod.2)
# drop year

# 532 is influental; variance looks nonconstant; normality iffy
mod.3 <- lm(log(totexp) ~ totstu + phdawd + phdfld + fac + vols + gradstu, data=dat, 
            subset= inam!="VIRGINIA")
# plot(mod.3)

mod.3 <- lm(log(totexp) ~ totstu + phdawd + phdfld + fac + vols + gradstu, data=dat, 
            subset= inam!="VIRGINIA", na.action=na.exclude)
# plot(mod.3)

# under-predicting by about 8 million
dollar(exp(predict(mod.3, newdata = dat[dat$inam=="VIRGINIA",])))
dollar(dat[dat$inam=="VIRGINIA","totexp"])

summary(mod.3)

# without phdawd and phdhld
mod.4 <- lm(log(totexp) ~ totstu + fac + vols + gradstu, data=dat, 
                     subset= inam!="VIRGINIA", na.action=na.exclude)

summary(mod.4)
# plot(mod.4)


# modeling by year --------------------------------------------------------

for(i in 2010:2014){
  tmp.mod <- lm(log(totexp) ~ totstu +  fac + vols + gradstu, 
                data=subset(dat, year==i), 
                subset= inam!="VIRGINIA")
  cat("####", i ,"####", "\n")
  cat("PREDICTED:", dollar(exp(predict(tmp.mod, dat[dat$inam=="VIRGINIA" & dat$year==i,]))),"\n")
  cat("ACTUAL:", dollar(dat[dat$inam=="VIRGINIA" & dat$year==i,"totexp"]),"\n")
}


# regression trees --------------------------------------------------------


# regression tree for fun
library(rpart)
# Build tree without UVa
tfit <- rpart(totexp ~ totstu + phdawd + phdfld + fac + vols + gradstu, data=dat, 
              subset= inam!="VIRGINIA" & year==2012)
plot(tfit, margin=0.1)
text(tfit, use.n=TRUE)
printcp(tfit)
# plotcp(tfit)
m <- tfit$cptable[which.min(tfit$cptable[,4]),1]
# prune the tree
ptfit <- prune(tfit, cp=m)
plot(ptfit, margin=0.1)
text(ptfit, use.n=TRUE)
# predict UVa totexp
pred <- predict(ptfit, newdata = subset(dat, inam=="VIRGINIA" & year==2012))
pred
real <- subset(dat, inam=="VIRGINIA" & year==2012, select=totexp)
real
# compare to actual totexp
pred > real

library(randomForest)
rf.fit <- randomForest(totexp ~ totstu + phdawd + phdfld + fac + vols + gradstu, data=dat, 
                       na.action = na.omit)
varImpPlot(rf.fit)
