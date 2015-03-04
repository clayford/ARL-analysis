# read in data
# data in excel file on 5 different worksheets

library(xlsx)

dat <- vector(mode = "list", length = 5)
for(i in 1:5){
  dat[[i]] <- read.xlsx(file = "5yearRegressiondata.xlsx", sheetIndex = i)
}

# how many vars each year?
# 2010 and 2011 have 81 variables; 2012 - 2014 have 52
# sapply(dat,length)


# all(names(dat[[1]]) == names(dat[[2]]))
# all(names(dat[[3]]) == names(dat[[4]]))
# all(names(dat[[3]]) == names(dat[[5]]))
# all(names(dat[[4]]) == names(dat[[5]]))

# all columns in common between the 5 years
common <- intersect(x = names(dat[[1]]), y= names(dat[[3]]))
# select columns in common
dat <- lapply(dat, function(x)`[`(x, common))
# combine data frames in list into one data frame
dat <- do.call(rbind,dat)

# inspection
# rows with all missing data?
# any(apply(dat,1,function(x)all(is.na(x))))
# # columns with all missing data
# any(apply(dat,2,function(x)all(is.na(x))))
# # any missing values in the inam column?
# any(is.na(dat$inam))
# 
# summary(dat)

dat$year <- factor(dat$year)
dat$instno <- factor(dat$instno)


# write out for analysis
# save(dat, file="ARLdata.Rda")

rm(i, common)




# adding index data -------------------------------------------------------

# load("ARLdata.Rda")
dat <- dat[order(dat$inam, dat$year),]

# get indices for 2010 - 2013
ind <- vector(mode = "list", length = 4)
for(i in 1:4){
  ind[[i]] <- read.xlsx("index13.xlsx", sheetIndex = i+2, startRow = 2)
}
# select just the two needed columns
ind <- lapply(ind, function(x) subset(x, select = c("Institution.Name", "Index")))


for(i in 1:4){
  ind[[i]]$year <- (2014-i)
}

ind <- do.call(rbind,ind)
ind <- subset(ind, !is.na(Index))
ind <- subset(ind, Institution.Name!="115")
ind$Institution.Name <- droplevels(ind$Institution.Name)
names(ind)[1] <- "inam"
ind$year <- factor(ind$year)

# merge ind with dat; left join; retain everything in dat, merge only what
# matches in ind
dat <- merge(dat, ind, all.x = TRUE)
# sort by inam and year

dat <- dat[order(dat$inam, dat$year),]
row.names(dat) <- NULL

save(dat, file="ARLdata.Rda")

rm(ind)
# scrap

# 
# # read in as separate data frames
# for(i in 1:5){
#   assign(x = paste0("dat",i), value = read.xlsx(file = "5yearRegressiondata.xlsx", sheetIndex = i))
# }
