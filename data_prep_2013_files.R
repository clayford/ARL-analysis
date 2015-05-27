# 2013 data prep
# files: law, health, special collections

library(xlsx)
law <- read.xlsx("data/2013LawPubl.xlsx",sheetIndex = 1)
sc <- read.xlsx("data/2013SpecCollPubl.xlsx", sheetIndex = 1)
hs <- read.xlsx("data/2013HealthSciePubl.xlsx", sheetIndex = 1)

# length(intersect(names(law),names(sc)))
# length(intersect(names(law),names(hs)))
# length(intersect(names(sc),names(hs)))
# 
# setdiff(names(hs), names(sc))
# setdiff(names(hs), names(sc)) == setdiff(names(law), names(sc))
# d <- setdiff(names(hs), names(sc))

common <- intersect(names(hs), names(sc))

law <- law[,common]
hs <- hs[,common]
sc <- sc[,common]

# all(names(law) == names(hs))
# all(names(law) == names(sc))
# all(names(sc) == names(hs))

# identify origin
law$origin <- "law"
hs$origin <- "health.sciences"
sc$origin <- "special.collections"

dat <- rbind(hs,law,sc)
dat$origin <- factor(dat$origin)


# explore data
# number of missing in each column
# sapply(dat,function(x)sum(is.na(x)))

save(dat,file="ARL2013data.Rda")
