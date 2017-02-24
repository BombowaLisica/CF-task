
library(dplyr)
require(data.table)
library(tidyr)

#setwd("")
# movieLens data
dane <- read.table("u.data")
names <- c("user","movie","rate","time")
names(dane) <- names

test1 <- mutate(dane, row.numb = row_number()) %>%
  group_by(user) %>%
  arrange(-time)
test1 <- data.table(test1, key = "user")
test1 <- test1[, head(.SD, 12), by = user]

ucz <- mutate(dane, row.numb = row_number()) %>%
  group_by(user) %>%
  arrange(-time)
ucz <- data.table(ucz, key = "user")
ucz <- subset(ucz, !unique(ucz$row.numb) %in% (test1$row.numb))
head(ucz)
head(test1)

test <- test1[, head(.SD, 6), by = user]
wal <- subset(test1, !(unique(test1$row.numb) %in% unique(test$row.numb)))
dim(ucz)[1]/100000  # 0.94342
dim(test1)[1]/100000 # 0.05658
dim(wal)[1]/100000 # 0.05658
dim(test)[1]/100000 # 0.05658

rm(test1)

ucz <- data.frame(ucz)
wal <- data.frame(wal)
test <- data.frame(test)