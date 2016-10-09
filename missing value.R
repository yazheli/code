library(mice)
library(VIM)
numerical = subset(D1, select=c("score", "CLTV", "DTI", "UPB", "LTV",
                                         "OIR", "orig.loan.term", 
                                         "number.borrowers","def_flag"))


pattern = md.pattern(numerical);pattern

#locate index of na with out column DTI
index_table = which(is.na(numerical[,-3]), arr.ind=TRUE)
numerical = numerical[-index_table[,1],]
pattern = md.pattern(numerical);pattern


