library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)

census2016 = read.csv(file = "Data/CensusLocalAreaProfiles2016.csv",header = TRUE, skip=4)
pop2016 = census2016[1:78,]

name= c('id','age','AR','CBD','DS','FAIR','GW','HS', 'KC','KERR','KIL','KITS','MARP','MP','OAK','RC','RP','SHAU','SC','STR','SUN','VF','WE','WPG','vancsd','vancma')

colnames(pop2016) = name

pop2016$gender <- 'total'
pop2016[27:52, ]$gender <- 'male'
pop2016[53:78, ]$gender <- 'female'
pop2016$year <- 2016

pop2016$id <- NULL
pop2016$vancsd <- NULL
pop2016$vancma <- NULL


pop2016 = pop2016[-c(1,2,6,17,22,27,28,32,43,48,53,54,58,69,74),]
pop2016$age <- paste0('a_', str_pad(word(pop2016$age,-4), width=3, side="left", pad="0"))

pop2016[,2:23] <- lapply(pop2016[,2:23], gsub, pattern=',', replacement='')
pop2016[,2:23] <- lapply(pop2016[,2:23], as.numeric)
pop2016[is.na(pop2016)] <- 0

pop2016_pct = pop2016
pop2016_pct[,2:23]=transmute_all(pop2016_pct[,2:23], funs("pct"= ./sum(.)*2))

pop2016_pct_reshape = gather(pop2016_pct,"area","pct", -c(age, gender, year))
pop2016_reshape = gather(pop2016,"area","people", -c(age, gender, year))

pop2016_reshape = merge(pop2016_reshape, pop2016_pct_reshape,by=c("age","area","gender","year"))
rm(pop2016_pct, pop2016_pct_reshape)


###############################2011################################
census2011 = read.csv(file = "Data/CensusLocalAreaProfiles2011.csv",header = TRUE, skip=4)
pop2011 = census2011[1:78,]

name= c('age','AR','CBD','DS','FAIR','GW','HS', 'KC','KERR','KIL','KITS','MARP','MP','OAK','RC','RP','SHAU','SC','STR','SUN','VF','WE','WPG','vancsd','vancma')
colnames(pop2011) = name

pop2011$vancsd <- NULL
pop2011$vancma <- NULL

pop2011$gender <- 'total'
pop2011[27:52, ]$gender <- 'male'
pop2011[53:78, ]$gender <- 'female'
pop2011$year <- 2011

pop2011 = pop2011[-c(1,6:10,25:27,32:36,51:53,58:62,77:78),]
pop2011$age <- str_trim(pop2011$age, side = "left")
pop2011$age <- paste0('a_', str_pad(word(pop2011$age,1), width=3, side="left", pad="0"))

pop2011[,2:23] <- lapply(pop2011[,2:23], gsub, pattern=',', replacement='')
pop2011[,2:23] <- lapply(pop2011[,2:23], as.numeric)
pop2011[is.na(pop2011)] <- 0

pop2011_pct = pop2011
pop2011_pct[,2:23]=transmute_all(pop2011_pct[,2:23], funs("pct"= ./sum(.)*2))

pop2011_pct_reshape = gather(pop2011_pct,"area","pct", -c(age, gender, year))
pop2011_reshape = gather(pop2011,"area","people", -c(age, gender, year))

pop2011_reshape = merge(pop2011_reshape, pop2011_pct_reshape,by=c("age","area","gender","year"))
rm(pop2011_pct, pop2011_pct_reshape)

pop = rbind(pop2011_reshape,pop2016_reshape )
pop$pct = NULL

pop_reshape <-reshape(pop,timevar="age",idvar=c("area","gender","year"),direction="wide")
pop_reshape[is.na(pop_reshape)]<-0
colnames(pop_reshape) = gsub("people.","",colnames(pop_reshape))

# Redefining a_85 as 85 years and above to match age bracket in census2016 with census 2011
pop_reshape$a_85sum = pop_reshape$a_085 + pop_reshape$a_090 + pop_reshape$a_095 + pop_reshape$a_100

pop_reshape$a_085 = NULL
pop_reshape$a_090 = NULL
pop_reshape$a_095 = NULL
pop_reshape$a_100 = NULL
colnames(pop_reshape)[ncol(pop_reshape)]  = 'a_085'

colnames(pop_reshape)[4:21]
pop_reshape$totalpop = rowSums(pop_reshape[,4:21])

###################GeoData##############################

area =  read.csv(file = "Data/local-area-boundary.csv",header = TRUE, sep=';')
colnames(area)[1] = 'area'
pop_reshape = merge(pop_reshape, area, by='area')


###################Commute Data##############################
workcommute = census2016[2342:2356,]

name= c('id','commute','AR','CBD','DS','FAIR','GW','HS', 'KC','KERR','KIL','KITS','MARP','MP','OAK','RC','RP','SHAU','SC','STR','SUN','VF','WE','WPG','vancsd','vancma')
colnames(workcommute) = name

workcommute[,3:ncol(workcommute)] <- lapply(workcommute[,3:ncol(workcommute)], gsub, pattern=',', replacement='')
workcommute[,3:ncol(workcommute)] <- lapply(workcommute[,3:ncol(workcommute)], as.numeric)

workcommute$gender <- 'total'
workcommute[6:10, ]$gender <- 'male'
workcommute[11:15, ]$gender <- 'female'
workcommute$year <- 2016
workcommute$id <- NULL
workcommute$vancsd <- NULL
workcommute$vancma <- NULL

workcommute = workcommute[-c(1,6,11),]
workcommute$commute= c('incsd','incd','inprov','outprov') 

workcommute_reshape <-gather(workcommute,"area","people", -c(commute, gender, year))
workcommute_reshape <-reshape(workcommute_reshape,timevar="commute",idvar=c("area","gender","year"),direction="wide")

colnames(workcommute_reshape) = gsub("people.","",colnames(workcommute_reshape))
workcommute_reshape$commutepop= rowSums(workcommute_reshape[,4:7])

# Have to convert commute population to percentage, since only 25% sample of commuting pop are taken
workcommute_reshape[,4:7]=transmute_all(workcommute_reshape[,4:7], funs("pct"= ./workcommute_reshape$commutepop))
workcommute_reshape$commutepop <- NULL

pop_reshape = merge(pop_reshape, workcommute_reshape, by=c('area','year','gender'), all.x = TRUE)


###################Mode of Transport Data##############################
mot = census2016[2358:2378,]

name= c('id','mode','AR','CBD','DS','FAIR','GW','HS', 'KC','KERR','KIL','KITS','MARP','MP','OAK','RC','RP','SHAU','SC','STR','SUN','VF','WE','WPG','vancsd','vancma')
colnames(mot) = name

mot[,3:ncol(mot)] <- lapply(mot[,3:ncol(mot)], gsub, pattern=',', replacement='')
mot[,3:ncol(mot)] <- lapply(mot[,3:ncol(mot)], as.numeric)

mot$gender <- 'total'
mot[8:14, ]$gender <- 'male'
mot[15:21, ]$gender <- 'female'
mot$year <- 2016
mot$id <- NULL
mot$vancsd <- NULL
mot$vancma <- NULL

mot = mot[-c(1,8,15),]
mot$mode= c('car_drv','car_psg','transit','walk','bicycle','other') 

mot_reshape <-gather(mot,"area","people", -c(mode, gender, year))
mot_reshape <-reshape(mot_reshape,timevar="mode",idvar=c("area","gender","year"),direction="wide")

colnames(mot_reshape) = gsub("people.","",colnames(mot_reshape))
mot_reshape$transitpop= rowSums(mot_reshape[,4:9])

# Have to convert commute population to percentage, since only 25% sample of commuting pop are taken
mot_reshape[,4:9]=transmute_all(mot_reshape[,4:9], funs(./mot_reshape$transitpop))

mot_reshape$transitpop <- NULL

###################Final Processing##############################

pop_final = merge(pop_reshape, mot_reshape, by=c('area','year','gender'), all.x = TRUE)
pop_final$youth = pop_final$a_000 + pop_final$a_005 + pop_final$a_010 +pop_final$a_015
pop_final$workingage = pop_final$a_020 + pop_final$a_025 + pop_final$a_030 + pop_final$a_035 + pop_final$a_040
pop_final$middleage = pop_final$a_045 + pop_final$a_050 + pop_final$a_055 +pop_final$a_060 
pop_final$senior = pop_final$a_065 + pop_final$a_070 + pop_final$a_075 + pop_final$a_080 + pop_final$a_085

pop_final$Latitude =  as.numeric(gsub(",.*$", "", pop_final$geo_point_2d))
pop_final$Longitude =  as.numeric(gsub("^.*,", "", pop_final$geo_point_2d))

write.csv(pop_final,'Census.csv')

###################Graph##############################

ggplot(data=pop2016_reshape[pop2016_reshape$gender=='total',], aes(x = age, y = pct, color=area)) + geom_bar(stat='identity') + facet_wrap(facets=vars(area)) + xlab('Age Distribution (Pct By Area)') + ylab('') + theme(axis.text.x=element_blank(), axis.text.y=element_text(size=5), axis.title=element_text(size=12,face="bold")) 

ggsave("output/pop-by-age-2016.jpg")

ggplot(data=pop2011_reshape[pop2011_reshape$gender=='total',], aes(x = age, y = pct, color=area)) + geom_bar(stat='identity') + facet_wrap(facets=vars(area)) + xlab('Age Distribution (Pct By Area)') + ylab('') + theme(axis.text.x=element_blank(), axis.text.y=element_text(size=5), axis.title=element_text(size=12,face="bold")) 

ggsave("output/pop-by-age-2011.jpg")