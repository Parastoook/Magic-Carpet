dd <- read.csv('all_alt_with_gravity.csv')


jobtract<-read.csv('jobtract.csv')
bgtract<-read.csv('bgtract.csv')
bgtract<-bgtract$x


#Driving data
dis <- read.csv("distance-dr.csv")
tt <- read.csv("travel time-dr.csv")

tt <- tt[, -1]
dis <- dis[, -1]

dis <- dis[-bgtract, -bgtract]
tt <- tt[-bgtract, -bgtract]

#Walking data
ttw <- read.csv("travel time-walk.csv")
disw <- read.csv("distance-walk.csv")

ttw <- ttw[, -1]
disw <- disw[, -1]

disw <- disw[-bgtract, -bgtract]
ttw <- ttw[-bgtract, -bgtract]

#Transit data
ttt <- read.csv("travel time-transit.csv")
dist <- read.csv("distance-transit.csv")

ttt <- ttt[, -1]
dist <- dist[, -1]

dist <- dist[-bgtract, -bgtract]
ttt <- ttt[-bgtract, -bgtract]

tt<-tt/60
ttw<-ttw/60
ttt<-ttt/60


##Calculating logsum accessibility

bgtr1<-read.csv('bgtr1.csv')
bgtr1<-bgtr1$x

cd <- 0.1 #cost of driving assumed to be $0.1 per mile

dc <- dis *cd/1609.34

dd$MCCAR <- NA
dd$MCtransit <- NA
dd$MCwalk <- NA

dd$transit_subsidy[dd$transit_subsidy == -99] <- 7
dd$transit_subsidy[is.na(dd$transit_subsidy) == T] <- 7

dd$ct <- 2.5


dd$ct<-ifelse(dd$transit_subsidy == 1 | dd$transit_subsidy == 3, 0 ,dd$ct) 
dd$ct<-ifelse(dd$transit_subsidy == 2 | dd$transit_subsidy == 4,1.5,dd$ct) 


yw <- c()
yw <- ifelse(ttw == 0 , NA, 1)

yt <- c()
yt <- ifelse(ttt== 0 , NA, 1)

for (i in 1:dim(dd)[1]) {
  if (is.na(dd$wtract[i]) == F & (dd$wtract[i] %in% bgtr1) == F) {
    hnum <- which(jobtract$dtract == dd$choice_tract[i])
    dnum <- which(jobtract$dtract == dd$wtract[i])
    
    dd$MCCAR[i] <-
      (-37.2 * ( dc[hnum, dnum] / (dd$income[i])) - 0.0161 * tt[hnum, dnum] )
    dd$MCtransit[i] <-
      (-1.13 - (37.2 * dd$ct[i] / dd$income[i]) - 0.000377 * ttt[hnum, dnum] ) * yt[hnum, dnum]
    dd$MCwalk[i] <- (1.48 - 0.0844 * ttw[hnum, dnum] ) * yw[hnum, dnum]
  }
}
write.csv(dd,'all_alt_log.csv')
dd<-read.csv('all_alt_log.csv')
dd<-data.frame(dd)
dd<-dd[,-c(1,2,4,5,8,22,24,27)]
dd<-dd[,-c(7,9,10,11)]
gg<-read.csv('all alt.csv')

x<-which(is.na(dd$wtract)==T)

for (i in x){
  num<-c()

  num<-which(gg$hhid == dd$hhid[i] & gg$choice_tract == dd$choice_tract)
  dd$wtract[i]<-gg$wtract[num]
  dd$h_tract1[i]<-gg$h_tract1[num]
  
  if (is.na(dd$wtract[i]) == F & (dd$wtract[i] %in% bgtr1) == F) {
    hnum <- which(jobtract$dtract == dd$choice_tract[i])
    dnum <- which(jobtract$dtract == dd$wtract[i])
    
    dd$MCCAR[i] <-
      (-37.2 * ( dc[hnum, dnum] / (dd$income[i])) - 0.0161 * tt[hnum, dnum] )
    dd$MCtransit[i] <-
      (-1.13 - (37.2 * dd$ct[i] / dd$income[i]) - 0.000377 * ttt[hnum, dnum] ) * yt[hnum, dnum]
    dd$MCwalk[i] <- (1.48 - 0.0844 * ttw[hnum, dnum] ) * yw[hnum, dnum]

  }}


dd<-write.csv(dd,'all_alt_log2.csv')
dd<-read.csv('all_alt_log2.csv')

x<-which(is.na(dd$h_tract1)==T)

cc<- unique(gg[,c('h_tract1','hhid')])

for (i in x){
  num<-c()
  num<-which(cc$hhid == dd$hhid[i] )
  dd$h_tract1[i]<-(cc$h_tract1[num])
}


dd<-data.frame(dd)
dd<-write.csv(dd,'all_alt_log3.csv')
dd<-read.csv('all_alt_log3.csv')

dd$choice<-ifelse(is.na(dd$choice)==T,0,dd$choice)

for ( i in 1:dim(cc)[1]){
  dd$choice<-ifelse(dd$hhid == cc$hhid[i] & dd$choice_tract == cc$h_tract1[i],1,dd$choice)
}


write.csv(dd,'all_alt_log4.csv')
dd<-read.csv('all_alt_log4.csv')

dd1<-dd[order(dd$hhid,dd$choice),]


dd1$transitexp = exp(dd1$MCtransit)
dd1$walkexp = exp(dd1$MCwalk)
dd1$carexp = exp(dd1$MCCAR)

dd1$transitexp[is.na(dd1$transitexp)]<- 0
dd1$walkexp[is.na(dd1$walkexp)]<- 0
dd1$carexp[is.na(dd1$carexp)]<- 0

dd1$logsum <- NA

dd1$logsum <- log(dd1$carexp+dd1$transitexp+dd1$walkexp)


dd1$choice_id<-NA


bb<-unique(dd1$choice_tract)
bb<-bb[order(bb)]

for (i in 1:length(bb)){
  dd1$choice_id<-ifelse(dd1$choice_tract == bb[i],i,dd1$choice_id)
}







dd1$workhomesame<-0

dd1$workhomesame <-ifelse(dd1$choice_tract == dd1$wtract,1,dd1$workhomesame )


dd1$logsum.whsame <- dd1$logsum*(1-dd1$workhomesame)
write.csv(dd1,'finalchoiceset.csv')

dd1<-read.csv('finalchoiceset.csv')
dd1$child<-ifelse(dd1$lifecycle == 2,1,0)

dd1<-data.frame(dd1)
dd1<-dd1[,-c(1,2,3,4,9,11,12,18,21,22,23,24,25)]
dd1<-dd1[,-c(14,15,16)]

write.csv(dd1,'finalchoicesetshort.csv')
