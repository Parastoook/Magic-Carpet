
dd <- read.csv('all alt.csv')
jobtract<-read.csv('jobtract.csv')
bgtract<-read.csv('bgtract.csv')
bgtract<-bgtract$x

## Calculating Gravity Aceessibility for each household

B_Emp <- 0.221
B_impedence <- 3.94

Bw <- 0.697
Bt <- 1.01

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

#adding a row for gravity accessibility
dd$gaccess <- 0



for (i in 1:dim(dd)[1]) {
  income <- c()
  income <- dd$income[i]
  
  num <- which(jobtract$dtract == dd$choice_tract[i])
  
  for (j in 1:dim(jobtract)[1]) {
    dcost <-  dis[num, j] * 0.1 * 365 / (income * 1609.34)
    Cc <- c()
    Cc <- (tt[num, j] / 60) + (7.304 * dcost)
    
    Ww <- 3.108 * ttw[num, j] / 60
    yw <- c()
    yw <- ifelse(ttw[num, j] == 0 , 0, 1)
    
    yt <- c()
    yt <- ifelse(ttt[num, j] == 0 , 0, 1)
    
    Tt <- 1.492 * ttt[num, j] / 60 + 7.304 * 2.5 * 365 / income
    
    CompImp <-
      (yw * (1 - yt) * (Cc  / (1 + (Cc / (
        Ww ^ Bw
      )))) + (yt * yw * (Cc / (
        1 + (Cc / (Tt ^ Bt)) + (Cc / (Ww ^ Bw))
      ))))
    
    if (CompImp != 0 & is.na(CompImp) == F)  {
      dd$gaccess[i] <-
        dd$gaccess[i] + (jobtract$Estimate..Total....Retail.trade[j] ^ (B_Emp)) / ((CompImp) ^
                                                                                     B_impedence)
    }
  }
  
  dd$gaccess[i] <- dd$gaccess[i] / dim(jobtract)[1]
}


##Calculating logsum accessibility
bgtr1<-read.csv('bgtr1.csv')
bgtr1<-bgtr1$x

cd <- 0.1 #cost of driving assumed to be $0.1 per mile

dd$MCCAR <- NA
dd$MCtransit <- NA
dd$MCwalk <- NA

dd$transit_subsidy[dd$transit_subsidy == -99] <- 7
dd$transit_subsidy[is.na(dd$transit_subsidy) == T] <- 7

for (i in 1:dim(dd)[1]) {
  if (is.na(dd$wtract[i]) == F & (dd$wtract[i] %in% bgtr1) == F) {
    hnum <- which(jobtract$dtract == dd$choice_tract[i])
    dnum <- which(jobtract$dtract == dd$wtract[i])
    yw <- c()
    yw <- ifelse(ttw[hnum, dnum] == 0 , NA, 1)
    
    yt <- c()
    yt <- ifelse(ttt[hnum, dnum] == 0 , NA, 1)
    
    ct <- 2.5
    
    if (dd$transit_subsidy[i] == 1 | dd$transit_subsidy[i] == 3) {
      ct <- 0
    }
    
    if (dd$transit_subsidy[i] == 2 | dd$transit_subsidy[i] == 4) {
      ct <- 1.5
    }
    
    dd$MCCAR[i] <-
      (-37.2 * (cd * dis[hnum, dnum] / (dd$income[i] * 1609.34)) - 0.0161 * tt[hnum, dnum] / 60)
    dd$MCtransit[i] <-
      (-1.13 - (37.2 * ct / dd$income[i]) - 0.000377 * ttt[hnum, dnum] / 60) * yt
    dd$MCwalk[i] <- (1.48 - 0.0844 * ttw[hnum, dnum] / 60) * yw
    
  }
}

dd$child<-ifelse(dd$lifecycle == 2,1,0)
dd<-dd[which(dd$hh_income_detailed != 98),]


write.csv(dd,"lca-choiceset-v4.csv")

#dd1<-read.csv("lca-choicesetfin-v2.csv")

dd1<-dd[which(is.na(dd$wtract)==F),]
dd1<-dd1[which((dd1$wtract%in% bgtr1)==F),]

dd1$transitexp = exp(dd1$MCtransit)
dd1$walkexp = exp(dd1$MCwalk)
dd1$carexp = exp(dd1$MCCAR)

dd1$transitexp[is.na(dd1$transitexp)]<- 0
dd1$walkexp[is.na(dd1$walkexp)]<- 0
dd1$carexp[is.na(dd1$carexp)]<- 0

dd1$logsum <- NA


dd1$logsum <- log(dd1$carexp+dd1$transitexp+dd1$walkexp)
dd1$choice<-0
dd1$choice_id<-NA

for (i in 1:dim(dd1)){
  if(i %% 39 == 0){
    dd1$choice[i]<-1
  }
}

for (i in 1:dim(dd1)[1]){
  dd1$choice_id[i]<-i %% 39
}

dd1$choice_id<-ifelse(dd1$choice_id == 0,39,dd1$choice_id)


dd1$schoolrate<-dd1$gsRating
sch_score<-sch_score[which(is.na(sch_score$gsRating)==F),]

for (i in 1:dim(dd1)[1]){
  if(is.na(dd1$gsRating[i]) ==T){
    num<- which(tract1$NAME10 == as.character(dd1$choice_tract[i]))
    xx<-gTouches(tract1[num,],tract1, byid=TRUE)
    neigh<-which(xx == TRUE)
    tract_name<-tract1$NAME10[neigh]
    dd1$schoolrate[i]<-mean(sch_score$gsRating[which(((sch_score$tract/100) %in% as.numeric(as.character(tract_name)))==T)])
  }
}

dd1$workhomesame<-NA
dd1$workhomesame <-ifelse(dd1$choice_tract == dd1$wtract,1,0 )

dd1$logsum.whsame <- dd1$logsum*(1-dd1$workhomesame)




write.csv(dd1,"lca-choicesetfin-v5.csv")

