library(lpSolveAPI)
nbaData <- data.frame(optimal = list(1,2,3,4))
nbaData <- read.csv("~/Documents/NBAValueFinder/prediction_age.csv", header = TRUE)
col<-c("isPG","isSG","isSF","isPF","isC")
pos<-c("PG","SG","SF","PF","C")
for(i in seq(1,NROW(nbaData))){
  for(j in seq(1,5)){
    nbaData[i,col[j]]<-0
    if(grepl(pos[j],nbaData[i,'pos'])){
      nbaData[i,col[j]]<-1
    }
  } 
}
nbaData['cost']=1
numPlay<-12
total<-14000
return<-list(1,2,3,4,5)
df<-data.frame(optimal=return)
vals<-data.frame(optimal=return)
p<-numeric(numPlay)
second_time<-0
while(total>13000) {
  count<-1
  model<-make.lp(0,NROW(nbaData))
  set.objfn(model,c(nbaData[['fantasy.points']]))
  add.constraint(model,c(nbaData[['cost']]),"<=",200)
  add.constraint(model,numeric(NROW(nbaData))+1,"=",numPlay)
  add.constraint(model,c(nbaData[['isPG']]),">=",2)
  add.constraint(model,c(nbaData[['isSG']]),">=",2)
  add.constraint(model,c(nbaData[['isSF']]),">=",2)
  add.constraint(model,c(nbaData[['isPF']]),">=",2)
  add.constraint(model,c(nbaData[['isC']]),">=",2)
  set.type(model,seq(1,NROW(nbaData)),"binary")
  lp.control(model,sense='max')
  solve(model)
  player<-get.variables(model)
  total<-get.objective(model)
  for(i in seq(1,NROW(nbaData))){
    if(player[i]==1) {    
      p[count]<- i
      count<-count+1
    }
  }
  for(a in seq(1,numPlay)){
    nbaData[p[a],'cost']<-nbaData[p[a],'cost']+1
  }
  if(total<13000 & second_time<3){
    second_time<-second_time+1
    nbaData['cost']<-floor(nbaData['cost']/2)
    total<-14000
  }
}
for(i in seq(1,NROW(nbaData))){
  nbaData[i,'cost']<-max(nbaData[i,'cost']-2,1)
}
write.csv(nbaData,file = "~/Documents/NBAValueFinder/model_cost.csv")