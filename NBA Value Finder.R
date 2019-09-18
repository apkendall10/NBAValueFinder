NBA.data <- read.csv("~/Sports stats/prediction_age.csv")
col<-c("isPG","isSG","isSF","isPF","isC")
pos<-c("PG","SG","SF","PF","C")
for(i in seq(1,NROW(NBA.data))){
  for(j in seq(1,5)){
    NBA.data[i,col[j]]<-0
    if(grepl(pos[j],NBA.data[i,'pos'])){
      NBA.data[i,col[j]]<-1
    }
  } 
}
NBA.data['cost']=1
numPlay<-12
total<-14000
df<-data.frame(optimal=return)
vals<-data.frame(optimal=return)
p<-numeric(numPlay)
second_time<-0
while(total>13000) {
  count<-1
  model<-make.lp(0,NROW(NBA.data))
  set.objfn(model,c(NBA.data[['fantasy.points']]))
  add.constraint(model,c(NBA.data[['cost']]),"<=",200)
  add.constraint(model,numeric(NROW(NBA.data))+1,"=",numPlay)
  add.constraint(model,c(NBA.data[['isPG']]),">=",2)
  add.constraint(model,c(NBA.data[['isSG']]),">=",2)
  add.constraint(model,c(NBA.data[['isSF']]),">=",2)
  add.constraint(model,c(NBA.data[['isPF']]),">=",2)
  add.constraint(model,c(NBA.data[['isC']]),">=",2)
  set.type(model,seq(1,NROW(NBA.data)),"binary")
  lp.control(model,sense='max')
  solve(model)
  player<-get.variables(model)
  total<-get.objective(model)
  for(i in seq(1,NROW(NBA.data))){
    if(player[i]==1) {    
      p[count]<- i
      count<-count+1
    }
  }
  for(a in seq(1,numPlay)){
    NBA.data[p[a],'cost']<-NBA.data[p[a],'cost']+1
  }
  if(total<13000 & second_time<3){
    second_time<-second_time+1
    NBA.data['cost']<-floor(NBA.data['cost']/2)
    total<-14000
  }
}
for(i in seq(1,NROW(NBA.data))){
  NBA.data[i,'cost']<-max(NBA.data[i,'cost']-2,1)
}
write.csv(NBA.data,file = "~/Sports stats/model_cost.csv")