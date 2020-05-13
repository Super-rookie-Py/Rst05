# 2019/11/26 by SeJoon Park

Age=rep(1:100,time=4)
Sex=rep(1:2,each=2,time=100)
Weight=10+Age*0.5+Sex*Age*0.1+rnorm(400,1,3)

data_1<-data.frame(Weight,Age,Sex)

data_2<-data.frame(Avg_Weight=rep(0,time=20),SD_Weight=rep(0,time=20),
                   Age_Level=rep(1:10,each=2),Sex=rep(1:2,time=10))

data_3<-data.frame(Avg_Weight=rep(0,time=20),SD_Weight=rep(0,time=20),
                   Age_Level=rep(1:10,each=2),Sex=rep(1:2,time=10))

data_4<-data_1
Age_Level=ceiling(Age/10)
data_4<-cbind(data_4,Age_Level)

for(i in 1:10){ # Age_Level
  for(j in 1:2){ # Sex
    data_2$Avg_Weight[(i-1)*2 + j]=
      mean(data_1$Weight[which(data_1$Sex==j & ceiling(data_1$Age/10)==i)])
    data_2$SD_Weight[(i-1)*2 + j]=
      sd(data_1$Weight[which(data_1$Sex==j & ceiling(data_1$Age/10)==i)])
    data_3$Avg_Weight[(i-1)*2 + j]=
      mean(data_1$Weight[which(data_1$Sex==j & data_1$Age %in% (1:10+(i-1)*10))])
    data_3$SD_Weight[(i-1)*2 + j]=
      sd(data_1$Weight[which(data_1$Sex==j & data_1$Age %in% (1:10+(i-1)*10))])
  }
}

GCM<-function(x1,x2){
  while(x1!=x2){
    if(x1>x2){
      x1=x1-x2
    }else{
      x2=x2-x1
    }
  }
  return(x1)
}

# Ranking
a<-sample(1:100,1000,replace=T)
count_a<-rep(0,100)
for(i in 1:1000){count_a[a[i]]=count_a[a[i]]+1}
for(i in 99:1){count_a[i]=count_a[i]+count_a[i+1]}
count_a=c(count_a[2:100]+1,1)
b<-count_a[a]
c<-data.frame(a,b)

# Prime Number
Prime_Number_1<-function(n){
  a1=3:n
  a2=2
  k=1
  for(i in 3:n){
    Flag=F
    for(j in 1:k){
      if((i %% a2[j])==0){
        Flag=T
        break
      }
    }
    if(Flag==F){
      k=k+1
      a2[k]=i
    }
  }
  return(a2)
}

Prime_Number_2<-function(n){
  a1=2:n
  a2=2
  k=2
  for(i in 2:n){
    a1<-a1[which((a1%%a1[1])!=0)]
    a2[k]=a1[1]
    k=k+1
    if(length(a1)==1){
      break
    }
  }
  return(a2)
}



