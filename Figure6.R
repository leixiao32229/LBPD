#This code is for figure 6. Line 1-150 are functions for simulation. Line 152 generates budget, Line 156, 175, 195 are the start of uniform, log-normal, and exponential simulation. 
optimaltb_general_budget<-function(L,U){
  pricechoice=seq(L,U,0.01)
  revlist=numeric()
  for (i in 1:length(pricechoice)){
    print(pricechoice[i])
    rev=0
    for (j in 1:50000){
      data=rw[j,1:N]
      data=cumsum(rw[j,1:N])
      test=which(data<N*pricechoice[i])
      if (length(test)==0){
        rev=rev+pricechoice[i]*sum(N/(1:N))
      }
      if (length(test)>=1 & length(test)<=N-1){
        uniq=N-max(test)
        rev=rev+min(pricechoice[i]*N*sum(1/(N:(N-uniq+1))),budget[j]%/%pricechoice[i]*pricechoice[i])
      }
    }
    revlist[i]=rev/50000
  }
  optpsb=pricechoice[which.max(revlist)]
  optrevsb=max(revlist)
  c('price'=optpsb,'revenue'=optrevsb)
}
optimalub_general_budget<-function(L,U){
  pricechoice=seq(L,U,0.01)
  revlist=numeric()
  countlist=numeric()
  for (i in 1:length(pricechoice)){
    print(pricechoice[i])
    rev=0
    count=0
    for (j in 1:50000){
      data=rw[j,1:N]
      data=cumsum(rw[j,1:N]-pricechoice[i])
      if (data[N]>=0){
        test=which(data>data[N])
        if (length(test)==0){
          count=count+N
          rev=rev+pricechoice[i]*min(N,budget[j]%/%pricechoice[i])
        }
        if (length(test)>=1 & length(test)<=N-1){
          sale=max(test)
          rev=rev+min(pricechoice[i]*sale,budget[j]%/%pricechoice[i]*pricechoice[i])
          count=count+sale
        }
      }
    }
    revlist[i]=rev/50000
    countlist[i]=count/50000
  }
  optpsb=pricechoice[which.max(revlist)]
  optrevsb=max(revlist)
  salesb=countlist[which.max(revlist)]
  c('price'=optpsb,'revenue'=optrevsb,salesb)
}
optimalgb_general_budget<-function(L,U){
  pricechoice=seq(L,U,0.001)
  revlist=numeric()
  totalvalue=numeric()
  for (j in 1:50000){
    totalvalue[j]=sum(rw[j,1:N])
  }
  for (i in 1:length(pricechoice)){
    revlist[i]=pricechoice[i]*N*sum((totalvalue>=pricechoice[i]*N)*(budget>=pricechoice[i]*N))/50000
  }
  optp=pricechoice[which.max(revlist)]
  optrev=max(revlist)
  sale=length(which(totalvalue>=optp))/50000
  surplus=sum(totalvalue[which(totalvalue>=optp)]-optp)/50000
  c(price=optp,revenue=optrev,sale=sale,surplus=surplus)
}
optimalss_budget<-function(L,U){
  pricechoice=seq(L,U,0.01)
  revlist=rep(0,length(pricechoice))
  for (i in 1:length(pricechoice)){
    budg=budget%/%pricechoice[i]
    sale=rw[,1:N]>=pricechoice[i]
    revlist[i]=revlist[i]+sum(pmin(rowSums(sale),budg))*pricechoice[i]
  }
  revlist=revlist/50000
  optpss=pricechoice[which.max(revlist)]
  optrevss=max(revlist)
  c('price'=optpss,'revenue'=optrevss)  
}
detailtb<-function(p){
  pricechoice=p
  revlist=numeric()
  countlist=numeric()
  surlist=numeric()
  for (i in 1:length(pricechoice)){
    rev=0
    count=0
    sur=0
    for (j in 1:50000){
      data=rw[j,1:N]
      data=cumsum(rw[j,1:N])
      test=which(data<N*pricechoice[i])
      if (length(test)==0){
        count=count+sum(N/(1:N))
        rev=rev+pricechoice[i]*sum(N/(1:N))
        sur=sur+sum(rw[j,1:N])-pricechoice[i]*sum(N/(1:N))
      }
      if (length(test)>=1 & length(test)<=N-1){
        uniq=N-max(test)
        count=count+N*sum(1/(N:(N-uniq+1)))
        rev=rev+pricechoice[i]*N*sum(1/(N:(N-uniq+1)))
        sur=sur+sum(rw[j,N:(N-uniq+1)])-pricechoice[i]*N*sum(1/(N:(N-uniq+1)))
      }
    }
    revlist[i]=rev/50000
    countlist[i]=count/50000
    surlist[i]=sur/50000
  }
  c('revenue'=revlist[1],'sale'=countlist[1],'surplus'=surlist[1])
}
detailub<-function(p){
  pricechoice=p
  revlist=numeric()
  countlist=numeric()
  surlist=numeric()
  for (i in 1:length(pricechoice)){
    rev=0
    count=0
    sur=0
    for (j in 1:50000){
      data=rw[j,1:N]
      data=cumsum(rw[j,1:N]-pricechoice[i])
      if (data[N]>=0){
        test=which(data>data[N])
        if (length(test)==0){
          count=count+N
          rev=rev+pricechoice[i]*N
          sur=sur+sum(rw[j,1:N])-pricechoice[i]*N
        }
        if (length(test)>=1 & length(test)<=N-1){
          sale=max(test)
          rev=rev+pricechoice[i]*sale
          count=count+sale
          sur=sur+sum(rw[j,1:sale])-pricechoice[i]*sale
        }
      }
    }
    revlist[i]=rev/50000
    countlist[i]=count/50000
    surlist[i]=sur/50000
  }
  c(revenue=revlist[1],sale=countlist[1],surplus=surlist[1])
}

#Generating budget for 50000 users
set.seed(100)
budget=rexp(50000,1/75)

#uniform 0,2
set.seed(5)
rw=matrix(runif(3000*50000,0,2),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=4,ncol=10)
revtable=matrix(data=NA,nrow=4,ncol=10)
for (i in 1:10) {
  print(i)
  N=nlist[i]
  result=optimalub_general_budget(0,1)
  ptable[1,i]=result[1];revtable[1,i]=result[2]
  result=optimaltb_general_budget(0.01,0.7)
  ptable[2,i]=result[1];revtable[2,i]=result[2]
  result=optimalgb_general_budget(0.01,1.2)
  ptable[3,i]=result[1];revtable[3,i]=result[2]
  result=optimalss_budget(0.01,2)
  ptable[4,i]=result[1];revtable[4,i]=result[2]
}

###log-normal 0,1
set.seed(5)
rw=matrix(rlnorm(3000*50000,0,1),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=4,ncol=10)
revtable=matrix(data=NA,nrow=4,ncol=10)
for (i in 1:10) {
  print(i)
  N=nlist[i]
  result=optimalub_general_budget(0.01,2)
  ptable[1,i]=result[1];revtable[1,i]=result[2]
  result=optimaltb_general_budget(0.01,2)
  ptable[2,i]=result[1];revtable[2,i]=result[2]
  result=optimalgb_general_budget(0.01,2)
  ptable[3,i]=result[1];revtable[3,i]=result[2]
  result=optimalss_budget(0.01,2)
  ptable[4,i]=result[1];revtable[4,i]=result[2]
}


#exp 1
set.seed(5)
rw=matrix(rexp(3000*50000),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=4,ncol=10)
revtable=matrix(data=NA,nrow=4,ncol=10)
for (i in 1:10) {
  print(i)
  N=nlist[i]
  result=optimalub_general_budget(0.01,1.5)
  ptable[1,i]=result[1];revtable[1,i]=result[2]
  result=optimaltb_general_budget(0.01,1.5)
  ptable[2,i]=result[1];revtable[2,i]=result[2]
  result=optimalgb_general_budget(0.01,1.5)
  ptable[3,i]=result[1];revtable[3,i]=result[2]
  result=optimalss_budget(0.01,1.5)
  ptable[4,i]=result[1];revtable[4,i]=result[2]
}

