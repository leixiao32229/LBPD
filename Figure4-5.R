#Line 1-139 are functions. Simulation starts at line 140. The code is responsible for Figure 4 and 5.
optimaltb_general<-function(L,U){
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
        rev=rev+pricechoice[i]*N*sum(1/(N:(N-uniq+1)))
      }
    }
    revlist[i]=rev/50000
  }
  optpsb=pricechoice[which.max(revlist)]
  optrevsb=max(revlist)
  c('price'=optpsb,'revenue'=optrevsb)
}
optimalub_general<-function(L,U){
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
          rev=rev+pricechoice[i]*N
        }
        if (length(test)>=1 & length(test)<=N-1){
          sale=max(test)
          rev=rev+pricechoice[i]*sale
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
optimalgb_general<-function(L,U){
  pricechoice=seq(L,U,0.01)
  revlist=numeric()
  totalvalue=numeric()
  for (j in 1:50000){
    totalvalue[j]=sum(rw[j,1:N])
  }
  for (i in 1:length(pricechoice)){
    revlist[i]=pricechoice[i]*length(which(totalvalue>=pricechoice[i]))/50000
  }
  optp=pricechoice[which.max(revlist)]
  optrev=max(revlist)
  sale=length(which(totalvalue>=optp))/50000
  surplus=sum(totalvalue[which(totalvalue>=optp)]-optp)/50000
  c(price=optp,revenue=optrev,sale=sale,surplus=surplus)
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


#uniform 0,2
set.seed(5)
rw=matrix(runif(3000*50000,0,2),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=7,ncol=10)
revtable=matrix(data=NA,nrow=7,ncol=10)
saletable=matrix(data=NA,nrow=7,ncol=10)
surtable=matrix(data=NA,nrow=7,ncol=10)
for (i in 1:10){
  print(i)
  N=nlist[i]
  p=optimaltb_general(0.3,1)[1]
  result=detailtb(p)
  ptable[1,i]=p;revtable[1,i]=result[1]/N;saletable[1,i]=result[2]/N;surtable[1,i]=result[3]/N
  result=detailtb(exp(-1))
  ptable[2,i]=exp(-1);revtable[2,i]=result[1]/N;saletable[2,i]=result[2]/N;surtable[2,i]=result[3]/N
  zeta=sum(1/(1:N))-log(N)-0.5772156649
  p=1/exp(1+log(1-N^(-1/3))-zeta)
  result=detailtb(p)
  ptable[3,i]=p;revtable[3,i]=result[1]/N;saletable[3,i]=result[2]/N;surtable[3,i]=result[3]/N
  p=optimalub_general(0.7,1)[1]
  result=detailub(p)
  ptable[4,i]=p;revtable[4,i]=result[1]/N;saletable[4,i]=result[2]/N;surtable[4,i]=result[3]/N
  p=1-N^(-1/5)
  result=detailub(p)
  ptable[5,i]=p;revtable[5,i]=result[1]/N;saletable[5,i]=result[2]/N;surtable[5,i]=result[3]/N
  result=optimalgb_general(0.5*N,N)
  ptable[6,i]=result[1]/N;revtable[6,i]=result[2]/N;saletable[6,i]=result[3];surtable[6,i]=result[4]
  result=detailub(ptable[6,i])
  ptable[7,i]=ptable[6,i];revtable[7,i]=result[1]/N;saletable[7,i]=result[2]/N;surtable[7,i]=result[3]/N
}


unifp=ptable
unifrev=revtable
unifsale=saletable
unifsur=surtable
save(unifp,unifrev,unifsale,unifsur,file='unifp.RData')

#exponential 1
set.seed(5)
rw=matrix(rexp(3000*50000),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=7,ncol=10)
revtable=matrix(data=NA,nrow=7,ncol=10)
saletable=matrix(data=NA,nrow=7,ncol=10)
surtable=matrix(data=NA,nrow=7,ncol=10)
for (i in 3:10){
  print(i)
  N=nlist[i]
  p=optimaltb_general(0.3,1)[1]
  result=detailtb(p)
  ptable[1,i]=p;revtable[1,i]=result[1]/N;saletable[1,i]=result[2]/N;surtable[1,i]=result[3]/N
  result=detailtb(exp(-1))
  ptable[2,i]=exp(-1);revtable[2,i]=result[1]/N;saletable[2,i]=result[2]/N;surtable[2,i]=result[3]/N
  zeta=sum(1/(1:N))-log(N)-0.5772156649
  p=1/exp(1+log(1-N^(-1/3))-zeta)
  result=detailtb(p)
  ptable[3,i]=p;revtable[3,i]=result[1]/N;saletable[3,i]=result[2]/N;surtable[3,i]=result[3]/N
  p=optimalub_general(0.5,1)[1]
  result=detailub(p)
  ptable[4,i]=p;revtable[4,i]=result[1]/N;saletable[4,i]=result[2]/N;surtable[4,i]=result[3]/N
  p=1-N^(-1/5)
  result=detailub(p)
  ptable[5,i]=p;revtable[5,i]=result[1]/N;saletable[5,i]=result[2]/N;surtable[5,i]=result[3]/N
  result=optimalgb_general(0.5*N,N)
  ptable[6,i]=result[1]/N;revtable[6,i]=result[2]/N;saletable[6,i]=result[3];surtable[6,i]=result[4]
  result=detailub(ptable[6,i])
  ptable[7,i]=ptable[6,i];revtable[7,i]=result[1]/N;saletable[7,i]=result[2]/N;surtable[7,i]=result[3]/N
  }
expp=ptable
exprev=revtable
expsale=saletable
expsur=surtable

for (i in 1:length(nlist)){
  print(i)
  result=detailub(expp[6,i])
  expp[7,i]=expp[6,i];exprev[7,i]=result[1]/N;expsale[7,i]=result[2]/N;expsur[7,i]=result[3]/N
}

save(expp,exprev,expsale,expsur,file='exp.RData')

###log-normal 0,1
set.seed(5)
rw=matrix(rlnorm(3000*50000,0,1),nrow=50000)
nlist=c(3,5,10,30,50,100,300,500,1000,3000)
ptable=matrix(data=NA,nrow=7,ncol=10)
revtable=matrix(data=NA,nrow=7,ncol=10)
saletable=matrix(data=NA,nrow=7,ncol=10)
surtable=matrix(data=NA,nrow=7,ncol=10)
for (i in 1:10){
  print(i)
  N=nlist[i]
  p=optimaltb_general(0.3,1)[1]
  result=detailtb(p)
  ptable[1,i]=p;revtable[1,i]=result[1]/N;saletable[1,i]=result[2]/N;surtable[1,i]=result[3]/N
  result=detailtb(exp(-1)*exp(0.5))
  ptable[2,i]=exp(-1);revtable[2,i]=result[1]/N;saletable[2,i]=result[2]/N;surtable[2,i]=result[3]/N
  zeta=sum(1/(1:N))-log(N)-0.5772156649
  p=1/exp(1+log(1-N^(-1/3))-zeta)
  result=detailtb(p)
  ptable[3,i]=p;revtable[3,i]=result[1]/N;saletable[3,i]=result[2]/N;surtable[3,i]=result[3]/N
  p=optimalub_general(1,1.65)[1]
  result=detailub(p)
  ptable[4,i]=p;revtable[4,i]=result[1]/N;saletable[4,i]=result[2]/N;surtable[4,i]=result[3]/N
  p=(1-N^(-1/5))*exp(0.5)
  result=detailub(p)
  ptable[5,i]=p;revtable[5,i]=result[1]/N;saletable[5,i]=result[2]/N;surtable[5,i]=result[3]/N
  result=optimalgb_general(0.5*N,exp(0.5)*N)
  ptable[6,i]=result[1]/N;revtable[6,i]=result[2]/N;saletable[6,i]=result[3];surtable[6,i]=result[4]
  result=detailub(ptable[6,i])
  ptable[7,i]=ptable[6,i];revtable[7,i]=result[1]/N;saletable[7,i]=result[2]/N;surtable[7,i]=result[3]/N
}




