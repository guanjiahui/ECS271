load("supervised.RData")
library(compiler)
##############
Knn=function(k,train,test,cl){
  if (k>1){
    distance=sapply(1:nrow(train),function(i) dist(rbind(test,train[i,])))
    ind=order(distance)[1:k]
    
    classBelong=cl[ind]
    result.class=unique(classBelong)
    find_max=sapply(1:length(result.class),
                    function(i) length(which(classBelong==result.class[i])))
    index= which(find_max==max(find_max))
    result=result.class[index]
  }
  
  if (k==1){
    distance=sapply(1:nrow(train),function(i) dist(rbind(test,train[i,])))
    ind=order(distance)[1]
    
    classBelong=cl[ind]
    result=classBelong
  }
  return (result)
} #KNN function
Knn=cmpfun(Knn)
#################
#next do the 10-fold cross validation 
Knn.CV=function(train,cl,k,n.cv){
  N=nrow(train)
  n=floor(N/n.cv)
  
  x=sample(N)
  g=as.factor(c(rep(1:n.cv,rep(n,n.cv)),rep(n.cv,(N-n*n.cv))))
  S=split(x,g)
  for (i in 1:n.cv){
    training=train[-S[[i]],]
    testing=train[S[[i]],]
    cl.cv=cl[-S[[i]]]
    true.value=cl[S[[i]]]
    result=c()
    err=numeric(n.cv)
    count=0
    
    
    for (l in 1:nrow(testing)){
      result[l]= Knn(k,training,testing[l,],cl.cv)
      if (result[l]!=true.value[l])
        count=count+1
     
    } #end of for loop for each testing individual
    
    err[i]=count/nrow(testing)
  }
  
  total_error= sum(err)
  return(c(err,(total_error/3)*100))
}
Knn.CV=cmpfun(Knn.CV)
#####################


result5= Knn.CV(pendigits.train[,1:16],y,k=5,3)

result50=Knn.CV(pendigits.train[,1:16],y,k=50,3)
result1=Knn.CV(pendigits.train[,1:16],y,k=1,3)

save(result1,result5,result50,file="knnTotal.RData")
