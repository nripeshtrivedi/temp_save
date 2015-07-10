#parallel computation for SI index

library(foreach)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(nsm6)
clusts <-sort(unique(nsm6))
sigma_1<-0
sigma_2<-0
sigma<-0.0055

s<-rep(-2,length(nsm6))
stopCluster(cl)
cl<-makeCluster(37)
registerDoParallel(cl)
values<-foreach(i = 1:10,.packages='kernlab') %dopar% my(i)

  my<-function(i)
  {
    cluster_size<-size(nsm6)
    clusts <-sort(unique(nsm6))
    sigma_1<-0
    sigma_2<-0
    sigma<-0.0055
  temp<-rep(200000000000000,length(clusts)-1)
  for(j in 1:length(nsm6))
  {
    if((nsm6[j]==nsm6[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[nsm6[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==nsm6[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(nsm6))
      {
        if(nsm6[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==nsm6[i]))
      temp[k]<-(exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}

  #for sigma=0.0055
  store_values<-rep(0,6) #for k=2,3,5,7,9
  store_values[4]<-mean(values)# for k=7
  
