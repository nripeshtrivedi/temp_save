
library(foreach)
library(doMC)
library(doParallel)
library(ROCR)
library(cluster)
library(kernlab)
cluster_size<-size(of_sm2_1_0.007)
clusts <-sort(unique(of_sm2_1_0.007))
sigma_1<-0
sigma_2<-0
sigma<-0.0055

s<-rep(-2,length(of_sm2_1_0.007))
registerDoMC(cores=37)
values<-foreach(i = 1:100,.packages='kernlab') %dopar% my(i)

my<-function(i)
{
  cluster_size<-size(of_sm2_1_0.007)
  clusts <-sort(unique(of_sm2_1_0.007))
  sigma_1<-0
  sigma_2<-0
  sigma<-0.0055
  temp<-rep(200000000000,length(clusts)-1)
  for(j in 1:length(of_sm2_1_0.007))
  {
    if((of_sm2_1_0.007[j]==of_sm2_1_0.007[i])&&(i!=j))
    {
      sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
      sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma)
    }
  }   
  a<-(exp(-(dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma))+sigma_1+sigma_2)/cluster_size[of_sm2_1_0.007[i]]
  sigma_1<-0
  sigma_2<-0
  for(k in 1:length(clusts))
  {
    if((k==of_sm2_1_0.007[i]))
    {
      next();
    }
    else
    {
      for(j in 1:length(of_sm2_1_0.007))
      {
        if(of_sm2_1_0.007[j]==k)
        {
          sigma_1<-sigma_1-2*exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[j,]))/sigma*sigma)
          sigma_2<-sigma_2+exp(-dist(rbind(Allpara_nazero[j,],Allpara_nazero[j,]))/sigma*sigma) 
        }
      }
    }
    
    if(!(k==of_sm2_1_0.007[i]))
      temp[k]<-(exp(-dist(rbind(Allpara_nazero[i,],Allpara_nazero[i,]))/sigma*sigma)+sigma_1+sigma_2)/cluster_size[k]
    sigma_1<-0
    sigma_2<-0
  }
  b<-min(temp)
  return(b-a[1])/max(b,a[1])
}



