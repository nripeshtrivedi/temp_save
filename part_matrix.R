
library(gtools)
part_matrix<-matrix(,nrow=16771,ncol=9) #computing matrix for partial data
i<-1
c<-1
while(i<67085)
{
  part_matrix[c,]<-Allpara_nazero[i,]
      i=i+4
      c<-c+1
}

