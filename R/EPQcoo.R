EPQcoo <-
function(n=NA,a=NA,d=NA,h=NA,m=NA,r=NA,s=NA){
coalicion<-coalitions(n)
matriz<-as.matrix(coalicion[[1]])
matriz0<-matriz
matrizf<-matriz
costes<-c();costes[1]<-0
if (sum(is.na(h)==T)==length(h)){
  cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
}  else { 
  if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    if (sum(is.na(h)==T)==length(h)|sum(is.na(s)==T)==length(s)|sum(is.na(r)==T)==length(r)){
      cat("Values for r and s are necessary to determinate the optimal orders and shortages.", sep="\n")
    }  else {  
      if (sum(is.na(m)!=T)==length(m)&sum(is.na(d)==T)==length(d)){
        Qepq<-EPQ(n,a,d=NA,h,m,r,s)[[1]]
        d<-Qepq*m
      }
    }
    if (sum(r>d)==length(r)){
      Qepq<-EPQ(n,a,d,h,m=NA,r,s)[[1]]
      for (i in 2:nrow(matriz0)){
        aux<-which(matriz0[i,]!=0)
        for (j in 1:length(aux)){
          matriz0[i,aux[j]]<-sqrt(2*a*d[aux[j]]^2/sum(d[aux]*h[aux]*s[aux]*(1-d[aux]/r[aux])/(h[aux]+s[aux])))
          matrizf[i,aux[j]]<-matriz0[i,aux[j]]*h[aux[j]]*(1-d[aux[j]]/r[aux[j]])/(h[aux[j]]+s[aux[j]])
        }
        costes[i]<-2*a*sqrt(sum((d[aux]/Qepq[aux])^2))
      }
      
      cat("Cooperative case", sep="\n")
      matriz0<-data.frame(coalicion[[2]],matriz0,costes)
      rownames(matrizf)<-rep("",2^n)
      colnames(matriz0)<-c("Coalition",1:n,"Coalitional costs")
      colnames(matrizf)<-1:n
      sol<-list(matriz0,matrizf)
      names(sol)<-c("Optimal order","Optimal shortages")
      return(sol)
    } else{
      cat("The values for r_i are not bigger that d_i.", sep="\n")
    }
    
    
  } else {
    cat("Values for d or m are necessary to determinate the optimal orders.", sep="\n")
  }
}

}