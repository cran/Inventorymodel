STIcoo <-
function(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA){
  coalicion<-coalitions(n)
  matriz<-as.matrix(coalicion[[1]])
  coa<-as.vector(coalicion[[2]])
  matriz0<-matriz
  costes<-c();costes[1]<-0
  
  if (sum(is.na(h)==T)==length(h)|sum(is.na(av)==T)==length(av)){
    cat("Values for h and av are necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=TRUE)==length(m)|sum(is.na(d)==TRUE)==length(d)){
      Qsti<-STI(n,a,av,d=NA,h,m)[[1]]
      for (i in 2:nrow(matriz0)){
        aux<-which(matriz0[i,]!=0)
        aS<-max(av[aux])
        for (j in 1:length(aux)){ 
          matriz0[i,aux[j]]<-Qsti[aux[j]]*m[aux[j]]*sqrt(2*(a+aS)/sum(m[aux]*Qsti[aux]*h[aux]))
        }
        costes[i]<-sqrt(2*(a+aS)*sum(m[aux]*Qsti[aux]*h[aux]))
      }
  } else {
    for (i in 2:nrow(matriz0)){
      aux<-which(matriz0[i,]!=0)
      aS<-max(av[aux])
      for (j in 1:length(aux)){
        matriz0[i,aux[j]]<-sqrt(2*(a+aS)*d[aux[j]]^2/sum(d[aux]*h[aux]))
      }
      costes[i]<-sqrt(2*(a+aS)*sum(d[aux]*h[aux]))
    }
  }
 
  matriz0<-data.frame(matriz0,coa,costes)
  colnames(matriz0)<-c(1:n,"Coalition","Order cost")
  sol<-list(matriz0)
  names(sol)<-c("Optimal order")
return(sol)}
}

