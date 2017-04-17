twolines <-
function(n=NA,a=NA,av=NA,d=NA,K=NA){
  if (is.na(a)==T|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)|sum(is.na(av)==T)==length(av)){ 
    cat("Values for a, av, d and K are necessary. Please, check them.", sep="\n")
  } else {
    
costes<-mct(n,a,av=av,d=d,K=K,cooperation=1)
#library(e1071) #requiere paquete e1071
cat("Two-lines rule", sep="\n")
p<-permutations(n)
aux1<-c()
aux2<-c()
for (k in 1:nrow(p)){
  aux<-c()
  aux0<-c()
  for (i in 1:n){
    for (j in 1:n){
      if (i !=j){
        if(av[i]<av[j] & which(p[k,]==i)<=which(p[k,]==j)){aux<-c(aux,1)} 
        if(d[i]/K[i]<d[j]/K[j] & which(p[k,]==i)<=which(p[k,]==j)){aux0<-c(aux0,1)} 
      }
    }
  }
  if (length(aux)==0){aux1<-c(aux1,k)}
  if (length(aux0)==0){aux2<-c(aux2,k)}
} 
p1<-p[aux1,]
p2<-p[aux2,]
TL<-1/2*marginal_contribution_mean(p1,costes[,n+2])+1/2*marginal_contribution_mean(p2,costes[,n+2])
return(TL)}
}
