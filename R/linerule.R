linerule <-
function(n=NA,a=NA,av=NA,d=NA,h=NA,m=NA){
  
  if (sum(is.na(h)==T)==length(h)|sum(is.na(av)==T)==length(av)){
    cat("Values for h and av are necessary to determinate the optimal orders.", sep="\n")
  }  else { 
    if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    
    coalicion<-STIcoo(n,a,av,d,h,m)
    coalicion1<-coalicion[[1]]
    coalicion2<-as.vector(coalicion1[,n+1])
    matriz<-as.matrix(coalicion1[,1:n])
    costes<-as.vector(coalicion1[,n+2])
    matriz<-cbind(matriz,rep(0,2^n),rep(0,2^n))
    for (i in 2:nrow(matriz)){
      aux<-which(matriz[i,]!=0)
      matriz[i,n+1]=coalicion2[i]
      matriz[i,n+2]=costes[i]
    }
    p<-permutations(n)
    aux1<-c()
    for (k in 1:nrow(p)){
      aux<-c()
      for (i in 1:n){
        for (j in 1:n){
          if (i !=j){if(av[i]<av[j] & (which(p[k,]==i)<=which(p[k,]==j))){aux<-c(aux,1)}}
        }
      }
      if (length(aux)==0){aux1<-c(aux1,k)}
    }
    permute<-p[aux1,]
    phi<-marginal_contribution_mean(permute,costes)
    return(phi)
    } else {
      cat("Values for m or d are necessary. Please, check them.")
    }
  }
}