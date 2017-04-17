mwhcct<-function(n=NA, a=NA, av=NA, d=NA, K=NA,cooperation=c(0,1),allocation=c(0,1)){

  if (is.na(a)==T|sum(is.na(av)==T)==length(av)|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)){ 
    cat("Values for a, b, d and K are necessary. Please, check them.", sep="\n")
  } else {
    cat("MWHC with Transportation Costs model", sep="\n")    
    coa<-coalitions(n)
    
    if (cooperation==1){
      cat("Cooperative case", sep="\n")
      cost<-numeric(nrow(coa[[1]]))
      cost1<-numeric(nrow(coa[[1]]))
      cost2<-numeric(nrow(coa[[1]]))

      matriz<-data.frame(coa[[1]],coa[[2]],rep(0,2^n))
      for (i in 2:nrow(matriz)){
        ind<-which(matriz[i,1:n]==1)
        cost1[i]<-a*max(d[ind]/K[ind])
        cost2[i]<-(av[i])*max(d[ind]/K[ind])
        cost[i]<-cost1[i]+cost2[i]
        matriz[i,n+2]<-cost[i]
      }
      colnames(matriz)<-c(1:n,"Coalition","Cost")
      sol<-matriz
    
      if (allocation==1){
        p <- permutations(n)
        aux <- c()
        for (k in 1:nrow(p)) {
          aux0 <- c()
          for (i in 1:n) {
            for (j in 1:n) {
              if (i != j) {
                if (d[i]/K[i]<d[j]/K[j] & which(p[k, ]==i) <= which(p[k, ]==j)){aux0 <- c(aux0, 1)}
             }
           }
         }
          if (length(aux0) == 0) {aux <- c(aux, k)}
        }
        p2 <- p[aux, ]
        TL <- marginal_contribution_mean(p2, cost2)
        R<-Shapley_value(cost1,game="cost")+TL
        sol<-list(sol,R)
        names(sol)<-c("Optimal solution","Allocation R rule")
      }
    }
  
  return(sol)}
}