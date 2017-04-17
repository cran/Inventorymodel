mfoc <-
function(n=NA,a=NA,d=NA,K=NA,cooperation=c(0,1)){
  if (is.na(a)==T|sum(is.na(d)==T)==length(d)|sum(is.na(K)==T)==length(K)){ 
    cat("Values for a, d and K are necessary. Please, check them.", sep="\n")
  } else {
    cat("MFOC model", sep="\n")
    if (cooperation==0){
	    costs<-a*d/K
	    sol<-costs
    }
    if (cooperation==1){
      cat("Cooperative case", sep="\n")
      coalition<-coalitions(n)
      matrix0<-as.matrix(coalition[[1]])
      costs<-c();costs[1]=0
      coa<-c();coa[1]=0
      for (i in 2:nrow(matrix0)){
        aux<-which(matrix0[i,]!=0)
        costs[i]<-a*max(d[aux]/K[aux])
      }
      sol<-data.frame(matrix0,coalition[[2]],costs)
      colnames(sol)<-c(1:n,"Coalition","Coalicional costs")
    }
  return(sol)
    }
}