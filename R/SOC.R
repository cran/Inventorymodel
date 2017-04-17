SOC <-
function(n=NA,a=NA,d=NA,h=NA,m=NA,r=NA,s=NA,model=c("EOQ","EPQ"),cooperation=c(0,1)){
coalicion<-coalitions(n)
matriz<-as.matrix(coalicion[[1]])
matriz0<-matriz
matrizcostes<-matriz

if (sum(is.na(h)==T)==length(h)){
  cat("A value for h is necessary to determinate the optimal orders.", sep="\n")
}  else { 
  if (sum(is.na(m)!=T)==length(m)|sum(is.na(d)!=T)==length(d)){ 
    if (model=="EOQ"){Q<-EOQ(n,a,d,h,m)[[1]]}
    if (model=="EPQ"){
      if (sum(is.na(h)==T)==length(h)|sum(is.na(s)==T)==length(s)|sum(is.na(r)==T)==length(r)){
        cat("Values for r and s are necessary to determinate the optimal orders and shortages.", sep="\n")
      } else {
        Q<-EPQ(n,a,d,h,r,s,m)[[1]]
      }
    }
  } else {
    cat("Value for m or d are necessary to determinate the optimal orders.", sep="\n")
  }
}

if (sum(is.na(d)!=T)==length(d)&length(Q)==n){m=d/Q}


if (sum(is.na(m)!=T)==length(m)){
if (cooperation==0){coste<-2*a*m}
if (cooperation==1){
  for (i in 2:nrow(matriz0)){
    aux<-which(matriz0[i,]!=0)
    for (j in 1:length(aux)){
      matrizcostes[i,aux[j]]<-2*a*m[aux[j]]^2/sqrt(sum(m[aux]^2))
    }
}
}

if (cooperation==0){
sol<-list(coste)
names(sol)<-c("Share the ordering costs rule (individually)")
}
if (cooperation==1){
colnames(matrizcostes)<-1:n
rownames(matrizcostes)<-rep(" ",2^n)
sol<-list(matrizcostes)

names(sol)<-c("Share the ordering costs rule (individually)")
}
return(sol)
} else {
  cat("Check the associated values to the inventory problem.", sep="\n")
}
}