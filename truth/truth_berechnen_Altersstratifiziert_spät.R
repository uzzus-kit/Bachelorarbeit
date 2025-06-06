truth=function(Datensatz){
Datensatz_truth=Datensatz[,1:6]
colnames(Datensatz_truth)[6]="truth"
for (i in 1:length(Datensatz_truth[,1])){
    for (j in which(colnames(Datensatz)=="value_0w"):which(colnames(Datensatz)=="value_4w")){
      if (is.na(Datensatz[i,j])){
        Datensatz[i,j]=0
      }
    }
    Datensatz_truth[i,6]=sum(Datensatz[i,which(colnames(Datensatz)=="value_0w"):which(colnames(Datensatz)=="value_4w")])
}
return(Datensatz_truth)
}

