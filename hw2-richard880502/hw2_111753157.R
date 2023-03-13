evaluate<-function(name,d, target,threshold)
{
  predicted <-c()
  for(i in 1:nrow(d['pred.score']))
  {     
    if(d[i,3] < threshold){
      predicted <- c(predicted, "bad")
    }
    else{
      predicted <- c(predicted, "good")  
    }
  }
 
  confusion_table = table(predicted, d[,'reference'])  
  if (target == "bad"){
    TP = confusion_table[2,1]
    TN = confusion_table[1,2]
    FN = confusion_table[1,1]
    FP = confusion_table[2,2]
  }else{
    TP = confusion_table[1,2]
    TN = confusion_table[2,1]
    FN = confusion_table[2,2]
    FP = confusion_table[1,1]
  }

  Sensitivity = round(TP / (TP + FN), 2)
  Specificity = round(TN / (TN + FP), 2)  

  precision = round(TP / (TP + FP), 2)
  f1_score = round((2 * precision * Sensitivity) / (precision + Sensitivity), 2)  

  likeli_model <- round(sum(ifelse(d$reference=="bad", log(d$pred.score), log(1-d$pred.score))),2)  

  pNull <- sum(ifelse(d$reference=="bad",1,0))/dim(d)[[1]]
  likeli_nullModel <- sum(ifelse(d$reference=="bad",1,0))*log(pNull) + sum(ifelse(d$reference=="bad",0,1))*log(1-pNull)
  S <- 0
  pseudoR <- round(1 - (-2*(likeli_model-S))/(-2*(likeli_nullModel-S)),2)

  return (data.frame(method = name,sensitivity = Sensitivity,specificity = Specificity,F1 = f1_score,logLikelihood= likeli_model,pseudoRsquared=pseudoR))
}

# read parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_yourID.R --target male|female --input file1 file2 ... filen --output out.csv", call.=FALSE)
}

# parse parameters
i<-1 
while(i < length(args))
{
  if(args[i] == "--target"){
    target<-args[i+1]
    i<-i+1
  }
  else if(args[i] == "--badthre"){
    Threshold <- args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

# read files
result<-c()

for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  result<-rbind(result, evaluate(name, d, target, Threshold))
}

result<-rbind(result,data.frame(method = "best",sensitivity = result[result$sensitivity == max(result$sensitivity),1][1],specificity = result[result$specificity == max(result$specificity),1][1],F1 = result[result$F1 == max(result$F1),1][1],logLikelihood= result[result$logLikelihood == max(result$logLikelihood),1][1],pseudoRsquared= result[result$pseudoRsquared == max(result$pseudoRsquared),1][1]))

write.csv(result,file=out_f,row.names = FALSE,quote=FALSE)