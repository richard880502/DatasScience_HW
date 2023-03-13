args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw1_exam.R input", call.=FALSE)
} else if (length(args)==4) {
  if (args[1] == "--input"){
    input <- args[2]    
    output <- args[4]   
  }else{
    input <- args[4]
    output <- args[2]      
  }   
}

if (!grepl(".csv",input))input = paste(input,".csv",sep='')
if (!grepl(".csv",output))output = paste(output,".csv",sep='')

d <- read.csv(input)
input <- strsplit(input,"/")
input <- input[[1]][length(input[[1]])]
df <- data.frame(set = c(substr(input,start = 1,stop = nchar(input)-4)), weight = c(round(max(d['weight']),2)), height = c(round(max(d['height']),2)))
write.csv(df,file=output,row.names = FALSE)
