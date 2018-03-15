# script to plot results/performance of various algorithms (genie3, spurgeon, etc) on Dream4 datasets

library(flux)

# Input: condition labels, scores (high scores representing higher confidence)
# Output: receiver operating characteristic (TPR & FPR at all thresholds)
simple_roc <- function(labels, scores){
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(TPR=cumsum(labels)/sum(labels),FPR=cumsum(!labels)/sum(!labels))
}

# Input: condition labels, scores (high scores representing higher confidence)
# Output: precision and recall at all thresholds
simple_pr <- function(labels, scores) {
  labels <- labels[order(scores, decreasing=TRUE)]
  data.frame(precision=cumsum(labels)/1:length(labels),recall=cumsum(labels)/sum(labels))
}

# Input: merged FPR and TPR from multiple roc curves, sorted according to FPR
# Output: single roc curve, with TPR values averaged at each unique FPR
curveMean <- function(dv,iv) {
  breakpoints <- cbind(c(1,1+which(diff(iv)!=0)),c(c(1,1+which(diff(iv)!=0))[-1]-1,length(iv)))
  dv.mean <- apply(breakpoints,1,function(x) mean(dv[x[1]:x[2]]))
  return(cbind(dv.mean,unique(iv)))
}

# load in results
load("genie3_orig_out.RData")
load("genie3_spurgeon_out.RData")

results <- list(interaction.list.orig,interaction.list.redo)

n.methods <- length(results)
n.datasets <- length(results[[1]])

################################ calculate performance ################################

# produce performance curves and calculate area-under-curve
roc.all <- roc.means.all <- pr.all <- pr.means.all <- vector(mode="list",length=n.methods)
auc.roc.all <- auc.pr.all <- matrix(NA,n.datasets,n.methods)
for (midx in 1:n.methods) {roc.all[[midx]] <- roc.means.all[[midx]] <- vector(mode="list",length=n.datasets)}
for (data.idx in 1:n.datasets) {
  
  # load in goldstandard network
  gold.standard <- read.table(sprintf("C:/Users/Mark/Desktop/genie3spurgeon/random forest results/DREAM4_GoldStandard_InSilico_Size100_multifactorial_%d.tsv",data.idx),sep="\t")
  
  # alter format of gold standard
  key.interaction <- apply(gold.standard,1,function(x) paste(strsplit(x[1],split="G")[[1]][2],strsplit(x[2],split="G")[[1]][2],sep="/"))
  gold.standard <- cbind(key.interaction,gold.standard[,3])
  colnames(gold.standard) <- c("Interaction","Label")
  
  for (midx in 1:n.methods) {
    # alter format of method results
    results.temp <- results[[midx]][[data.idx]]
    results.temp <- cbind(apply(results.temp,1,function(x) paste(x[1],x[2],sep="/")),results.temp[,3])
    
    # match order of interaction scores to order of key
    scores <- as.numeric(results.temp[match(gold.standard[,1],results.temp[,1]),2])
    
    # roc curves
    roc.temp <- simple_roc(as.numeric(gold.standard[,2]),scores)
    roc.all[[midx]][[data.idx]] <- roc.temp
    auc.roc.all[data.idx,midx] <- auc(roc.temp[,2],roc.temp[,1])
    
    # pr curves
    pr.temp <- simple_pr(as.numeric(gold.standard[,2]),scores)
    pr.all[[midx]][[data.idx]] <- pr.temp
    auc.pr.all[data.idx,midx] <- auc(pr.temp[,2],pr.temp[,1])
  }
}

################################ plot performance ################################

png("Performance_ROC_DREAM4.png",height=2000,width=3000,pointsize=60)
layout(matrix(1:6,2,3,byrow=T))
for (data.idx in 1:n.datasets) {
par(font.lab=2)
method.indices <- c(1,2)
col.key <- c("blue","red","black")
leg.key <- c(sprintf("Genie3 Original: AUC=%s",as.character(round(auc.roc.all[data.idx,1],3))),
             sprintf("Genie3 Original: AUC=%s",as.character(round(auc.roc.all[data.idx,2],3))))
plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),cex.lab=1.3,
     xlab="FPR",ylab="TPR",main=sprintf("Dataset %d",data.idx))
grid(NULL,NULL)
lines(0:1,0:1,col="gray",lty="dotted")
par(font=1)
legend("bottomright",legend=leg.key[method.indices],col=col.key[method.indices],cex=0.75,lty="solid",lwd=5)
par(font=1)
for (midx in method.indices) lines(roc.all[[midx]][[data.idx]][,2],roc.all[[midx]][[data.idx]][,1],col=col.key[midx])
}
dev.off()

png("Performance_PR_DREAM4.png",height=2000,width=3000,pointsize=60)
layout(matrix(1:6,2,3,byrow=T))
for (data.idx in 1:n.datasets) {
  par(font.lab=2)
  method.indices <- c(1,2)
  col.key <- c("blue","red","black")
  leg.key <- c(sprintf("Genie3 Original: AUC=%s",as.character(round(auc.pr.all[data.idx,1],3))),
               sprintf("Genie3 Original: AUC=%s",as.character(round(auc.pr.all[data.idx,2],3))))
  plot(0,0,type="n",xlim=c(0,1),ylim=c(0,1),cex.lab=1.3,
       xlab="FPR",ylab="TPR",main=sprintf("Dataset %d",data.idx))
  grid(NULL,NULL)
  par(font=1)
  legend("topright",legend=leg.key[method.indices],col=col.key[method.indices],cex=0.75,lty="solid",lwd=5)
  par(font=1)
  for (midx in method.indices) lines(pr.all[[midx]][[data.idx]][,2],pr.all[[midx]][[data.idx]][,1],col=col.key[midx])
}
dev.off()




