#

rf <- function(data,K=sqrt(dim(data)[2]),n.trees=1000,min.groupfraction=0.1,seed=1) {
  
  # set and display random seed
  set.seed(seed)
  ### function which generates a feature ###
  featureGen <- function(feature.specs) {
    # response and predictor data is accessed globally. the predictor index is provided in feature specs
    response.var <- response.data
    predict.var <- predict.data[,feature.specs[1]]
    predict.val <- sort(unique(predict.var))[feature.specs[2]]
    
    # split the response variable based on the splitting criterion. notice that predictor values that are equal
    # to the specified value are grouped in with those less than the predictor value (subset1)
    predict1 <- predict.var <= predict.val
    subset1 <- response.var[predict1]
    mean1 <- mean(subset1)
    
    predict2 <- predict.var > predict.val
    subset2 <- response.var[predict2]
    mean2 <- mean(subset2)
    
    feature <- mean1*predict1 + mean2*predict2
    
    # for visualizing the split
    #plot(response.var,predict.var,col='white')
    #points(response.var[predict1],predict.var[predict1],col='green',pch=20)
    #points(response.var[predict2],predict.var[predict2],col='orange',pch=20)
    
    mse <- sum((response.var - feature)^2)
    
    return(mse)
  }
  
  ### function to sum up variable importances for a single tree ###
  variableImp <- function(tree) {
    importance <- array(0,N-1)
    
    split.nodes <- which(!is.na(tree[,1]))
    for (node in split.nodes) {
      importance[tree[node,1]] <- importance[tree[node,1]] + tree[node,6]
    }
    
    return(importance)
  }
  
  n <- dim(data)[1]
  N <- dim(data)[2]
  
  # specify parameters upfront for testing
  #n.trees <- 100
  #min.groupfraction <- 0.1
  #K <- round(sqrt(N-1))
  
  # initialize list of interactions
  interactions <- NULL
  forest.list <- vector(mode="list",length=N)
  start.time <- proc.time()[3]
  for (response.idx in 1:N) {
    # initialize forest
    forest <- array(NA,dim=c(1000,6,n.trees))
    
    for (tree.idx in 1:n.trees) {
      #print(sprintf("growing tree %d/%d",tree.idx,n.trees))
      
      ### bootstrap data for each tree, normalize bootstrap data ###
      bootstrap.indices <- sample(1:n,n,replace=T)
      data.boot <- data[bootstrap.indices,]
      data.boot <- apply(data.boot,2,function(x) {x/sd(x)})
      
      # number of unique samples (bootstrap sampling done with replacement) which determines number of possible splits
      n.unique <- length(unique(bootstrap.indices))
      
      # the smallest size of a group that is allowed to be split
      min.groupsize <- min.groupfraction*n
      
      ### grow tree ###
      
      # initialize assignments - this variable will hold the group assignments of training data
      assignments <- matrix(NA,1000,n)
      assignments[1,] <- rep(1,n)
      
      # initialize where the tree is stored
      tree <- matrix(NA,1000,6)
      tree[1,5] <- mean(data.boot[,response.idx])
      
      # initialize level counting variable
      level <- 0
      while (1 == 1) {
        level <- level + 1
        #print(sprintf("level %d",level))
        # exit if more than 1000 loops have been initiated
        if (level > 1000) {break}
        
        # find number of unique samples in each group; determine which groups contain enough of these to continue splitting
        effective.group.sizes <- sapply(unique(assignments[level,]),function(x) length(unique(data[assignments[level,]==x,response.idx])))
        active.groups <- unique(assignments[level,])[effective.group.sizes > min.groupsize]
        inactive.groups <- unique(assignments[level,])[effective.group.sizes <= min.groupsize]
        
        # exit loop if all threads are extinct
        if (length(active.groups) == 0) {break}
        
        # right off the bat, drag down inactive group labels to the current level in 'assignments' since they will not henceforth be operated upon
        dragdown.indices <- assignments[level,] %in% inactive.groups
        assignments[level+1,dragdown.indices] <- assignments[level,dragdown.indices]
        
        # split active groups
        for (group.idx in active.groups) {
          n.group <- sum(assignments[level,] == group.idx)
          
          group.indices <- assignments[level,] == group.idx
          response.data <- data.boot[group.indices,response.idx]
          predict.data <- data.boot[group.indices,-response.idx]
          
          n.unique <- length(unique(response.data))
          
          # while loop is required due to bugs that have been occurring. loops until specific situation is not encountered
          proceed <- 'false'
          count <- 0
          while (proceed == 'false') {
            count <- count + 1
            # number of possible features is (N-1)*(n.boot-1), sample from a indexing of these features
            # choose K variables randomly
            predict.variables <- sample(1:(N-1),K,replace=T)
            # find how many unique splits are possible for each chosen variable
            unique.variables <- as.integer(names(table(predict.variables)))
            unique.splits <- sapply(unique.variables,function(x) (length(unique(predict.data[,x]))-1))
            variable.instances <- as.integer(table(predict.variables))
            
            if (sum(variable.instances > unique.splits) == 0) {proceed <- 'true'}
            if (count > 10) {
              predict.variables <- unique(predict.variables)
              proceed <- 'true'
            }
          }
          
          # this was originally used to fix the bug but it created more trouble
          ## choose randomly from available splits for each variable. 
          ## If a variable is to be tested more times than exist splits, only test as many times as unique splits allow
          #predict.values <- apply(cbind(unique.splits,variable.instances),1,function(x) if (x[2] > x[1]) {1:x[1]} else {sample(1:x[1],x[2],replace=FALSE)})
          
          # determine predict values by sampling from 1 to # of unique splits for each variable
          unique.splits2 <- sapply(predict.variables,function(x) (length(unique(predict.data[,x]))-1))
          predict.values <- sapply(unique.splits2,function(x) sample(1:x,1,))
          
          predict <- cbind(predict.variables,predict.values)
          
          # apply feature function to list of features to test; identify lowest mse
          mse.list <- apply(predict,1,featureGen)
          best.variable <- predict[which(mse.list == min(mse.list))[1],1]
          best.index <- predict[which(mse.list == min(mse.list))[1],2]
          best.feature <- c(best.variable,sort(predict.data[,best.variable])[best.index])
          
          # split variable and split value are columns 1 and 2, respectively
          tree[group.idx,1:2] <- best.feature
          
          # how many spots in the tree matrix are taken/how many nodes are there?
          num.nodes <- sum(!is.na(tree[,5]))
          split.idx.left <- num.nodes + 1
          split.idx.right <- num.nodes + 2
          tree[group.idx,3:4] <- split.idx.left:split.idx.right
          
          # calculate means for the two offshooting groups - this come in when a group that is created satisfies the stopping threshold
          left.group.membership <- predict.data[,best.feature[1]] <= best.feature[2]
          right.group.membership <- predict.data[,best.feature[1]] > best.feature[2]
          tree[split.idx.left,5] <- mean(response.data[left.group.membership])
          tree[split.idx.right,5] <- mean(response.data[right.group.membership])
          
          # calculate variable importance = #S*Var(S) - #S_t*Var(S_t) - #S_f*Var(S_f)
          if (sum(left.group.membership) == 1) {left.group.variance <- 0} else {left.group.variance <- var(response.data[left.group.membership])}
          if (sum(right.group.membership) == 1) {right.group.variance <- 0} else {right.group.variance <- var(response.data[right.group.membership])}
          importance <- length(response.data)*var(response.data) - sum(left.group.membership)*left.group.variance - sum(right.group.membership)*right.group.variance
          tree[group.idx,6] <- importance
          
          # track group membership with assignments matrix
          newgroup.assignments <- split.idx.left*left.group.membership + split.idx.right*right.group.membership
          assignments[level+1,group.indices] <- newgroup.assignments
          
        }
      }
      
      forest[,,tree.idx] <- tree
    }
    
    # store complete forest in list
    forest.list[[response.idx]] <- forest
    
    ### assess variable importance ###
    
    # calculate and sum variable importances from all trees
    importance.ensemble <- array(0,(N-1))
    for (tree.idx in 1:n.trees) {
      importance.ensemble <- importance.ensemble + variableImp(forest[,,tree.idx])
    }
    
    # tack on variable labels
    importance.ensemble <- data.frame((1:N)[-response.idx],rep(response.idx,(N-1)),importance.ensemble)
    
    # add list of importances to table which holds all interactions
    interactions <- rbind(interactions,importance.ensemble)
    
    # inform user of progress
    elapsed.time <- proc.time()[3] - start.time
    print(sprintf("Gene %d/%d Complete; %f Hours Elapsed; Projected %f Hours Remaining",response.idx,N,round(elapsed.time/60^2,digits=2),
                  (elapsed.time/response.idx)*(N-response.idx)/60^2))
  }
  
  # sort interactions high --> low
  interactions <- interactions[rev(order(interactions[,3])),]
  
  return(list(interactions,forest.list))
}

directory <- "location of DREAM4 data"
setwd(directory)

n.datasets <- 5
interaction.list.redo <- vector(mode = "list", length = n.datasets)
for (data.idx in 1:n.datasets) {
  
  # load DREAM4 data
  data.dream4 <- read.table(sprintf("insilico_size100_%d_multifactorial.tsv",data.idx),sep="\t",header=TRUE)
  
  interaction.list.redo[[data.idx]] <- rf(data.dream4)
}

save(file="genie3_spurgeon_out.RData",interaction.list.redo)