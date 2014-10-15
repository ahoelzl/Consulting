library(foreign)
library(glmnet)
library(psych)




read.data.measurement.error <- function(emotion) {
  total.data.frame <- data.frame()
  m.errors <- c()
  for(i in 1:4) {
    one.time.data.frame <- data.frame()
    variable <- c()
    variable.names <- c()
    #  for(j in 1:20) {
    variable <- c(variable, paste0(emotion,"[0-9]+","_",i))
    #variable.names <- c(variable.names, paste0(emotion,j))
    #}
    
    variables <- data2[,(names(data2)[grep(variable,names(data2))])]
    
    numbers <- apply(variables, FUN=as.numeric, MARGIN=c(1,2))
    sum.scores <- apply(numbers, FUN=mean, MARGIN=1)
    
    colnames(variables) <- variable.names 
    #y <- data[,paste0("fges_",i+1)]
    y <- data2[,paste0("gma_jz",(i+5))]
    y_before <- data2[,paste0("gma_jz",(i+4))]
    time <- rep(i, length(y))
    
   # numbers <- na.omit(numbers)
  #  calpha <-  as.numeric(alpha(as.data.frame(numbers))$total[1])
    time <- rep(i,length(y))
    colnames(numbers) <- paste0(emotion, 1:length(colnames(numbers)))
   # numbers <- cbind(y,y_before,numbers,time)
  #  cat("alpha partial",i, ": ", calpha)
    #print(summary(is.na(one.time.data.frame)))
    calpha <-  as.numeric(alpha(numbers)$total[1])
    
    
    sumscore <<- apply(numbers, MARGIN=1, FUN=sum)
    sd <- sd(sumscore)
    
    sem <- sd * sqrt(1 - calpha)
    m.errors[i] <- sem
  
  }
  m.errors
}






if(1 %in% vars) {
sem_jo <- read.data.measurement.error("jo")
} else {
  sem_jo <-0
}
if(2 %in% vars) {
sem_ax <- read.data.measurement.error("ax")
} else {
  sem_ax  <- 0
}
if(3 %in% vars) {
sem_ag <- read.data.measurement.error("ag")
} else {
  sem_ag  <- 0
}
if(4 %in% vars) {
sem_hl <- read.data.measurement.error("hl")
} else {
  sem_hl  <- 0
}
if(5 %in% vars) {
sem_bo <- read.data.measurement.error("bo")
} else {
  sem_bo  <- 0
}
if(6 %in% vars) {
sem_sh <- read.data.measurement.error("sh")
} else {
  sem_sh  <- 0
}
if(7 %in% vars) {
sem_pr <- read.data.measurement.error("pr")
} else {
  sem_pr <- 0
}

alpha.errors <- rbind(sem_jo,sem_ax,sem_ag,sem_hl,sem_bo,sem_sh,sem_pr)

#fs_list <- list("jo" = fs_jo, "ax" = fs_ax, "ag"=fs_ag,"hl"=fs_hl,"bo"=fs_bo,"sh"=fs_sh,"pr"=fs_pr)
sem <- list("jo" = sem_jo, "ax"=sem_ax,"ag"=sem_ag,"hl"=sem_hl,"bo"=sem_bo,"sh"=sem_sh, "pr"=sem_pr)
sem <- unlist(sem)



########################measurement error from sem


sem2 <- rep(0,7)

for( j in 1:length(vars)) {
  i <- vars[j]
  emo <- emos[i]
  
  results <- colnames(data2[,(grep(emo,names(data2)))])
  
  emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
  emo2 <- results[grep(pattern=("[[:digit:]]_2"), results)]
  emo3 <- results[grep(pattern=("[[:digit:]]_3"), results)]
  emo4 <- results[grep(pattern=("[[:digit:]]_4"), results)]
  emo5 <-  results[grep(pattern=("[[:digit:]]_5"), results)]
  
  
  emo1 <- emo1[order(emo1)]
  emo2 <- emo2[order(emo2)]
  emo3 <- emo3[order(emo3)]
  emo4 <- emo4[order(emo4)]
  emo5 <- emo5[order(emo5)]
  
  data.partial <- data2[,c(emo1,emo2,emo3,emo4, emo5, "gma_jz5" ,"gma_jz6", "gma_jz7", "gma_jz8","gma_jz9")]
  
  
  data.partial1 <- data2[,c(emo1, "gma_jz5", "gma_jz6")]
  data.partial2 <- data2[,c(emo2, "gma_jz6","gma_jz7")]
  data.partial3 <- data2[,c(emo3, "gma_jz7","gma_jz8")]
  data.partial4 <- data2[,c(emo4, "gma_jz8","gma_jz9")]
  
  
  #data.partial <- na.omit(data.partial)
  data.partial <- as.data.frame(apply(data.partial, FUN=as.numeric, MARGIN=2))
  data.partial1 <- na.omit(as.data.frame(apply(data.partial1, FUN=as.numeric, MARGIN=2)))
  data.partial2 <- na.omit(as.data.frame(apply(data.partial2, FUN=as.numeric, MARGIN=2)))
  data.partial3 <- na.omit(as.data.frame(apply(data.partial3, FUN=as.numeric, MARGIN=2)))
  data.partial4 <- na.omit(as.data.frame(apply(data.partial4, FUN=as.numeric, MARGIN=2)))
  dp <- na.omit(data.partial)
  
  score1 <- apply(dp[grep(pattern=("[[:digit:]]_1"), colnames(dp))], FUN=sumf, MARGIN=1)
  score2 <-  apply(dp[grep(pattern=("[[:digit:]]_2"), colnames(dp))], FUN=sumf, MARGIN=1)
  score3 <-  apply(dp[grep(pattern=("[[:digit:]]_3"), colnames(dp))], FUN=sumf, MARGIN=1)
  score4 <- apply(dp[grep(pattern=("[[:digit:]]_4"), colnames(dp))], FUN=sumf, MARGIN=1)
  score5 <- apply(dp[grep(pattern=("[[:digit:]]_5"), colnames(dp))], FUN=sumf, MARGIN=1)
  
#  score1 <- scale(score1)
#  score2 <- scale(score2)
#  score3 <- scale(score3)
#  score4 <- scale(score4)
#  score5 <- scale(score5)
  
  scoreframce<- as.data.frame(cbind(score1,score2,score3,score4))
  colnames(scoreframce) <- c("score1","score2","score3","score4")
  
  text <- ""
  
  
  model.sem <- paste0("s1 =~ 1 * score1 \n ",
                  "s2 =~ 1 * score2 \n  ",
                  "s3 =~ 1 * score3 \n  ",
                  "s4 =~ 1 * score4 \n  ",
                  "s1  ~ s2 ",  "\n ",
                  "s2 ~ s3 ",  "\n ",
                  "s3 ~ s4 ",  "\n ",
                  "score1 ~~ vare * score1",  "\n ",
                  "score2 ~~ vare * score2",  "\n ",
                  "score3 ~~ vare * score3",  "\n ",
                  "score4 ~~ vare * score4")
  

  
  fit <- lavaan(model.sem, data = scoreframce, mimic="MPLUS",auto.var = TRUE, meanstructure = TRUE,
                     int.ov.free = TRUE)
  

  var1 <- sqrt(lavaan::coef(fit)["vare"])

 
  sem2[i] <- var1
  
 
  
  
}


