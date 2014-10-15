library(foreign)
library(glmnet)
library(lavaan)


write.as.sum <- function(vec) {
  sum <- " "
  for(i in 1:(length(vec)-1)) {
    sum <- paste0(sum, vec[i], " + ")
  }
    sum <- paste0(sum,vec[length(vec)])
}


sem.one.variable <- function(to.use.emos, correlation) {
#data3 <- na.omit(data3)

#data2 <- na.omit(data2)

coefs.matrix.sem <- matrix(0,nrow=7, ncol=4)
coefs.matrix.sem.simple <- matrix(0,nrow=7, ncol=4)

true.coeffs <- matrix(0,nrow=9,ncol=4)
fits <- list()

emotions <- emos[to.use.emos]

model.text <- ""

data.parts <- data.frame()
for(r in 1:length(emotions)) {
  emo <- emotions[r]
  index <- to.use.emos[r]
  
  results <- colnames(data2[,(grep(emo,names(data2)))])
  
  emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
  emo2 <- results[grep(pattern=("[[:digit:]]_2"), results)]
  emo3 <- results[grep(pattern=("[[:digit:]]_3"), results)]
  emo4 <- results[grep(pattern=("[[:digit:]]_4"), results)]
  emo5 <-  results[grep(pattern=("[[:digit:]]_5"), results)]
  
  
  data.partial <- data2[,c(emo1,emo2,emo3,emo4, emo5)]
  if(dim(data.parts)[1] == 0) {
    data.parts <- data.partial
  } else {
    data.parts <- cbind(data.parts, data.partial)
  }
  
  emo1 <- sort(emo1)
  emo2 <- sort(emo2)
  emo3 <- sort(emo3)
  emo4 <- sort(emo4)
  emo5 <- sort(emo5)
  
  all.info <-  ""
  new.info <- ""
  for(e in 1:length(emo1)) {
    for(f in 1:(e)) {
      new.info <- paste0(new.info, emo1[e]," ~~ ", emo1[f], " \n ")
    }
  }
  
  all.info <- paste0(all.info, new.info)
  
  new.info <- ""
  for(e in 1:length(emo2)) {
    for(f in 1:(e)) {
      new.info <- paste0(new.info, emo2[e]," ~~ ", emo2[f], " \n ")
    }
  }
  
  all.info <- paste0(all.info, new.info)
  
  new.info <- ""
  for(e in 1:length(emo3)) {
    for(f in 1:(e)) {
      new.info <- paste0(new.info, emo3[e]," ~~ ", emo3[f], " \n ")
    }
  }
  
  all.info <- paste0(all.info, new.info)
  
  new.info <- ""
  for(e in 1:length(emo4)) {
    for(f in 1:(e)) {
      new.info <- paste0(new.info, emo4[e]," ~~ ", emo4[f], " \n ")
    }
  }
  
  new.info <- ""
  for(e in 1:length(emo5)) {
    for(f in 1:(e)) {
      new.info <- paste0(new.info, emo5[e]," ~~ ", emo5[f], " \n ")
    }
  }
 
  all.info <- paste0(all.info, new.info)
  
  final.info <- ""
  for(u in 1:length(emo1)) {
    final.info <- paste0(final.info, emo1[u], " ~~ ", emo2[u], " \n ")
    final.info <- paste0(final.info, emo2[u], " ~~ ", emo3[u], " \n ")
    final.info <- paste0(final.info, emo3[u], " ~~ ", emo4[u], " \n ",
    final.info <- paste0(final.info, emo4[u], " ~~ ", emo5[u], " \n ")
  }
  
  all.info <- paste0(all.info, final.info)
  
  model.text <- paste0(model.text, " \n ",  "emo",index,"_1 =~ ", write.as.sum(emo1)," \n ",
                       "emo",index,"_2 =~ ", write.as.sum(emo2)," \n ",
                       "emo",index,"_3 =~ ",write.as.sum(emo3)," \n ",
                       "emo",index,"_4 =~ ",write.as.sum(emo4)," \n ",
                       "emo",index,"_5 =~ ",write.as.sum(emo5)," \n ")

}
  
data.partial <- cbind(data.parts, data2[,c("gma_jz5" ,"gma_jz6", "gma_jz7", "gma_jz8","gma_jz9")])
t1 <- "gma_jz6 ~  gma_jz5"
t2 <- "gma_jz7 ~  gma_jz6"
t3 <- "gma_jz8 ~  gma_jz7"
t4 <- "gma_jz9 ~  gma_jz8"


  for(r in  to.use.emos) {
    t1 <- paste0(t1, " + emo",r,"_1" ," \n ")
    t2<- paste0(t2, " + emo",r,"_2"," \n ")
    t3 <- paste0(t3, " + emo",r,"_3"," \n ")
    t4 <- paste0(t4, " + emo",r,"_4"," \n ")
    t5 <- paste0(t4, " + emo",r,"_5"," \n ")
  }

even.more.info <- ""
for(r in  1:length(to.use.emos)) {
  for(u in 1:(r-1)) {
    if(u > 0 && r!=u) {
 even.more.info <- paste0(even.more.info, " emo",r,"_1 ~~ emo ",u,"_1 \n" )
 even.more.info <- paste0(even.more.info, " emo",r,"_2 ~~ emo ",u,"_2 \n" )
 even.more.info <- paste0(even.more.info, " emo",r,"_3 ~~ emo ",u,"_3 \n" )
 even.more.info <- paste0(even.more.info, " emo",r,"_4 ~~ emo ",u,"_4 \n" )
 even.more.info <- paste0(even.more.info, " emo",r,"_5 ~~ emo ",u,"_5 \n" )
    }
  }
}

#####mit der Information, dass bestimmte Items manchmal korrelieren, konvergiert dann aber manchmal nicht!
if(correlation == T) {
model <- paste0(model.text, t1,t2,t3,t4, all.info)
} else {
model <- paste0(model.text, t1,t2,t3,t4)
}
#model <- paste0(model.text, t1,t2,t3,t4)
model.save <<- model


da <- apply(na.omit(data.partial),as.numeric,MARGIN=c(1,2))

fit <- lavaan::sem(model, data = da, mimic="MPLUS", meanstructure=T, std.lv=T)

#predict(fit)

loadings <- inspect(fit, "coefficients")$lambda
covmatrix <- inspect(fit,"coefficients")$theta

#predict(fit, data.partial)

parameterEstimates(fit)

coefs.matrix.sem <- matrix(0,nrow=7,ncol=4)

for(u in 1:7) {
coefs.matrix.sem[u,1] <- coef(fit)[paste0("gma_jz6~emo",u,"_1")]
coefs.matrix.sem[u,2] <-coef(fit)[paste0("gma_jz7~emo",u,"_2")]
coefs.matrix.sem[u,3] <-coef(fit)[paste0("gma_jz8~emo",u,"_3")]
coefs.matrix.sem[u,4] <- coef(fit)[paste0("gma_jz9~emo",u,"_4")]
}



re.list <- list()
coefs.matrix.sem <<- coefs.matrix.sem 

re.list[["loadings"]] <- loadings 
re.list[["covmatrix"]] <- covmatrix

re.list
}


model = "
  ax1 =~  axc_1 + axl_1 + axt_1 
  ax2 =~  axc_2 + axl_2 + axt_2 
  ax3 =~  axc_3 + axl_3 + axt_3
  ax4 =~  axc_4 + axl_4 + axt_4 
  ax5 =~  axc_5 + axl_5 + axt_5 
gma_jz6 ~ ax1 + gma_jz5 + 1
gma_jz7 ~ ax2 + gma_jz6 + 1
gma_jz8 ~ ax3 + gma_jz7 + 1
gma_jz9 ~ ax4 + gma_jz8 + 1
ax2 ~ ax1 + gma_jz5 +1  
ax3 ~ ax2 + gma_jz6 + 1
ax4 ~ ax3 + gma_jz7 + 1
ax5 ~ ax4 + gma_jz8 + 1
axc_1 ~~ axc_2
axc_1 ~~ axc_3
axc_1 ~~ axc_4
axc_1 ~~ axc_5
axc_2 ~~ axc_3
axc_2 ~~ axc_4
axc_2 ~~ axc_5
axc_3 ~~ axc_4
axc_3 ~~ axc_5
axc_4 ~~ axc_5
axl_1 ~~ axl_2
axl_1 ~~ axl_3
axl_1 ~~ axl_4
axl_1 ~~ axl_5
axl_2 ~~ axl_3
axl_2 ~~ axl_4
axl_2 ~~ axl_5
axl_3 ~~ axl_4
axl_3 ~~ axl_5
axl_4 ~~ axl_5
axt_1 ~~ axt_2
axt_1 ~~ axt_3
axt_1 ~~ axt_4
axt_1 ~~ axt_5
axt_2 ~~ axt_3
axt_2 ~~ axt_4
axt_2 ~~ axt_5
axt_3 ~~ axt_4
axt_3 ~~ axt_5
axt_4 ~~ axt_5
ax1 ~~ gma_jz5" 


model.1 = 
"ax1 =~ axc_1 + axl_1 + axt_1
gma_jz6 ~ ax1 + gma_jz5" 



model.t = "
ax1 =~  axc_1 + axl_1 + axt_1 
ax2 =~  axc_2 + axl_2 + axt_2 
ax3 =~  axc_3 + axl_3 + axt_3
ax4 =~  axc_4 + axl_4 + axt_4 
ax5 =~  axc_5 + axl_5 + axt_5 
gma_jz6 ~ ax1 + gma_jz5 + 1
gma_jz7 ~ ax2 + gma_jz6 + 1
gma_jz8 ~ ax3 + gma_jz7 + 1
gma_jz9 ~ ax4 + gma_jz8 + 1
ax2 ~ ax1 + gma_jz5 +1  
ax3 ~ ax2 + gma_jz6 + 1
ax4 ~ ax3 + gma_jz7 + 1
ax5 ~ ax4 + gma_jz8 + 1" 