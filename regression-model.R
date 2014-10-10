emos <- c("jo","ax","ag","hl","bo","sh", "pr")
#sems <- c(sem_jo, sem_ax,sem_ag,sem_hl,sem_bo,sem_sh,sem_pr)

#emotions <- read.data.several(emotions=emos)
#emotions <- na.omit(emotions)


get.lm.coeffs <- function(use.emos) {

  emotions <- read.data.several(emotions=emos)
  
  coefs.matrix.normal <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.alpha <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.sem <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.fit <- matrix(0,nrow=7, ncol=4)
  
  
  coefs.matrix.simex.pca <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.alpha.2d <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.sem.2d <- matrix(0,nrow=7, ncol=4)
  
  coefs.matrix.simex.sem.regularized <- matrix(0,nrow=7, ncol=4)  

for(t in 1:4) {
simex.var <- emos[use.emos]
select <- use.emos + 3

emotions.1 <- emotions[emotions$time==t,c(1,2,3,select)]
#to.use <- na.omit(emotions.1[,c(1,2,(i+3))])
  
 to.use.scale <- emotions.1[,-3]
  var.whole <<- apply(to.use.scale[-c(1,2)],var,MARGIN=2)
#  to.use.scale[,3:dim(to.use.scale)[2]] <- scale(to.use.scale[,3:dim(to.use.scale)[2]])

var.score <- var.whole - alpha.errors[use.emos,t]^2

 # new.est <- (simex.linear.model.pca) %*% (t(pca.to.use$loadings))
  sd <- sqrt(var(emotions.1$y))
  
  
  linear.model <- lm(y ~ .,data=to.use.scale)
#sems[i] <- 0.069
  sem.corrected <- alpha.errors[use.emos,t]
  sem2.corrected <- sem2[use.emos]
  


  pca.to.use <- princomp(to.use.scale[-c(1)])
  scores <- pca.to.use$scores
  
  fr <- as.data.frame(cbind(y=(to.use.scale)[,1], scores))
  linear.model.pca <- lm(y~.,data=fr)
  
  pca.measurement.error <- c(0,sem2.corrected) %*% (t(pca.to.use$loadings))^2
 # new.est2 <- coef(linear.model.pca)[-1] %*% (t(pca.to.use$loadings))
  simex.var.pca <- colnames(linear.model.pca$model)[-1]
  measurement.error = (as.matrix( pca.measurement.error))
  #simex.linear.model.pca <- simex(model= linear.model.pca, SIMEXvariable=simex.var.pca, measurement.error = (as.matrix( pca.measurement.error)), asymptotic=F)
  
 # new.est <- coef(simex.linear.model.pca )[-1] %*% (t(pca.to.use$loadings))
simex.linear.model.alpha <<- simex(model=linear.model, SIMEXvariable=simex.var,measurement.error = t(as.matrix(sem.corrected)), asymptotic=F)
simex.linear.model.sem <<- simex(model=linear.model, SIMEXvariable=simex.var, measurement.error = t(as.matrix(sem2.corrected)), asymptotic=F)
  
#  start=coef(simex.linear.model.sem )
#  goal=coef(linear.model)
#  sd=sd(linear.model$residuals)
#  measurement.error=sem2.corrected [use.emos]

#simex.linear.model.fit <<- getfit(start=coef(linear.model ),goal=coef(linear.model), sd=sd(linear.model$residuals), measurement.error=measurement.error,use.emos=use.emos,to.use.scale=to.use.scale) 
 simex.linear.model.fit <- 0 
  

#  simex.linear.model.alpha.2d <- simex2d(model=linear.model, SIMEXvariable=simex.var, 
#                                         measurement.error = t(as.matrix(sems[use.emos])),
#                                         asymptotic=F, jackknife.estimation=F)
  
  #simex.linear.model.sem.2d <- simex2d(model=linear.model,
  #                                     SIMEXvariable=simex.var, 
  #                                     measurement.error = t(as.matrix(sem2[use.emos])), 
   #                                    asymptotic=F, 
    ##                                  jackknife.estimation=F)
  
  
 # coefs.matrix.normal[i,t] <- linear.model$coef[3]
 # coefs.matrix.simex.alpha[i,t] <-simex.linear.model.alpha$coef[3]
#  coefs.matrix.simex.sem[i,t] <-simex.linear.model.sem$coef[3]
#new.est <- c(0,0,new.est)
  for(c in 3:length(simex.linear.model.alpha$coef)) {
    a <- c - 2

    coefs.matrix.normal[use.emos[a],t] <- linear.model$coef[c]*sqrt(var.score[a])
    coefs.matrix.simex.alpha[use.emos[a],t] <-simex.linear.model.alpha$coef[c]*sqrt(var.score[a])
    coefs.matrix.simex.sem[use.emos[a],t]  <-simex.linear.model.sem$coef[c]*sqrt(var.score[a])
    
 #   coefs.matrix.simex.alpha.2d[use.emos[a],t] <- simex.linear.model.alpha.2d$coef[c]
#    coefs.matrix.simex.sem.2d[use.emos[a],t] <- simex.linear.model.sem.2d$coef[c]
    coefs.matrix.simex.fit[use.emos[a],t] <- simex.linear.model.fit[c]*sqrt(var.score[a])
    #coefs.matrix.simex.pca[use.emos[a],t]  <-new.est[c]
  }
  
#coefs.matrix.simex.sem.regularized[i,t] <-  simex.linear.model.sem.regularized$coef[3]
#estimates <-simex.linear.model.sem$SIMEX.estimate 

#plot(estimates[,1], estimates[,4], main=emo)
}


colnames(coefs.matrix.normal) <- c("T1", "T2","T3","T4")
colnames(coefs.matrix.simex.alpha) <- c("T1", "T2","T3","T4")
#colnames(coefs.matrix.simex.sem) <- c("T1", "T2","T3","T4")
#olnames(coefs.matrix.simex.fit) <- c("T1", "T2","T3","T4")

rownames(coefs.matrix.normal) <- emos
rownames(coefs.matrix.simex.alpha) <- emos
#rownames(coefs.matrix.simex.sem) <- emos
#rownames(coefs.matrix.simex.fit ) <- emos
  
  coefs.matrix.normal <<- coefs.matrix.normal
  coefs.matrix.simex.alpha <<- coefs.matrix.simex.alpha
  coefs.matrix.simex.sem<<- coefs.matrix.simex.sem

  coefs.matrix.simex.fit<<-coefs.matrix.simex.fit
}
