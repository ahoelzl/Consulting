emos <- c("jo","ax","ag","hl","bo","sh", "pr")
#sems <- c(sem_jo, sem_ax,sem_ag,sem_hl,sem_bo,sem_sh,sem_pr)

#emotions <- read.data.several(emotions=emos)
#emotions <- na.omit(emotions)


get.lm.coeffs <- function(use.emos) {

  emotions <- read.data.several(emotions=emos)
  
  coefs.matrix.normal <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.alpha <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.sem <- matrix(0,nrow=7, ncol=4)
  
  coefs.matrix.normal.auto <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.alpha.auto <- matrix(0,nrow=7, ncol=4)
  coefs.matrix.simex.sem.auto <- matrix(0,nrow=7, ncol=4)
  
  coefs.matrix2.normal <- matrix(0,nrow=7, ncol=4)
  coefs.matrix2.simex.alpha<- matrix(0,nrow=7, ncol=4)
  coefs.matrix2.simex.sem <- matrix(0,nrow=7, ncol=4)
  
  coefs.matrix2.normal.auto <- matrix(0,nrow=7, ncol=4)
  coefs.matrix2.simex.alpha.auto <- matrix(0,nrow=7, ncol=4)
  coefs.matrix2.simex.sem.auto <- matrix(0,nrow=7, ncol=4)
  
for(t in 1:4) {
simex.var <- emos[use.emos]
select <- use.emos + 3
emotions.1 <- emotions[emotions$time==t,c(1,2,3,select)]
emotions.2 <- emotions[emotions$time==(t+1),c(1,2,3,select)]
#to.use <- na.omit(emotions.1[,c(1,2,(i+3))])
  
 to.use.scale <- emotions.1[,-3]
to.use.scale2 <- emotions.2[,-3]
  var.whole <<- apply(na.omit(to.use.scale[-c(1,2)]),var,MARGIN=2)

var.score <- var.whole - alpha.errors[use.emos,t]^2

  sd <- sqrt(var(emotions.1$y))
  
 
  linear.model <- lm(y ~ .,data=to.use.scale)





  sem.corrected <- alpha.errors[use.emos,t]
  sem2.corrected <- sem2[use.emos]
  


  #simex.linear.model.pca <- simex(model= linear.model.pca, SIMEXvariable=simex.var.pca, measurement.error = (as.matrix( pca.measurement.error)), asymptotic=F)
  
 # new.est <- coef(simex.linear.model.pca )[-1] %*% (t(pca.to.use$loadings))
simex.linear.model.alpha <<- simex(model=linear.model, SIMEXvariable=simex.var,measurement.error = t(as.matrix(sem.corrected)), asymptotic=F)
simex.linear.model.sem <<- simex(model=linear.model, SIMEXvariable=simex.var, measurement.error = t(as.matrix(sem2.corrected)), asymptotic=F)

#new.x <- calibrate(b=simex.linear.model.alpha$coef, y0=to.use.scale[,1])



if(t<4) {


  new.frame <- data.frame(cbind(to.use.scale2[,3], to.use.scale[,c(3)] , to.use.scale[,2]))
  colnames(new.frame) <- c("y",emos[use.emos[1]],"y_before")
  linear.model2 <- lm(y~.,data=new.frame)
  
  simex.linear.model.alpha2 <<- simex(model=linear.model2, SIMEXvariable=simex.var[1],measurement.error = t(as.matrix(sem.corrected))[,1], asymptotic=F)
  simex.linear.model.sem2 <<- simex(model=linear.model2, SIMEXvariable=simex.var[1], measurement.error = t(as.matrix(sem2.corrected))[,1], asymptotic=F)
  
  
  simex.linear.model.alpha2.save <- coef(simex.linear.model.alpha2) * (sd(to.use.scale2[,3])/sd(to.use.scale[,3]))
  simex.linear.model.sem2.save <- coef(simex.linear.model.sem2 ) * (sd(to.use.scale2[,3])/sd(to.use.scale[,3]))
  
}


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

    coefs.matrix.normal[use.emos[a],t] <- linear.model$coef[c]*sqrt(var.whole[a])
    coefs.matrix.simex.alpha[use.emos[a],t] <-simex.linear.model.alpha$coef[c]*sqrt(var.whole[a])
    coefs.matrix.simex.sem[use.emos[a],t]  <-simex.linear.model.sem$coef[c]*sqrt(var.whole[a])
    
    coefs.matrix.normal.auto[use.emos[a],t] <- linear.model$coef[2]
    coefs.matrix.simex.alpha.auto[use.emos[a],t] <-simex.linear.model.alpha$coef[2]
    coefs.matrix.simex.sem.auto[use.emos[a],t]  <-simex.linear.model.sem$coef[2]
    

    
    #[1,t] <- linear.model2$coef[3]
    coefs.matrix2.simex.alpha[1,t]  <- simex.linear.model.alpha2.save[2]
    coefs.matrix2.normal[1,t]  <- linear.model2$coef[2]
    coefs.matrix2.simex.sem[1,t]  <- simex.linear.model.sem2.save[2]
    
    
    coefs.matrix2.normal.auto[1,t] <- linear.model2$coef[3]
    coefs.matrix2.simex.alpha.auto[1,t]  <- simex.linear.model.alpha2$coef[3]
    coefs.matrix2.simex.sem.auto[1,t]  <- simex.linear.model.sem2$coef[3]
    #coefs.matrix.simex.pca[use.emos[a],t]  <-new.est[c]
  }
  
#coefs.matrix.simex.sem.regularized[i,t] <-  simex.linear.model.sem.regularized$coef[3]
#estimates <-simex.linear.model.sem$SIMEX.estimate 

#plot(estimates[,1], estimates[,4], main=emo)
}


#colnames(coefs.matrix.normal) <- c("T1", "T2","T3","T4")
#colnames(coefs.matrix.simex.alpha) <- c("T1", "T2","T3","T4")
#colnames(coefs.matrix.simex.sem) <- c("T1", "T2","T3","T4")
#olnames(coefs.matrix.simex.fit) <- c("T1", "T2","T3","T4")

#rownames(coefs.matrix.normal) <- emos
#rownames(coefs.matrix.simex.alpha) <- emos
#rownames(coefs.matrix.simex.sem) <- emos
#rownames(coefs.matrix.simex.fit ) <- emos
  
  coefs.matrix.normal <<- coefs.matrix.normal
  coefs.matrix.simex.alpha <<- coefs.matrix.simex.alpha
  coefs.matrix.simex.sem<<- coefs.matrix.simex.sem
  coefs.matrix2.simex.alpha <<- coefs.matrix2.simex.alpha
  coefs.matrix2.simex.alpha.auto <<- coefs.matrix2.simex.alpha.auto
  
  
  coefs.matrix2.normal <<- coefs.matrix2.normal
  
  coefs.matrix2.simex.sem <<-  coefs.matrix2.simex.sem
  
  coefs.matrix2.normal.auto <<- coefs.matrix2.normal.auto
  coefs.matrix2.simex.alpha.auto <<- coefs.matrix2.simex.alpha.auto
  coefs.matrix2.simex.sem.auto <<-  coefs.matrix2.simex.sem.auto
  
  coefs.matrix.normal.auto <<- coefs.matrix.normal.auto 
  coefs.matrix.simex.alpha.auto <<- coefs.matrix.simex.alpha.auto
  coefs.matrix.simex.sem.auto <<- coefs.matrix.simex.sem.auto
  
}

