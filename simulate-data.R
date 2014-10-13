library(foreign)
library(glmnet)
library(lavaan)
library(psych)
library(lattice)
#data <- data2 <- data.o  <- read.spss("PALMA_Crosslagged_all_emos_MPlus.sav", use.missings=99)
#data.o <- as.data.frame(data.o)



read.data <- function(emotion) {
  total.data.frame <- data.frame()
  for(i in 1:4) {
    one.time.data.frame <- data.frame()
    variable <- c()
    variable.names <- c()
    #  for(j in 1:20) {
    variable <- c(variable, paste0(emotion,"[0-9]+","_",i))
    #variable.names <- c(variable.names, paste0(emotion,j))
    #}
    
    variables <- data[,(names(data)[grep(variable,names(data))])]
    
    numbers <- apply(variables, FUN=as.numeric, MARGIN=c(1,2))
    sum.scores <- apply(numbers, FUN=sumf, MARGIN=1)
    
    colnames(variables) <- variable.names 
    #y <- data[,paste0("fges_",i+1)]
    y <- data[,paste0("gma_jz",(i+5))]
    y_before <- data[,paste0("gma_jz",(i+4))]
    time <- rep(i, length(y))
    one.time.data.frame <-cbind(y,y_before,sum.scores, time)
    
    
    
    colnames(one.time.data.frame) <- c("y","y_before",emotion,"time")
    #print(summary(is.na(one.time.data.frame)))
    total.data.frame <- rbind(total.data.frame, one.time.data.frame)
  }
  total.data.frame
  
}



read.data.several <- function(emotions) {
  all.emotions <- data.frame()
  y <- c()
  for(e in emotions) {
    ems <- read.data(emotion=e)
    y <- ems[,1]
    y_before <- ems[,2]
    emo <- ems[,3]
    time <- ems[,4]
    if(dim(all.emotions)[2] == 0) {
      all.emotions <- data.frame(y=y,y_before=y_before, time=time)
    } 
    all.emotions <- cbind(all.emotions, emo)
    colnames(all.emotions)[dim(all.emotions)[2]] <- e
  }
  all.emotions
}


sumf <- function(v)
{
  sum(v)
}
#emotions <- read.data.several(emotions=c("jo","ax","ag","hl","bo","sh", "pr"))


#emotions <- na.omit(emotions)
write.as.sum <- function(vec) {
  sum <- " "
  for(i in 1:(length(vec)-1)) {
    sum <- paste0(sum, vec[i], " + ")
  }
  sum <- paste0(sum,vec[length(vec)])
}

emos <- c("jo","ax","ag","hl","bo","sh", "pr")

#source("measurementError.R")
#source("simple-model.R")
#source("SEM.R")
#source("simex.R")


data.o.cov <- function() {
  results <- colnames(data2[,(grep(emo,names(data2)))])
  
  emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
  emo2 <- results[grep(pattern=("[[:digit:]]_2"), results)]
  emo3 <- results[grep(pattern=("[[:digit:]]_3"), results)]
  emo4 <- results[grep(pattern=("[[:digit:]]_4"), results)]
  emo5 <-  results[grep(pattern=("[[:digit:]]_5"), results)]  
  
}

  
  simulate.data2 <- function(vars.to.use,  measurement.corrs, var.factor,regression.error) {
    data.o <- as.data.frame(data.o)
    data <- data.o
    data2 <- data.o
    
    data.simulated <- data.frame()
    
    
    theoretical.values <- matrix( c( -0.05804094, -0.05409345, -0.06746890, -0.08087556,  0.15558478,
                                     0.08829224 , 0.09330492,  0.11410267,  0.05264895,  0.09977383, 
                                     0.14901828 , 0.09602152, 0.05632117 , 0.09531798,  0.12203596,  
                                     0.13641518, -0.02005966,  0.09421507,  0.10512067,  0.04837811, 
                                     0.07144132  ,0.12090780 , 0.07013713,  0.09041474, -0.09039762, 
                                     -0.04707176 ,-0.08964692, -0.07172357), ncol=4,byrow=T)
    
    
    theoretical.values2 <- matrix( c( -0.05804094, -0.05409345, -0.06746890, -0.08087556,  0.15558478,
                                      0.08829224 , 0.09330492,  0.11410267,  0.05264895,  0.09977383, 
                                      0.14901828 , 0.09602152, 0.05632117 , 0.09531798,  0.12203596,  
                                      0.13641518, -0.02005966,  0.09421507,  0.10512067,  0.04837811, 
                                      0.07144132  ,0.12090780 , 0.07013713,  0.09041474, -0.09039762, 
                                      -0.04707176 ,-0.08964692, -0.07172357), ncol=4,byrow=T)
    

    intercepts <- c(2.012   ,   2.950 ,  3.225,  2.5   )
    
    year_before <- c(0.679  ,0.583,0.627,0.696)
    
    emo_before <- c(0.679  ,0.583,0.627,0.696)
    
    first_emo <- matrix(0, ncol=length(vars.to.use),nrow=dim(data2)[1])
    for( i in 1:length(vars.to.use)) {
    
      emo <- emos[vars.to.use[i]]
      
      results <- colnames(data2[,(grep(emo,names(data2)))])

      emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
     
      time1 <- impute((data2[,c(emo1[1])]), fun="random")
  
      first_emo[,i] <- time1
      
    }
    
    
    first_year <- data.o$gma_jz5
    first_year[is.na(first_year)] <-  mean(first_year, na.rm=T)
    
    
    sc1 <- as.numeric(impute( data.o[,emo1[1]]  ,fun="random"))
    
    emotions <- matrix(-1, nrow=dim(first_emo)[1], ncol=length(vars.to.use))
    
    result_grade <- matrix(first_year,ncol=1)
    result_emos <- apply(first_emo, FUN=scale,MARGIN=2)
    
    grades <- matrix(0, nrow=dim(first_emo)[1],ncol=5)
    
    
    all.scores1.theor <- matrix(1,ncol=7,nrow=dim(data.o)[1])
    all.scores2.theor <-  matrix(1,ncol=7,nrow=dim(data.o)[1])
    all.scores3.theor <-  matrix(1,ncol=7,nrow=dim(data.o)[1])
    all.scores4.theor <-  matrix(1,ncol=7,nrow=dim(data.o)[1])
    all.scores5.theor <-  matrix(1,ncol=7,nrow=dim(data.o)[1])
    
    for(t in 1:5) {
      
      grades[,t] <- result_grade[,1]
      
      if(t==1) {
        all.scores1.theor <-result_emos
      } else if(t==2) {
        all.scores2.theor <-result_emos
      } else if( t==3) {
        all.scores3.theor <-result_emos
      } else if( t==4) {
        all.scores4.theor <-result_emos
      } else if(t==5) {
        all.scores5.theor <-result_emos
      }
      
      if(t==5) {
        break
      }

      selected.coeffs.1 <- matrix(theoretical.values[vars.to.use,t],ncol=length(vars.to.use))
      selected.coeffs.2 <-  matrix(theoretical.values2[vars.to.use,t],ncol=length(vars.to.use))
      
      
      result_grade.new <-  result_emos  %*%  t(selected.coeffs.1) + result_grade * year_before[t] 
      
      result_emos.new <- matrix(0,nrow=length(first_year),ncol=length(vars.to.use))
      
      for(e in 1:length(vars.to.use)) {
      result_emos.new[,e] <- result_grade * selected.coeffs.2[,e] + result_emos[,e] * emo_before[t]
      }
      
      result_emos <- apply(result_emos.new, FUN=scale,MARGIN=2)

      result_grade <- result_grade.new
    
    }
    
   print( lm(grades[,2] ~ all.scores1.theor + grades[,1]))
    print(lm(grades[,3] ~ all.scores2.theor + grades[,2]))
    print(lm(grades[,4] ~ all.scores3.theor + grades[,3]))
    print(lm(grades[,5] ~ all.scores4.theor + grades[,4]))
    
    if(length(vars.to.use) > 1) {
    t1 <- lm( all.scores2.theor  ~ all.scores1.theor + grades[,1])$coef[3,1]
    t2 <- lm( all.scores3.theor  ~ all.scores2.theor + grades[,2])$coef[3,1]
    t3 <- lm( all.scores4.theor  ~ all.scores3.theor + grades[,3])$coef[3,1]
    
    t1.auto <- lm( all.scores2.theor  ~ all.scores1.theor + grades[,1])$coef[2,1]
    t2.auto <- lm( all.scores3.theor  ~ all.scores2.theor + grades[,2])$coef[2,1]
    t3.auto <- lm( all.scores4.theor  ~ all.scores3.theor + grades[,3])$coef[2,1]
    } else {
      t1 <- lm( all.scores2.theor  ~ all.scores1.theor + grades[,1])$coef[3]
      t2 <- lm( all.scores3.theor  ~ all.scores2.theor + grades[,2])$coef[3]
      t3 <- lm( all.scores4.theor  ~ all.scores3.theor + grades[,3])$coef[3]
      
      t1.auto <- lm( all.scores2.theor  ~ all.scores1.theor + grades[,1])$coef[2]
      t2.auto <- lm( all.scores3.theor  ~ all.scores2.theor + grades[,2])$coef[2]
      t3.auto <- lm( all.scores4.theor  ~ all.scores3.theor + grades[,3])$coef[2]
    }
    
    standardized.values2 <<- c(t1,t2,t3)
    standardized.values2.auto <<- c(t1.auto,t2.auto,t3.auto)
    
   su<-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    all.scores1.actual <-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    all.scores2.actual <-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    all.scores3.actual <-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    all.scores4.actual <-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    all.scores5.actual <-  matrix(1,ncol=length(vars.to.use),nrow=dim(data.o)[1])
    
    all.error.items <- data.frame()
    
    olds <- data.frame()
    
    
    truescore.list <- list()
    mean.values <- matrix(0,nrow=7, ncol=4)
    for( r in 1:length(vars.to.use)) {
      
      
      emo <- emos[vars.to.use[r]]
      
      results <- colnames(data2[,(grep(emo,names(data2)))])
      
      emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
      emo2 <- results[grep(pattern=("[[:digit:]]_2"), results)]
      emo3 <- results[grep(pattern=("[[:digit:]]_3"), results)]
      emo4 <- results[grep(pattern=("[[:digit:]]_4"), results)]
      
      i <- vars.to.use[r]
      loading <- list.loadings[[i]]
      theta <- list.theta[[i]]
      
 
      sc1 <- all.scores1.theor[,r]
      sc2 <- all.scores2.theor [,r]
      sc3 <- all.scores3.theor [,r]
      sc4 <- all.scores4.theor[,r] 
      sc5 <- all.scores5.theor[,r] 
      
      item.matrix <- matrix(0,nrow=length(sc1),ncol=dim(loading)[2])
      
      loadings <- loading[-c(dim(loading)[1]:(dim(loading)[1]-4)),-c(dim(loading)[2]:(dim(loading)[2]-4)) ]
      thetas <- theta[-c(dim(theta)[1]:(dim(theta)[1]-4)),-c(dim(theta)[2]:(dim(theta)[2]-4)) ]
      
      if(!measurement.corrs) {
      thetas.new <- matrix(0, nrow=dim(thetas)[1],ncol=dim(thetas)[2])  
      diag(thetas.new) <- diag(thetas)
      thetas <-thetas.new
      }
      
      all.cs <- cbind(scale(sc1),scale(sc2),scale(sc3),scale(sc4))
      
      items <- all.cs %*% t(loadings)
      
      pd <- nearPD(thetas)$mat

      errors <- var.factor *  mvrnorm(n = dim(all.cs)[1], mu=rep(0,dim(items)[2]), Sigma=pd)
   
      items.errors <- items + errors
      
      
      
      
      all.scores1.actual[,r] <- scale(apply(items.errors[,emo1], sum,MARGIN=1))
      all.scores2.actual[,r] <-  scale(apply(items.errors[,emo2], sum,MARGIN=1))
      all.scores3.actual[,r] <-  scale(apply(items.errors[,emo3], sum,MARGIN=1))
      all.scores4.actual[,r] <- scale(apply(items.errors[,emo4], sum,MARGIN=1))
      
      
      t1 <- lm( all.scores2.actual  ~ all.scores1.actual + grades[,1])$coef
      t2 <- lm( all.scores3.actual  ~ all.scores2.actual + grades[,2])$coef
      t3 <- lm( all.scores4.actual  ~ all.scores3.actual + grades[,3])$coef
      
      
      if(r == 1) {
        all.error.items <- items.errors
        
      } else {
        all.error.items  <- cbind(all.error.items ,items.errors)
      }
      
    }
    
    lm(grades[,2] ~ all.scores1.actual + grades[,1])
    lm(grades[,3] ~ all.scores2.actual + grades[,2])
    lm(grades[,4] ~ all.scores3.actual + grades[,3])
    lm(grades[,5] ~ all.scores4.actual + grades[,4])
    
    lm(grades[,2] ~ scale(all.scores1.actual) + grades[,1])
    lm(grades[,2] ~ scale(all.scores1.theor) + grades[,1])
    
    
    
    lm( scale(all.scores2.actual) ~ scale(all.scores1.actual) + grades[,1])
    lm( scale(all.scores2.theor) ~ scale(all.scores1.theor) + grades[,1])
    
    var.ges <- apply(all.scores1.actual ,FUN=var, MARGIN=2)
    var.score <- apply(all.scores1.theor ,FUN=var, MARGIN=2)
    var.error <- apply((all.scores1.actual -  all.scores1.theor),FUN=var, MARGIN=2)
    
    sd.score1 <- apply((all.scores1.actual -  all.scores1.theor),FUN=sd, MARGIN=2)
    sd.score2 <- apply((all.scores2.actual -  all.scores2.theor),FUN=sd, MARGIN=2)
    sd.score3 <- apply((all.scores3.actual -  all.scores3.theor),FUN=sd, MARGIN=2)
    sd.score4 <- apply((all.scores4.actual -  all.scores4.theor),FUN=sd, MARGIN=2)
    
    true.error.scores <<- cbind(sd.score1,sd.score2,sd.score3,sd.score4)
    #all.error.items
    # all.scores1.theor
    
    
    #theoretical.coeffs <- coefs.matrix.sem
    #theoretical.values
    #  [,1]        [,2]        [,3]        [,4]
    #  [1,] -0.05804094 -0.05409345 -0.06746890 -0.08087556
    #  [2,]  0.15558478  0.08829224  0.09330492  0.11410267
    #  [3,]  0.05264895  0.09977383  0.14901828  0.09602152
    #  [4,]  0.05632117  0.09531798  0.12203596  0.13641518
    #  [5,] -0.02005966  0.09421507  0.10512067  0.04837811
    #  [6,]  0.07144132  0.12090780  0.07013713  0.09041474
    #  [7,] -0.09039762 -0.04707176 -0.08964692 -0.07172357
 grades.sd <- apply(grades,sd,MARGIN=2)
    for(e in 1:5) {
    grades[,e] <- grades[,e] + matrix(rnorm(n=dim(grades)[1],sd=regression.error*grades.sd[e]), nrow=dim(grades)[1])
    }
    grades_all <- cbind(grades)
    #grades_all <-  scale(grades_all)
    colnames(grades_all) <- c("gma_jz5", "gma_jz6", "gma_jz7", "gma_jz8","gma_jz9")
    
    data <- na.omit(as.data.frame(cbind(all.error.items, grades_all)))
   
    
    data2 <- data
    
    standardized.values <- matrix(theoretical.values[vars.to.use,], ncol=4)
    standardized.values.auto <- matrix(theoretical.values[vars.to.use,], ncol=4)
    standardized.values[,1] <- (theoretical.values[vars.to.use,1]*  apply(all.scores1.theor, FUN=sd,MARGIN=2))
    standardized.values[,2] <- (theoretical.values[vars.to.use,2]*  apply(all.scores2.theor, FUN=sd,MARGIN=2))
    standardized.values[,3] <- (theoretical.values[vars.to.use,3]*  apply(all.scores3.theor, FUN=sd,MARGIN=2))
    standardized.values[,4] <- (theoretical.values[vars.to.use,4]*  apply(all.scores4.theor, FUN=sd,MARGIN=2))
    
    
    standardized.values.auto[,1] <- (theoretical.values2[vars.to.use,1]*  apply(all.scores1.theor, FUN=sd,MARGIN=2))
    standardized.values.auto[,2] <- (theoretical.values2[vars.to.use,2]*  apply(all.scores2.theor, FUN=sd,MARGIN=2))
    standardized.values.auto[,3] <- (theoretical.values2[vars.to.use,3]*  apply(all.scores3.theor, FUN=sd,MARGIN=2))
    standardized.values.auto[,4] <- (theoretical.values2[vars.to.use,4]*  apply(all.scores4.theor, FUN=sd,MARGIN=2))
    
    
    standardized.values <<-  standardized.values
    standardized.values.auto <<-  standardized.values.auto
    

   print( lm(grades_all[,2] ~ scale(apply(data[,emo1], sum,MARGIN=1)) + grades_all[,1]))
   print(  lm(grades_all[,3] ~ scale(apply(data[,emo2], sum,MARGIN=1))+ grades_all[,2]))
   print(  lm(grades_all[,4] ~scale(apply(data[,emo3], sum,MARGIN=1)) + grades_all[,3]))
    print(  lm(grades_all[,5] ~ scale(apply(data[,emo4], sum,MARGIN=1)) + grades_all[,4]))
    
    
    
    data2
  }








