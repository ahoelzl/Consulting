library(Hmisc)
library(lavaan)
library(MASS)
library(foreign)
source("simulation-types.R")
source("SEM.R")
source("simulate-data.R")
source("simple-model.R")



svd <- function (x, nu = min(n, p), nv = min(n, p), LINPACK = TRUE) 
{
  x <- as.matrix(x)
  if (any(!is.finite(x))) 
    stop("infinite or missing values in 'x'")
  dx <- dim(x)
  n <- dx[1L]
  p <- dx[2L]
  if (!n || !p) 
    stop("a dimension is zero")
  La.res <- La.svd(x, nu, nv)
  res <- list(d = La.res$d)
  if (nu) 
    res$u <- La.res$u
  if (nv) {
    if (is.complex(x)) 
      res$v <- Conj(t(La.res$vt))
    else res$v <- t(La.res$vt)
  }
  res
}

assignInNamespace("svd", svd, "base") 


data <- data2 <- data.o  <- as.data.frame(read.spss("PALMA_Crosslagged_all_emos_MPlus.sav", use.missings=99))
emos <- c("jo","ax","ag","hl","bo","sh", "pr")
get.sem.coeffs()

extrapolation <- "extrapolation"

v.normal <- c()
v.simex.alpha <- c()
v.simex.sem <- c()
v.sem <- c()

b.normal <- c()
b.simex.alpha <- c()
b.simex.sem <- c()
b.sem <- c()


###test only one influence-variable

para.list <- list()
para.list[[1]] <- c(3,4,5)
para.list[[2]] <- c(5,6,2)
para.list[[3]] <- c(1,2,4)
para.list[[4]] <- c(3,6,7)

theoretical.coeffs <- theoretical.values

library(Hmisc)
library(simex)
library(sem)

data <- data2 <- data.o  <- as.data.frame(read.spss("PALMA_Crosslagged_all_emos_MPlus.sav", use.missings=99))
list.loadings <- list()
list.theta <- list()

for(e in 1:length(emos)) {
  e <- c(e)
  re.list <- sem.one.variable(e, correlation=T)
  
  list.loadings[[e]] <- re.list[["loadings"]]
  list.theta[[e]] <- re.list[["covmatrix"]]
}


list.loadings.save <- list.loadings
list.theta.save <- list.theta


#list.loadings <-  list.loadings.save
#list.theta <- list.theta.save

s <- 1
varlist <- list.one
var.factor <- c(0.01,0.5,1,1.5,2)
regression.error <- c(0,0.5,1,1.2)
r.error <- 0.5
resultmatrix <- matrix(0, ncol=4, nrow=length(var.factor))
biasmatrix <-  matrix(0, ncol=4, nrow=length(var.factor))
b2 <- 3
variance <- var.factor[b2]
index <- 1

v.normal <- c()
v.simex.alpha <- c()
v.simex.sem <- c()
v.sem <- c()

b.normal <- c()
b.simex.alpha <- c()
b.simex.sem <- c()
b.sem <- c()  

for(s in 1:4) {
  varlist <- list()
  if(s == 1) {
    varlist <- list.one
  } else if(s==2) {
    varlist <- list.two
  } else if(s==3) {
    varlist <- list.three
  } else if(s==4) {
    varlist <- list.five
  } else if(s==5) {
    varlist <- list.seven
  }
  
  var.factor <- c(0.1,0.5,1,1.5,2)
  regression.error <- c(0.2,0.5,1,1.2)
  
  for(r.error in regression.error) {
    
    resultmatrix <- matrix(0, ncol=4, nrow=length(var.factor))
    biasmatrix <-  matrix(0, ncol=4, nrow=length(var.factor))
    for(b2 in 1:length(var.factor)) {
variance <- var.factor[b2]
      
      v.normal <- c()
      v.simex.alpha <- c()
      v.simex.sem <- c()
      v.sem <- c()
      
      b.normal <- c()
      b.simex.alpha <- c()
      b.simex.sem <- c()
      b.sem <- c()  
      
for(index in 1:length(varlist)) {
vars <-varlist[[index]]

data <- data2 <- simulate.data2(vars, measurement.corrs=T, var.factor=variance, regression.error = r.error) 
# true.error.scores 
#actual_grades
#actual_scores

source("measurementError.R")
#alpha.errors
#sem2

get.lm.coeffs(vars)
#coefs.matrix.normal
#coefs.matrix.simex.alpha
#coefs.matrix.simex.sem
#coefs.matrix.simex.pca
#coefs.matrix2.normal
#coefs.matrix2.simex.sem


if(s < 4) {
sem.one.variable(vars, correlation=F)
}
#coefs.matrix.sem
#coefs.matrix.simex.fit


diff.matrix.normal <- mean(abs(coefs.matrix.normal[vars,] - standardized.values))
diff.matrix.simex.alpha <- mean(abs(coefs.matrix.simex.alpha[vars,] - standardized.values))
diff.matrix.simex.sem <- mean(abs(coefs.matrix.simex.sem[vars,] - standardized.values))
diff.matrix.sem <- mean(abs(coefs.matrix.sem[vars,] - standardized.values))
#diff.matrix.simex.alpha.2d <- mean(abs(coefs.matrix.simex.alpha.2d[vars,] - theoretical.coeffs[vars,]))
#diff.matrix.simex.sem.2d <- mean(abs(coefs.matrix.simex.sem.2d[vars,] - theoretical.coeffs[vars,]))
#diff.matrix.pca <- mean(abs(coefs.matrix.simex.pca[vars,] - theoretical.coeffs[vars,]))

#diff.matrix.fit <- mean(abs(coefs.matrix.simex.fit[vars,] - theoretical.coeffs[vars,]))

bias.normal <- mean(abs(coefs.matrix.normal[vars,]) - abs( standardized.values))
bias.simex.alpha <- mean(abs(coefs.matrix.simex.alpha[vars,]) - abs(standardized.values))
bias.simex.sem <- mean(abs(coefs.matrix.simex.sem[vars,]) - abs( standardized.values))
#bias.simex.pca <- mean(abs(coefs.matrix.simex.pca[vars,]) - abs(theoretical.coeffs[vars,]))
bias.matrix.sem <- mean(abs(coefs.matrix.sem[vars,]) - abs(standardized.values))
#bias.matrix.fit <- mean(abs(coefs.matrix.simex.fit[vars,]) - abs(theoretical.coeffs[vars,]))

print("-------------")
print(diff.matrix.normal)
print(diff.matrix.simex.alpha)
print(diff.matrix.simex.sem)
print(diff.matrix.sem)
#print(diff.matrix.pca)
#print(diff.matrix.fit)

print(bias.normal)
print(bias.simex.alpha)
print(bias.simex.sem)
print(bias.matrix.sem)
print("-------------")


v <- index

 v.normal[v] <- diff.matrix.normal
v.simex.alpha[v] <- diff.matrix.simex.alpha
v.simex.sem[v] <- diff.matrix.simex.sem 
v.sem[v] <- diff.matrix.sem


b.normal[v] <- bias.normal 
b.simex.alpha[v] <- bias.simex.alpha
b.simex.sem[v] <- bias.simex.sem 
b.sem[v] <- bias.matrix.sem
}
      
      resultmatrix[b2, 1] <- mean(v.normal)
      resultmatrix[b2, 2] <- mean(v.simex.alpha)
      resultmatrix[b2, 3] <- mean(b.simex.sem)
      resultmatrix[b2, 4] <- mean(v.sem)
      
      biasmatrix[b2, 1] <- mean(b.normal)
      biasmatrix[b2, 2] <- mean(b.simex.alpha)
      biasmatrix[b2, 3] <- mean(b.simex.sem)
      biasmatrix[b2, 4] <- mean(b.sem)
      
      colnames(biasmatrix) <- c("normal", "simex-alpha","simex-sem","sem")
      rownames(biasmatrix) <- var.factor
      
      colnames(resultmatrix) <- c("normal", "simex-alpha","simex-sem","sem")
      rownames(resultmatrix) <- var.factor
      

      write.table(resultmatrix, file=paste0(getwd(),"/results/numberemos",s,"rerror",r.error ))
     

}
    
  }
  
}

###alle 1er
#mean(v.normal)
#[1] 0.02448617
#> mean(v.simex.alpha)
#[1] 0.01721923
#> mean(v.simex.sem)
#[1] 0.01362365
#> mean(v.sem)
#[1] 0.01994626

