library(Hmisc)
library(lavaan)
library(MASS)
library(foreign)
source("simulation-types.R")
source("SEM.R")
source("simulate-data.R")
source("simple-model.R")




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

var.factor <- c(1)
regression.error <- c(0.1)
corre <- T

for(s in 1:2) {
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
  

  for(r.error in regression.error) {
     
   
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
      
      resultmatrix <- matrix(0, ncol=4, nrow=4)
      resultmatrix2 <- matrix(0, ncol=4, nrow=4)
      biasmatrix <-  matrix(0, ncol=4, nrow=4)
      biasmatrix2 <-  matrix(0, ncol=4, nrow=4)
for(index in 1:length(varlist)) {
vars <-varlist[[index]]

data <- data2 <- simulate.data2(vars, measurement.corrs=corre, var.factor=variance, regression.error = r.error) 
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
#coefs.matrix2.normal
#coefs.matrix2.simex.sem
#coefs.matrix.normal.auto
#coefs.matrix.simex.alpha.auto
#coefs.matrix.simex.sem.auto
#coefs.matrix2.normal.auto
#coefs.matrix2.simex.sem.auto
###richtig:
###standardized.values 
###standardized.values2
##standardized.values.auto
###standardized.values2.auto
if(s < 4) {
sem.one.variable(vars, correlation=corre)
}
#coefs.matrix.sem
#coefs.matrix.simex.fit


diff.matrix.normal <- mean(abs(coefs.matrix.normal[vars,] - standardized.values)/abs(standardized.values))
diff.matrix.simex.alpha <- mean(abs(coefs.matrix.simex.alpha[vars,] - standardized.values)/abs(standardized.values))
diff.matrix.simex.sem <- mean(abs(coefs.matrix.simex.sem[vars,] - standardized.values)/abs(standardized.values))
diff.matrix.sem <- mean(abs(coefs.matrix.sem[vars,] - standardized.values)/abs(standardized.values))

diff2.normal <-     mean(abs(coefs.matrix2.normal[1,1:3] - standardized.values2)/abs(standardized.values2))
diff2.alpha <-   mean(abs(coefs.matrix2.simex.alpha[1,1:3]  - standardized.values2)/abs(standardized.values2))
diff2.sem <-   mean(abs(coefs.matrix2.simex.sem[1,1:3]  - standardized.values2)/abs(standardized.values2))

diff.normal.auto <- mean(abs(coefs.matrix.normal.auto[vars,] - standardized.values.auto)/abs(standardized.values.auto))
#diff.alpha.auto <- mean(abs( coefs.matrix.alpha.auto[vars,] - standardized.values.auto))
diff.sem.auto <- mean(abs( coefs.matrix.simex.sem.auto[vars,] - standardized.values.auto)/abs(standardized.values.auto))

diff2.normal.auto <- mean(abs(coefs.matrix2.normal.auto[1,1:3] - standardized.values2.auto)/abs(standardized.values2.auto))
diff2.alpha.auto <- mean(abs( coefs.matrix2.simex.alpha.auto[1,1:3] - standardized.values2.auto)/abs(standardized.values2.auto))
diff2.sem.auto <- mean(abs( coefs.matrix2.simex.sem.auto[1,1:3] - standardized.values2.auto)/abs(standardized.values2.auto))


bias.normal <- mean(abs(coefs.matrix.normal[vars,]) - abs( standardized.values))
bias.simex.alpha <- mean(abs(coefs.matrix.simex.alpha[vars,]) - abs(standardized.values))
bias.simex.sem <- mean(abs(coefs.matrix.simex.sem[vars,]) - abs( standardized.values))
bias.matrix.sem <- mean(abs(coefs.matrix.sem[vars,]) - abs(standardized.values))


bias2.normal <-     mean(abs(coefs.matrix2.normal[1,1:3]) - abs(standardized.values2))
bias2.alpha <-   mean(abs(coefs.matrix2.simex.alpha[1,1:3])  - abs(standardized.values2))
bias2.sem <-   mean(abs(coefs.matrix2.simex.sem[1,1:3])  - abs(standardized.values2))


print("-------------")
print(diff.matrix.normal)
print(diff.matrix.simex.alpha)
print(diff.matrix.simex.sem)
print(diff.matrix.sem)


print(diff2.normal)
print(diff2.alpha)
print(diff2.sem)

print(diff.normal.auto)
#print(diff.alpha.auto)
print(diff.sem.auto)

print(diff2.normal.auto)
#print(diff2.alpha.auto)
print(diff2.sem.auto)

print(bias.normal)
print(bias.simex.alpha)
print(bias.simex.sem)
print(bias.matrix.sem)
print("-------------")


v <- index

 v.normal[v] <- diff.matrix.normal
v.simex.alpha[v] <- diff2.alpha
v.simex.sem[v] <- diff2.sem
v.sem[v] <- diff.matrix.sem

print(v.normal)
print(v.simex.alpha)
print(v.simex.sem)
print(v.sem)

b.normal[v] <- bias.normal 

b.simex.alpha[v] <- bias.simex.alpha
b.simex.sem[v] <- bias.simex.sem 
b.sem[v] <- bias.matrix.sem


resultmatrix[v, 1] <- diff.matrix.normal
resultmatrix[v, 2] <-diff.matrix.simex.alpha
resultmatrix[v, 3] <-diff.matrix.simex.sem 
resultmatrix[v, 4] <-  diff.matrix.sem


resultmatrix2[v, 1] <- diff2.normal
resultmatrix2[v, 2] <-diff2.alpha
resultmatrix2[v, 3] <-diff.matrix.simex.sem 
#resultmatrix2[4, v] <-  diff2.sem 


biasmatrix[v, 1]  <- bias.normal 
biasmatrix[v, 2]  <- bias.simex.alpha
biasmatrix[v, 3]  <- bias.simex.sem 
biasmatrix[v, 4]  <- bias.matrix.sem 

biasmatrix2[v, 1]  <- bias2.normal 
biasmatrix2[v, 2]  <- bias2.alpha
biasmatrix2[v, 3]  <- bias2.sem 

colnames(resultmatrix) <- colnames(resultmatrix2)<- colnames(biasmatrix) <- colnames(resultmatrix2) <- c("norm. Regression", "Simex-Alpha", "Simex-SEM", "SEM")  


if(s==1) {
  rownames(resultmatrix) <- rownames(resultmatrix2)<- rownames(biasmatrix) <- rownames(resultmatrix2) <- c("joy", "anxiety", "anger", "hopelessness")  
} else if(s==2) {
  rownames(resultmatrix) <- rownames(resultmatrix2)<- rownames(biasmatrix) <- rownames(resultmatrix2) <- c("joy,anxiety", "anger,hopelesness", "anger,boredom", "hopelessness,shame")
}

table <- tableGrob(resultmatrix)
grid.newpage()
h <-2* grobHeight(table)
w <-2*grobWidth(table)


title <- textGrob("Abweichung von wahren Parameterwerten des Einflusses der Emotionen auf die Note", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=0, gp=gpar(fontsize=20))

footnote <- textGrob(paste0("bei Fehlervarianz ", variance, " mal so groß wie im Datensatz","   Regressionsfehler ", r.error," mal so groß wie erklärte Abweichung", " correlation ", corre), 
                     x=unit(0.5,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))

jpeg(filename=paste0(getwd(),"/results/ongradesnumberemos",s,"rerror",r.error," variance ", variance ,"corr",corre,".jpeg"),width = 1000, height = 1000)
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)
dev.off()
write.table(resultmatrix, file=paste0(getwd(),"/results/ongradesnumberemos",s,"rerror",r.error," variance ", variance ,"corr",corre))

table <- tableGrob(biasmatrix)
grid.newpage()
h <-1* grobHeight(table)
w <-1*grobWidth(table)


title <- textGrob("Bias von wahren Parameterwerten des Einflusses der Emotionen auf die Note ", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=0, gp=gpar(fontsize=20))

footnote <- textGrob(paste0("bei Fehlervarianz ", variance, " mal so groß wie im Datensatz","   Regressionsfehler ", r.error," mal so groß wie erklärte Abweichung", " correlation ", corre), 
                     x=unit(0.5,"npc") + 1*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=1,gp=gpar( fontface="italic"))

jpeg(filename=paste0(getwd(),"/results/biasongradesnumberemos",s,"rerror",r.error," variance ", variance ,"corr",corre,".jpeg"),width = 1000, height = 1000)
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)
dev.off()
write.table(biasmatrix, file=paste0(getwd(),"/results/biasongradessnumberemos",s,"rerror",r.error," variance ", variance,"corr",corre ))

table <- tableGrob(resultmatrix2)
grid.newpage()
h <-1.5* grobHeight(table)
w <-1.5*grobWidth(table)


title <- textGrob("Abweichung von wahren Parameterwerten der Note auf die erste Emotion ", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=0, gp=gpar(fontsize=20))

footnote <- textGrob(paste0("bei Fehlervarianz ", variance, " mal so groß wie im Datensatz","   Regressionsfehler ", r.error," mal so groß wie erklärte Abweichung", " correlation ", corre), 
                     x=unit(0.5,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))

jpeg(filename=paste0(getwd(),"/results/onemotionsnumberemos",s,"rerror",r.error," variance ", variance,"corr",corre,".jpeg" ) ,width = 1000, height = 1000)
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)
dev.off()
write.table(resultmatrix2, file=paste0(getwd(),"/results/onemotionsnumberemos",s,"rerror",r.error," variance ", variance,"corr",corre ))




table <- tableGrob(biasmatrix2)
grid.newpage()
h <-1.5* grobHeight(table)
w <-1.5*grobWidth(table)

title <- textGrob("Bias vom Einfluss der wahren Parameterwerten der Note auf die erste Emotion ", y=unit(0.5,"npc") + 0.5*h, 
                  vjust=0, gp=gpar(fontsize=20))

footnote <- textGrob(paste0("bei Fehlervarianz ", variance, " mal so groß wie im Datensatz","   Regressionsfehler ", r.error," mal so groß wie erklärte Abweichung", " correlation ", corre), 
                     x=unit(0.5,"npc") - 0.5*w,
                     y=unit(0.5,"npc") - 0.5*h, 
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))
jpeg(filename=paste0(getwd(),"/results/biasonemotionsnumberemos",s,"rerror",r.error," variance ", variance,"corr",corre,".jpeg" ),width = 1000, height = 1000)
gt <- gTree(children=gList(table, title, footnote))
grid.draw(gt)
dev.off()


write.table(biasmatrix2, file=paste0(getwd(),"/results/biasonemotionsnumberemos",s,"rerror",r.error," variance ", variance,"corr",corre ))

}
      

      
   #   colnames(biasmatrix) <- c("normal", "simex-alpha","simex-sem","sem")
  #    rownames(biasmatrix) <- var.factor
      
  #    colnames(resultmatrix) <- c("normal", "simex-alpha","simex-sem","sem")
  #    rownames(resultmatrix) <- var.factor
      

    
     

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

