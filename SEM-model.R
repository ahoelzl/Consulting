library(foreign)
library(glmnet)
library(lavaan)

##Hilfsfunktion um die Summenscores zu bilden,
##die normale sum-Funktion funktioniert manchmal nicht
write.as.sum <- function(vec) {
  sum <- " "
  for(i in 1:(length(vec)-1)) {
    sum <- paste0(sum, vec[i], " + ")
  }
    sum <- paste0(sum,vec[length(vec)])
}

##berechnet das SEM Modell für die Emotionen in to.use.emos.
#to.use.emos ist ein Vektor, der die entsprechenden Emotionen angibt
##1 : joy
##2 : anxiety
##3 : anger
##4 : hopelesness
##5 : boredom
##6 : shame
##7 : pride
###correlation gibt an, ob mit oder ohne Korrelation der Messfehler in einer Emotion gerechnet werden soll
sem.one.variable <- function(to.use.emos, correlation) {
##hier werden die Ergebnisse gespeichert
coefs.matrix.sem <- matrix(0,nrow=7, ncol=4)

true.coeffs <- matrix(0,nrow=9,ncol=4)
fits <- list()

emotions <- emos[to.use.emos]

model.text <- ""

data.parts <- data.frame()

##erstellt dynamisch den Text für die Modellbeschreibung
for(r in 1:length(emotions)) {
  emo <- emotions[r]
  index <- to.use.emos[r]
  
  results <- colnames(data2[,(grep(emo,names(data2)))])
  
  ##die Items, die verwendet werden
  emo1 <- results[grep(pattern=("[[:digit:]]_1"), results)]
  emo2 <- results[grep(pattern=("[[:digit:]]_2"), results)]
  emo3 <- results[grep(pattern=("[[:digit:]]_3"), results)]
  emo4 <- results[grep(pattern=("[[:digit:]]_4"), results)]
#  emo5 <-  results[grep(pattern=("[[:digit:]]_5"), results)]
  
  
  data.partial <- data2[,c(emo1,emo2,emo3,emo4)]
  if(dim(data.parts)[1] == 0) {
    data.parts <- data.partial
  } else {
    data.parts <- cbind(data.parts, data.partial)
  }
  
  emo1 <- sort(emo1)
  emo2 <- sort(emo2)
  emo3 <- sort(emo3)
  emo4 <- sort(emo4)
  
  ###Fehlerkorrelationen zwischen Items einer Zeiteinheit
  
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
  

 
  
  ###Fehlerkorrelationen zwischen den gleichen Items zu unterschiedlichen Zeitpunkten
  all.info <- paste0(all.info, new.info)
  
  final.info <- ""
  for(u in 1:length(emo1)) {
    final.info <- paste0(final.info, emo1[u], " ~~ ", emo2[u], " \n ")
    final.info <- paste0(final.info, emo2[u], " ~~ ", emo3[u], " \n ")
    final.info <- paste0(final.info, emo3[u], " ~~ ", emo4[u], " \n ")

  }
  
  all.info <- paste0(all.info, final.info)
  
  model.text <- paste0(model.text, " \n ",  "emo",index,"_1 =~ ", write.as.sum(emo1)," \n ",
                       "emo",index,"_2 =~ ", write.as.sum(emo2)," \n ",
                       "emo",index,"_3 =~ ",write.as.sum(emo3)," \n ",
                       "emo",index,"_4 =~ ",write.as.sum(emo4)," \n ")

}
  
###erstelle die Regressionsgleichungen
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
  }



#####mit der Information, dass bestimmte Items manchmal korrelieren, konvergiert dann aber manchmal nicht!
if(correlation == T) {
model <- paste0(model.text, t1,t2,t3,t4, all.info)
} else {
model <- paste0(model.text, t1,t2,t3,t4)
}

model.save <<- model


da <- apply(na.omit(data.partial),as.numeric,MARGIN=c(1,2))

fit <- lavaan::sem(model, data = da, mimic="MPLUS", meanstructure=T, std.lv=T)


loadings <- inspect(fit, "coefficients")$lambda
covmatrix <- inspect(fit,"coefficients")$theta

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

data <- data2 <- data.o  <- as.data.frame(read.spss("PALMA_Crosslagged_all_emos_MPlus.sav", use.missings=99))

emos <- c("jo","ax","ag","hl","bo","sh", "pr")


###SEM mit joy als Einflußgröße
sem.one.variable(to.use.emos <- c(1,2),correlation=T)

###SEM mit joy und anxiety als Einflußgrößen
sem.one.variable(to.use.emos <- c(1,2),correlation=T)

print(coefs.matrix.sem)
