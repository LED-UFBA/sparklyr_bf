#pacotes
require(randomForest)
require(kernlab)
require(rpart)

#For 100 repetitions
load("list_data_2_class_100_rep")

#For 30 repetitions
load("list_data_2_class_30_rep")

#conjunto de dados (spam do kernlab)
dados<-rbind(list_data[[1]]$training,list_data[[1]]$test)


#número de variáveis
p <- ncol(dados)-1

#tuning arvores
runs <- 100   #número de repletições



##########
#SELECTION
##########

#Matriz de importânias (para os runs)
IMPS.ACC=matrix(0,nrow=p,ncol=runs)
IMPS.GINI <- IMPS.ACC

for (k in 1:runs){
  model.rep <- resultado_random_forest_one[[1]][[k]]
  Imp <- importance(model.rep)  
  IMPS.ACC[,k]=Imp[,1] #armezenando importancias
  IMPS.GINI[,k]=Imp[,2]
  cat("running",k,"... \n")
}

nomes.var <- names(Imp[,2])
Imp.geral=apply(IMPS.ACC,1,mean)
names(Imp.geral)=nomes.var
Var.Imp.geral=apply(IMPS.ACC,1,sd)
names(Var.Imp.geral)=nomes.var



#VI mean
imp.geral <- sort(Imp.geral,decreasing = T)
barplot(imp.geral,las=2,space=0.7, border=F, col="orange")



#standard deviations of VI
var.imp.geral <- Var.Imp.geral[names(imp.geral)] 
plot(1:length(var.imp.geral),var.imp.geral,
     las=2,col="orange",type='b',axes=F,xlab="")
axis(1,at=1:length(var.imp.geral), las=2,
     labels=names(imp.geral))
axis(2)


#modelagem via car para definir o threshold
#via sds
id <- 1:length(var.imp.geral)
fit <- rpart(var.imp.geral ~ id)
pred <- predict(fit,data=seq(1,p))
points(seq(1,p),pred,type='l',lwd=2)
thr=min(pred)
abline(h=thr,lwd=2,lty=2,col="red")

lv=var.imp.geral>=thr
corte=max(which(lv == FALSE))
corte.imp=min(imp.geral[1:corte])

#VI mean
imp.geral <- sort(Imp.geral,decreasing = T)
barplot(imp.geral,las=2,space=0.7, border=F, col="orange")
abline(h=corte.imp,lwd=2,lty=6)

#var.selecionadas via critério CART
vars=names(imp.geral[1:corte])

n.var=length(vars)
OBB=matrix(0,nrow=n.var,ncol=runs)

#criando estimativa do erro via nested models
#modelo 1= a mais importante
#modelo 2= as duas mais importantes
#modelo n.var = as n.vars seleciondadas anteriormente

for (k in 1:runs) {
for (i in 1:n.var){
  dados.mod <- dados[c("class",vars[1:i])]
  model2 <- randomForest(class ~ ., 
                         data = dados.mod,
               importance = TRUE)
  OBB[i,k] <- 1-sum(diag(model2$confusion))/sum(model2$confusion)
}
  cat("running",k,"... \n")
}

OBBs <- apply(OBB,1,mean)
plot(OBBs,type='h')
points(OBBs,type='p',pch=19)


OBBS.min.position=which(OBBs==min(OBBs))

#classical trick
trh2 <- mean(OBB[OBBS.min.position,])+sd(OBB[OBBS.min.position,])
abline(h=trh2,lwd=2,lty=6)

ind.thr=trh2>OBBs
corte.final=min(which(ind.thr== TRUE))

#SELEÇÃO FINAL

#variáveis selecionadas
vars[1:corte.final]

#VI mean
imp.geral <- sort(Imp.geral,decreasing = T)
barplot(imp.geral,las=2,space=0.7, border=F, col="orange")
abline(h=imp.geral[corte.final],lwd=2,lty=6)

selected_vars<-vars[1:corte.final]

save(selected_vars,file="var.selecionada.class.Rdata")
