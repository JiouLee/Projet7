


table<-read.table("prBiden.txt", col.names = c("Etat","pr.Biden"))
head(table)

######################################

library(ggplot2)
library(parallel)
#install.packages("gridExtra")
library(gridExtra)

######################################

nEtats <- 51
N<-1000

######################################
##IC1 et IC2 ne sont plus utilises

IC2 <- function(prob, u = qnorm(0.975),n=100){
  ICmin<- (prob + ((u**2)/(2*n))-(u/sqrt(n))*sqrt((u**2)/(4*n)+prob*(1-prob)))/(1+((u**2)/n))
  ICmax<- (prob + ((u**2)/(2*n))+(u/sqrt(n))*sqrt((u**2)/(4*n)+prob*(1-prob)))/(1+((u**2)/n))
  return(list(ICmin=ICmin, ICmax = ICmax))
}

IC1 <- function(prob, u = qnorm(0.975),n=100){
  ICmin<- prob-u*sqrt(prob*(1-prob)/n)
  ICmax<- prob+u*sqrt(prob*(1-prob)/n) 
  return(list(ICmin=ICmin, ICmax = ICmax))
}

##Fonction qui regroupe les 2 IC
IC <- function(prob, u = qnorm(0.975),n=100){
  ICmin1<- prob-u*sqrt(prob*(1-prob)/n)
  ICmax1<- prob+u*sqrt(prob*(1-prob)/n) 
  ICmin2<- (prob + ((u**2)/(2*n))-(u/sqrt(n))*sqrt((u**2)/(4*n)+prob*(1-prob)))/(1+((u**2)/n))
  ICmax2<- (prob + ((u**2)/(2*n))+(u/sqrt(n))*sqrt((u**2)/(4*n)+prob*(1-prob)))/(1+((u**2)/n))
  return(list(ICmin1=ICmin1, ICmax1 = ICmax1,ICmin2=ICmin2, ICmax2 = ICmax2))
}




####### Test pour verifier si au moins 95% des elements entrent dans l'intervalle de confiance
simu1 <-function(N = 1000, n = 100, p = 0.5)
{
  count1<-0##Compteur pour méthode 1 
  count2<-0##Compteur pour méthode 2
  count3<-0
  u <- qnorm(0.975)
  list<-replicate(N,mean(rbinom(n,size = 1,prob = p)))##Génération de n proportions compris entre 0 et 1 suivant une loi binomiale
  
  for(x in list)
  {
    ICtest<-IC(prob=x,u = qnorm(0.975),n=n)
    if(p>ICtest$ICmin1 && p< ICtest$ICmax1)
    {##Si le x-ième événement est dans l’intervalle généré par la méthode 1
      count1<-count1+1##On incrémente 1 au compteur de la méthode 1
    }
    if(p>ICtest$ICmin2 && p<ICtest$ICmax2)
    {##Si le x-ième événement est dans l’intervalle généré par la méthode 1
      count2<-count2+1##On incrémente 1 au compteur de la méthode 1
    }
  }##Calcul du taux d’éléments dans l’intervalle généré par les deux méthodes
  return(list(pcount1=(count1/N)*100, pcount2=(count2/N)*100))
}
##Nous simulons la précision de l’intervalle avec un échantillon grandissant afin de simuler l’infini
simu1(N = 1000, n = 10, p = 0.5)


nb <- 10
res2 <- matrix(0,nrow = nb, ncol = 2)
for(i in 1:nb)
{
  s <- simu1(N = 1000, n = 10*i, p = 0.5)
  res2[i,] <- c(s$pcount1, s$pcount2)
}
ylim <- c(min(res2),max(res2))
plot(res2[,1], type = 'b', col = 1, ylim = ylim, ylab="pourcentage")
par(new = TRUE)
plot(res2[,2], type = 'b', col = 2, ylim = ylim, ylab="pourcentage")
abline(h=95, col = 3)


replicate(10,simu1(n=100))
replicate(10,simu1(n=200))
replicate(10,simu1(n=500))
replicate(10,simu1(n=1000))

######################################


simu1.2 <- function(n = 100, prob)
{
  tirage <- rbinom(n,size = 1,prob = prob)
  p1 <- mean(tirage)                 
  p2 <- 1 - p1
  
  ICmin1.p1<- IC(prob = p1)$ICmin1
  ICmax1.p1<- IC(prob = p1)$ICmax1
  ICmin2.p1 <- IC(prob = p1)$ICmin2
  ICmax2.p1 <- IC(prob = p1)$ICmax2
  
  ICmin1.p2<- IC(prob = p2)$ICmin1
  ICmax1.p2<- IC(prob = p2)$ICmax1
  ICmin2.p2 <- IC(prob = p2)$ICmin2
  ICmax2.p2 <- IC(prob = p2)$ICmax2
  
  df <- data.frame(
    Donnees = rep(c("evenement1","evenement2"),2),
    Proba = rep(c(p1,p2),2),
    method = rep(c("method1","method2"), each=2),
    ICmin = c(ICmin1.p1,ICmin1.p2,ICmin2.p1,ICmin2.p2),
    ICmax = c(ICmax1.p1,ICmax1.p2,ICmax2.p1,ICmax2.p2),
    stringsAsFactors = TRUE
  )
  return(df)
}

list.2 <- mclapply(1:N, FUN = simu1.2,
                 n = 1000,
                 prob = 0.5,
                 mc.cores = 1) #si pas sous windows, remplacer 1 par nb_cores

df.2 <- do.call(rbind, list)
df

ev1 <- df.2[df.2[,1] == "evenement1",1:5]
ev2 <- df.2[df.2[,1] == "evenement2",1:5]

pICmin.1 <- ggplot(ev1, aes(x = method, y = ICmin, color=method)) + geom_violin()
pICmin.1
pICmax.1 <- ggplot(ev1, aes(x = method, y = ICmax, color=method)) + geom_violin()
pICmax.1
pICmin.2 <- ggplot(ev2, aes(x = method, y = ICmin, color=method)) + geom_violin()
pICmin.2
pICmax.2 <- ggplot(ev2, aes(x = method, y = ICmax, color=method)) + geom_violin()
pICmax.2

grid.arrange(pICmin.1, pICmax.1, pICmin.2, pICmax.2, ncol=2, nrow=2)

######################################

simu2 <- function(i, n = 100, etat, prob)
{
  B <- prob[i]
  Tr <- 1 - B 
  
  ICmin1.B<- IC(prob = B)$ICmin1
  ICmax1.B<- IC(prob = B)$ICmax1
  ICmin2.B <- IC(prob = B)$ICmin2
  ICmax2.B <- IC(prob = B)$ICmax2
  
  ICmin1.Tr<- IC(prob = Tr)$ICmin1
  ICmax1.Tr<- IC(prob = Tr)$ICmax1
  ICmin2.Tr <- IC(prob = Tr)$ICmin2
  ICmax2.Tr <- IC(prob = Tr)$ICmax2
  
  df <- data.frame(
    Etat = etat[i],
    Candidats = rep(c("Biden","Trump"),2),
    Votes = rep(c(B,Tr),2),
    method = rep(c("method1","method2"),each=2),
    ICmin = c(ICmin1.B,ICmin1.Tr,ICmin2.B,ICmin2.Tr),
    ICmax = c(ICmax1.B,ICmax1.Tr,ICmax2.B,ICmax2.Tr),
    stringsAsFactors = TRUE
  )
  return(df)
}

list_results <- mclapply(1:nEtats, FUN = simu2,
                         n = 1000,
                         etat = table$Etat,
                         prob = table$pr.Biden,
                         mc.cores = 1) #si pas sous windows, remplacer 1 par nb_cores

df.BvsT <- do.call(rbind, list_results)
df.BvsT

#replicate au lieu de mclapply ; pas le r?sultat attendu, plus les titres 
#list_results <- replicate(nEtats, simu2(1:nEtats,n = 100, etat = Etats, prob = rpB))
#df1 <- do.call(rbind, list_results)
#df1

pB<- df.BvsT[df.BvsT[,2] == "Biden",1:6]
pT<- df.BvsT[df.BvsT[,2] == "Trump",1:6]

pICmin.B <- ggplot(pB, aes(x = method, y = ICmin, color=method)) + geom_violin()
pICmin.B
pICmin.T <- ggplot(pT, aes(x = method, y = ICmin, color=method)) + geom_violin()
pICmin.T
pICmax.B <- ggplot(pB, aes(x = method, y = ICmax, color=method)) + geom_violin()
pICmax.B
pICmax.T <- ggplot(pT, aes(x = method, y = ICmax, color=method)) + geom_violin()
pICmax.T

bp <- ggplot(df.BvsT, aes(x=Candidats, y=ICmin, color=Candidats)) +
  geom_boxplot() + 
  theme(legend.position = "none")
bp

grid.arrange(pICmin.B, pICmax.B, pICmin.T, pICmax.T, ncol=2, nrow=2)


