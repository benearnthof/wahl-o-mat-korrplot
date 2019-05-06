library(corrplot)
library(RColorBrewer)
# data ==== 
parties <- c("cdu","sdp","gruen","linke","afd","fdp","freiewaehler","piraten",
             "npd","familienpart","oedp","diepartei",
             "mlpd","dritteweg","diegrauen","diehumanisten","volt", "ludi", "bene")
data <- as.data.frame(matrix(NA, 38, length(parties)))
names(data) <- parties
# data copied from www.wahl-o-mat.de/europawahl2019/ ####
data$cdu <- c(F,NA,T,NA,F,F,T,NA,T,F,T,T,F,T,NA,F,NA,T,T,F,T,T,NA,NA,T,F,T,F,NA,F,NA,T,T,T,T,F,F,F)
data$sdp <- c(T,T,T,T,F,T,T,F,F,T,T,NA,T,F,T,F,F,T,T,F,T,T,T,T,NA,T,F,F,F,T,F,T,T,T,T,F,T,T)
data$gruen <- c(T,T,F,T,F,T,T,F,F,T,T,F,T,F,T,F,F,T,T,F,F,T,T,T,NA,T,F,F,F,F,T,T,T,T,T,F,T,T)
data$linke <- c(T,T,F,T,F,T,T,F,F,F,T,F,T,F,T,F,F,NA,T,F,NA,F,T,T,F,T,F,T,F,T,T,T,F,T,T,F,F,T)
data$afd <- c(F,F,F,F,T,F,NA,T,T,F,F,NA,F,F,F,T,T,F,F,T,T,F,F,F,T,F,T,F,T,T,NA,NA,F,F,F,T,F,F)
data$fdp <- c(NA,F,T,F,F,NA,F,T,T,T,NA,F,F,T,F,F,T,T,T,NA,T,T,F,NA,T,F,F,F,F,F,F,T,T,T,NA,F,T,F)
data$freiewaehler <- c(T,T,T,NA,F,F,T,F,T,F,T,T,F,T,F,F,F,T,T,F,F,T,F,NA,T,T,T,F,T,F,F,T,F,T,F,F,F,F)
data$piraten <- c(T,T,F,T,F,T,T,F,F,T,T,F,T,F,F,F,F,T,T,F,T,F,T,T,F,F,F,F,F,F,T,T,T,T,T,F,T,T)
data$npd <- c(F,F,F,T,T,F,T,F,T,F,F,T,F,F,F,T,NA,F,F,T,F,T,F,F,F,F,T,F,T,T,T,F,F,F,T,T,F,F)
data$familienpart <- c(T,NA,T,NA,F,T,T,F,T,F,T,T,T,F,NA,F,F,F,T,F,NA,T,T,F,T,T,T,F,F,T,T,T,T,T,T,F,F,T)
data$oedp <- c(T,T,F,T,F,T,T,F,NA,F,T,F,T,NA,F,F,F,NA,T,F,F,T,T,NA,T,T,NA,F,F,T,T,T,F,T,T,F,F,T)
data$diepartei <- c(T,T,F,T,F,T,T,F,T,F,T,F,T,F,T,F,F,T,T,F,F,F,T,T,T,T,F,F,F,T,T,T,T,T,T,F,T,T)
data$mlpd <- c(T,T,F,NA,NA,T,T,F,F,T,NA,F,T,F,NA,NA,F,F,T,F,T,F,T,T,F,T,F,T,F,T,T,T,NA,F,T,F,NA,T)
data$dritteweg <- c(F,F,F,T,T,F,T,F,T,F,F,F,T,F,F,T,NA,F,F,T,F,F,F,F,F,T,F,T,T,T,NA,F,F,F,T,T,F,F)
data$diegrauen <- c(T,T,T,T,F,F,T,F,NA,F,NA,T,T,T,T,F,F,F,T,T,T,T,T,T,T,T,NA,NA,T,T,T,NA,NA,T,T,T,F,T)
data$diehumanisten <- c(F,T,T,F,F,F,NA,T,F,T,T,F,T,NA,F,F,T,T,T,F,T,T,T,T,T,F,F,F,F,F,F,T,T,T,NA,F,T,T)
data$volt <- c(F,T,T,T,F,T,T,T,F,T,T,F,T,NA,T,F,T,T,T,F,T,T,T,T,F,T,F,F,F,F,T,T,T,T,T,F,T,T)
data$ludi <- c(F,T,T,T, F,F,T,F, T,F,T,T, T,T,F,F, T,T,T,F, F,T,T,T, T,NA,F,F, F,F,T,T, T,T,T,T, F,F)
data$bene <- c(F,T,F,NA,F,F,T,T,F,F,F,F,T,T,F,F,T,F,T,NA,T,F,T,T,T,T,F,NA,NA,F,T,NA,F,T,T,F,NA,F)

parties.label <- c("CDU","SPD","Grüne","Linke","AFD","FDP","Freie Wähler","Piraten",
                   "NPD","Familien Partei","ÖDP","Die Partei",
                   "MLPD","Der Dritte Weg","Die Grauen","Humanisten","Volt", "Ludi", "Bene")
names(data) <- parties.label

data[data==T] <- 1
data[data==F] <- -1
data[is.na(data)] <- 0

# correlation plot ====

data.corr <- cor(data, method = c("spearman"))
par(oma=c(0,0,0,0))
corrplot(data.corr, tl.col="black", tl.cex=0.8, mar=c(0.5,0,3,0))
title("Correlationmatrix: Wahl-O-Mat answers of the Parties EU elections 2019", 
      cex.main=0.7)

# attempting to sort the matrix 
require(psych)
data.corr.sorted <- mat.sort(data.corr, f = NULL)
corrplot(data.corr.sorted, tl.col="black", tl.cex=0.8, mar=c(0.5,0,3,0), method = "pie", col=brewer.pal(n=8, name="RdYlBu"))
title("Correlationmatrix: Wahl-O-Mat answers of the Parties EU elections 2019", 
      cex.main=0.7)


# prime component analysis ====

data <- t(data)
pca <- prcomp(data,scale=T)
importance <- round(100*summary(pca)$importance[2,], 1)

# basistransformation
data.projected <- scale(data, pca$center, pca$scale) %*% pca$rotation

# mirror data in order to get the right leaning parties to the right side of the plot
data.projected[,1] <- -data.projected[,1]

# plot
xlimz <- range(data.projected[,1]) + c(-0.5,0.5)
ylimz <- range(data.projected[,2]) + c(-0.5,0.5)

cex.t <- 0.8
lwd.t <- 1.0
par(oma=c(0,0,0,0),mar=c(2.5,2.1,1,1))

# empty plot with grid
plot(xlimz, ylimz, axes=F, xlab="", ylab="", type="n")
grid()
abline(h=0, v=0, col="grey")

# add data points to plot
points(data.projected[,1],data.projected[,2], pch=16, col="darkblue")
text(data.projected[,1], data.projected[,2], labels=parties.label, font=2, cex=0.7, pos=3, offset=0.4)

# axis and title
axis(1,lwd=lwd.t,labels=FALSE)
mtext(side=1,line=0.6,at=axTicks(1),axTicks(1),cex=cex.t,las=1)
# bold and variables in title
x.label <- paste("(",importance[1],"% of Variance)",sep="")
mtext(side=1,substitute(bold("PCA1")~italic(x.label), list(x.label=x.label)),line=1.5,cex=cex.t)

axis(2,lwd=lwd.t,labels=FALSE)
mtext(side=2,line=0.6,at=axTicks(2),axTicks(2),cex=cex.t,las=1)
y.label <- paste("(",importance[2],"% of Variance)",sep="")
mtext(side=2,substitute(bold("PCA2")~italic(y.label), list(y.label=y.label)),line=1.1,cex=cex.t)

title("PCA analysis: EU elections 2019 - Wahl-O-Mat answers", 
      cex.main=0.8)
box()

