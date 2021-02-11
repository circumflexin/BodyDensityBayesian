### plot results of the selected model
dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"

modelDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\SW_Models"
out_dir <- "C:\\Users\\Alec Burslem\\Dropbox\\St Andrews Body Condition PHD\\Writing\\Glide detection report\\plots"

setwd(modelDir)

setwd(modelDir)

library(lattice)
library(R2jags)
set.seed(1990)

# Select model
fitName <- "_f_pitch30_depth100_rpitch999_thinned"      # name of the fit, based on selection of "good glides"
modelName <- "model(SW12)"              # insert best model name here
load(paste("data", fitName, ".Rd", sep=""))
load(paste(modelName, fitName, ".Rd", sep=""))



#body density ####
minBD <- min(fit$BUGSoutput$sims.list$body.density)
maxBD <- max(fit$BUGSoutput$sims.list$body.density)
m3 = parse(text='m^-3')
xlabel = expression(paste("Tissue density (kg m"^"-3",")"))
#tiff(paste(out_dir,"\\body_density_indiv.tiff",sep=""), width = 5000, height = 3200, res= 800)
#par(mar = c(5, 5, 3, 5))
tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/10),
     ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior density - Individual",bty = "n")
#grid()
for(w in 1:length(fit.whales)) {
  polygon(density(fit$BUGSoutput$sims.list$body.density[,w]),col=adjustcolor("forestgreen", alpha.f=0.5), border=NA)
}
numG<-c(1)
for(w in 1:length(fit.whales)) {
  filterW<-tab$whale.id==fit.whales[w]
  numG[w]<-sum(filterW)
}
# legend("topright",legend=c("Global","Individual"), lty=1, lwd=7, col=c("blue","#00000050"), cex=0.6)
# par(new=TRUE)
# plot(density(fit$BUGSoutput$sims.list$body.density.g),cex=0.1, col=c("#00000050"), lwd=4,
#     xlim=c(minBD, maxBD+(maxBD-minBD)/10),xlab="",ylab="",yaxt = "n", main ="")
# axis(4)
# mtext("Posterior density - Global", side = 4, line = 3)

# Air volume ####
xlabel = expression(paste("Diving Gas Volume (ml kg"^"-1",")"))
#tiff(paste(out_dir,"\\air.tiff",sep=""), width = 5000, height = 3200, res= 800)
#par(mar = c(5, 5, 3, 5)) 
minVair <- min(fit$BUGSoutput$sims.list$Vair.d)
maxVair <- max(fit$BUGSoutput$sims.list$Vair.d)
tempI <-  which.min(fit$BUGSoutput$sd$Vair.d) 
test<-density(fit$BUGSoutput$sims.list$Vair.d[,tempI])
plot(1,1,xlim=c(minVair, maxVair), ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior Probability Density",bty="n")
for(w in 1:length(unique(sp.data$dive.id))) {
  polygon(density(fit$BUGSoutput$sims.list$Vair.d[,w]), col=rgb(0, 0, 1,0.1), border = NA)
}
# lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000050"), lwd=3)
# legend(x=maxVair*0.8, y=max(test$y)*0.8, "global", lty=1, col=c("blue"), lwd=3, cex=0.7)




# CDAM ####
xlabel = "Combined drag coefficient (unitless)"
#tiff(paste(out_dir,"\\CDAM.tiff",sep=""), width = 5000, height = 3200, res= 800)
#par(mar = c(5, 5, 3, 5)) 

if(length(fit$BUGSoutput$mean$CdAM)==length(fit.whales)){
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM)
  tempI <-  which.min(fit$BUGSoutput$sd$CdAM)
  test<-density(fit$BUGSoutput$sims.list$CdAM[,tempI])
  plot(1,1,xlim=c(minCdAM, maxCdAM), ylim=c(0,max(test$y)), col=NA,
       xlab=xlabel, ylab="posterior density", bty="n")
  #grid()
  for(w in 1:length(fit.whales)) {
    polygon(density(fit$BUGSoutput$sims.list$CdAM[,w]),
          col=adjustcolor("red", alpha.f=0.5), border = NA)
  }
  #legend(x=maxCdAM, y=max(test$y), legend=fit.whales, lty= 1, col=adjustcolor("red", alpha.f=0.5), cex=0.5)
  if(length(fit$BUGSoutput$sims.list$CdAM.g)>0){
    #lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
    #legend(x=maxCdAM, y=max(test$y)*0.1, "global mean", lty=1, lwd=5, col=c("red"), cex=0.5)
  }
} else {
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM.g)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM.g)
  test<-density(fit$BUGSoutput$sims.list$CdAM.g)
  plot(1,1,xlim=c(minCdAM,maxCdAM+(maxCdAM-minCdAM)/3), ylim=c(0,max(test$y)), col=NA,
       xlab="CdAM.g", ylab="posterior density",
       main=paste("mean global CdAM.g = ",
                  signif(mean(fit$BUGSoutput$sims.list$CdAM.g), digits=3), " (sd = ",
                  signif(sd(fit$BUGSoutput$sims.list$CdAM.g), digits=3), ")", sep=""))
  #lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  #legend(x=maxCdAM, y=max(test$y), "CdAM.g", lty=1, col=c("#00000040"), lwd=5, cex=0.7)
}

width = 5000
# multi panel ####
tiff(paste(out_dir,"\\all.tiff",sep=""), width = width, height=width*1.41, res= 800)
par(mfrow=c(3,1), mar = c(5,0,0,0),cex=1, oma=c(0,5,0.5,0))
minBD <- min(fit$BUGSoutput$sims.list$body.density)
maxBD <- max(fit$BUGSoutput$sims.list$body.density)
m3 = parse(text='m^-3')
xlabel = expression(paste("Tissue density (kg m"^"-3",")"))
tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/10),
     ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="",bty = "n")
for(w in 1:length(fit.whales)) {
  polygon(density(fit$BUGSoutput$sims.list$body.density[,w]),col=adjustcolor("#009061", alpha.f=0.5), border=NA)
}
numG<-c(1)
for(w in 1:length(fit.whales)) {
  filterW<-tab$whale.id==fit.whales[w]
  numG[w]<-sum(filterW)
}
# Air volume 
xlabel = expression(paste("Diving Gas Volume (ml kg"^"-1",")"))
minVair <- min(fit$BUGSoutput$sims.list$Vair.d)
maxVair <- max(fit$BUGSoutput$sims.list$Vair.d)
tempI <-  which.min(fit$BUGSoutput$sd$Vair.d) 
test<-density(fit$BUGSoutput$sims.list$Vair.d[,tempI])
plot(1,1,xlim=c(minVair, maxVair), ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior Probability \nl Density",bty="n")
for(w in 1:length(unique(sp.data$dive.id))) {
  polygon(density(fit$BUGSoutput$sims.list$Vair.d[,w]), col=adjustcolor("#004ba1", alpha.f=0.1), border = NA)
}
# lines(density(fit$BUGSoutput$sims.list$Vair), col=c("#00000050"), lwd=3)
# legend(x=maxVair*0.8, y=max(test$y)*0.8, "global", lty=1, col=c("blue"), lwd=3, cex=0.7)

# CDAM
xlabel = expression(paste("Combined Drag Term (m"^"2"," kg"^"-1",")"))
if(length(fit$BUGSoutput$mean$CdAM)==length(fit.whales)){
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM)
  tempI <-  which.min(fit$BUGSoutput$sd$CdAM)
  test<-density(fit$BUGSoutput$sims.list$CdAM[,tempI])
  plot(1,1,xlim=c(minCdAM, maxCdAM), ylim=c(0,max(test$y)), col=NA,
       xlab=xlabel, ylab="", bty="n")
  #grid()
  for(w in 1:length(fit.whales)) {
    polygon(density(fit$BUGSoutput$sims.list$CdAM[,w]),
            col=adjustcolor("#F0431D", alpha.f=0.5), border = NA)
  }
  #legend(x=maxCdAM, y=max(test$y), legend=fit.whales, lty= 1, col=adjustcolor("red", alpha.f=0.5), cex=0.5)
  if(length(fit$BUGSoutput$sims.list$CdAM.g)>0){
    #lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
    #legend(x=maxCdAM, y=max(test$y)*0.1, "global mean", lty=1, lwd=5, col=c("red"), cex=0.5)
  }
} else {
  minCdAM <- min(fit$BUGSoutput$sims.list$CdAM.g)
  maxCdAM <- max(fit$BUGSoutput$sims.list$CdAM.g)
  test<-density(fit$BUGSoutput$sims.list$CdAM.g)
  plot(1,1,xlim=c(minCdAM,maxCdAM+(maxCdAM-minCdAM)/3), ylim=c(0,max(test$y)), col=NA,
       xlab="CdAM.g", ylab="",
       main=paste("mean global CdAM.g = ",
                  signif(mean(fit$BUGSoutput$sims.list$CdAM.g), digits=3), " (sd = ",
                  signif(sd(fit$BUGSoutput$sims.list$CdAM.g), digits=3), ")", sep=""))
  #lines(density(fit$BUGSoutput$sims.list$CdAM.g), col=c("#00000040"), lwd=5)
  #legend(x=maxCdAM, y=max(test$y), "CdAM.g", lty=1, col=c("#00000040"), lwd=5, cex=0.7)
}

mtext("Posterior Probability Density", side=2, line=3, outer=TRUE)
while (!is.null(dev.list()))  dev.off()

# ratio:density plot ####

val_tab = data.frame(read.csv("D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Internal validation\\validation.csv", header = TRUE))

require(ggplot2)
library(grid)
library(ggstance)

val_tab$Year =  factor(val_tab$Year)
plot(density(val_tab$Density_m))
plot(density(val_tab$Rat_ov_mean))

cor(val_tab$Density_m,val_tab$Rat_ov_mean,method ="spearman")
cor.test(val_tab$Density_m,val_tab$Rat_ov_mean,method ="spearman",exact=F) 

cor(val_tab$Density_m,val_tab$Rat_ov_mean,method ="pearson")
cor.test(val_tab$Density_m,val_tab$Rat_ov_mean,method ="pearson") 


grob = grobTree(textGrob("Spearman's rho = -0.58", x=0.45,  y=0.85, hjust=0,
gp=gpar( fontsize=13, fontface="italic")))

ggplot(data = val_tab) +
  geom_point(aes(x=Density_m, y =Rat_ov_mean,color = Year)) +
  geom_linerange(aes(x=Density_m, y =Rat_ov_mean, ymax = Rat_ov_mean+Rat_ov_STD, ymin = Rat_ov_mean-Rat_ov_STD, color = Year)) +
  geom_linerangeh(aes(x=Density_m, y =Rat_ov_mean,xmax = Density_U95, xmin = Density_L95,color = Year)) +
  labs (x = expression(paste("Tissue density (kg m"^"-3",")")), y ="Glide % (ascent - descent)") +
  theme_classic() +
  theme(legend.position = "right") +
  #annotation_custom(grob)
  
  ggsave(paste(out_dir,"\\ratio.png",sep=""),width=5,height=5)
getwd()  





