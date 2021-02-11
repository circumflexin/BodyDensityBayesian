### plot results of the selected model
dataDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"
modelDir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\SW_Models"
out_dir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\outputs"

setwd(modelDir)

library(lattice)
library(R2jags)
set.seed(0)

# Select model

fitName <- "_f_pitch30_depth100_rpitch999_thinned"      # name of the fit, based on selection of "good glides"
modelName <- "model(SW12)"              # insert best model name here

# Load modelled data and model results

# tab = filtered data frame of glides
# filterBool = true/false vector that identifies included data from original data (all_whales.Rd)
# fit.whales = list of included whales
# fit.dives = list of included dives

load(paste("data", fitName, ".Rd", sep=""))
load(paste(modelName, fitName, ".Rd", sep=""))


############################### COLOURS

whaleCol = seq(1:length(fit.whales))

rbPal<-colorRampPalette(c("purple","blue", "deepskyblue", "cyan", "lawngreen",
                          "yellow", "orange", "orangered", "darkred"))

pitchBreaks <- seq(-90, 90, 10)
mycols <- rbPal(length(pitchBreaks))[as.numeric(cut(tab$mean.pitch,breaks = pitchBreaks))]

#############################

#pdf(paste("plot_best_", modelName, ".pdf", sep=""), width=6,height=5)
#options(graphics.record=TRUE)

minBD <- min(fit$BUGSoutput$sims.list$body.density)
maxBD <- max(fit$BUGSoutput$sims.list$body.density)

m3 = parse(text='m^-3')
xlabel = expression(paste("Tissue density (kg m"^"-3",")"))

## body density
tiff("D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\Outputs\\bd_leg.tiff", width = 3200, height = 3200, res= 800)
tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/2),
     ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior density")
#grid()
for(w in 1:length(fit.whales)) {
  lines(density(fit$BUGSoutput$sims.list$body.density[,w]),col=whaleCol[w])
}
numG<-c(1)
for(w in 1:length(fit.whales)) {
  filterW<-tab$whale.id==fit.whales[w]
  numG[w]<-sum(filterW)
}
legend("topright", title = 'Individual means', 
       legend=paste(fit.whales[1:length(fit.whales)], sep=""),
       lty=1, col=whaleCol, cex=0.6)

par(new = T)
lines(density(fit$BUGSoutput$sims.list$body.density.g), col=c("#00000050"), lwd=4)
legend("bottomright", "Global mean", lty=1, lwd=4, col=c("#00000050"), cex=0.6)

## body density
tiff("D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\Outputs\\bd_noleg.tiff", width = 3200, height = 3200, res= 800)
tempI <-  which.min(fit$BUGSoutput$sd$body.density) # set y-axis maximum to most narrow posterior density
test<-density(fit$BUGSoutput$sims.list$body.density[,tempI])
plot(1,1,xlim=c(minBD, maxBD+(maxBD-minBD)/10),
     ylim=c(0,max(test$y)), col=NA,
     xlab=xlabel, ylab="Posterior density")
#grid()
for(w in 1:length(fit.whales)) {
  lines(density(fit$BUGSoutput$sims.list$body.density[,w]),col="blue")
}
numG<-c(1)
for(w in 1:length(fit.whales)) {
  filterW<-tab$whale.id==fit.whales[w]
  numG[w]<-sum(filterW)
}
#legend("topright",legend=c("Global","Individual"), lty=1, lwd=7, col=c("blue","#00000050"), cex=0.6)
lines(density(fit$BUGSoutput$sims.list$body.density.g),cex=0.1, col=c("#00000050"), lwd=4,)
#legend("bottomright","Global mean", lty=1, lwd=7, col=c("#00000050"), cex=0.6)
dev.off()


    
    
    
