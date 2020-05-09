dataDir <- "E:\\Projects\\body_density_manual\\GitHub\\Private\\"
modelDir <- "E:\\Projects\\body_density_manual\\GitHub\\"

setwd(modelDir)

library(R2jags)
set.seed(0)

## load & filter data 

  # -> defines "fitName" that is used to store model output in working directory
  # store filtered data & the names of whales and dives that are included

  load(paste(dataDir,"all_whales.Rd",sep=""))

  filterBool <- abs(tab$mean.pitch)>30 & tab$dive.max.depth>100 & abs(tab$phase)>0 & is.na(tab$acceleration)==F &
    is.na(tab$mean.speed)==F & tab$r.roll>0.9  & is.na(tab$DswGG)==F
                #& tab$mean.depth>200 & tab$sonar==0 & tab$vertglide==1
  tab <- tab[filterBool & !is.na(filterBool),]       
  
  fitName <- "_f_pitch30_depth100"

  # Which numeric identifier corresponds to which whale/dive
  fit.whales <- tapply(tab$whale.id, as.numeric(as.factor(tab$whale.id)), unique)
  fit.dives <- tapply(tab$dive.all, as.numeric(as.factor(tab$dive.all)), unique)
  
  save(tab, filterBool, fit.whales, fit.dives, file=paste("data", fitName, ".Rd", sep=""))
  
# Summary of model structures

# Type of variability in parameter:
  # D = Dive-by-dive variability
  # I = Individual variability
  # G = Parameter pooled across individuals and dives

  modTab <- data.frame(
    modNum    = c(1,  2,  3,  4,  5,  6,  7,  8,  9,  10,  11,  12),
    Vair.type = c("D","D","D","G","G","G","I","I","I","G","I","D"),  # Vair term
    CdAM.type = c( "G","I","G","G","I","G","G","I","G","I", "I", "I"), # drag term
    BD.type   = c( "I","G","G","I","G","G","I","G","G","I", "I", "I")) # body density

  modelName <- paste("model(", modTab$modNum, ")", sep="")

## Loop over all 12 models

for(m in 1:length(modelName)) {

      ## Re-organise data for jags
      
        sp.data <- list(N=length(tab$duration))
        
        sp.data$a <- tab$acceleration
        sp.data$DswGG <- tab$DswGG
        sp.data$mean.speed <- tab$mean.speed
        sp.data$sin.pitch <- tab$sin.pitch
        sp.data$depth <- tab$mean.depth
        sp.data$tau <- (1/(tab$se.accel+0.001))^2   ## use measured SE for tau  
        
        # Indexing for individual variability
        if(modTab$Vair.type[m]=="I" | modTab$CdAM.type[m]=="I" | modTab$BD.type[m]=="I") {
          sp.data$NW <- length(unique(as.numeric(as.factor(tab$whale.id))))
          sp.data$whale.id <- as.numeric(as.factor(tab$whale.id))
        }
          
        # Indexing for dive-by-dive variability
        if(modTab$Vair.type[m]=="D") {
          sp.data$ND <- length(unique(as.numeric(as.factor(tab$dive.all))))
          sp.data$dive.id <- as.numeric(as.factor(tab$dive.all))
        }
      
      # Monitored parameters
      
        # Parameters in all models
        sp.params <- c("CdAM.g", "body.density.g", "Vair",
                       "compr", "a.mu", "body.density.t") # when tau is not estimated
        
        # individual CdAM
        if(modTab$CdAM.type[m]=="I") {sp.params <- c(sp.params, c("CdAM", "CdAM.var"))} 
        
        # individual body.density
        if(modTab$BD.type[m]=="I") {sp.params <- c(sp.params, c("body.density", "body.var"))}
  
        # individual or dive-by-dive Vair
        if(modTab$Vair.type[m]=="I" | modTab$Vair.type[m]=="D") {
          sp.params <- c(sp.params, c("Vair.var", "Vair.d"))}

      
      ## Load initial values
      
        source("make_inits.R")
        
        # Number of initial values for each parameter
        n.CdAM <- 1
        n.body.density <- 1
        n.body.density.g <- 1
        n.Vair <- 1
        n.Vair.var <- 1
        n.compr <- 1
        n.loc <-1
        
        if(modTab$CdAM.type[m]=="I") {n.CdAM <- sp.data$NW}
        if(modTab$BD.type[m]=="I") {n.body.density <- sp.data$NW}
        if(modTab$Vair.type[m]=="I") {n.Vair <- sp.data$NW}
        if(modTab$Vair.type[m]=="D") {n.Vair <- sp.data$ND}
      
      ## Check data and monitored parameters
      
        print(str(sp.data))
        print(sp.params)
        
      ## Fit model
      
      fit = jags(data=sp.data, inits=sp.inits, parameters.to.save=sp.params, 
                 model.file=paste(modelName[m], ".txt",sep=""),
                 n.chains=3, n.iter=24000, n.burnin=12000, 
                 n.thin=max(1, floor(3 * (24000-12000) /1000)))
      
      
      ## Save model inputs and outputs
      
        save(sp.data, sp.params, fit, filterBool, 
             file=paste(modelName[m], fitName, ".Rd", sep=""))

} # loop for different models


