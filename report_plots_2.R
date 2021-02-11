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

tab = tab[tab$whale.id=="SW19_244a",]

plot(tab$mean.depth,tab$acceleration)



velocity = tab$mean.speed # m/s
dens_sw = tab$DswGG # kg/m^3
dens_air = 1.225 # kg/m^3
depth = tab$mean.depth # m 
g = 9.80665 # acceleration due to gravity m/s/s
Vair = 25*10^-6 #m^3/kg
drag_term <- 9.6*10^-6# 
pitch_deg = tab$mean.pitch
pitch = pitch_deg * (pi/180)
dens_tis = 1029.87 # kg/m^3
Vair = Vair*mass
#pitch <- (runif(n,-90,90))
compress =  0.392 # proportion×109 per Pascale

dens_tis_d = dens_tis/(1-compress * (1 + 0.1 * depth) * 101325 * 10^-9)
acc_drag = -0.5*drag_term*dens_sw*velocity^2
acc_air = (Vair/mass)*g*sin(pitch)*(dens_sw-dens_air*(1+0.1*depth))/(1+0.1*depth)
acc_tis = ((dens_sw/dens_tis_d)-1)*g*sin(pitch)

acc_boy = acc_tis+acc_air
acc_net = acc_tis+acc_air+acc_drag

plot(tab$mean.depth)

