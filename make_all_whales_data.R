upstream_dir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Glide detection\\Summaries\\"
downstream_dir <- "D:\\Analysis\\SW sonar chapter\\Body Density Analysis\\Bayesian_BC\\data\\"

setwd(downstream_dir)

whales <- c("sw08_152a","sw09_141a","sw09_142a","sw09_160a","sw10_147a","sw10_149a","sw10_150a","SW16_126a","SW16_131a","SW16_134b","SW16_136a","sw17_179a","sw17_180a","sw17_182a","sw17_182b","sw17_184a","sw17_186a","sw17_186b","sw17_188a","sw17_191a","SW19_241a","SW19_241b","SW19_244a","SW19_245a","SW19_248a","SW19_250b","SW19_253b","SW19_254a","SW19_255b","SW19_255c","SW19_259a" ,"SW19_259b")
prevMax <- 0

for(w in 1:length(whales)) {
  
  tab <- read.csv(paste(upstream_dir, whales[w], ".csv",sep=""))  
  
  names(tab) <- c(
    
    "start.pt",               # Start point of the glide  
    "end.pt",                 # End point of the glide     
    "duration",               # duration (s) of glide (i.e. all sub-glide has duration of 5s)     
    "mean.depth",             # Mean depth (m) of the glide     
    "delta.depth",            # Total depth change durin a 5-s glide
    "mean.speed",             # Mean swim speed during the glide (m/s)    
    "mean.pitch",             # Mean pitch angle (deg) during the glide (calculated using general statisitics)     
    "sin.pitch",              # Sine of mean pitch      
    "sd.pitch",               # SD of pitch angle (calculated using general statistics)     
    "mean.temp",              # Mean ambient temperature during the glide     
    "DswGG",                  # density of seawater at the glide depth   
    "acceleration",           # Acceleration during the glide (i.e. slope of swim speed vs time)     
    "R",                      # R values for the regression swim speed vs time     
    "se.accel",               # SE of the gradient (acceleration)
    "phase",                  # 0 for bottom phase, -1 for descent phase, 1 for asscent phase     
    "dive",                   # Number of dive in which the glide occurred     
    "dive.max.depth",         # Maximum dive depth (m) of the dive     
    "dive.duration",          # Dive duration (s)     
    "mean.pitch.c",           # Mean pitch angle (deg) during the glide (calculated using circular statistics)     
    "r.pitch",                # Measure of concentration (r) of  pitch angles in 5s     
    "mean.roll",              # Mean roll angle (deg) during the glide (calculated using circular statistics)     
    "r.roll",                 # Measure of concentration (r) of  roll angles in 5s     
    "mean.heading",           # Mean heading angle (deg) during the glide (calculated using circular statistics)     
    "r.heading",              # Measure of concentration (r) of  heading angles in 5s
    "sg.index"                # Index of the subglide within the larger glide 
  )
  
  tab <- tab[is.na(tab$duration)==F,]
  tab$whale.id <- whales[w]       # whale.id
  tab$dive.all <- as.numeric(as.factor(tab$dive)) + prevMax # index number for all dives (required for jags model)
  
  if(w>1) {
    tab_new <- rbind(tab_new, tab)
  } else {
    tab_new <- tab
  }
  prevMax <- max(tab_new$dive.all)
  
}

tab <- tab_new

save(tab, file=paste(downstream_dir, "all_whales.Rd", sep=""))


