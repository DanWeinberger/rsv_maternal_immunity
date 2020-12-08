##This code generates a range of values for key parameters, representing N individuals,
#and then estimate age until which the individuals are protected
#This can then be compared to the age distribution of hospitalizations/deaths to determine
#what percent of people are under a protective threshold

source('wane_func.R')

N=2000
ds <- as.data.frame(matrix(NA, nrow=N, ncol=1))
#Ranges here to capture biological variability. Should switch to different distribution 
#that more accurately captures the distribution of values
#Could also incorporate covariance among key variables when simulating
ds$Set.ab_m0 <- runif (N, 30,35)
ds$Set.g_age_vax <- runif (N, 28*7,32*7) 
ds$Set.g_age_birth <- runif(N ,34*7, 41*7) # need to restrict so age_vax>age_birth
ds$Set.half_life <- runif(N,38,43) 


#Fixed values (could also put distributions on these to capture uncertainty)
ds$Set.eff <- 5 
ds$Set.protective_thresh <- 40 #
ds$id <- 1:nrow(ds)

#Call wane_func, subbing in value of each row one by one
spl.ds <- split(ds, ds$id)
age.protect<- sapply(spl.ds, function(x){
  z <- wane_func(ab_m0=x[,'Set.ab_m0'], g_age_vax=x[,'Set.g_age_vax'], g_age_birth=x[,'Set.g_age_birth'], half_life=x[,'Set.half_life'], eff=x[,'Set.eff'], protective_thresh=x[,'Set.protective_thresh'])
  return(z) 
})
hist(age.protect)

