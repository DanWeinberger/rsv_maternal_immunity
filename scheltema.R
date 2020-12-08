ab_m0 <- 33 #Initial Maternal Ab (33)
g_age_vax <- 30*7 #gestational age at vax (weeks): default 30 weeks
g_age_birth <- 38*7 #gestational age in days at birth
half_life = 41 #Ab halflife in child (days)
eff= 5 #efficiency of transfer from mother to child 5 or 10
protective_thresh = 40 #protective threshold (40)

#Ab levels in mom, indexed by time since vaccination
tvax <- g_age_birth - g_age_vax
ab_m <- rep(ab_m0, length(tvax))
ab_m[tvax>=7 & tvax<=21] <- ab_m0* exp((tvax[tvax>=7 & tvax<=21]-7)*log(eff)/14)
ab_m[tvax>21] <- ab_m0*eff
plot(ab_m)

#Mother to fetus transfer ratio if vaccinated at g_age_vax
r_t= exp(-4.97+ 0.13*g_age_birth/7)

#Ab in cord blood, by time since vaccination:
Ab_Cb = ab_m * r_t

t_birth <- 0:180 #time since birth
Ab_t <- Ab_Cb*0.5^(t_birth/half_life)

age_unprotected <- t_birth[min(which(Ab_t<protective_thresh))]

plot(Ab_t)
abline(h=protective_thresh)
abline(v=age_unprotected)

#Fit age distribution in RESCEU and then simulate from that
# Age distributon of cases #Increase shape to shift distribution; scale to change spread on x axis
#could also use a gamma
set.seed(123)
fw <- readRDS('resceu.weibull.rds')

age_dist_cases <- 7*rweibull(n=1000, scale=fw$estimate['scale'], shape=fw$estimate['shape'])
hist(age_dist_cases, breaks=100)
abline(v=age_unprotected, col='red')

#What proportion of the cases are preventable?
mean(age_dist_cases < age_unprotected)


