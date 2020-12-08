
#ab_m0: Initial Maternal Ab (33) ab_m0 
#g_age_vax  #gestational age at vax (days): default 30 weeks (30*7 dats)
#g_age_birth  #gestational age in days at birth
#half_life Ab halflife in child (days)
#eff  #efficiency of transfer from mother to child 5 or 10
#protective_thresh  #protective threshold (40)

wane_func <- function(ab_m0=33, g_age_vax=30*7, g_age_birth=38*7, half_life=41, eff=5, protective_thresh=40) {

  #Ab levels in mom, indexed by time since vaccination
  tvax <- g_age_birth - g_age_vax
  ab_m <- rep(ab_m0, length(tvax))
  ab_m[tvax>=7 & tvax<=21] <- ab_m0* exp((tvax[tvax>=7 & tvax<=21]-7)*log(eff)/14)
  ab_m[tvax>21] <- ab_m0*eff
  
  #Mother to fetus transfer ratio if vaccinated at g_age_vax
  r_t= exp(-4.97+ 0.13*g_age_birth/7)
  
  #Ab in cord blood, by time since vaccination:
  Ab_Cb = ab_m * r_t
  
  t_birth <- 0:180 #time since birth
  Ab_t <- Ab_Cb*0.5^(t_birth/half_life)
  
  age_unprotected <- t_birth[min(which(Ab_t<protective_thresh))]
}
