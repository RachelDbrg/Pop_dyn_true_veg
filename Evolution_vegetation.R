# Makes the vegetation vary if a perturbation occurs, 
# see with auto_gen_PP_delta_related_parameters

evol_vg <- function(t,data,PP){
  
  
  # Need to specify where to find the columns
  # where the values are stored
  kUstable <- data$kUstable
  kUpeak <- data$kUpeak
  t_kstable <- data$t_kstable
  t_kpeak <- data$t_kpeak
  t_low <- data$t_low
  t_perturb <- data$t_perturb
  kUcoeff1 <- data$kUcoeff1
  kUcoeff2 <- data$kUcoeff2
  chi_M <- data$chi_M
  chi_N <- data$chi_N
  kVnorm <- data$kVnorm
  kVlow <- data$kVlow
  
  
  
  # Makes the carrying capacity of vegetation vary with time
  if (t_perturb <= t & t < t_low) {
    k_U = kUlow
    k_V = kVlow
  } else if (t_low <= t & t < t_kpeak) {
    k_U = kUpeak
    k_V = kVnorm
  } else if (t_kpeak <= t & t <= t_kstable) {
    k_U = kUcoeff1*t + kUcoeff2
    k_V = kVnorm
  } else {
    k_U = kUstable
    k_V = kVnorm
  }
  

  res <- c(k_U,k_V)
  
  
}

