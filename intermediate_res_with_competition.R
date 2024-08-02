# Script computes that intermediary results that are computed at each
# time step based on the current relative species abundances


intermediate_res <-  function(y,parms){
  
  ## --- Load current species densities --- 
  
  # Load the `y` vector, that contains all initial values of the species 
  # Species should be in the same order that in yini
  
  V  <- y[1]
  #print(paste0("V", V))
  U  <- y[2]
  # print(paste0("U", U))
  
  
  # Verifies that species density are not negative, which could not crash 
  # the model and produce absurd results. 
  
  if (U <= 0){
    U = 0}
  else{
    U  <- U
  }
  # print(paste0("U", U))
  
  Na <- y[3]
  
  if (Na <= 0){
    Na = 0}
  else{
    Na  <- Na
  }
  
  
  Nj <- y[4]
  
  
  if (Nj <= 0){
    Nj = 0}
  else{
    Nj  <- Nj
  }
  
  
  Ma <- y[5]
  
  
  
  if (Ma < 0){
    Ma = 0}
  else{
    Ma = Ma
  }
  
  
  
  Mj <- y[6]
  
  if (Mj <= 0){
    Mj = 0}
  else{
    Mj = Mj
  }
  
 
  
  Ca <- y[7]
  
  if (Ca <= 0){
    Ca = 0}
  else{
    Ca  <- Ca
  }
  
  
  Cj <- y[8]
  
  if (Cj <= 0){
    Cj = 0}
  else{
    Cj  <- Cj
  }
  
  Pa  <- y[9]
  
  
  if (Pa <= 0){
    Pa = 0}
  else{
    Pa = Pa
  }
  
  
  Pj  <- y[10]
  
  if (Pj <= 0){
    Pj = 0}
  else{
    Pj = Pj
  }
  
  
  Qa <- y[11]
  
  if (Qa <= 0){
    Qa = 0}
  else{
    Qa  <- Qa
  }
  
  
  Qj <- y[12]
  
  if (Qj <= 0){
    Qj = 0}
  else{
    Qj  <- Qj
  }
  
  
  # --- Computes total densities of species 
  # Moose
  M_tot <- Ma+Mj*epsi_Maj
  # Caribou
  N_tot <- Na+Nj*epsi_Naj
  # Deer
  C_tot <- Ca+Cj*epsi_Caj
  
  # Other prey
  Q_tot <- Qa+Qj
  
 
  # All prey together (weigthed in fucntion of their species)
  proies_tot <- M_tot+N_tot* epsi_MN + C_tot*epsi_MC + Q_tot*epsi_MQ + 2.2204e-16
  
  
  
  # --- Predator preferences based on relative abundance
  
  # Moose
  pref_P_Ma_i <- Ma / proies_tot
  pref_P_Mj_i <- Mj / proies_tot
  
  # Total moose preference
  pref_P_M <- pref_P_Ma_i + pref_P_Mj_i
  
  # Force predation of 10% on juveniles
  pref_P_Mj <- 0.1 * pref_P_M
  pref_P_Ma <- 0.9 * pref_P_M
  
  
  
  # Caribou
  pref_P_Nj_i <- Nj / proies_tot
  pref_P_Na_i <- Na / proies_tot
  
  # Total caribou preference
  pref_P_N <- pref_P_Nj_i + pref_P_Na_i
  
  # Force predation of 10% on juveniles
  pref_P_Nj <- 0.1 * pref_P_N
  pref_P_Na <- 0.9 * pref_P_N


  
  # Deer
  pref_P_Cj_i <- Cj / proies_tot
  pref_P_Ca_i <- Ca / proies_tot
  
  # Total deer preference
  pref_P_C <- pref_P_Cj_i + pref_P_Ca_i
  
  # Force predation of 10% on juveniles
  pref_P_Cj <- 0.1 * pref_P_C
  pref_P_Ca <- 0.9 * pref_P_C
  
  
  # Other prey
  pref_P_Qj <- Qj / proies_tot
  pref_P_Qa <- Qa / proies_tot
  
  # Total other prey preference
  pref_P_Q <- pref_P_Qj + pref_P_Qa
  
  # Force predation of 10% on juveniles
  pref_P_Qj <- 0.1 * pref_P_Q
  pref_P_Qa <- 0.9 * pref_P_Q
  
  # ==============================================================================  
  
  # Parameters de preferences en fonction de l'abondance et de la masse
  
  # denom_pref = w_Mj * Mj + w_Ma * Ma + w_Nj * Nj + w_Na * Na + w_Cj * Cj +
  #     w_Ca * Ca + w_Qa * Qa + + w_Qj * Qj
  # 
  #   pref_P_Mj = (w_Mj * Mj)/denom_pref
  #   pref_P_Ma = (w_Ma * Ma)/denom_pref
  #   pref_P_Nj = (w_Nj * Nj)/denom_pref
  #   pref_P_Na = (w_Na * Na)/denom_pref
  #   pref_P_Cj = (w_Cj * Cj)/denom_pref
  #   pref_P_Ca = (w_Ca * Ca)/denom_pref
  #   pref_P_Qa = (w_Qa * Qa)/denom_pref
  #   pref_P_Qj = (w_Qj * Qj)/denom_pref
  
  # ==============================================================================
  
  
  
  # ---- Functional response ---------------------------------------------------

  
  # ---- Browsers-vegetation ---------------------------------------------------
  # Functional response of moose on deciduous (in kg/year per moose individual)
  rep_fonc_MU = ((a_M * U)/(1 + a_M * h_UM * U))
  
  # Total response of moose on deciduous (total biomass eaten by the moose population)
  rep_totale_MU = ((a_M * U * (Ma+Mj))/(1 + a_M * h_UM * U))
  
  # Functional response of moose on deciduous (in kg/year per moose individual)
  rep_fonc_CU = ((a_C * U)/(1 + a_C * h_UC * U))
  
  # Total response of moose on deciduous (total biomass eaten by the moose population)
  rep_totale_CU = ((a_C * U * (Ca+Cj))/(1 + a_C * h_UC * U))
  
  
  ## ---- Computes denominator of predator's functional response
  
  den_rfonc_P <- 1 + a_P *
    ((pref_P_Mj * h_P_Mj * Mj +
        pref_P_Ma * h_P_Ma * Ma)+
       (pref_P_Nj * h_P_Nj * Nj +
          pref_P_Na * h_P_Na * Na) + 
       (pref_P_Cj * h_P_Cj * Cj +
          pref_P_Ca * h_P_Ca * Ca)+
       (pref_P_Qj * h_P_Cj * Qj +
          pref_P_Qa * h_P_Ca * Qa))
  
  

  # den_rfonc_P = max(den_rfonc_P, 1e-06)
  
  
  ## ---- Species functional response 
  # -- Moose
  rfonc_P_Ma <- (a_P * pref_P_Ma * Ma) / den_rfonc_P
  rfonc_P_Mj <- (a_P * pref_P_Mj * Mj) / den_rfonc_P 
 
  # -- Caribou
  rfonc_P_Na <- (a_P * pref_P_Na * Na) / den_rfonc_P
  rfonc_P_Nj <- (a_P * pref_P_Nj * Nj) / den_rfonc_P 
  
  # Deer
  rfonc_P_Cj <- (a_P * pref_P_Cj * Cj) / den_rfonc_P
  rfonc_P_Ca <- (a_P * pref_P_Ca * Ca) / den_rfonc_P
  
  #BEAVER
  rfonc_P_Qa = (a_P * pref_P_Qa * Qa) / den_rfonc_P
  rfonc_P_Qj = (a_P * pref_P_Qj * Qj) / den_rfonc_P
   
  
  # -- Total functional response
  rfonc_tot = (rfonc_P_Ma + rfonc_P_Mj * epsi_Maj +
                 epsi_MN * (rfonc_P_Na +
                              epsi_Naj * rfonc_P_Nj)+
                 epsi_MC * (rfonc_P_Ca + 
                              epsi_Caj * rfonc_P_Cj)+
                 epsi_MQ * (rfonc_P_Qa + 
                              epsi_Qaj * rfonc_P_Qj))



  
  # ---- Predator's energy intake based on prey consumption --------------------

  NRJ_intake = (rfonc_P_Ma + rfonc_P_Mj * (epsi_Maj)
                + epsi_MN * (rfonc_P_Na + rfonc_P_Nj *epsi_Naj)+
                  epsi_MC * (rfonc_P_Ca + rfonc_P_Cj *epsi_Caj)+
                  epsi_MQ * (rfonc_P_Qa + rfonc_P_Qj *epsi_Qaj))
  
  
  # Energy that can be invested into new individuals
  surplus_NRJ = NRJ_intake-mu_P
  

  # Add of a condition that states that if wolf doesn't
  # have enough energy to fulfill their basal needs, 
  # then they won't put NRJ in reproduction (so chi_P = 0)
  
  if (surplus_NRJ <= 0 ){
    chi_P = 0
    surplus_NRJ = 0}
  else{
    chi_P = 0.236
  }
  
  
  croissance_loup = chi_P * surplus_NRJ * Pa
  
  
  # --- Predator's carrying capacity -------------------------------------------
  
  
  # Following Johnson2019 and Kuzyk2014
  ungulate_biomass  = 6*(Ma + Mj*epsi_Maj) + 2*(Na + Nj*epsi_Naj) + 1*(Ca + Cj*epsi_Caj) + 0.25*(Qa+Qj)
  
  
  k_P_Johnson = 5.4 * ungulate_biomass - 0.166*ungulate_biomass^2 
  k_P = k_P_Johnson/1000
  
  
  # Doesn't allow k_P to be < 0 (otherwise, leads to growth
  # of the population even if there is no preys)
  
  if (k_P <= 0.0001){
    k_P = 0.000000001

  } else if (k_P > 0.01){
    k_P = 0.01


  } else{
    k_P = k_P
  }


  # # Conversion rates based on weight
  # epsi_MN = w_Na / w_Ma
  # epsi_Maj = w_Mj/w_Ma
  # epsi_Naj = w_Nj/w_Na
  # epsi_WC = 0.934
  # epsi_MC = w_Ca / w_Ma
  # epsi_Caj = w_Cj/w_Ca
  # epsi_Caj = w_Cj/w_Ca
  # epsi_MQ = w_Qa / w_Ma
    
  
  
  # --- Prey supplementary energy
  # -- Moose
 Ma_supplementary_NRJ <- (((a_M * e_UM * U)/(1 + a_M * h_UM * U)) - mu_M)
  
  
  # Bound so this value can never be negative
  if (Ma_supplementary_NRJ <= 0 ){
    Ma_supplementary_NRJ = 0
  }
  else{
    Ma_supplementary_NRJ <- Ma_supplementary_NRJ
  }
  
  
 # -- Caribou
  Na_supplementary_NRJ <- ((a_N * e_VN * V)/(1 + a_N * h_VN * V) - mu_N)
  
  if (Na_supplementary_NRJ <= 0 ){
    Na_supplementary_NRJ = 0
  }
  else{
    Na_supplementary_NRJ <- Na_supplementary_NRJ
  }
  
  
  # -- Deer
  Ca_supplementary_NRJ <- ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C)

  if (Ca_supplementary_NRJ <= 0 ){
    Ca_supplementary_NRJ = 0
  }
  else{
    Ca_supplementary_NRJ <- Ca_supplementary_NRJ
  }
  
  
  # -- Beaver
  Qa_supplementary_NRJ <- ((a_Q * e_UC * U)/(1 + a_Q * h_UQ * U) - mu_Q)
  
  if (Qa_supplementary_NRJ <= 0 ){
    Qa_supplementary_NRJ = 0
  }
  else{
    Qa_supplementary_NRJ <- Qa_supplementary_NRJ
  }
  
  
  
  # Return all these variables, that will be used in the species equations.
  # The structure of this vector is: all functional response in the first 
  # list, preferences parameters, then every other parameter
  
  return(list(c(rfonc_P_Mj,
                rfonc_P_Ma,
                rfonc_P_Nj,
                rfonc_P_Na,
                rfonc_P_Cj,
                rfonc_P_Ca,
                rfonc_tot,
                rfonc_P_Qj,
                rfonc_P_Qa),
              c(pref_P_Mj,
                pref_P_Ma,
                pref_P_Nj,
                pref_P_Na,
                pref_P_Cj,
                pref_P_Ca,
                pref_P_Qa,
                pref_P_Qj),
              den_rfonc_P,
              k_P,
              M_tot,
              N_tot,
              C_tot,
              proies_tot,
              chi_P,
              surplus_NRJ,
              rep_fonc_MU,
              croissance_loup,
              mu_P,
              rep_totale_MU,
              rep_fonc_CU,
              rep_totale_CU,
              Ma_supplementary_NRJ,
              Na_supplementary_NRJ,
              Ca_supplementary_NRJ,
              k_P_Johnson,
              ungulate_biomass,
              Qa_supplementary_NRJ,
              Q_tot
              ))
  
}


