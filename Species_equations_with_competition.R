
equa_diff_sp <- function(t,y, Parms){
  # On decrit dans le vecteur `initial_values` la place
  # de chaque variable. Il faut toujours garder le même ordre.
  # En gros, on dit que pour trouver la valeur de V au temps 
  # t, il faut regarder dans la première colonne du 
  # vecteur y. etc
  
 
  parms <- Parms[[1]]
  data <- Parms[[2]]
  PP <- Parms[[3]]
 
  
  
  
  V  <- y[1]
  U  <- y[2]
  
  # print(paste0("U", U))
  
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
    Ma  <- Ma
  }
  
  
  Mj <- y[6]
  # 
  if (Mj <= 0){
    Mj = 0}
  else{
    Mj  <- Mj
  }
  
 
  Pa  <- y[9]
  

  if (Pa <= 0){
    Pa = 0}
  else{
    Pa  <- Pa
  }
  
  
  #print(paste0(Pa, "Pa"))

  
  Pj  <- y[10]
  
  
  
  if (Pj < 0){
    Pj = 0}
  else{
    Pj  <- Pj
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
  
  
  
  with(as.list(parms), {
    
    # -- Load data and parameters
    # Predator prey functional response
    
    # Run the intermediate_res function that computes the following variables
    # (see intermediate_res script)
    int_rlt <- intermediate_res(y)
    
    
    # Access the first list that contains all the functional responses
    r_fonc <- int_rlt[[1]]
    
    rfonc_P_Mj <- r_fonc[1]
    rfonc_P_Ma <- r_fonc[2]
    rfonc_P_Nj <- r_fonc[3]
    rfonc_P_Na <- r_fonc[4]
    rfonc_P_Cj <- r_fonc[5]
    rfonc_P_Ca <- r_fonc[6]
    rfonc_tot <- r_fonc[7] 
    rfonc_P_Qj <- r_fonc[8]
    rfonc_P_Qa <- r_fonc[9]
    
    # Access the second list that contains all the preferences parameters
    par_pref <- int_rlt[[2]]
    
    pref_P_Mj <- par_pref[1]
    pref_P_Ma <- par_pref[2]
    pref_P_Nj <- par_pref[3]
    pref_P_Na <- par_pref[4]
    pref_P_Cj <- par_pref[5]
    pref_P_Ca <- par_pref[6]
    pref_P_Qj <- par_pref[7]
    pref_P_Qa <- par_pref[8]
    
    # Access all other parameters
    den_rfonc_P <- int_rlt[[3]]
    k_P <- int_rlt[[4]]
    M_tot <- int_rlt [[5]]
    #print(paste0("mtot", M_tot))
    N_tot <- int_rlt [[6]]
    C_tot <- int_rlt[[7]]
    proies_tot <- int_rlt[[8]]
    
    chi_P <- int_rlt[[9]]
    
    
    surplus_NRJ <- int_rlt[[10]]
    rep_fonc_MU <- int_rlt[[11]]
    croissance_loup <- int_rlt[[12]]
    mu_P <- int_rlt[[13]]
    rep_totale_MU <- int_rlt[[14]]
    rep_fonc_CU <- int_rlt[[15]]
    rep_totale_CU <- int_rlt[[16]]
    Ma_supplementary_NRJ <- int_rlt[[17]]
    Na_supplementary_NRJ <- int_rlt[[18]]
    Ca_supplementary_NRJ <- int_rlt[[19]]
    k_P_Johnson <- int_rlt[[20]]
    ungulate_biomass <- int_rlt[[21]]
    Qa_supplementary_NRJ <- int_rlt[[22]]
    Q_tot <- int_rlt[[23]]
    
    
    
    
    
    # ---- Vegetation carrying capacity from the evol_vg script
    
    capacite_vg <- evol_vg(t,data, PP)
    k_U <- capacite_vg[[1]]
    k_V <- capacite_vg[[2]]
    
    # If a pertrubation occurs
    kUcoeff1 <- data$kUcoeff1 
    kUcoeff2 <- data$kUcoeff2
    
    
    # --- Species productivity dependent parameters from Auto_gen_delta_related_params
    u_croiss = data$u_croiss
    
    
    # Carrying capacities
    # Caribou
    
    k_n = data$k_n
    
    if (k_n <= 0){
      k_n = 0.0079
      
    } else if (k_n > 0.077){
      k_n = 0.077
      
    } else{
      k_n  <- k_n
    }
    
    
    # Moose
    k_m <- data$k_m
    
    if (k_m <= 0){
      k_m = 0.0047

    } else if (k_m > 0.6){
      k_m = 0.6

    } else{
      k_m  <- k_m
    }

    
    # Deer
    k_c <- data$k_c
    
    if (k_c <= 0){
      k_c = 0.0047

    } else if (k_c > 4){
      k_c = 4

    } else{
      k_c  <-  k_c
    }
    
    # Other prey
    k_q <- data$k_q
    

    if (k_q <= 0){
      k_q = 0.0483630

    } else if (k_q > 6.18){
      k_q = 6.1740000

    } else{
      k_q  <- k_q
    }
    
    
    
    # --- Conversion rate of energy to juveniles
    chi_M <- data$chi_M
    chi_N <- data$chi_N
    chi_Q <- data$chi_Q
    chi_C <- data$chi_C
    
    
    
    
    # ==========================================================================
    # ---- Species equations ---------------------------------------------------
    
    ## --- Lichen --------------------------------------------------------------
    # Modèle logistique
    dVdt <- v_croiss * V * (1 - V/k_V) -
      ((a_N * V * (Na))/(1 + a_N * h_VN * V))
    
    ## --- Deciduous -----------------------------------------------------------
    dUdt <- u_croiss * (1 - U/k_U) -
      ((a_M * U * (Ma))/(1 + a_M * h_UM * U)) -
      ((a_C * U * (Ca))/(1 + a_C * h_UC * U)) -
      ((a_Q * U * (Qa))/(1 + a_Q * h_UQ * U))
    
    
    ## --- Caribou -------------------------------------------------------------
    ### -- Adults --------------------------------------------------------------
    dNadt <- tau_n * Nj * (1-((Na)/k_n)) - rfonc_P_Na * Pa
    
    ### -- Juveniles -----------------------------------------------------------
    dNjdt <- chi_N * Na_supplementary_NRJ * Na -
      tau_n * Nj -
      rfonc_P_Nj * Pa
    
    juv_growth_N <- chi_N * Na_supplementary_NRJ * Na 
    
    
    ## --- Competiting species -------------------------------------------------
    # Pour toutes les proies qui se nourrissent sur U, on exprime 
    # leur densité totale, en tenant compet de la pondération:
    

    # ===     A CONTINUER ICI ===============
    # Selon l'intake rate
    M_equi <- Ma + alpha_MC *(Ca)+ alpha_MQ *(Qa) 
    C_equi <- Ca + alpha_CQ *(Qa) + alpha_CM *Ma
    Q_equi <- Qa + alpha_QM *(Ma) + alpha_QC *Ca
    
    
    
    # # Orignal adulte
    # dMadt <- tau_m * Mj -
    #   ((m_croiss/k_m) * Ma * (Ma+Mj)) - rfonc_P_Ma * P
    
    # dMadt <- tau_m * Mj -
    #   ((m_croiss/k_m) * Ma * (Ma)) - rfonc_P_Ma * P
    
    # dMadt <- tau_m * Mj
    
    # dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma * P
    
    # dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma * Pa
    #alpha_MC = 0.29
    # alpha_MC = 1
    
    #dMadt <- tau_m * Mj * (1-((Ma+alpha_MC*Ca)/k_m)) - rfonc_P_Ma * Pa
    dMadt <- tau_m * Mj * (1-(M_equi/k_m)) - rfonc_P_Ma * Pa
    
    
    
    #C_equi = Ca + Ma * alpha_CM
    #M_equi = Ma + Ca * alpha_MC
    satu_M = M_equi/k_m
    satu_C = C_equi/k_c
    satu_Q = Q_equi/k_q
    croissance_adultes_M = tau_m * Mj * (1 - satu_M)
    
    
    # dMadt <- croissance_adultes_M
    
    # dMadt <- tau_m * Mj * (1-((Ma+epsi_MC*Ca)/k_m)) - rfonc_P_Ma * Pa
    
    
    # Following Turchin2003
    # dMadt <- 0.2 * Ma * (1 - (Ma/k_m)) - (12.3*P*Ma/0.47+Ma)
    # dMadt <- 0.2 * Ma * (1 - (Ma/2)) - (12.3*P*Ma/0.47+Ma)
    
    # dMadt <- tau_m * Mj * (1-((Ma)/k_m)) - rfonc_P_Ma_test * P
    
    rep_fonc_turchin <- (12.3*Pa*Ma/0.47+Ma)
    
    
    limit_enviro1 <- ((m_croiss/k_m) * Mj * (Ma+Mj)) 
    limit_enviro2 <- Ma/k_m 
    
    # print(paste("Ma=", Ma))
    # print(paste("Mj=", Mj))
    # print(paste("dMadt_sp_eq=", dMadt))
    # print(paste("Ma_eq=", Ma))
    
    # print(paste("tau_m * Mj=", tau_m * Mj))
    # print(paste("(m_croiss/k_m) * Ma * (Ma+Mj)=", (m_croiss/k_m) * Ma * (Ma+Mj)))
    # print(paste("m_croiss=", m_croiss))
    # print(paste("k_m_sp_eq=", k_m))
    # print(paste("(m_croiss/k_m)=", (m_croiss/k_m)))
    
    # print(paste("Mj_sp_eq=", Mj))
    # print(paste("Ma_sp_eq=", Ma))
    # print(paste("rfonc_P_Ma_sp_eq=", rfonc_P_Ma))
    # print(paste("P_sp_eq=", P))
    # 
    
    
    # Orignal juvenile
    # dMjdt <- chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma -
    #   tau_m * Mj - 
    #   ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   rfonc_P_Mj * P
    
    # Ma_supplementary_NRJ <- (((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M))
    
    
    dMjdt <- chi_M * Ma_supplementary_NRJ * Ma -
      tau_m * Mj -
      # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
      # ((m_croiss/k_m) * Ma * (Ma)) -
      rfonc_P_Mj * Pa
    
    # dMjdt <- m_croiss * Ma_supplementary_NRJ * Ma -
    #   tau_m * Mj -
    #   # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   # ((m_croiss/k_m) * Ma * (Ma)) -
    #   rfonc_P_Mj * P
    
    # dMjdt <- chi_M * Ma_supplementary_NRJ * Ma -
    #   tau_m * Mj - 
    #   # ((m_croiss/k_m) * Mj * (Ma+Mj)) -
    #   # ((m_croiss/k_m) * Ma * (Ma)) -
    #   rfonc_P_Ma_test * P
    
    
    # print(paste("chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma=",
    # chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M) * Ma))
    
    # print(paste("chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)=",
    # chi_M * ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)))
    
    # print(paste("chi_M= ", chi_M))
    
    juv_growth <- chi_M * Ma_supplementary_NRJ * Ma 
    
    # print(paste("chi_M= ", chi_M))
    # print(paste("(a_M * e_UM * U)= ", (a_M * e_UM * U)))
    # print(paste("(1 + a_M * h_UM * U)= ", (1 + a_M * h_UM * U)))
    # print(paste("((a_M * e_UM * U)/(1 + a_M * h_UM * U)= ", ((a_M * e_UM * U)/(1 + a_M * h_UM * U))))
    # print(paste("((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)= ", ((a_M * e_UM * U)/(1 + a_M * h_UM * U) - mu_M)))
    
    
    conv_adu <- tau_m * Mj 
    
    # limit_enviro <- ((m_croiss/k_m) * Mj * (Ma+Mj)) 
    
    predation <- rfonc_P_Mj * Pa
    
    # Cerfs adultes
    # dCadt <- tau_c * Cj -
    #   ((c_croiss/k_c) * Ca * (Ca+Cj)) - rfonc_P_Ca * P
    
    # dCadt <- tau_c * Cj -
    # ((c_croiss/k_c) * Ca * (Ca)) - rfonc_P_Ca * P # donne un peu plus
    # que la valeur de capacite de cahrge
    
    # dCadt <- tau_c * Cj -
    #   ((c_croiss/k_c) * Ca * (Ca)) - rfonc_P_Ca * P
    
    #dCadt <- tau_c * Cj * (1-((Ca)/k_c)) - rfonc_P_Ca * Pa
    
    # alpha_CM = 3.4
    # # alpha_CM = 1
    # epsi_CM = w_Ma / w_Ca
    # 
    # dCadt <- tau_c * Cj * (1-((Ca+alpha_CM*Ma)/k_c)) - rfonc_P_Ca * Pa
    #dCadt <- tau_c * Cj * (1-((Ca+epsi_CM*Ma)/k_c)) - rfonc_P_Ca * Pa
    dCadt <- tau_c * Cj * (1-(C_equi/k_c)) - rfonc_P_Ca * Pa
    
    
    
    # conv_adu_C <- tau_c * Cj 
    
    # Ca_growth <-  tau_c * Cj
    juv_growth_C <- chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca 
    conv_adu_C <- tau_c * Cj
    predation_C <- rfonc_P_Cj * Pa
    
    
    
    # Cerf juveniles
    # dCjdt <- chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca -
    #   tau_c * Cj - 
    #   ((c_croiss/k_c) * Cj * (Ca+Cj)) -
    #   rfonc_P_Cj * P
    
    dCjdt <- chi_C * Ca_supplementary_NRJ * Ca -
      tau_c * Cj - 
      # ((c_croiss/k_c) * Cj * (Ca+Cj)) -
      rfonc_P_Cj * Pa
    
    # 
    # # Factice additional prey Q
    # dQadt <- tau_q * Qj * (1-((Qa)/k_c)) - rfonc_P_Qa * Pa
    # 
    # dQjdt <- chi_C * Qa_supplementary_NRJ * Qa -
    #   tau_q * Qj -
    #   # ((c_croiss/k_c) * Cj * (Ca+Cj)) -
    #   rfonc_P_Qj * Pa
    
    ## BEAVER
    
    #k_Q = 2.4
    
    # dQadt <- chi_Q * Qa_supplementary_NRJ * Qa * (1-(Q_equi/k_q)) - rfonc_P_Qa * Pa
    
    dQadt <- tau_q * Qj * (1-(Q_equi/k_q)) - rfonc_P_Qa * Pa
    dQjdt <- chi_Q * Qa_supplementary_NRJ * Qa -
      tau_q * Qj - 
      rfonc_P_Qj * Pa
    

    
    
    # print(paste("Ca=", Ca))
    # print(paste("Cj=", Cj))
    # print(paste("dCadt_sp_eq=", dCadt))
    # print(paste("tau_c * Cj=", tau_c * Cj))
    # print(paste("tau_c * Cj - (1-((Ca)/k_c))=", tau_c * Cj - (1-((Ca)/k_c))))
    # print(paste("c_croiss=", c_croiss))
    # print(paste("k_c_sp_eq=", k_c))
    
    # print(paste("Mj_sp_eq=", Mj))
    # print(paste("Ma_sp_eq=", Ma))
    # print(paste("rfonc_P_Ca_sp_eq=", rfonc_P_Ca))
    # print(paste("predation Ca=", rfonc_P_Ca * P ))
    # print(paste("P_sp_eq=", P))
    
    # print(paste("chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U)", chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U))))
    
    # print(paste("chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca", chi_C * ((a_C * e_UC * U)/(1 + a_C * h_UC * U) - mu_C) * Ca))
    
    
    # Loup
    # dPdt <- chi_P * 
    #   ((rfonc_P_Ma + epsi_Maj * rfonc_P_Mj + 
    #                    epsi_MN * (rfonc_P_Na + rfonc_P_Nj * epsi_Naj))- mu_P) * P - (p_croiss/k_P) * P^2
    # 
    # dPdt <- chi_P * surplus_NRJ * P - ((p_croiss/k_P) * P^2)
    
    # dPdt_test <- chi_P * surplus_NRJ_test * P - ((p_croiss/k_P) * P^2)
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2)
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2) - 0.5*P
    
    # dPdt <- chi_P * surplus_NRJ_Messier * P - ((p_croiss/k_P) * P^2) - 0.5*P
    
    # dPadt <- chi_P * surplus_NRJ_Messier * P * (1 - (P/k_P)) - 0.5*P
    
    dPadt <- tau_p * Pj * (1 - (Pa/k_P)) - 0.2*Pa
    
    # dPjdt <- chi_P * surplus_NRJ_Messier * Pa * (1 - (Pa/k_P)) - tau_p * Pj #- 0.4*Pj #(pup death)
    
    dPjdt <- chi_P * surplus_NRJ * Pa - tau_p * Pj #- 0.4*Pj #(pup death)
    
    
    
    
    
  
    
    
    
    return (list(c(dVdt, dUdt, dNadt, dNjdt, dMadt, dMjdt,
                   dCadt, dCjdt,  dPadt, dPjdt, dQadt, dQjdt),
                 
                 # Functional response
                 rfonc_P_Mj,
                 rfonc_P_Ma,
                 rfonc_P_Nj,
                 rfonc_P_Na,
                 rfonc_P_Cj,
                 rfonc_P_Ca,
                 rfonc_tot,
                 # rfonc_P_Qj,
                 # rfonc_P_Qa,
                 
                 # Preference parameters
                 pref_P_Mj,
                 pref_P_Ma,
                 pref_P_Nj,
                 pref_P_Na,
                 pref_P_Cj,
                 pref_P_Ca,
                 # pref_P_Qa,
                 # pref_P_Qj,
                 den_rfonc_P,
                 
                 # All other parameters
                 k_P,
                 chi_P,
                 surplus_NRJ,
                 M_tot,
                 N_tot,
                 C_tot,
                 proies_tot,
                 PP,
                 p_croiss,
                 a_P,
                 mu_P,
                 rep_fonc_MU,
                 ma_init,
                 na_init,
                 ca_init,
                 p_init,
                 k_U,
                 k_V,
                 rep_totale_MU,
                 rep_fonc_CU,
                 rep_totale_CU,
                 k_m,
                 Pa,
                 Ma,
                 Mj,
                 U,
                 juv_growth,
                 conv_adu,
                 limit_enviro1,
                 limit_enviro2,
                 predation,
                 k_c,
                 Ca,
                 Cj,
                 conv_adu_C,
                 juv_growth_C,
                 predation_C,
                 k_n,
                 Na,
                 Nj,
                 Pj,
                 Ma_supplementary_NRJ,
                 chi_M,
                 Na_supplementary_NRJ,
                 Ca_supplementary_NRJ,
                 k_P_Johnson,
                 ungulate_biomass,
                 Qa,
                 Qj,
                 rfonc_P_Qj,
                 rfonc_P_Qa,
                 pref_P_Qj,
                 pref_P_Qa,
                 Qa_supplementary_NRJ,
                 croissance_adultes_M,
                 k_q,
                 juv_growth_N
    ))
  })
}
