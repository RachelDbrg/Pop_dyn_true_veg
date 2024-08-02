# ------ Initial conditions and parameters for animals -----



# --- Caribou 
n_croiss <- params_combination[j,1]

# k_n = 0.2       # Carrying capacity (for a limited 
# disturbed landscape, see Stewart et al 2020)

# k_n <- params_combination[j,12]

# FEEDING
# a_N = 0.05      # Prospection/Feeding area
a_N <- params_combination[j,7]      # Prospection/Feeding area

# h_VN  = 8.81e-4 * 0.33 # Handling time
h_VN <- params_combination[j,12]

# tau_n = 2 # Inverse of the time an individual is juvenile 
tau_n <- params_combination[j,17]

# mu_N = 5.7467e+06 # NRJ required for maintenance
mu_N <- params_combination[j,22]

# e_VN = 11.8e3 # NRJ intake
e_VN <- params_combination[j,27]

# Conversation rate of NRJ --> juveniles
# chi_N = n_croiss * ((a_N * e_VN * kVnorm)
#                     /(1+a_N * h_VN * kVnorm) - mu_N)^-1


# ---- MOOSE ---- 
coef = 1

# m_croiss = 0.25
m_croiss <- params_combination[j,2]

# a_M = 0.05
a_M <- params_combination[j,8]

# h_UM  = 1.0959e-03*0.33
h_UM <- params_combination[j,13]
# k_m = (2-0.84)*PP + 0.84

# k_m = ((2-0.84)*PP + 0.84) * coef

# tau_m = 2 # inverse du temps où les orignaux restent 
tau_m <- params_combination[j, 18]

#juveniles (estime a 6 mois)
# mu_M = 1.5272e+07
mu_M <- params_combination[j, 23]

# e_UM = 1.8410e+04
# e_UM = mu_M * (912.5)^-1
e_UM = params_combination[j,28]
# chi_M = m_croiss * ((a_M * e_UM * kUpeak)/
#                       (1+a_M * h_UM * kUpeak) - mu_M)^-1



# ---- DEER ---- 


h_UC <- params_combination[j,14]

# a_C = 0.05
a_C <- params_combination[j,9]

# e_UC = 1.8410e+04
e_UC <- params_combination[j,29]

# mu_C =  4.3967e+06
mu_C <- params_combination[j,24]

# c_croiss = 0.25
c_croiss <- params_combination[j,3]

# tau_c = 2
tau_c <- params_combination[j,19]

# ========================


h_UQ <- params_combination[j,16]

# a_C = 0.05
a_Q <- params_combination[j,11]

# e_UC = 1.8410e+04
e_UQ <- params_combination[j,30]

# mu_C =  4.3967e+06
mu_Q <- params_combination[j,26]

# c_croiss = 0.25
q_croiss <- params_combination[j,6]
# chi_C = c_croiss * 
#   ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
# k_c= (11.43-4.74)*PP +4.74
# k_c= ((11.43-4.74)*PP +4.74) * coef

# tau_c = 2
tau_q <- params_combination[j,21]


# ---- WOLF ----


p_init = 0
# p_init = 0.004

# p_init = 0
# a_P = 65.116 #km2/an
a_P <- params_combination[j,10]



tau_p <- params_combination[j,20]

# a_P = 30
# a_P = 130

# p_croiss = 0.36/2
p_croiss <- params_combination[j,4]

# mu_P = 2.0683
mu_P <- params_combination[j,25] 


# Prey biomass
# w_Ma = 400
w_Ma <- params_combination[j,31] 

# w_Mj = 0.08 * w_Ma
w_Mj <- params_combination[j,32] 

# w_Na = 100
w_Na <- params_combination[j,33] 

# w_Nj = 0.08 * w_Na
w_Nj <- params_combination[j,34] 

# w_Ca = 70
w_Ca <- params_combination[j,35] 

# w_Cj = 0.08 * w_Ca
w_Cj <- params_combination[j,36] 



w_Qa <- params_combination[j,37] 

# w_Cj = 0.08 * w_Ca
w_Qj <- params_combination[j,38] 


# Conversion rates based on weight
epsi_MN = w_Na / w_Ma
epsi_Maj = w_Mj/w_Ma
epsi_Naj = w_Nj/w_Na
epsi_WC = 0.934
epsi_MC = w_Ca / w_Ma
epsi_Caj = w_Cj/w_Ca
epsi_CM = w_Ma / w_Ca
epsi_MQ = w_Qa / w_Ma
epsi_Qaj = w_Qj/w_Qa

# Conversion rates based on intake rate
alpha_MC = 0.29
alpha_CM = 3.4
alpha_MQ = 0.0917
alpha_QM = 10.29
alpha_CQ = 0.33
alpha_QC = 3


# Handling times of preys 
# h_P_Ma = 0.105
h_P_Ma <- params_combination[j,15]

h_P_Mj = h_P_Ma * epsi_Maj
h_P_Na = h_P_Ma * epsi_MN
h_P_Nj = h_P_Na * epsi_Naj
h_P_Ca = h_P_Ma * epsi_MC
h_P_Cj = h_P_Ma * epsi_Caj


# Create a parameters vector, that stocks all the fixed 
# values of the model
parms <- c(v_croiss,a_P,w_Ma,w_Mj,w_Na,w_Nj,w_Cj,w_Ca,
           h_P_Ma, h_P_Mj, h_P_Na, h_P_Nj,h_P_Cj,h_P_Ca,
           epsi_Maj, epsi_MN, epsi_Naj, epsi_Caj,epsi_MC,
           mu_P, p_croiss, tau_p)

# Stock all the initials values of parameters of 
# animals in a vector
# initial_conditions_animals <- c(na_init, nj_init,
#                                 ma_init, mj_init, p_init,
#                                 ca_init, cj_init)

# na_init <- fauna_init_df[[1,2]]
# nj_init <- na_init *0.1
# ma_init <- fauna_init_df[[1,1]]
# mj_init <- ma_init *0.1
# ca_init <- fauna_init_df[[1,3]]
# cj_init <- ca_init *0.1
# p_init = 0.004
# 
# initial_conditions_animals <- c(na_init, nj_init,
#                                 ma_init, mj_init, p_init,
#                                 ca_init, cj_init)
# 
# # Concatenate both vegetation and animals initials values
# y0 <- c(initial_conditions_vegetation
#         ,initial_conditions_animals)
# 
# y0_test <- c(initial_conditions_vegetation)

# parametres_ax <- as.data.frame(do.call(rbind,list(initial_conditions_animals)))
# 
# return()
# }