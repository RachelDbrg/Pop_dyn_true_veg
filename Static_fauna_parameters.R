# ------ Initial conditions and parameters for animals -----

# ------------------------- PREY -----------------------------------------------

# Prey biomass
w_Ma = 400
w_Mj = 0.08 * w_Ma
w_Na = 100
w_Nj = 0.08 * w_Na
w_Ca = 70
w_Cj = 0.08 * w_Ca
w_Qa = 15
w_Qj = 0.08 * w_Qa

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


# --- Caribou ------

n_croiss = 0.25  # Growth
a_N = 0.05      # Prospection/Feeding area
h_VN  = 8.81e-4 # Handling time #year/kg
tau_n = 1/16
mu_N = 70*w_Na^0.75*365*1.7*4.184
e_VN = 11.8e3 # NRJ intake



# ---- Moose ---- 

m_croiss = 0.25
a_M = 0.05
h_UM = 0.000034 #an/kg
tau_m = 1/16
mu_M = 70*w_Ma^0.75*365*1.7*4.184
e_UM = 20083


# ---- Deer ---- 

c_croiss = 0.25
a_C = 0.05
h_UC = 0.000100274
tau_c = 1/6
mu_C =  70*w_Ca^0.75*365*1.7*4.184
e_UC = 20083


# ===========================
# # Factice additional prey Q
# 
# 
# #qa_init = 0
# # qa_init = 2.1
# #qj_init = qa_init*0.1
# 
# h_UQ = 1.0959e-03
# 
# a_Q = 0.05
# 
# e_UQ = 1.6 * 10^4
# 
# mu_Q =  4.3967e+06
# q_croiss = 0.25
# # chi_C = c_croiss * 
# #   ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
# # k_c= (11.43-4.74)*PP +4.74
# # k_c= ((11.43-4.74)*PP +4.74) * coef
# # tau_c = 2
# 
# # tau_c = 8 (12/1.5)
# 
# #(voir notes "deer/reproduction")
# # Les cerfs de Virginie peuvent atteindre la maturité sexuelle dès 6 ou 7 mois.
# tau_q = 1/6


# ==============================================================================
# ---- Beaver ---- 



h_UQ = 3.29*10^-3

a_Q = 0.417

e_UQ = 1.72 * 10^4

mu_Q =  1.85*10^6

q_croiss = 0.25

tau_q = 1/5
# chi_C = c_croiss * 
#   ((a_C*e_UC*kUpeak)/(1+a_C*h_UC*kUpeak)- mu_C)^-1
# k_c= (11.43-4.74)*PP +4.74
# k_c= ((11.43-4.74)*PP +4.74) * coef
# tau_c = 2

# tau_c = 8 (12/1.5)

#(voir notes "deer/reproduction")
# Les cerfs de Virginie peuvent atteindre la maturité sexuelle dès 6 ou 7 mois.
# ==============================================================================


# ---- Wolf ----


tau_p = 1/24

p_init = 5
a_P = 65.116 #km2/an


# P <- 0

p_croiss = 0.36/2


mu_P = 3.88





# Handling times of preys 
h_P_Ma = 0.105
h_P_Mj = h_P_Ma * epsi_Maj
h_P_Na = h_P_Ma * epsi_MN
h_P_Nj = h_P_Na * epsi_Naj
h_P_Ca = h_P_Ma * epsi_MC
h_P_Cj = h_P_Ma * epsi_Caj
h_P_Qa = 4.515*10^-3
h_P_Qj = h_P_Qa * epsi_Qaj


# Create a parameters vector, that stocks all the fixed 
# values of the model
parms <- c(v_croiss,a_P,w_Ma,w_Mj,w_Na,w_Nj,w_Cj,w_Ca,
           h_P_Ma, h_P_Mj, h_P_Na, h_P_Nj,h_P_Cj,h_P_Ca,
           epsi_Maj, epsi_MN, epsi_Naj, epsi_Caj,epsi_MC,
           mu_P, p_croiss, tau_p)




