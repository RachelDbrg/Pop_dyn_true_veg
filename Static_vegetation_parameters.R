# ---- Initial conditions and parameters for vegetation ----


# Lichen 
v_init = 5e5


v_croiss = 0.06 # facteur de croissance



# Feuillus
u_init = 1e3

kUlow = 2e5
kUpeak_0 = 5.6e5

kUstable_0 = 1e5 * 2.4


# TODO: Voir si on garde ca?
##Arbustes W :
# Stades de succession, variation selon le temps et la productivite PP
kWlow = 2250             # kg/km^2
kWpeak_0 = 9700          # kg/km^2
kWstable_0 = 4500        # kg/km^2


# Stock all the initials values of parameters of 
# vegetation in a vector
initial_conditions_vegetation <- c(v_init, u_init)


