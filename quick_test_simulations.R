lichen <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_only.R")
moose <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/moose_only.R")
M <- readRDS(file = "~/Automation_Primary_productivity/Final_plots/M.R")
lichen_caribou <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou.R")
lichen_caribou_wolf <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou_wolf.R")
lichen_caribou_moose_wolf <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou_moose_wolf.R")
lichen_caribou_moose_deer_wolf <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou_moose_deer_wolf.R")
lichen_caribou_moose_deer_additional_prey_wolf <- readRDS(file = "~/Automation_Primary_productivity/Decreasing_lichen_PP_test/lichen_caribou_moose_deer_additional_prey_wolf.R")
wolfQ <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/wolf_Q.R")

NMCQP <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/NMCQP.R")
MP_nodeath <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MP_nodeath.R")
NP_nodeath <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/NP_nodeath.R")
noprey <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/noprey.R")
noprey_nodeath <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/noprey_nodeath.R")
moose_nodeath <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/_nodeath.R")
caribou_nodeath <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/caribou_nodeath.R")

library(tidyverse)

####################### 
test <- readRDS(file = "D:/Rachel/Auto_PP_scenarios_beaver/Results/simulations/18_june_24/MP.R")
test2 <- readRDS(file = "D:\\Rachel\\Auto_PP_scenarios_beaver_modif_vg\\Pop_dyn_true_veg\\Results\\simulations\\23_july_2024\\NMCP.R")


test <- test %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "old") %>% 
  filter(PP == 1) %>% 
  mutate(pduction_jeunes_loups = chi_P * surplus_NRJ * Pa,
         trasnfert_jeunes_adultes = tau_p * Pj,
         pduction_adultes =tau_p * Pj * (1 - (Pa/k_P)))


test2 <- test2 %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "CP") 
  #filter(PP == 1)


carrying_capacities <- test |> 
  filter(time == 800) |> 
  select(c(PP, zone, k_m, k_U, k_V, k_c, k_n, k_P)) |> 
  pivot_longer(cols = -c(PP, zone),
               names_to = "species",
               values_to = "value") |> 
  group_by(zone, PP) |> 
  #filter(sc %in% c("N", "M", "C")) %>%
  mutate(species = case_when(
    species == "k_m" ~ "moose",
    species == "k_U" ~ "U",
    species == "k_V" ~ "V",
    species == "k_c" ~ "deer",
    species == "k_n" ~ "caribou",
    species == "k_P" ~ "wolf",
    TRUE ~ species
  )) %>%
  filter(species %in% c("wolf")) 



test_court <- test %>% 
  select(c(V, U, Na, Nj, Ma, Mj, Pa, Pj, Ca, Cj, PP, time, zone, pduction_jeunes_loups,trasnfert_jeunes_adultes, pduction_adultes,
           surplus_NRJ, k_P))

test2_court <- test2 %>% 
  select(c(V, U, Na, Nj, Ma, Mj, Pa, Pj, Ca, Cj, PP, time, zone, 
           pduction_jeunes_loups,trasnfert_jeunes_adultes, pduction_adultes,
           surplus_NRJ, k_P))

test <- rbind(test_court, test2_court)





test2 %>% 
  filter(time == 3.0) %>% View()

test2 %>%
  # filter(PP == 1) %>%
  pivot_longer(cols = c(Na,  Pa, Ma, Ca, V, Nj, Pj),
               names_to = "species",
               values_to = "density") %>%
  # mutate(species = case_when(
  #   species == "V"  ~ "lichen",
  #   species == "Ma"  ~ "moose",
  #   species == "Na"  ~ "caribou",
  #   species == "Qa"  ~ "other",
  #   species == "Pj"  ~ "young_wolf",
  #   species == "Pa"  ~ "wolf",
  #   species == "k_P"  ~ "carryingcap"
  #   )) |> 
  #filter(species %in% c("caribou", "lichen", "wolf", "moose", "carryingcap", "young_wolf")) |>
  filter(species %in% c("Pa", "Ca")) %>% 
  filter(time ==800) %>% 
  # filter(species %in% c("moose", "caribou", "deer")) |> 
  ggplot()+
  geom_point(aes(x = PP, y = density,
                color = species), size = 1.5)+
  #geom_hline(data = carrying_capacities,
  #        aes(yintercept = value, linetype = "Carrying Capacity"),
  #       linetype = "dashed", linewidth = 1, color ="black") +
  facet_grid(species~PP, scales = "free")
  # facet_wrap(~PP)


test %>% 
  filter(PP == 1) %>% 
  select(c(Pa, proies_tot, k_P, zone, time)) %>% 
  pivot_longer(cols = c(Pa, proies_tot, k_P)) %>% 
  ggplot(aes(time, value, col = name))+
  geom_point()+
  facet_grid(name~zone, scales = "free")



Mnew <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_new.R")
Mold <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_old.R")

Mnew <- Mnew %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Mnew")

Mold <- Mold %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Mold")


Cnew <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/C_new.R")
Cold <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/C_old.R")


Cnew <- Cnew %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Cnew")

Cold <- Cold %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "Cold")

MCnew <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_new.R")
MCold <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_old.R")
MC_same_k <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_same_k.R")
MC_kc_eq_3km <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_kc_eq_3km.R")
MC_kc_eq_5km <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_kc_eq_5km.R")
MC_ratio_sup_alpha <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_ratio_sup_alpha.R")
MC_idem <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_idem.R")
MC_km_tiers_kc  <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_km_tiers_kc.R")
MC_idem_same_init <- MC_km_tiers_kc  <- readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_idem_same_init.R")
MC_espi <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_epsi.R")
M_espi <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_epsi.R")
C_espi <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/C_epsi.R")
MC_alpha <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_alpha.R")
MC_epsi3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_epsi3.R")
MC_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_alpha3.R")
MC_alpha_long <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_alpha_long.R")
M_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_alpha3.R")
C_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/C_alpha3.R")
MCP_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MCP_alpha3.R")
MP_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MP_alpha3.R")
CP_alpha3 <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/CP_alpha3.R")
M_alpha3_moins_NRJ_basale  <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_alpha3_moins_NRJ_basale.R")
M_alpha3_fixed_chiM <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_alpha3_fixed_chiM.R")
M_alpha3_fixed_chiM_moins_basalNRJ <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/M_alpha3_fixed_chiM_moins_basalNRJ.R")
MCP_muM_egal_muC <-  readRDS(file = "~/Copie_locale_modele_2024_05_14/Results/MCP_muM_egal_muC.R")
MCNP_muM_egal_muC <-  readRDS(file = "~/Copie_locale_modele_2024_05_14/Results/MCNP_muM_egal_muC.R")
MCNP <-  readRDS(file = "~/Copie_locale_modele_2024_05_14/Results/MCNP.R")
MC_GPT <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MC_GPT.R")
MCP <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MCP.R")
MCP <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MCP.R")


NMCP <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/test/NMCP.R")
NMCP_increase_taup <-  readRDS(file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/test/NMCPtest.R")

NMCP <- NMCP %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "NMCP")

NMCP_increase_taup <- NMCP_increase_taup %>% 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "NMCP_increase_taup")



mreged <- bind_rows(NMCP, NMCP_increase_taup)
mreged <- bind_rows(mreged, MC_same_k)
mreged <- bind_rows(Mnew, Mold)
mreged <- bind_rows(MCP_alpha3, MP_alpha3)
mreged <- bind_rows(mreged, CP_alpha3)


mreged %>%
  #  filter(PP == 1) %>%
  #filter(time >= 600) |> 
  pivot_longer(cols = c(Na, V, Pa, Ma, Ca, Qa, U, Cj, Mj),
               names_to = "species",
               values_to = "density") %>%
  mutate(species = case_when(
    species == "V"  ~ "lichen",
    species == "U"  ~ "deciduous",
    species == "Ma"  ~ "moose",
    species == "Mj" ~"young_moose",
    species == "Na"  ~ "caribou",
    species == "Qa"  ~ "other",
    species == "Ca"  ~ "deer",
    species == "Pa"  ~ "wolf")) |> 
  filter(species %in% c("deer", "moose", "deciduous", "wolf","young_moose", "caribou")) |>
  # filter(species %in% c("deer", "moose", "deciduous","young_moose")) |>
  ggplot()+
  geom_line(aes(x = time, y = density,
                color = zone), linewidth = 1.2)+
  #geom_hline(data = carrying_capacities,
  #           aes(yintercept = value, linetype = "Carrying Capacity"),
  #           linetype = "dashed", linewidth = 1, color ="black") +
  facet_grid(species~PP, scales = "free")



MCP |> 
  filter(time == 800) |>
  #filter(PP %in% c(0.1, 0.2)) |>
  select(c(Ca, Ma, k_c, k_m, k_P, Pa, Na, k_n, Mj, Cj, k_U)) |>
  # select(c(Ca, Ma, k_c, k_m, Mj, Cj)) |> 
  mutate(C_equi = Ca + Ma * alpha_CM,
         M_equi = Ma + Ca * alpha_MC,
         satu_M = M_equi/k_m,
         satu_C = C_equi/k_c) |> View()



MCP |> 
  filter(PP == 0.5) |> 
  select(c(Ca, Ma, Cj, Mj, k_m, k_c, juv_growth, juv_growth_C, conv_adu, conv_adu_C, Ma_supplementary_NRJ, Ca_supplementary_NRJ)) |> 
  mutate(C_equi = Ca + Ma * alpha_CM,
         M_equi = Ma + Ca * alpha_MC,
         satu_M = M_equi/k_m,
         satu_C = C_equi/k_c,
         croissance_adultes_M = tau_m * Mj * (1 - satu_M)) |> View()
  # ggplot(aes(x = time, y = croissance_adultes_M)) + 
  # geom_line()



# ====================================
MCP <-  readRDS(file = "~/Automation_Primary_productivity/scripts/Auto_PP_scenarios_beaver/Results/test/NMCPpref.R")

MCP <- MCP |> 
  pull(outputs) %>%   
  map_dfr(as.data.frame) %>% 
  mutate_all(as.numeric) %>% 
  mutate(zone = "NMCP_increase_taup")


MCP |> 
pivot_longer(cols = c(Na, V, Pa, Ma, Ca, Qj, U, Cj, Mj),
             names_to = "species",
             values_to = "density") %>%
  mutate(species = case_when(
    species == "V"  ~ "lichen",
    species == "U"  ~ "deciduous",
    species == "Ma"  ~ "moose",
    species == "Mj" ~"young_moose",
    species == "Na"  ~ "caribou",
    species == "Qj"  ~ "other",
    species == "Ca"  ~ "deer",
    species == "Pa"  ~ "wolf")) |> 
  filter(species %in% c("deer", "moose", "deciduous", "wolf","young_moose", "caribou", "other")) |>
  # filter(species %in% c("deer", "moose", "deciduous","young_moose")) |>
  ggplot()+
  geom_line(aes(x = time, y = density,
                color = zone), linewidth = 1.2)+
  #geom_hline(data = carrying_capacities,
  #           aes(yintercept = value, linetype = "Carrying Capacity"),
  #           linetype = "dashed", linewidth = 1, color ="black") +
  facet_grid(species~PP, scales = "free")



MCP |> 
  # filter(PP == 0.1) |>
  filter(time == 800) |> 
  mutate(M_equi <- Ma + alpha_MC *(Ca)+ alpha_MQ *(Qa) ,
         C_equi <- Ca + alpha_CQ *(Qa) + alpha_CM *Ma,
         Q_equi <- Qa + alpha_QM *(Ma) + alpha_QC *Ca,
         satuM = M_equi/k_m,
         satuC = C_equi/k_c,
         satuQ = Q_equi/k_q)


alpha_MQ = 0.0917
alpha_QM = 10.29
alpha_CQ = 0.33
alpha_QC = 3

# Selon l'intake rate
