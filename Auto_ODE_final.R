# __________________________________________________________

# == 0. Initialization and packages loading ================
# __________________________________________________________


# Remove everything from global environment
rm(list = ls(all.names = TRUE))

# Load packages
# ode solving
library(deSolve)
# tidyverse, tidyr, pipe, EVERYTHING!
library(tidyverse)

# TODO: Check if aren't already in tidyverse
library(magrittr)
library(dplyr)
library(purrr)

# Load working directory
# setwd("~/Pop_dyn_model_Rachel/Auto_PP_scenarios_beaver")
setwd("D:/Rachel/Auto_PP_scenarios_beaver_modif_vg/Pop_dyn_true_veg")
# setwd("C:/Users/radub33/Documents/Copie_locale_17_mai")



# __________________________________________________________

# == 1. Load initial parameters from CSV  ==================
# __________________________________________________________

# Import the initial animal densities
# input_parameters <- read_csv("parameters.csv")
input_parameters <- read.table("input_parameters.txt", sep = ",", header = TRUE)
# input_parameters <- input_parameters[9,]
# input_parameters[,4] <- 0.001
# input_parameters <- read_csv("test_beaver.csv")
# input_parameters <- read_csv("test1.csv")
#input_parameters <- read_csv("eumaumg.csv", na = character())

# Load the script that verifies if the user did not 
# properly entered the input values
source("verifications.R")
#checkinputparameters(input_parameters)


# TODO: get rid of phi
phi = 1



# __________________________________________________________

# == 2. Iterate the population dynamics model ==============
#            over input parameters        
# __________________________________________________________

# This loops runs the model for all parameters combinations set in the 
# input_parameters file. This way, all the models can be run at once. 
# This avoids forgetting some combinations and stores all the result dataframe
# in a standard and uniform notation. 


for (i in 1:nrow(input_parameters)) {
  # #   # Extract parameters from current row
  # i = 1
  na_init <- as.numeric(input_parameters[[i, 1]])
  nj_init = na_init*0.1
  
  ma_init <- as.numeric(input_parameters[[i, 2]])
  mj_init = ma_init*0.1
  
  ca_init <- as.numeric(input_parameters[[i, 3]])
  cj_init = ca_init*0.1
  pa_init <- as.numeric(input_parameters[[i, 4]])
  pj_init = pa_init * 0.1
  
  
  qa_init <- as.numeric(input_parameters[[i, 5]])
  qj_init = qa_init * 0.1
  
  initial_conditions_animal <- c(na_init, nj_init, ma_init, mj_init, 
                                 ca_init, cj_init, pa_init, pj_init, 
                                  qa_init, qj_init)
  
  
  # Load dataset that compute a set of productivity-dependent 
  # variables
  
  # CLEANED
  source("Auto_gen_PP_delta_related_parameters.R")
  
  # Define initial vegetation parameters
  # CLEANED
  source("Static_vegetation_parameters.R")
  
  # Yet some cleaning to do, to see if we keep the beaver
  # Define initial animals parameters
  source("Static_fauna_parameters.R")
  
  
  # Build the initial values parameters based on the 
  yini <- c(initial_conditions_vegetation, initial_conditions_animal)

  
  # CLEANED
  source("Evolution_vegetation.R")
  
  
  source("intermediate_res_with_competition.R")
  
  
  # source("intermediate_res_DEBUG.R")
  
  source("make_ODE_function.R")
  
  source("Species_equations_with_competition.R")
  
  # source("Species_equations_caribou_feuillus.R")
  # 
  
  
  #my_function <- function(simulation_index, ...){
  
  #   for (i in 1:nrow(input_parameters)) {
  # #   # Extract parameters from current row
  #   #i = 1 
  #   print(i)
  #   na_init_qwer <- as.numeric(input_parameters[[i, 1]])
  #   #print(na_init_qwer)
  #   ma_init <- as.numeric(input_parameters[[i, 2]])
  #   ca_init <- as.numeric(input_parameters[[i, 3]])
  #   pa_init <- as.numeric(input_parameters[[i, 4]])
  #    
  #   initial_values <- c(na_init_qwer, ma_init, ca_init, pa_init)
  #   print(initial_values)
  # #   
  # # }
  
  # Run the "auto_gen_PP_related_parameters"
  result_df <- generate_parameter_dataframe()
  
  
  nested_test <- result_df %>% 
    # mutate(pouic = delta) %>%
    # group_by(PP,delta) %>% 
    group_by(PP) %>% 
    nest()
  
  
  # list <- list(nested_test$PP, nested_test$data, nested_test$delta)
  
  # Apply the ODE solver
  res <- nested_test |> 
    # group_by(PP) |> 
    ungroup() |> 
    mutate(outputs = map2(nested_test$PP, nested_test$data, ~make_ODE(.x, .y)))  
    # rename("pouic" = "delta")
  # mutate(outputs = pmap(list, ~make_ODE(..1, ..2, ..3)))
  # mutate(outputs = make_ODE(PP, data))
  
  
  # EXPERIMENTAL
  # Define the name of the simulation
  presence_N <- ifelse(na_init !=0, "N", "")
  presence_M <- ifelse(ma_init !=0, "M", "")
  presence_C <- ifelse(ca_init !=0, "C", "")
  presence_P <- ifelse(pa_init !=0, "P", "")
  presence_Q <- ifelse(qa_init !=0, "Q", "")
  
  my_string <- paste0(presence_N,
                      presence_M,
                      presence_C,
                      presence_P,
                      presence_Q)
  
  
  name_iteration <- my_string
  print(name_iteration)
  
  
  # filename <- paste0("~/Copie_locale_17_mai/Results/test/", name_iteration, ".R")
  filename <- paste0("D:\\Rachel\\Auto_PP_scenarios_beaver_modif_vg\\Pop_dyn_true_veg\\Results\\simulations\\23_july_2024\\", name_iteration, ".R")
  print(filename)
  # filename <- paste0("~/Automation_Primary_productivity/scripts/Auto_PP_scenarios/Results/test/", name_iteration, "eumaugmente.R")
  # saveRDS(res, file = "~/Automation_Primary_productivity/scripts/One_more_prey_decrease_lichen_PP/Results/MCP.R")
  # saveRDS(res, file = "Results/MC_GPT.R")
  saveRDS (res, file = filename)
  
  
  
  
  # filename <- paste0("~/Automation_Primary_productivity/res_simulations2/all_simulations_scenario", simulation_index, ".R")
  # saveRDS(res, file = filename)
  
}
#}

#simulation_index <- 1

#my_function(simulation_index)

# Increment simulation index
# simulation_index <- simulation_index + 1


# my_function(simulation_index)
# 
# # Increment simulation index
# simulation_index <- simulation_index + 1

# test_model_auto <- test$outputs[[1]] %>% 
#   filter(time == 666)
# 
# # Check si les datas sont les memes 
# data_test_auto <- test_model_auto <- test$data[[1]]


