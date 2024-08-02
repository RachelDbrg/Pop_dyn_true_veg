
# Check input parameters and returns 
checkinputparameters <- function(df){
  
  
  ## Check for any NA/empty cases in the input dataframe
  #checkemptycell <- !any(is.na((input_parameters)))
  #if(checkemptycell == "FALSE") stop("Missing at least one variable in the input parameters ")
  
  ## Check if every row of the input df has 4 cases (ie 4 ",")
  ### TO DO
  
  # Using VROOM
  vroomdiag <-vroom::problems(input_parameters)
  if(nrow(vroomdiag) != 0) stop("Missing at least one variable in the input parameters ")
}








