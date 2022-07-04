#' @export
fit_offspring = function(base, var, data, trace = F){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  ##
  
  if(!is.null(var)){
    formula = paste(base, var, sep = " + ")  
    formula = as.formula(formula)
  } else{
    formula = paste(base)  
    formula = as.formula(formula)
  }
  
  # Print model formula
  if(trace == T){
    print(formula)  
  }
  
  current_model = glm(formula, data = data, family = "poisson")
  
  aic = current_model$aic
  
  # print AIC
  if(trace == T){
    writeLines(paste("AIC =", cyan(aic)))
    print("--------------------")
  }
  
  out = list(current_model, formula, aic)
  names(out) = c("model", "formula","aic")
  
  ##
  options(warn = oldw)
  
  return(out)
  
}


##############################


# Function to fit_offspring one additional variable into a base model from a set vector of variables
# Loop through all possible variables and return the model with the lowest AIC
# Model returned is the (base model + one additional variable)
# Requires the fit_offspring function and its dependencies

#' @export
systematic_fit_offspring = function(base, vars, data, trace = F){
  
  bgcfblue = make_style("cornflowerblue", bg = TRUE)
  bglightblue = make_style("lightblue", bg = TRUE)
  bgpeach = make_style("peachpuff", bg = TRUE)
  bglightgreen = make_style("lightgreen", bg = TRUE)
  bglightpink = make_style("lightpink", bg = TRUE)
  purple = make_style("purple2")
  
  previous_best_model = fit_offspring(base, NULL, data, trace = F)
  previous_best_aic = previous_best_model$aic
  
  candidate = list()
  for(i in 1:length(vars)){
    candidate[[i]] = fit_offspring(base, vars[i], data, trace)
  }
  
  aic_vec = vector()
  for(i in 1:length(candidate)){
    aic_vec[i] = candidate[[i]]$aic
  }
  
  min_aic_loc = order(aic_vec)[1]
  
  winner = candidate[[min_aic_loc]]
  current_best_aic = candidate[[min_aic_loc]]$aic
  
  if(trace == T){
    writeLines(bglightblue(black("A winning model has been selected")) %+%
                 # ("\nThe previous lowest AIC was ") %+% 
                 ("\nThe AIC of the previous model ") %+% 
                 purple(previous_best_aic) %+%
                 ("\nThe lowest AIC from the current set of candidates is ") %+%
                 purple(current_best_aic) )
    
    if(current_best_aic < previous_best_aic){
      
      all_lines = deparse(winner$formula)
      longest_index = which.max(nchar(all_lines))
      longest_line = all_lines[longest_index]
      
      banner = paste(rep("*", nchar(longest_line) + 0), collapse="") # change 0 to an integer for an offset
      
      writeLines( ("Since (Lowest AIC of Current Candidates) < (Previous lowest AIC), ") %+%
                    bglightgreen$bold(black("a new variable has been added"))  %+% 
                    (" to the model.\n") %+% 
                    ("The best model is now"))
      cat(bgpeach(black(banner),"\n"))
      cat(bgpeach(black(deparse(winner$formula)),"\n"))
      cat(bgpeach(black(banner),"\n"))
      
    } else {
      
      all_lines = deparse((previous_best_model$model)$formula)
      longest_index = which.max(nchar(all_lines))
      longest_line = all_lines[longest_index]
      
      banner = paste(rep("*", nchar(longest_line) + 0), collapse="") # change 0 to an integer for an offset
      
      # \u2265 is unicode for >=
      writeLines( ("Since (Lowest AIC of Current Candidates) \u2265 (Previous lowest AIC), ") %+%
                    bglightpink$bold(black("a new variable has not been added"))  %+% 
                    (" to the model.\n") %+% 
                    ("The best model is still"))
      cat(bgpeach(black(banner),"\n"))
      cat(bgpeach(black( deparse((previous_best_model$model)$formula) ), "\n"))
      cat(bgpeach(black(banner),"\n"))
      
    }
  }
  
  # winner[["prev_best"]] = previous_best_model
  return(winner)
  
}



##############################

# Input is a base model, set of variables and data.
# Start with a base model and add a variables from the input set of variables.
# Repeat process with remaining variables unti AIC of new model is not less than AIC of previous model
# Requires the systematic_fit_offspring function and its dependencies

#' @export
systematic_fit_component_offspring = function(base, vars, data, trace = F){
  
  #initial fit_offspring
  # base = as.formula(base)
  # winning_model = glm(base, data = data, family = "poisson")
  # ### base_coeffs = names(coefficients(winning_model)[-c(1,2)])
  # base_coeffs = names(coefficients(winning_model)[-c(1,2)])
  
  winning_model = fit_offspring(base, NULL, data, trace = F)
  # winning_model = glm(base, data = data, family = "poisson")
  base = as.formula(base)
  # base_coeffs = names(coefficients(winning_model$model)[-c(1,2)])
  base_coeffs = names(coefficients(winning_model)[-1])
  
  previous_AIC = winning_model$aic
  current_AIC = winning_model$aic
  winning_coeffs = vector()
  
  candidate_vars = vars
  initial = T
  
  # for(i in 1:length(vars)){
  # Repeat if AIC of new model is lower than AIC of previous model
  # AND all variables have not been used up
  while( (current_AIC < previous_AIC & length(candidate_vars) > 0) || (initial == T) ){
    
    initial = F   # Set initial to F since we have entered the loop
    
    # Reduce, paste, deparse required since we are sending in an object 
    # which is a formula (class)
    if(length(winning_coeffs) == 0){
      model_formula = Reduce(paste, deparse(base))
    } else {
      unique_winning_coeffs = winning_coeffs[!(winning_coeffs %in% base_coeffs)]
      
      model_coefs = paste(unique_winning_coeffs, collapse = " + ")
      model_formula = paste(Reduce(paste, deparse(base)), model_coefs, sep = " + ")
    }
    
    candidate_model = systematic_fit_offspring(model_formula, candidate_vars, data, trace)
    
    previous_AIC = current_AIC
    current_AIC = candidate_model[["aic"]]
    
    if(current_AIC < previous_AIC){
      winning_model = candidate_model
    }
    
    ## winning_coeffs = names(coefficients(winning_model[["model"]])[-c(1,2)])
    ## winning_coeffs = names(coefficients(winning_model[["model"]]))
    # winning_coeffs = names(coefficients(winning_model[["model"]])[-c(1)])
    
    winning_coeffs = names(coefficients(winning_model[["model"]])[-c(1)])
    
    candidate_vars = vars[!(vars %in% winning_coeffs)]
    
  }
  
  # out = winning_model
  # if(!is.null(winning_coeffs)){
  # out[["coeffs"]] = winning_coeffs 
  # } else {
  #   out[["coeffs"]] = names(coefficients(winning_model))[-1]  
  # }
  
  out = winning_model
  out[["coeffs"]] = winning_coeffs 
  
  return(out)
  #   
}

# systematic_fit_component_offspring(base, vars, data, trace = F)
# systematic_fit_component_offspring("Nombre_fils ~ 1", vars_l2, tree_2_CT, trace = T)

##############################


# Systematic fit_offspring of variables
# First fit_offspring linear, then fit_offspring quadratic terms and interaction terms.
# Requires the systematic_fit_component_offspring function and its dependencies.
# 'base' is the base formula to enter
# 'vars' is a vector of candidate variables
# 'linear' is a logical argument. 
#     If set to TRUE, then determine the linear terms that should be included.
# 'quad_int' is a logical argument. 
#     If set to TRUE, then determine the quadratic and interaction terms That should be included.
# 'hierarchical' is a logical argument. 
#     If set to TRUE, then only use the linear terms added as candidates for the 
#     quadratic and interaction terms.
# 'Trace' is a logical argument. 
#     If set to TRUE, then display output for the selection criteria.

#' @export
systematic_fit_full_offspring = function(base, vars, data, 
                                         linear = T, quad_int = T,
                                         hierarchical = T, 
                                         trace = F){
  
  bgcfblue = make_style("cornflowerblue", bg = TRUE)
  bglightblue = make_style("lightblue", bg = TRUE)
  bgpeach = make_style("peachpuff", bg = TRUE)
  bglightgreen = make_style("lightgreen", bg = TRUE)
  bglightpink = make_style("lightpink", bg = TRUE)
  purple = make_style("purple2")
  
  current_base = base
  current_model = NULL
  linear_coeffs = NULL
  
  # baseline output
  baseline = glm(as.formula(base), data = data, family = "poisson")
  baseline_aic = baseline$aic
  
  # Used for generalized R-squared
  # Using the definition by Nagelkerke (Biometrika, 1991)
  
  # Lnull = baseline$loglik # base is currently winning model
  # n = baseline$N
  
  Lnull = as.numeric(logLik(baseline))
  n = nrow(data)
  
  if(trace == T){
    print(as.formula(base))
    writeLines(bgcfblue("The AIC of the baseline model is", baseline_aic))
  }
  
  # linear
  if(linear == T){
    current_model = systematic_fit_component_offspring(current_base, vars, data, trace)
    
    linear_coeffs = current_model[["coeffs"]]
    model_coefs = paste(linear_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  # quadratic and interaction
  if(quad_int == T){
    
    if(length(linear_coeffs) > 0 & hierarchical == T){
      quad_vars = paste("I(",linear_coeffs,"^2)", sep="")
      int_vars = NULL
      if(length(linear_coeffs) > 1){
        int_vars = apply(combn(linear_coeffs,2), 2, function(x){ paste(x, collapse=":") }) 
      }
    } else {
      quad_vars = paste("I(",vars,"^2)", sep="")
      int_vars = apply(combn(vars, 2), 2, function(x){ paste(x, collapse=":") })
    }
    
    quad_int_vars = c(quad_vars, int_vars)
    
    current_model = systematic_fit_component_offspring(current_base, quad_int_vars, data, trace)
    
    quad_coeffs = current_model[["coeffs"]]
    model_coefs = paste(quad_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  
  # If only base model is required (i.e. linear = quad_int = F)
  if( sum(c(linear, quad_int)) ==  0 ){
    current_model = systematic_fit_component_offspring(base, 1, data)
  }
  
  #Generalized R-square
  Lfull = as.numeric(logLik(current_model$model))
  genR2 = 1 - exp((-2/n)*(Lfull - Lnull))
  
  current_model[["Generalized R-square"]] = genR2
  
  return(current_model)
}
