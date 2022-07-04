#' @export
fit_position = function(base, k = 1, var, data, trace = F){
  
  oldw <- getOption("warn")
  options(warn = -1)
  
  ##
  
  if(!is.null(var)){
    formula = paste(base, var, sep = " + ")  
    formula = as.formula(formula)
  } else {
    formula = paste(base)  
    formula = as.formula(formula)
  }
  
  # Print model formula
  if(trace == T){
    print(formula)  
  }
  
  # current_model = glm(formula, data = data, family = "poisson")
  # current_model = betamix(formula, data = data, k = 1:2)
  current_model = betamix(formula, data = data, k = k)
  
  aic = AIC(current_model)
  
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


#' @export
systematic_fit_position = function(base, k, vars, data, trace = F){
  
  bgcfblue = make_style("cornflowerblue", bg = TRUE)
  bglightblue = make_style("lightblue", bg = TRUE)
  bgpeach = make_style("peachpuff", bg = TRUE)
  bglightgreen = make_style("lightgreen", bg = TRUE)
  bglightpink = make_style("lightpink", bg = TRUE)
  purple = make_style("purple2")
  
  previous_best_model = fit_position(base, k, NULL, data, trace = F)
  previous_best_aic = previous_best_model$aic
  
  candidate = list()
  for(i in 1:length(vars)){
    candidate[[i]] = fit_position(base, k, vars[i], data, trace)
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
      
      all_lines = deparse((previous_best_model$formula))
      longest_index = which.max(nchar(all_lines))
      longest_line = all_lines[longest_index]
      
      banner = paste(rep("*", nchar(longest_line) + 0), collapse="") # change 0 to an integer for an offset
      
      # \u2265 is unicode for >=
      writeLines( ("Since (Lowest AIC of Current Candidates) \u2265 (Previous lowest AIC), ") %+%
                    bglightpink$bold(black("a new variable has not been added"))  %+% 
                    (" to the model.\n") %+% 
                    ("The best model is still"))
      cat(bgpeach(black(banner),"\n"))
      cat(bgpeach(black( deparse((previous_best_model$formula)) ), "\n"))
      cat(bgpeach(black(banner),"\n"))
      
    }
  }
  
  # winner[["prev_best"]] = previous_best_model
  return(winner)
  
}



##############################

#' @export
systematic_fit_component_position = function(base, k = 1, vars, data, trace = F){
  
  winning_model = fit_position(base, k, NULL, data, trace = F)
  base = as.formula(base)
  # base_coeffs = names(coefficients(winning_model)[-1])
  # base_coeffs = colnames(coefficients(winning_model$model))[-c(1,2)]
  base_coeffs = colnames(coefficients(winning_model$model))[!(colnames(coefficients(winning_model$model)) %in% c("(Intercept)", "(phi)_(Intercept)"))]
  
  previous_AIC = winning_model$aic
  current_AIC = winning_model$aic
  winning_coeffs = vector()
  
  candidate_vars = vars
  initial = T
  
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
    
    candidate_model = systematic_fit_position(model_formula, k, candidate_vars, data, trace)
    
    previous_AIC = current_AIC
    current_AIC = candidate_model[["aic"]]
    
    if(current_AIC < previous_AIC){
      winning_model = candidate_model
    }
    
    if( is.atomic(coef(winning_model$model)) ){
      break
    }
    
    if( sum(k) == 1 ){
      winning_coeffs = names((coef(winning_model$model))$mean)[!(names((coef(winning_model$model))$mean) %in% c("(Intercept)"))]
    } else {
      # eg k = c(1:2)
      winning_coeffs = colnames(coefficients(winning_model$model))[!(colnames(coefficients(winning_model$model)) %in% c("(Intercept)", "(phi)_(Intercept)"))]
    }
    
    candidate_vars = vars[!(vars %in% winning_coeffs)]
  }
  
  out = winning_model
  
  if( length(winning_coeffs) != 0 ){
    out[["coeffs"]] = winning_coeffs   
  }
  
  
  return(out)
  
}


##############################

#' @export
systematic_fit_full_position = function(base, k, vars, data,
                                        linear = T, quad_int = T,
                                        hierarchical = T,
                                        trace = F){
  
  current_base = base
  current_model = NULL
  linear_coeffs = NULL
  
  # baseline output
  # baseline = betamix(as.formula(base), data = data, k = 1:2)
  baseline = betamix(as.formula(base), data = data, k = 1)
  baseline_aic = AIC(baseline)
  
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
    current_model = systematic_fit_component_position(current_base, k, vars, data, trace)
    
    linear_coeffs = current_model[["coeffs"]]
    model_coefs = paste(linear_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  if(is.null(linear_coeffs)){
    return(current_model)
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
    
    current_model = systematic_fit_component_position(current_base, k, quad_int_vars, data, trace)
    
    quad_coeffs = current_model[["coeffs"]]
    model_coefs = paste(quad_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  
  # If only base model is required (i.e. linear = quad_int = F)
  if( sum(c(linear, quad_int)) ==  0 ){
    current_model = systematic_fit_component_position(base, k, vars = 1, data)
  }
  
  #Generalized R-square
  Lfull = as.numeric(logLik(current_model$model))
  genR2 = 1 - exp((-2/n)*(Lfull - Lnull))
  
  current_model[["Generalized R-square"]] = genR2
  
  return(current_model)
}

