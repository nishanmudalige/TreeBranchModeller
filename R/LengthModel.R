# library(flexsurv)
# library(crayon)

#' @export
getAIC_length = function(x){
  k = length(coefficients(x))
  LL = x$loglik
  AIC = 2*k - 2*LL
  return(AIC)
}


#' @importFrom stats as.formula
#' @export
fit_length = function(base, var, data, trace = F){
  
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
  
  current_model = flexsurvreg(formula, data = data, dist = "weibull")
  
  aic = getAIC_length(current_model)
  
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




systematic_fit_length = function(base, vars, data, trace = F){
  
  bgcfblue = make_style("cornflowerblue", bg = TRUE)
  bglightblue = make_style("lightblue", bg = TRUE)
  bgpeach = make_style("peachpuff", bg = TRUE)
  bglightgreen = make_style("lightgreen", bg = TRUE)
  bglightpink = make_style("lightpink", bg = TRUE)
  purple = make_style("purple2")
  
  previous_best_model = fit_length(base, 1, data, trace = F)
  previous_best_aic = previous_best_model$aic
  
  candidate = list()
  for(i in 1:length(vars)){
    candidate[[i]] = fit_length(base, vars[i], data, trace)
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
      
      # banner = paste(rep("*", sum(nchar(winner$formula)) ), collapse="") # +2 is an offset
      # all_lines = deparse(winner$formula, "\n")
      all_lines = deparse(winner$formula)
      longest_index = which.max(nchar(all_lines))
      longest_line = all_lines[longest_index]
      
      banner = paste(rep("*", nchar(longest_line) + 0), collapse="") # change 0 to an integer for an offset
      
      writeLines( ("Since (Lowest AIC of Current Candidates) < (Previous lowest AIC), ") %+%
                    bglightgreen$bold(black("a new variable has been added"))  %+% 
                    (" to the model.\n") %+% 
                    ("The best model is now"))
      cat(bgpeach(black(banner),"\n"))
      # cat(bgpeach(black(deparse(winner$formula)),"\n"))
      cat(bgpeach(black(deparse(winner$formula)),"\n"))
      cat(bgpeach(black(banner),"\n"))
      
    } else {
      
      # banner = paste(rep("*", sum(nchar(previous_best_model$formula)) + 2), collapse="") # +2 is an offset
      # all_lines = deparse( (previous_best_model$model)$concat.formula, "\n")
      all_lines = deparse( (previous_best_model$model)$concat.formula)
      longest_index = which.max(nchar(all_lines))
      longest_line = all_lines[longest_index]
      
      banner = paste(rep("*", nchar(longest_line) + 0), collapse="") # change 0 to an integer for an offset
      
      # \u2265 is unicode for >=
      writeLines( ("Since (Lowest AIC of Current Candidates) \u2265 (Previous lowest AIC), ") %+%
                    bglightpink$bold(black("a new variable has not been added"))  %+% 
                    (" to the model.\n") %+% 
                    ("The best model is still"))
      cat(bgpeach(black(banner),"\n"))
      # cat(bgpeach(black(deparse( (previous_best_model$model)$concat.formula )),"\n"))
      cat(bgpeach(black(deparse((previous_best_model$model)$concat.formula)), "\n"))
      cat(bgpeach(black(banner),"\n"))
      
    }
  }
  
  return(winner)
  
}



#' @export
systematic_fit_component_length = function(base, vars, data, trace = F){
  
  #initial fit_length
  # base = as.formula(base)
  # winning_model = flexsurvreg(base, data = data, dist = "weibull")
  # base_coeffs = names(coefficients(winning_model)[-c(1,2)])
  winning_model = fit_length(base, NULL, data, trace = F)
  base = as.formula(base)
  base_coeffs = names(coefficients(winning_model$model)[-c(1,2)])
  
  
  # Used for McFadden R-squared
  # Lnull = getAIC_length(winning_model)
  
  # Initialize
  previous_AIC = getAIC_length(winning_model$model)
  current_AIC = getAIC_length(winning_model$model) 
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
    
    candidate_model = systematic_fit_length(model_formula, candidate_vars, data, trace)
    
    previous_AIC = current_AIC
    current_AIC = candidate_model[["aic"]]
    
    if(current_AIC < previous_AIC){
      winning_model = candidate_model
    }
    
    winning_coeffs = names(coefficients(winning_model[["model"]])[-c(1,2)])
    
    candidate_vars = vars[!(vars %in% winning_coeffs)]
    
  }
  
  out = winning_model
  
  # McFadden R-squared
  # Lc = getAIC_length(winning_model)
  # McFaddenR2 =  1 - Lc/Lnull
  # out[["McFadden pseudo R-squared"]] = McFaddenR2
  
  # winning_coeffs = names(coefficients(winning_model[["model"]])[-c(1,2)])
  
  out[["coeffs"]] = winning_coeffs
  
  return(out)
  
}


systematic_fit_full_length = function(base, vars, data, 
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
  baseline = flexsurvreg(as.formula(base), data = data, dist = "weibull")
  baseline_aic = getAIC_length(baseline)
  
  # Used for generalized R-squared
  # Using the definition by Nagelkerke (Biometrika, 1991)
  Lnull = baseline$loglik # base is currently winning model
  n = nrow(data)
  # n = baseline$N
  
  if(trace == T){
    print(as.formula(base))
    writeLines(bgcfblue("The AIC of the baseline model is", baseline_aic))
  }
  
  # linear
  if(linear == T){
    current_model = systematic_fit_component_length(current_base, vars, data, trace)
    
    linear_coeffs = current_model[["coeffs"]]
    model_coefs = paste(linear_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  # quadratic and interaction
  if(quad_int == T){
    
    if(length(linear_coeffs) > 0 & hierarchical == T){
      quad_vars = paste("I(",linear_coeffs,"^2)", sep="")
      int_vars = apply(combn(linear_coeffs,2), 2, function(x){ paste(x, collapse=":") })
    } else {
      quad_vars = paste("I(",vars,"^2)", sep="")
      int_vars = apply(combn(vars, 2), 2, function(x){ paste(x, collapse=":") })
    }
    
    quad_int_vars = c(quad_vars, int_vars)
    
    current_model = systematic_fit_component_length(current_base, quad_int_vars, data, trace)
    
    quad_coeffs = current_model[["coeffs"]]
    model_coefs = paste(quad_coeffs, collapse = " + ")
    current_base = paste(base, model_coefs, sep = " + ")
  }
  
  
  # If only base model is required (i.e. linear = quad_int = F)
  if( sum(c(linear, quad_int)) ==  0 ){
    current_model = systematic_fit_component_length(base, 1, data)
  }
  
  # Generalized R-square
  Lfull = (current_model$model)$loglik
  genR2 = 1 - exp((-2/n)*(Lfull - Lnull))
  
  current_model[["Generalized R-square"]] = genR2
  
  return(current_model)
}
