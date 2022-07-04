# library(gtools)
# library(crayon)
# library(stringr)

##############################

#' @importFrom stats pnorm
#' @export
BM_LL <- function(param, u, v, mu.data){
  
  p.mu = dim(mu.data)[2]
  p = length(param)
  
  param.mu = param[1:(p-1)]
  tau = exp(param[p])
  
  # Using Identity link
  mu.i = t(param.mu) %*% t(mu.data)
  
  F.tau_mu.i = 
    pmax((sqrt(pi)/sqrt(tau)) *
           ( pnorm((1-mu.i)/sqrt(1/(2*tau))) - pnorm((-1-mu.i)/sqrt(1/(2*tau))) ), 10e-5)

  negloglik = -sum( -tau*(rowSums(u*v) - mu.i)^2 - log(F.tau_mu.i))
  
  return(negloglik)
}



#' @export
getAIC_direction <- function(x){
  
  k = length((x$par)) - 1
  ll = -x$objective
  aic = 2*k - 2*ll
  return(aic)
  
}


#' @importFrom stats nlminb
#' @export
create_model <- function(data, include_names, u, v, trace = F){
  
  must_include = cbind(1, data[,include_names])
  names(must_include) = c("intercept", include_names)
  
  init = c(rep(0, ncol(must_include)), 0.5)
  current_optim = nlminb(init, BM_LL, u=u, v=v, mu.data=must_include, hessian = T)
  
  current_optim$aic = getAIC_direction(current_optim)
  
  current_optim$variables = include_names
  
  return(current_optim)
  
}



#' @export
select_one <- function(data, include_names, u, v, trace = F){
  
  candidate_vars = names(data)[!(names(data) %in% include_names)]
  n = length(candidate_vars)
  
  must_include = cbind(1, data[,include_names])
  # must_include = as.matrix(must_include, nrow = nrow(data))
  
  names(must_include) = c("intercept", include_names)
  
  candidate_list = list()
  
  # Fix to vary initial later
  init = c(rep(0, ncol(must_include)+1), 0.5)
  
  for(i in 1:n){
    current_data = cbind(must_include, data[,candidate_vars[i]])
    
    current_optim = nlminb(init, BM_LL, u=u, v=v, mu.data=current_data, hessian = T)
    
    candidate_list[[i]] = current_optim
    
    candidate_list[[i]]$aic = getAIC_direction(current_optim)
    
    candidate_list[[i]]$variables = c(include_names, candidate_vars[i])
    
    if(trace == T){
      writeLines(paste("Adding variable", candidate_vars[i]))
      writeLines(paste("AIC =", cyan(candidate_list[[i]]$aic)))
      writeLines("---------------")
    }
    
  }
  
  get_aic = lapply(candidate_list, function(x){x[c("aic")]} )
  get_aic = unlist(lapply(get_aic, function(x){x[c("aic")]} ))
  
  return(candidate_list[[which.min(get_aic)]])
}



#' @export
select_multiple <- function(data, must_include = NULL, u, v, trace = F){
  
  bgcfblue = make_style("cornflowerblue", bg = TRUE)
  bglightblue = make_style("lightblue", bg = TRUE)
  bgpeach = make_style("peachpuff", bg = TRUE)
  bglightgreen = make_style("lightgreen", bg = TRUE)
  bglightpink = make_style("lightpink", bg = TRUE)
  purple = make_style("purple2")
  
  current_best_model = create_model(data, must_include, u, v, trace)
  
  if(trace == T){
    writeLines(bgcfblue("The baseline model contains an intercept"))
    if(!is.null(current_best_model$variables)){
      writeLines(bgcfblue("The baseline model contains", current_best_model$variables))   
    }
    writeLines(bgcfblue("The AIC of the baseline model is", current_best_model$aic))  
  }
  
  # if(ncol(data) == 1){
  #   return(current_best_model)
  # }
  
  previous_best_aic = current_best_model$aic
  current_best_aic = previous_best_aic
  
  while(current_best_aic <= previous_best_aic & length(current_best_model$variables) < ncol(data) ){
    
    winning_vars = current_best_model$variables
    
    candidate_model = select_one(data, winning_vars, u, v, trace)
    
    current_best_aic = candidate_model$aic
    
    # If nlminb returns a value of -Inf, then current_best_aic is also -Inf
    if(current_best_aic == -Inf){
      break
    }
    
    if(trace == T){
      
      added_var = tail(candidate_model$variables, 1)
      
      writeLines(bglightblue(str_pad("A round of variable selection is complete.", 50, "right")))
      writeLines(bgpeach(str_pad(paste("Previous best AIC = ", previous_best_aic), 50, "right")))
      writeLines(bgpeach(str_pad(paste("Current best AIC = ", current_best_aic), 50, "right")))
      
      if(current_best_aic < previous_best_aic){
        
        writeLines(bglightgreen(str_pad(paste("A new variable has been added.", added_var," was added."), 50, "right")))
        
      } else {
        writeLines(bglightpink(str_pad("A new variable has not been added.", 50, "right")))
      }
      
      print("----------------")
      
    }
    
    if(current_best_aic < previous_best_aic){
      
      previous_best_aic = current_best_aic
      current_best_model = candidate_model
      
    }
    
  }
  
  return(current_best_model)
  
}


#' @importFrom stats nlminb
#' @export
systematic_fit_direction <- function(data, u, v, trace = F){
  
  baseline_data = rep(1, nrow(data))
  Lnull = nlminb(c(0, 0.5), BM_LL, u=u, v=v, mu.data = baseline_data)
  Lnull = -Lnull$objective
  n = nrow(data)
  
  linear = select_multiple(data, NULL, u, v, trace)
  
  lin_var = linear$variables
  
  if(is.null(lin_var)){
    return(linear)
  }
  
  linear_terms = as.data.frame(data[, lin_var])
  names(linear_terms) = lin_var
  
  quad_terms = as.data.frame(linear_terms^2)
  names(quad_terms) = paste("I(", names(quad_terms), "^2)", sep="")
  
  if(length(lin_var)>1){
    int_lin_terms = model.matrix(~.^2, linear_terms)[,-1]
    temp_names = colnames(int_lin_terms)
    
    int_lin_terms = as.data.frame(int_lin_terms)
    names(int_lin_terms) = temp_names  
  } else {
    int_lin_terms = linear_terms
  }
  
  all = cbind(int_lin_terms, quad_terms)
  
  out = select_multiple(all, lin_var, u, v, trace)
  
  Lfull = -out$objective
  genR2 = 1 - exp((-2/n)*(Lfull - Lnull))
  
  out["R2"] = genR2
  
  return(out)
}
