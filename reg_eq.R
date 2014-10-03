reg_eq <- function(lin_mod){
  
  ## Find the number of regressors used
  regressors <- length(lin_mod$coefficients)

  ## Start by setting up the TeX-style formatting, adding the first term
  pretty_eq <-   paste('The equation of this regression is $', '\x5c', 'hat{y} = ', format(lin_mod$coefficients[1], digits = 4), sep = '')
  
  ## expl will label each variable  
  expl <- paste(' where $\\hat{y}$ is the mean estimator of `', all.vars(formula(lin_mod))[1], '`', sep = '')
  
  for(i in 2:regressors){
    # If the coefficients are larger than 0, put a '+' between them. If they aren't, the '-' will come from
    # the sign of the coefficient
    if(lin_mod$coefficients[i] > 0) {
      pretty_eq <- paste(pretty_eq, ' + ', format(lin_mod$coefficients[i], digits = 4), 'x_', i-1, sep = '')
    } else {
      pretty_eq <- paste(pretty_eq, format(lin_mod$coefficients[i], digits = 4), 'x_', i-1, sep = '')
    }
    
    # Prevent awkward commas after 'and'
    if(i < regressors) {
      expl <- paste(expl, ',', sep = '')
    }
    
    # Add to the explanation as you add to the equation
    expl <- paste(expl, ' $x_', i-1 , '$ is `', rownames(summary(lin_mod)$coefficients)[i], '`', sep = '')
   
    # Trying to add an 'and' before the last variable listing in the explanation
    if(i == regressors - 1) {
      expl <- paste(expl, ', and', sep = '')
    }
  }
  # Put the equation and the explanation together, return it for pretty printing in R markdown!
  paste(pretty_eq, '$,', expl, '.', sep = '')
}