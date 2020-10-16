fwl_function <- 
  function(data , 
           regressor_list , 
           x , 
           y ){
    require(broom)
    require(dplyr)
    
    fwl_formula_ls_x <- 
      as.formula(
        paste0(x," ~ ",paste(regressor_list,collapse = " + "))
      )
    fwl_formula_ls_y <- 
      as.formula(
        paste0(y," ~ ",paste(regressor_list,collapse = " + "))
      )
    
    fwl_model_ls_x <- 
      lm( data = data , 
          formula = fwl_formula_ls_x)
    fwl_model_ls_y <- 
      lm( data = data , 
          formula = fwl_formula_ls_y)
    
    x_resid <- 
      augment(fwl_model_ls_x) %>% 
      select(.resid) %>% 
      rename( "x_resid" = ".resid")
    y_resid <- 
      augment(fwl_model_ls_y) %>% 
      select(.resid) %>% 
      rename( "y_resid" = ".resid")
    
    fwl_vars_df <- 
      data.frame( fwl_x = x_resid , 
                  fwl_y = y_resid )
    
    return(fwl_vars_df)
  }