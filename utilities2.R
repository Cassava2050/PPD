

plot_res <- function(model, resp, colorvar = NULL){
  model_res <- model
  varres <- VarE(model_res)
  tmp <- lme4_res(model_res, T) %>%
    mutate(Index = 1:nrow(.),
           Residuals = residuals(model_res),
           l = -3*sqrt(varres), u = 3*sqrt(varres))
  
  #View(tmp) ######
  if(is.null(colorvar)){
    a <- tmp %>% ggplot(aes(x = Index , y = Residuals  ))
  } else {
    a <- tmp %>% ggplot(aes(x = Index , y = Residuals, color = .data[[colorvar]]  ))
  }
  
  a <-  a +
    geom_point(size = 3, alpha = 0.5)+
    geom_hline(aes(yintercept = u), linetype = 2, color = "black", size = 1) +
    geom_hline(aes(yintercept = l), linetype = 2, color = "black", size = 1) +  
    geom_hline(aes(yintercept = 0), linetype = 2)    +
    labs(x="Index", y="Residuals",
         title=paste0(resp," - Residuals"),
         subtitle= NULL) +
    theme_ipsum(base_size = 12) +
    theme( legend.position = 'bottom')
  return(a)
}


"update_asreml" <- function(mod){
  if(isTRUE(mod$converge)) return(mod)
  tmp <- mod$converge
  i = 1
  while(isFALSE(tmp)){
    print(paste("iteration:", i))
    mod <- update(mod)
    tmp <- mod$converge
    i = i + 1
  }
  return(mod)
}
