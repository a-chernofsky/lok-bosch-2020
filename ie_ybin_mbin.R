##################################################################
#
# ie_ybin_mcont: Calculate the organic indirect effect for a binary 
#           outcome with a binary mediator (Lok & Bosch 2020)
#
#
#################################################################


# Y: binary outcome variable
# M: binary mediator variable
# C: pre-treatment mediator-outcome common causes
# factor: factor for which treatment increases the odds of 
#         Mbar being 1 

ie_ybin_mbin <- function(Y, Mbar, C, factor){
  #create data.frame
  dat <- data.frame(cbind(Y, Mbar, C))
  
  #fit outcome model for P(Y = 1 | Mbar = m, C = c, A = 0) 
  out.model <- glm(data = dat, Y ~ Mbar + C, family = binomial())
  
  #predicted outcome probabilities for treated 
  dat$pY1 <- predict(out.model, 
                     newdata = data.frame(Mbar = rep(1,nrow(dat)),
                                                     C = dat$C),
                     type = "response")
  #predicted outcome probabilites for untreated
  dat$pY0 <- predict(out.model, newdata = data.frame(Mbar = rep(0,nrow(dat)),
                                                     C = dat$C),
                     type = "response")
  
  #fit mediator model. To model P(M = 0 | C) fit mediator inverse
  med.model <- glm(data =  dat, Mbar ~ C, family = binomial())
  dat$pM0 <- med.model$fitted.values
  
  #indirect effect
  mean(dat$pY1 * (3* dat$pM0 / (1+ 2* dat$pM0)) + 
         dat$pY0 * (1 - dat$pM0)/(1+ 2* dat$pM0)) - mean(dat$Y)
}