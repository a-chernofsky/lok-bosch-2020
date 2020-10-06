##################################################################
#
# ie_ybin_mcont: Calculate the organic indirect effect for a binary 
#           outcome with a continuous mediator with an assay limit
#            (Lok & Bosch 2020)
#
#
#################################################################



# Y: binary outcome variable
# M: continuous mediator variable
# BAL: Indicator of whether M is below assay limit
# C: pre-treatment mediator-outcome common causes
# shift: treatment mediator distribution shift
# AL: Assay limit

ie_ybin_mcont <- function(Y, M, BAL, C, shift, AL){
  #create data.frame
  dat <- data.frame(cbind(Y, M0 = M, MI0 = BAL, C))
  
  #fit outcome model
  out.model <- glm(data = dat, Y ~ MI0 + M0:MI0 + C, 
                   family = binomial())
  
  M1 <- dat$M - shift
  MI1 <- as.numeric(M1 > AL)
  #predicted outcome probabilities for treated 
  dat$py1 <- predict(out.model, 
                     newdata = data.frame(M = M1,
                                          C = dat$C, 
                                          MI0 = MI1),
                     type = "response")
  
  #indirect effect
  mean(dat$py1 - dat$Y)
}