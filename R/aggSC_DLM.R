temporal_lags<-function(burnin,
                        samples,
                        thin,
                        chains,
                        regularize,
                        dic,
                        n_full,
                        n_modeling,
                        y_modeling,
                        offset,
                        z,
                        x,
                        lag){
  
  ##############
  #Packages
  ##############
  require(rjags)
  
  #########################################################
  #DIC/chain Check
  #########################################################
  if((dic == 1) & (chains == 1)){
    
    print("Must have more than one chain to calculate DIC")
    return(NA)
    
  }
  
  #####################################
  #Upper Bound Value
  #Weight at Closest Lag can Equal 0.01
  #####################################
  upper<- -log(0.01)/1.00
  
  #####################################################################################
  #Statistical Model (No Regularization) 
  #####################################################################################
  if(regularize == 0){
    
    model_string<-"

  model{

  for(t in 1:(n_full - lag)){

     #Likelihood
     Y[t + lag] ~ dnegbin(p[t], 
                  r)
     p[t] <- r/(r + lambda[t]) 

     log(lambda[t]) <- offset[t + lag] + 
                       z[(t + lag), 1:p_z]%*%gamma[1:p_z] +
                       sum(lambda_piece[t, 1:p_x, 1:(lag + 1)])
                     
     for(j in 1:p_x){

        for(l in 1:(lag + 1)){
           lambda_piece[t,j,l] <- beta[j]*zeta[j,l]*x[(t + lag - l + 1), j]
           }

        }
   
     }

  #Temporal Weights
  for(j in 1:p_x){

     for(l in 1:(lag + 1)){
        zeta_temp[j,l] <- exp(-theta[j]*(l - 1))
        }
     zeta[j, 1:(lag + 1)] <- zeta_temp[j, 1:(lag + 1)]/sum(zeta_temp[j, 1:(lag + 1)])

     theta[j] ~ dunif(0.00, upper)
      
     }
  

  for(j in 1:p_z){
     gamma[j] ~ dnorm(0, 0.0001)
     }

  #beta Prior Distributions
  for(j in 1:p_x){
     beta[j] ~ dnorm(0, 0.0001)
     }

  r ~ dgamma(0.10,
             0.10)
  #Posterior Predictive Samples
  for(t in (n_modeling + 1):n_full){
     Y_pred[t - n_modeling] <- Y[t]
     }

  }
  "
    
  }
  
  #####################################################################################
  #Statistical Model (With Regularization) 
  #####################################################################################
  if(regularize == 1){
    
    model_string<-"

  model{

  for(t in 1:(n_full - lag)){

    #Likelihood
     Y[t + lag] ~ dnegbin(p[t], 
                  r)
     p[t] <- r/(r + lambda[t]) 

     log(lambda[t]) <- offset[t + lag] + 
                       z[(t + lag), 1:p_z]%*%gamma[1:p_z] +
                       sum(lambda_piece[t, 1:p_x, 1:(lag + 1)])
                    
     for(j in 1:p){

        for(l in 1:(lag + 1)){
           lambda_piece[t,j,l] <- beta[j]*zeta[j,l]*x[(t + lag - l + 1), j]
           }

        }
   
     }

  #Temporal Weights
  for(j in 1:p_x){

     for(l in 1:(lag + 1)){
        zeta_temp[j,l] <- exp(-theta[j]*(l - 1))
        }
     zeta[j, 1:(lag + 1)] <- zeta_temp[j, 1:(lag + 1)]/sum(zeta_temp[j, 1:(lag + 1)])

     theta[j] ~ dunif(0.00, upper)
      
     }

  for(j in 1:p_z){
     gamma[j] ~ dnorm(0, 0.0001)
     }

  #beta Priors: Horseshoe (Carvalho et al. (2009))
  for(j in 1:p_x){

     beta[j] ~ dnorm(0, beta_var_inv[j])
     beta_var_inv[j] <- 1/beta_var[j]
     beta_var[j] <- delta[j]*delta[j]*tau*tau
     delta[j] ~ dt(0,1,1)T(0,)

     }
  tau ~ dt(0,1,1)T(0,)
  r ~ dgamma(0.10,
           0.10)
  #Posterior Predictive Samples
  for(t in (n_modeling + 1):n_full){
     Y_pred[t - n_modeling] <- Y[t]
     }

  }
  "
    
  }
  
  ##################################################################
  #Model Fitting
  ##################################################################
  model_jags<-jags.model(textConnection(model_string),
                         data=list('n_full' = n_full,
                                   'n_modeling' = n_modeling,
                                   'Y' = y_modeling, 
                                   'offset' = offset,
                                   'z' = z,
                                   'x' = x,
                                   'p_z' = ncol(z),                              
                                   'p_x' = dim(x)[2],
                                   'lag' = lag,
                                   'upper' = upper),
                         n.chains = chains,
                         n.adapt = burnin)  
  
  if(regularize == 0){
    posterior_samples<-coda.samples(model_jags, 
                                    variable.names = c("Y_pred",
                                                       "gamma",
                                                       "beta",
                                                       "theta"),
                                    thin = thin,
                                    n.iter = samples)
  }
  
  if(regularize == 1){
    posterior_samples<-coda.samples(model_jags, 
                                    variable.names = c("Y_pred",
                                                       "gamma",
                                                       "beta",
                                                       "delta",
                                                       "tau",
                                                       "theta"),
                                    thin = thin,
                                    n.iter = samples)
  }
  
  dic_info<-NA
  if(dic == 1){
    dic_info<-dic.samples(model_jags, 
                          type = "pD",
                          n.iter = samples,
                          thin = thin)
    
  }
  
  return(list(posterior_samples,
              dic_info))
  
}