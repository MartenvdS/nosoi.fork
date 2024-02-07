library(nosoiMT)
#library(raster)
#library(data.table)
library(dplyr)

#detach("package:profvis", unload = TRUE)
# Benchmark parameters / set up -----------------

# Host A, Vertebrate/Humans -------------------------------------------------------------

#pExit
p_Exit_fct_human <- function(t){
  as.numeric(t>=6)
}

#nContact
n_contact_fct_human <- function(t){rpois(1,2.1)}
# n_contact_fct_human <- function(t,n){rpois(n,2.1)}
#pTrans
alt_p_Trans_fct_human <- function(t) {
  p <- switch(t,
              0, 0,
              1, rbeta(1, 19.80177, 3.558925),
              2, 1,
              3, rbeta(1, 25.94549, 1.212406),
              4, rbeta(1, 17.05153, 15.58698),
              5, rbeta(1, 0.2991454, 11.36752),
              0)  #for t >= 6
  return(p)

}

p_Trans_fct_human <- function(t) {
  if(t == 0) p = 0
  if(t == 1) p = rbeta(1,19.80177,3.558925)
  if(t == 2) p = 1
  if(t == 3) p = rbeta(1,25.94549,1.212406)
  if(t == 4) p = rbeta(1,17.05153,15.58698)
  if(t == 5) p = rbeta(1,0.2991454,11.36752)
  if(t >= 6) p = 0
  return(p)
}
# Host B, Mosquitoes --------------------------------------------------------------------

#nContact
nContact_fct_mosquito <- function(t){as.numeric(runif(1)<0.7)}

#pTrans
p_Trans_fct_mosquito <- function(t, t_EIP){
  as.numeric(t > t_EIP)
}

t_EIP_FrenchPolynesia <- function(t){exp(rlogis(t, location = log(16.60535), scale = 1/-10.828220))} #Changed to remove STAR package
param_pTrans_mosquito <- list(t_EIP=t_EIP_FrenchPolynesia)

# Simulator -----------------------
simulator <- function(param){
  p_Exit_fct_mosquito <- function(t){return(param)}

  SimulationDual <- nosoiSim(type="dual", popStructure="none",
                             length.sim=100,
                             max.infected.A=100000,
                             max.infected.B=1000000,

                             init.individuals.A=1,
                             init.individuals.B=0,

                             pExit.A=p_Exit_fct_human,
                             param.pExit.A=NA,
                             timeDep.pExit.A=FALSE,
                             nContact.A=n_contact_fct_human,
                             param.nContact.A=NA,
                             timeDep.nContact.A=FALSE,
                             pTrans.A=p_Trans_fct_human,
                             param.pTrans.A=NA,
                             timeDep.pTrans.A=FALSE,
                             prefix.host.A="H",

                             pExit.B=p_Exit_fct_mosquito,
                             param.pExit.B=NA,
                             timeDep.pExit.B=FALSE,
                             nContact.B=nContact_fct_mosquito,
                             param.nContact.B=NA,
                             timeDep.nContact.B=FALSE,
                             pTrans.B=p_Trans_fct_mosquito,
                             param.pTrans.B=param_pTrans_mosquito,
                             timeDep.pTrans.B=FALSE,
                             prefix.host.B="M",

                             print.progress=FALSE)

  # Internal data analysis ----------------------------
  Data_A <- SimulationDual$host.info.A$table.hosts
  Data_A <- subset(Data_A, select = c(hosts.ID, inf.by))

  # Data analysis 1 - Pseudo S0
  frequency_table <- table(Data_A$inf.by)
  result_table <- data.frame(hosts.ID = (names(frequency_table)),
                             Frequency = as.numeric(frequency_table))

  SS_1 <- mean(result_table$Frequency)

  # Data analysis 2 - Pseudo H
  result_table <- result_table[order(-result_table$Frequency), ]
  result_table$Cumulative <- cumsum(result_table$Frequency)

  SS_2 <- (which(result_table$Cumulative >= 0.5 * sum(result_table$Frequency))[1]) / length(result_table$Frequency)

  # Data analysis 3 - Variance
  SS_3 <- var(result_table$Frequency)

  # Data analysis 4 - IQR

  SS_4 <- IQR(result_table$Frequency)

  # List of summary statistics
  results <- c(SS_1, SS_2, SS_3, SS_4)
  return(results)
}
#edit(nosoi:::endMessageText)

# Benchmark -------------------------
# library(profvis)
# profvis({
  # system.time({
    parameter <- 0.1
    i <- 1
  simulated_data <- list()
    # for (i in 1:10){
      set.seed(1024)
      simulated_data[[i]] <- simulator(parameter)
    # }
  # })
# })
