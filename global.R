# load library
library(shiny)
library(shinydashboard) 
library(rhandsontable)
library(plotly)
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(magrittr)
library(fBasics)
library(gridExtra)
library(reshape2)
library(hash) 
library(htmlwidgets)
library(Rfast)

# function

# ttf function
ttf.formula = function(data_sim_main, data_sim_red, data_redundancy_status, sim_number){
  
  
  # sim = main component
  # red = redundant component
  
  # generate random number to simulate time to failure
  ttf_sim_main = as.data.frame(map_dfr(data_sim_main/365, 
                                       function(x) (-1/x)*log(1-(cbind(runif(sim_number))))))
  
  ttf_sim_red = as.data.frame(map_dfr(data_sim_red/365, 
                                      function(x) (-1/x)*log(1-(cbind(runif(sim_number))))))
  
  # select ttf based on redudancy selection
  ttf_sim = ttf_sim_main
  
  for(i in 1:ncol(ttf_sim_main)){
    ttf_sim[, i] = ifelse(data_redundancy_status[, i] == "no redundancy", ttf_sim_main[, i],
                          ifelse(data_redundancy_status[, i] == "hot redundancy", ifelse(ttf_sim_main[, i] > ttf_sim_red[, i], ttf_sim_main[, i], ttf_sim_red[, i]),
                                 ttf_sim_main[, i] + ttf_sim_red[, i])) 
    
  }
  ttf_sim
  
}











