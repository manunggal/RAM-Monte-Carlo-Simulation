
server = function(input, output, session) {
  
  # vector of time (year)
  mission_time_vec = eventReactive(input$simulate, {
    c(1:round(input$mission_time, digits = 0))
  })
  
  # main data input
  data = reactive({
    if (is.null(input$hot)) {
      DF = data.frame(components_input = paste0(rep("component_", 10), 1:10),
                      failure_rate = as.numeric(rep(0.01, 10)),
                      time_to_repair = as.numeric(rep(8, 10)),
                      redundancy = factor(rep("no redundancy", 10),
                                          levels = c("no redundancy", "hot redundancy", "cold redundancy")),
                      activate = rep(TRUE, 10)) %>% 
        mutate_at(vars(components_input), as.character)
    } else {
      DF = hot_to_r(input$hot)
    }
    DF
  })
  
  
  output$hot <- renderRHandsontable({
    DF <- data()
    rhandsontable(DF)
    # rhandsontable(DF, useTypes = FALSE, selectCallback = TRUE)
  })
  
  data_simulation = eventReactive(input$gen_rbd, {
    data() %>% dplyr::filter(activate == TRUE) %>% 
      mutate(redundant_id = paste0(components_input, "_B"),
             redundant_fr = ifelse(redundancy != "no redundancy", failure_rate, NA),
             components = ifelse(redundancy != "no redundancy", paste0(components_input, "_A"), components_input)) 
  })
  
  data_diagram = eventReactive(input$gen_rbd, {
    data_simulation() %>% dplyr::filterfilter(activate == TRUE) %>%
      mutate(upstream_conn = ifelse(redundancy != "no redundancy", TRUE, FALSE),
             downstream_conn = ifelse(redundancy != "no redundancy", TRUE, FALSE),
             diagram_id = ifelse(redundancy != "no redundancy", paste(components, "B"), NA)) %>% 
      select(upstream_conn, downstream_conn, components, redundant_id, diagram_id)
    
  })
  
  # pivot table (failure rate data)
  # main component failure rate data
  data_sim_main = eventReactive(input$gen_rbd, {
    data_simulation() %>% select(components, failure_rate) %>% 
      pivot_wider(names_from = components, values_from = failure_rate)
  })
  
  # redundant component failure rate data
  data_sim_red = eventReactive(input$gen_rbd, {
    data_simulation() %>% select(redundant_id, redundant_fr) %>% 
      pivot_wider(names_from = redundant_id, values_from = redundant_fr)
  })
  
  # redundancy option
  data_redundancy_status = eventReactive(input$gen_rbd, {
    data_simulation() %>% select(components, redundancy) %>% 
      pivot_wider(names_from = components, values_from = redundancy) %>% 
      slice(rep(1, input$sim_number))
  })
  
  # hash dictionary
  # maintenance duration
  maintenance_duration = eventReactive(input$gen_rbd, {
    hash(data()$components, data()$time_to_repair)
  })
  
  # plot component name
  
  # simulate time to failure
  data_ttf = eventReactive(input$simulate, {
    data_ttf = ttf.formula(data_sim_main(), data_sim_red(), data_redundancy_status(), input$sim_number)
  })
  
  # collect result, time to failure turbine
  ttf_turbine = eventReactive(input$simulate, {
    cbind(apply(data_ttf(), 1, min))
  })
  
  # collect result, component with lowest time to failure
  ttf_turbine_id = eventReactive(input$simulate, {
    cbind(colnames(data_ttf())[apply(data_ttf(), 1, which.min)])
  })
  
  avsys = function(ttf_turbine)
  {
    res = data.frame(rep = double(), fc.afrep = double(), cumt = double())
    sumr = data.frame(reptime = double(), failnum = double(), tottime = double(), totprod = double())
    repsum = data.frame(fc_afrep_id = double())
    time_res = data.frame(residual_time_prod = double())
    prod_res = data.frame(cumt_prod = double())
    
    if(ttf_turbine >= tail(mission_time_vec(), n=1)) {
      res[1L,] = c(0, 0, 0); 
      sumr[1L,] = c(0, 0, 0, 0); 
      repsum[1L,] = c(NA); 
      time_res[1L,] = c(0) ; 
      prod_res[1L,] = c(0)} # #sum.id=c(0) summary of reparation  
    else
    {i = 1L
    repeat
    {
      # reparation time
      
      
      #ttf components after reparation
      #--------------------------
      
      afrep_sim_main = (-1/(data_sim_main()/365))*log(1-(c(runif(length(data_sim_main())))))
      afrep_sim_red = (-1/(data_sim_red()/365))*log(1-(c(runif(length(data_sim_red())))))
      
      afrep_sim = afrep_sim_main
      
      for(i in 1:ncol(afrep_sim)){
        afrep_sim[, i] = ifelse(data_redundancy_status()[1, i] == "no redundancy", afrep_sim_main()[, i],
                                ifelse(data_redundancy_status()[1, i] == "hot redundancy", 
                                       ifelse(afrep_sim_main()[, i] > afrep_sim_red()[, i], afrep_sim_main()[, i], afrep_sim_red()[, i]),
                                       afrep_sim_main()[, i] + afrep_sim_red()[, i]))
        
      }
      
      
      fc_afrep = min(afrep_sim)
      fc_afrep_id = colnames(afrep_sim)[which.min(afrep_sim)]
      
      
      #################
      # reparation duration after first failure
      
      rep = values(maintenance_duration(), fc_afrep_id)
      
      
      # result 
      cumt = (rep+fc_afrep) # cumulative time after reparation and follow up random simulation
      res[i,] = c(rep, fc_afrep, cumt, NA) # summary of reparation, id of other failure, ttf of other failure, cumulative time)
      tottime = sum(res$cumt) # cummulative operation time to be compared with mission time
      reptime = sum(res$rep) # total reparation time
      failnum = length(res$rep) # total reparation numbers
      
      # production level after reparation
      residual_time_prod = ifelse((ttf_turbine + tottime) < tail(mission_time_vec(), n=1), 
                                  fc_afrep,
                                  ifelse((ttf_turbine + rep + tottime - cumt) > tail(mission_time_vec(), n=1), 0,
                                         tail(mission_time_vec(), n=1) - ((tottime - cumt) + ttf_turbine + rep)))
      prod_afrep = residual_time_prod*prod_daily
      # prod_afrep = fc_afrep_total %>% 
      #   select(line_a:line_b) %>% 
      #   mutate(prod_line_a = (ifelse(line_a >= residual_time_prod, residual_time_prod, line_a))*prod_daily*0.5,
      #          prod_line_b = (ifelse(line_b >= residual_time_prod, residual_time_prod, line_b))*prod_daily*0.5) %>% 
      #   mutate(prod_tot = sum(select(., prod_line_a:prod_line_b)))
      cumt_prod = prod_afrep
      time_res[i,] = c(NA, NA, residual_time_prod)
      prod_res[i,] = c(NA, NA, cumt_prod, )
      totprod = sum(y + sum(prod_res$cumt_prod))
      
      
      sumr[1L,] = c(reptime, failnum, tottime, totprod) # summary of reparation time, failure number, and total operating time
      repsum[i,] = c(fc_afrep_id, NA, NA, NA, NA) # summary of components contributing to system failure
      print(repsum)
      if (tottime+ttf_turbine >= tail(mission_time_vec(), n=1)) break
      i=i+1L
    }
    }
    return(list(res,sumr,repsum, time_res, prod_res))
  }
    
  # simulate repair and maintenance
  av_sys = eventReactive(input$simulate, {
    withProgress(message = 'Simulation in progress',
                 detail = 'Patience is a virtue...', value = 0, {
                   for (i in 1:10) {
                     incProgress(1/10)
                     av_sys = map(ttf_turbine(), avsys)
                   }
                 })
    
    av_sys 
    
  })
  
  sum_av_sys = eventReactive(input$simulate, {
    data = t(sapply(av_sys(), "[[", 2))
    
  })
  
  aval_sys = eventReactive(input$simulate, {
    melt(lapply(sum_av_sys()[1:input$sim_number, 1], function(x) ((tail(mission_time_vec(), n=1)-x)/tail(mission_time_vec(), n=1)))) #calculate availability for each iteration
  })
  
  
  # debugging
  ########
  output$data_1 <- renderDataTable({
    data_simulation()
  }) 
  
  output$data_2 <- renderDataTable({
    data.frame(ttf_turbine())
  })
  
  output$data_3 <- renderDataTable({
    data.frame(ttf_turbine_id())
    
  })
  
  output$text_output = renderText({
    str(av_sys()) 
  })
  ########
  
  
}


