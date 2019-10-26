
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
  
  # data for hashing time to repair
  data_repair_time = eventReactive(input$gen_rbd, {
    rbind(
      data.frame(components = data_simulation()$components, time_to_repair = data_simulation()$time_to_repair),
      data_simulation() %>% tidyr::drop_na(redundant_fr) %>%
        select(components = redundant_id, time_to_repair)
      )
  })
  
  # plot component name
  plot_legend_dictionary = eventReactive(input$gen_rbd, {
    plot_legend_input = rbind(
      data.frame(key = data_simulation()$components, value = data_simulation()$components_input),
      data_simulation() %>% tidyr::drop_na(redundant_fr) %>% 
        select(key = redundant_id, value = components_input)
    )
    plot_legend_dictionary = hash(key = plot_legend_input$key, value = plot_legend_input$value)
    
  })
  
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
  
  avsys = function(ttf_turbine){ 
    maintenance_duration = hash(data_repair_time()$components, data_repair_time()$time_to_repair)
    res = data.frame(rep = double(), fc_afrep = double(), cumt = double()) # multiple rows
    sumr = data.frame(reptime = double(), failnum = double(), tottime = double()) # single row
    repsum = data.frame(fc_afrep_id = double())
    # time_res = data.frame(residual_time_prod = double())
    # prod_res = data.frame(cumt_prod = double())
    
    if(ttf_turbine >= input$mission_time) {
      res[1L,] = c(0, 0, 0); 
      sumr[1L,] = c(0, 0, 0); 
      repsum[1L,] = c(NA); 
      # time_res[1L,] = c(0) ;
      # prod_res[1L,] = c(0)
    }   
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
        afrep_sim[, i] = ifelse(data_redundancy_status()[1, i] == "no redundancy", afrep_sim_main[, i],
                                ifelse(data_redundancy_status()[1, i] == "hot redundancy", 
                                       ifelse(afrep_sim_main[, i] > afrep_sim_red[, i], 
                                              afrep_sim_main[, i], afrep_sim_red[, i]),
                                       afrep_sim_main[, i] + afrep_sim_red[, i]))
      }
      
      fc_afrep = min(afrep_sim)
      fc_afrep_id = colnames(afrep_sim)[which.min(afrep_sim)]
      
      #################
      # reparation duration after first failure
      # hash dictionary
      # maintenance duration
      
      rep = values(maintenance_duration, fc_afrep_id)
      
      
      # result 
      cumt = (rep+fc_afrep) # cumulative time after reparation and follow up random simulation
      res[i,] = c(rep, fc_afrep, cumt) # summary of reparation, id of other failure, ttf of other failure, cumulative time)
      tottime = sum(res$cumt) # cummulative operation time to be compared with mission time
      reptime = sum(res$rep) # total reparation time
      failnum = length(res$rep) # total reparation numbers
      
      # production level after reparation
      # residual_time_prod = ifelse((ttf_turbine + tottime) < mission_time, 
      #                             fc_afrep,
      #                             ifelse((ttf_turbine + rep + tottime - cumt) > mission_time, 0,
      #                                    mission_time - ((tottime - cumt) + ttf_turbine + rep)))
      # prod_afrep = residual_time_prod*prod_daily
      # prod_afrep = fc_afrep_total %>% 
      #   select(line_a:line_b) %>% 
      #   mutate(prod_line_a = (ifelse(line_a >= residual_time_prod, residual_time_prod, line_a))*prod_daily*0.5,
      #          prod_line_b = (ifelse(line_b >= residual_time_prod, residual_time_prod, line_b))*prod_daily*0.5) %>% 
      #   mutate(prod_tot = sum(select(., prod_line_a:prod_line_b)))
      # cumt_prod = prod_afrep
      # time_res[i,] = c(NA, NA, residual_time_prod)
      # prod_res[i,] = c(NA, NA, cumt_prod, )
      # totprod = sum(y + sum(prod_res$cumt_prod))
      
      
      sumr[1L,] = c(reptime, failnum, tottime) # summary of reparation time, failure number, and total operating time
      repsum[i,] = c(fc_afrep_id) # summary of components contributing to system failure
      print(repsum)
      if (tottime+ttf_turbine >= input$mission_time) break
      i=i+1L
    }
    }
    return(list(res,sumr,repsum))
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
  
  mean_aval_sys = eventReactive(input$simulate, {
    mean(aval_sys[,1]) 
  })
  
  # calculate percentage of system sucess without failure
  rep_no_sys = eventReactive(input$simulate, {
    melt(sum_av_sys()[1:input$sim_number,2]) #number of failure for each iterations
  }) 
  
  sum_rep_no_sys = eventReactive(input$simulate, {
    (table(rep_no_sys()[,1])) # distribution of failure numbers and without failures
  }) 
  
  
  # comparison of number of failure (distribution of failure number for n simulation)
  mean_rep_no_sys = eventReactive(input$simulate, {
    mean(rep_no_sys()[,1])  # mean  number of failure during n iteration
  }) 
  
  sd_rep_no_sys = eventReactive(input$simulate, {
    sd(rep_no_sys()[,1]) # standard deviation
  }) 
  
  # distribution of components causing the system failure
  sum_fc_sys = eventReactive(input$simulate, {
    t(lapply(av_sys(), "[[", 3))
  }) 
  
  fc_comp_sum = eventReactive(input$simulate, { 
    unlist((lapply(sum_fc_sys(), "[[", 1)))
  })
  
  fc_total = eventReactive(input$simulate, { 
    as.data.frame(table(c(fc_comp_sum(), ttf_turbine_id()))) 
    # taking which component cause the failure for each iterations step 3
  })
  
  plot_legend = eventReactive(input$simulate, { 
    as.factor(values(plot_legend_dictionary(),  fc_total()$Var1))
  })
  
  fc_resume = eventReactive(input$simulate, { 
    as.data.frame(fc_total(), plot_legend = plot_legend()) %>%
      mutate(failure_pct = (round(100*Freq/sum(Freq),1))) %>% 
      arrange(failure_pct)
  })
  
  
  
  # render plot
  output$plot_reliability = renderPlotly({
    plot_ly() %>% 
      add_trace(x = mission_time_vec(), y = r_sys_det(), 
                type = 'scatter', mode = "lines", name = "Deterministic Reliability") %>%
      add_trace(x = mission_time_vec(), y = r_sys_sim(), 
                type = 'scatter', mode = "lines", name = "Simulated Reliability") %>%
      layout(yaxis = list(title = "Reliability (%)"), xaxis = list(title = "Year"), hovermode = 'compare')
  })
  
  output$plot_maintenance = renderPlotly({
    plot_ly(x = as.integer(names(sum_rep_no_sys())),
            y = (round(100*sum_rep_no_sys()/sum(sum_rep_no_sys()), 1)),
            name = paste("Failure Numbers Probability for", input$mission_time, "year(s)") , type = "bar") %>%
      layout(xaxis = list(title = "Number of Failures", dtick = 1),
             yaxis = list(title = "Probability of Occurence (%)", dtick= 5),
             title = paste(as.character(mean_rep_no_sys()), 
                           "Expected Failure Numbers", "<br>", "for", tail(mission_time_vec(), n=1), "year(s)"),
             font = list(size = 10))
  })
  
  output$plot_failure_cause = renderPlotly({
    plot_ly(fc_resume(), x = ~failure_pct, y = ~plot_legend, 
            type="bar", orientation = 'h') %>%
      layout(xaxis=list(title="System Failure  (%)"), 
             yaxis=list(title = "", categoryorder = "array", categoryarray = fc_resume$plot_legend), 
             title = "Distribution of Components Causing the System Failure")
  })
  
  # debugging
  ########
  output$data_1 <- renderDataTable({
    fc_total()
  }) 
  
  output$data_2 <- renderDataTable({
    data.frame(ttf_turbine())
  })
  
  output$data_3 <- renderDataTable({
    data.frame(ttf_turbine_id())
    
  })
  
  output$text_output = renderText({
    str(plot_legend_dictionary()) 
  })
  ########
  
  
}


