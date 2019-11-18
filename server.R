
server = function(input, output, session) {
  start.time <- Sys.time()
  
  # simulation setting
  sim_number = eventReactive(input$simulate, {
   input$sim_number
  })
  
  # vector of time (year)
  mission_time_vec = eventReactive(input$simulate, {
    c(1:round(input$mission_time, digits = 0))
  })
  
  # main data input
  data = reactive({
    if (is.null(input$hot)) {
      DF = data.frame(components_input = paste(rep("component", 10), 1:10),
                      failure_rate = as.numeric(rep(0.1, 10)),
                      time_to_repair = as.numeric(rep(8, 10)),
                      redundancy = factor(rep("no redundancy", 10),
                                          levels = c("no redundancy", "hot redundancy", "cold redundancy")),
                      select = rep(TRUE, 10)) %>% 
        mutate_at(vars(components_input), as.character) 
    } else {
      DF = hot_to_r(input$hot)
    }
    DF
  })
  
  
  output$hot <- renderRHandsontable({
    DF <- data()
    rhandsontable(DF) %>% 
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    # rhandsontable(DF, useTypes = FALSE, selectCallback = TRUE)
  })
  
  data_simulation = eventReactive(input$gen_rbd, {
    data() %>% dplyr::filter(select == TRUE) %>% 
      mutate(component_id = ifelse(redundancy != "no redundancy", paste0(components_input, "_A"), components_input),
             redundant_id = paste0(components_input, "_B"),
             redundant_fr = ifelse(redundancy != "no redundancy", failure_rate, NA),
             components = ifelse(redundancy != "no redundancy", paste0(components_input, "_A"), components_input)) 
  })
  

  data_diagram = eventReactive(input$gen_rbd, {
    data_diagram_input = 
    data_simulation() %>% dplyr::filter(select == TRUE) %>% 
      tibble::rownames_to_column() %>% 
      # add upstream downstream switch connector
      mutate(up_switch = ifelse(redundancy != "no redundancy", paste0("up_switch_", rowname), NA),
             down_switch = ifelse(redundancy != "no redundancy", paste0("down_switch_", rowname), NA),
             di_component_id = paste0("comp_", rowname),
             di_redundant_id = ifelse(redundancy != "no redundancy", paste0("comp_", rowname, "_B"), NA)) %>% 
      # move variable one row 
      mutate(redundancy_lead = lead(redundancy),
             di_component_id_lead = lead(di_component_id),
             component_id_lead = lead(component_id),
             up_switch_lead = lead(up_switch)) %>% 
      # select relevant variable to build the diagram
      select(-c(failure_rate, time_to_repair, redundant_fr)) %>% 
      # build text for mermaid input
      mutate(mermaid_input = 
               ifelse(redundancy != "no redundancy",
                      paste(
                        paste0(up_switch, "{", " ", "}", "-->",
                               di_component_id, "[", component_id, "]"),
                        paste0(up_switch, "{", " ","}", "-->",
                               di_redundant_id, "[", redundant_id, "]"),
                        paste0(di_component_id, "[", component_id, "]", "-->",
                               down_switch, "{", " ", "}"),
                        paste0(di_redundant_id, "[", redundant_id, "]", "-->",
                               down_switch, "{", " ", "}"),
                        paste0(down_switch, "{", " ", "}", "-->",
                               ifelse(is.na(redundancy_lead),
                                      "stop((End))",
                                      ifelse(redundancy_lead != "no redundancy",
                                             paste0(up_switch_lead, "{", " ", "}"),
                                             paste0(di_component_id_lead, "[", component_id_lead, "]"))
                               )
                               
                        ),
                        sep = "\n"
                      ),
                      paste0(di_component_id, "[", component_id, "]", "-->",
                             ifelse(is.na(redundancy_lead),
                                    "stop((End))",
                                    ifelse(redundancy_lead != "no redundancy",
                                           paste0(up_switch_lead, "{", " ", "}"),
                                           paste0(di_component_id_lead, "[", component_id_lead, "]"))
                             )
                             
                      )
               )
      )
    
    # collapse text 
    diagram_input = paste(data_diagram_input$mermaid_input, collapse = "\n")
    # add start block
    diagram = paste(
      paste0("start((Start)) -->", ifelse(data_diagram_input$redundancy[1] != "no redundancy",
                                          paste0(data_diagram_input$up_switch[1], "{ }"),
                                          paste0(data_diagram_input$di_component_id[1], "[", data_diagram_input$component_id[1] ,"]"))
      ),
      diagram_input,
      sep = "\n")
    
    
    
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
  data_redundancy_status = eventReactive(input$simulate, {
    data_simulation() %>% select(components, redundancy) %>% 
      pivot_wider(names_from = components, values_from = redundancy) %>% 
      slice(rep(1, sim_number()))
  })
  
  # data for hashing time to repair
  data_repair_time = eventReactive(input$gen_rbd, {
    rbind(
      data.frame(components = data_simulation()$components, time_to_repair = data_simulation()$time_to_repair/8760),
      data_simulation() %>% tidyr::drop_na(redundant_fr) %>%
        select(components = redundant_id, time_to_repair)
      )
  })
  
  # # plot component name
  # plot_legend_input = eventReactive(input$gen_rbd, {
  #   
  #   components_a = data.frame(key = data_simulation()$components, value = data_simulation()$components_input)
  #   components_b = data_simulation() %>% tidyr::drop_na(redundant_fr) %>%
  #     select(key = redundant_id, value = components_input)
  #   plot_legend_input = bind_rows(components_a, components_b)
  #   
  #   # plot_legend_dictionary = hash(key = plot_legend_input$key, value = plot_legend_input$value)
  # 
  # })
  # 
  # # plot component name
  # plot_legend_dictionary = eventReactive(input$gen_rbd, {
  # 
  #   plot_legend_dictionary = hash(keys = plot_legend_input()$key, values = plot_legend_input()$value)
  #   
  # })
  
  # data reliability from equation
  data_reliability_equation = eventReactive(input$simulate, {
    data_reliability = matrix(nrow = nrow(data_simulation()), ncol = length(mission_time_vec()))
    
    for(i in 1:nrow(data_reliability)){
      if(data_simulation()$redundancy[i] == "no redundancy"){
        data_reliability[i,] = exp((-1*data_simulation()$failure_rate[i]*mission_time_vec()))
      } else{
        if(data_simulation()$redundancy[i] == "hot redundancy"){
          data_reliability[i,] = 1-((1-exp((-1*data_simulation()$failure_rate[i])*mission_time_vec()))^2)
        } else{
          data_reliability[i,] = (1+(data_simulation()$failure_rate[i]*mission_time_vec()))*exp((-1*data_simulation()$failure_rate[i])*mission_time_vec())
        }
      }
    }
    
    system_reliability = vector()
    for(i in 1:ncol(data_reliability)){
      system_reliability[i] = round(prod(data_reliability[,i])*100, digits = 2)
    }
    system_reliability
    
  })
  
  
  # simulate time to failure
  data_ttf = eventReactive(input$simulate, {
    data_ttf = ttf.formula(data_sim_main(), data_sim_red(), data_redundancy_status(), sim_number())
  })
  
  # collect result, time to failure turbine
  ttf_turbine = eventReactive(input$simulate, {
    cbind(apply(data_ttf(), 1, min))
  })
  
  # collect result, component with lowest time to failure
  ttf_turbine_id = eventReactive(input$simulate, {
    cbind(colnames(data_ttf())[apply(data_ttf(), 1, which.min)])
  })
  
  # data reliability simulation
  data_reliability_simulation = reactive({
    r_sys_sim_data = 0
    for (i in 1:tail(mission_time_vec(), n=1)){
      r_sys_sim_data[i] = round(
        mean((ttf_turbine() >= i)*1)*100,
        digits = 2)
    }
    r_sys_sim_data
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
      
      afrep_sim_main = (-1/(data_sim_main()))*log(1-(c(runif(length(data_sim_main())))))
      afrep_sim_red = (-1/(data_sim_red()))*log(1-(c(runif(length(data_sim_red())))))
      afrep_sim = afrep_sim_main
      
      for(j in 1:ncol(afrep_sim)){
        afrep_sim[, j] = ifelse(data_redundancy_status()[1, j] == "no redundancy", afrep_sim_main[, j],
                                ifelse(data_redundancy_status()[1, j] == "hot redundancy", 
                                       ifelse(afrep_sim_main[, j] > afrep_sim_red[, j], 
                                              afrep_sim_main[, j], afrep_sim_red[, j]),
                                       afrep_sim_main[, j] + afrep_sim_red[, j]))
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
    melt(lapply(sum_av_sys()[1:sim_number(), 1], function(x) ((tail(mission_time_vec(), n=1)-x)/tail(mission_time_vec(), n=1)))) #calculate availability for each iteration
  })
  
  mean_aval_sys = eventReactive(input$simulate, {
    mean(aval_sys[,1]) 
  })
  
  # calculate percentage of system sucess without failure
  rep_no_sys = eventReactive(input$simulate, {
    melt(sum_av_sys()[1:sim_number(),2]) #number of failure for each iterations
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
    plot_legend_input = rbind(
      data.frame(key = data_simulation()$components, value = data_simulation()$components_input),
      data_simulation() %>% tidyr::drop_na(redundant_fr) %>% 
        select(key = redundant_id, value = components_input)
    )
    plot_legend_dictionary = hash(keys = plot_legend_input$key, values = plot_legend_input$value)
    plot_legend = hash::values(plot_legend_dictionary, fc_total()$Var1)
  })
  
  fc_resume = eventReactive(input$simulate, { 
    fc_total() %>%  
      mutate(plot_legend = plot_legend(),
             failure_pct = (round(100*Freq/sum(Freq),1))) %>% 
      arrange(failure_pct)
  }) 
  
  
  
  # render plot
  # rbd
  output$rbd = renderDiagrammeR({
    mermaid(paste("graph LR", data_diagram(), sep = "\n"))
  
  })
  
  output$plot_reliability = renderPlotly({
    plot_ly() %>% 
      add_trace(x = mission_time_vec(), y = data_reliability_equation(), 
                type = 'scatter', mode = "lines", name = "Reliability from Equation") %>%
      add_trace(x = mission_time_vec(), y = data_reliability_simulation(), 
                type = 'scatter', mode = "lines", name = "Reliability from Simulation") %>%
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
             yaxis=list(title = "", categoryorder = "array", categoryarray = fc_resume()$plot_legend), 
             title = "Distribution of Components Causing the System Failure")
  })
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  # debugging
  ########
  # output$data_1 <- renderDataTable({
  #   data_reliability()
  # })
  # 
  # output$data_2 <- renderDataTable({
  #   fc_resume()
  # })
  # 
  # output$data_3 <- renderDataTable({
  #   plot_legend()
  # 
  # })
  
  output$text_output = renderText({
    time.taken
  })
  ########
  
  
}


