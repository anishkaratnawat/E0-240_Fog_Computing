# This code will install required packages if they are not already installed
packages <- c("shiny", "shinythemes", "queueing", "magick", "ggplot2", "plotly", "DT")
installed_packages <- rownames(installed.packages())
for(p in packages){
  if(!(p %in% installed_packages)){
    install.packages(p)
  }
  library(p, character.only = TRUE)
}

# Define UI for application
ui <- fluidPage(
  
  # Apply a theme from shinythemes
  theme = shinytheme("cosmo"),
  
  # Include custom CSS
  tags$head(
    includeCSS("www/style.css")
  ),
  
  # Application title with an icon
  titlePanel(
    tags$div(
      tags$span(icon("cloud"), style = "font-size:24px;"),
      "Queueing Theory Model for Fog Computing"
    )
  ),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      # Add some padding and styling
      style = "padding: 20px;",
      
      # Use tabsetPanel within the sidebar to organize inputs
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "General Parameters",
          br(),
          numericInput(
            inputId = "sla",
            label = HTML("<i>SLA</i>: Guaranteed Response Time (units)"),
            value = 4,
            min = 0,
            max = 10,
            step = 1
          ),
          numericInput(
            inputId = "c",
            label = HTML("<i>J</i>: Number of Jobs (Customers)"),
            value = 100,
            min = 1,
            max = 500,
            step = 10
          )
        ),
        tabPanel(
          "Service Rates",
          br(),
          numericInput(
            inputId = "mu_es",
            label = HTML("&#181;<sup>E</sup>: Service Rate (ES)"),
            value = 0.9,
            min = 0.1,
            max = 1,
            step = 0.01
          ),
          numericInput(
            inputId = "mu_ds",
            label = HTML("&#181;<sup>D</sup>: Service Rate (DS)"),
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.01
          ),
          numericInput(
            inputId = "mu_ps",
            label = HTML("&#181;<sup>P</sup>: Service Rate (PS)"),
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.01
          ),
          numericInput(
            inputId = "mu_os",
            label = HTML("&#181;<sup>O</sup>: Service Rate (OS)"),
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.01
          ),
          numericInput(
            inputId = "mu_fs",
            label = HTML("&#181;<sup>F</sup>: Service Rate (FS)"),
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.01
          ),
          numericInput(
            inputId = "mu_cs",
            label = HTML("&#181;<sup>C</sup>: Service Rate (CS)"),
            value = 0.4,
            min = 0.1,
            max = 1,
            step = 0.01
          )
        ),
        tabPanel(
          "Servers Configuration",
          br(),
          numericInput(
            inputId = "m_ps",
            label = HTML("R: Number of Processing Servers"),
            value = 1,  # Set to 1 for DCNSFog
            min = 1,
            max = 500,
            step = 1
          ),
          numericInput(
            inputId = "m_fs",
            label = HTML("N: Number of Fog Servers"),
            value = 4,  # Set to 4 for DCNSFog
            min = 1,
            max = 500,
            step = 1
          ),
          numericInput(
            inputId = "m_cs",
            label = HTML("M: Number of Client Servers"),
            value = 16,  # Set to 16 for DCNSFog
            min = 1,
            max = 500,
            step = 1
          )
        ),
        tabPanel(
          "Probabilities",
          br(),
          sliderInput(
            inputId = "d",
            label = HTML("&delta;: Database Access Probability"),
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5  # Set to 0.5 for DCNSFog
          ),
          sliderInput(
            inputId = "t",
            label = HTML("&tau;: Output Server Probability"),
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5  # Set to 0.5 for DCNSFog
          ),
          sliderInput(
            inputId = "k",
            label = HTML("&kappa;: Fog Server Exit Probability"),
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5  # Set to 0.5 for DCNSFog
          )
        )
      ),
      
      # Action button with an icon
      actionButton(
        inputId = "calc",
        label = "Calculate",
        icon = icon("calculator"),
        class = "btn-primary",
        style = "margin-top: 20px; width: 100%;"
      )
    ),
    
    # Main panel with tabs for outputs
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Diagram",
          br(),
          imageOutput("imgDiagram")
        ),
        tabPanel(
          "Performance Plots",
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput("throughputPlot")
            ),
            column(
              width = 6,
              plotOutput("responseTimePlot")
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput("meanCustomersPlot")
            ),
            column(
              width = 6,
              plotOutput("ROkPlot")
            )
          )
        ),
        tabPanel(
          "Summary",
          br(),
          verbatimTextOutput("sum")
        )
      )
    )
  )
)

# Define server logic required
server <- function(input, output) {
    
    diagram <- image_read("www/fog-model.png")
    
    # Define cost coefficients (adjust these values as needed)
    cost_coefficients <- list(
      proc = 0.02,     # Cost per CPU cycle
      bw = 0.05,       # Cost per GB of data transferred
      storage = 0.01,  # Cost per GB of storage used
      energy = 0.15,   # Cost per kWh of energy consumed
      sla = 100        # Penalty cost for SLA violation (per second over SLA)
    )
    
    model <- reactive({
        req(input$calc) # Ensure calculation is triggered by the button
        
        isolate({
            # Model parameters
            es_service_rate <- input$mu_es * 2
            ps_service_rate <- input$mu_ps
            ds_service_rate <- input$mu_ds * 3.5
            os_service_rate <- input$mu_os * 3.5
            cs_service_rate <- input$mu_cs
            fs_service_rate <- input$mu_fs
            
            # Probabilities
            d <- input$d
            t <- input$t
            k <- input$k
            
            # Number of Customers
            n <- input$c
            
            # SLA
            sla_time <- input$sla
            
            # Define queueing nodes
            node_es <- NewInput.MM1(lambda = 0, mu = es_service_rate, n = 0)
            node_ps <- NewInput.MMC(lambda = 0, mu = ps_service_rate, c = input$m_ps, n = 0)
            node_ds <- NewInput.MM1(lambda = 0, mu = ds_service_rate, n = 0)
            node_os <- NewInput.MM1(lambda = 0, mu = os_service_rate, n = 0)
            node_cs <- NewInput.MMC(lambda = 0, mu = cs_service_rate, c = input$m_cs, n = 0)
            node_fs <- NewInput.MMC(lambda = 0, mu = fs_service_rate, c = input$m_fs, n = 0)
            
            # Think time = 0
            z <- 0
            
            # Operational value
            operational <- FALSE
            
            # Definition of the transition probabilities matrix
            #            ES           PS    DS        OS     CS    FS
            prob_es <- c(0, (1 - d) * (1 - t), d, (1 - d) * t, 0, 0)
            prob_ps <- c(0, (1 - d) * (1 - t), d, (1 - d) * t, 0, 0)
            prob_ds <- c(0, (1 - t), 0, t, 0, 0)
            prob_os <- c(0, 0, 0, 0, 1, 0)
            prob_cs <- c(0, 0, 0, 0, 0, 1)
            prob_fs <- c(k, 0, 0, 0, (1 - k), 0)
            
            prob <- matrix(data = c(prob_es, prob_ps, prob_ds, prob_os, prob_cs, prob_fs),
                           nrow = 6, ncol = 6, byrow = TRUE)
            
            # Validate transition matrix rows sum to 1
            row_sums <- rowSums(prob)
            if(any(abs(row_sums - 1) > 1e-6)){
              showNotification("Each row of the transition matrix must sum to 1.", type = "error")
              return(NULL)
            }
            
            # Initialize result vectors
            result_throughput <- numeric(n)
            result_mean_customers <- numeric(n)
            result_mean_time <- numeric(n)
            
            for(i in 1:n){
                # Define a new input for the Closed Jackson Network
                cjn1 <- NewInput.CJN(prob, i, z, operational, 0, 0.001, node_es, node_ps, node_ds, node_os, node_cs, node_fs)
                
                # Check the inputs and build the model
                m_cjn1 <- QueueingModel(cjn1)
                Inputs(m_cjn1)
                
                result_throughput[i] <- m_cjn1$Throughput
                result_mean_customers[i] <- m_cjn1$L
                result_mean_time[i] <- m_cjn1$W
            }
            
            # Summary of the last model
            sum <- summary(m_cjn1)
            
            # Calculate Cost Components
            # Assumptions:
            # - Each job transfers 1 GB of data
            # - Each customer uses 0.5 GB of storage
            # - Each server consumes 0.1 kWh per time unit
            
            # Processing Cost
            processing_cost <- sum(result_throughput) * cost_coefficients$proc
            
            # Network Bandwidth Cost
            data_per_job <- 1  # GB per job
            network_cost <- sum(result_throughput) * data_per_job * cost_coefficients$bw
            
            # Storage Cost
            storage_per_customer <- 0.5  # GB per customer
            total_storage <- sum(result_mean_customers) * storage_per_customer
            storage_cost <- total_storage * cost_coefficients$storage
            
            # Energy Cost
            energy_per_server <- 0.1  # kWh per time unit
            total_servers <- input$m_ps + input$m_fs + input$m_cs + 3  # Processing, Fog, Client, ES, OS, CS
            energy_cost <- total_servers * energy_per_server * cost_coefficients$energy
            
            # SLA Penalty
            mean_response_time <- mean(result_mean_time)
            sla_penalty <- ifelse(mean_response_time > sla_time, 
                                  (mean_response_time - sla_time) * cost_coefficients$sla, 
                                  0)
            
            # Total Cost
            total_cost <- processing_cost + network_cost + storage_cost + energy_cost + sla_penalty
            
            # Return all objects as a list
            list(
              throughput = result_throughput,
              mean_customers = m_cjn1$Lk,
              mean_time = result_mean_time,
              sum = sum,
              rok = m_cjn1$ROk,
              sla_time = sla_time,
              cost = list(
                processing = processing_cost,
                network = network_cost,
                storage = storage_cost,
                energy = energy_cost,
                sla_penalty = sla_penalty,
                total = total_cost
              )
            )
        })
    })
    
    # Render the annotated diagram
    output$imgDiagram <- renderImage({
      req(model()) # Ensure model is calculated
      tmp_file <- diagram %>%
        image_annotate(as.character(input$c), color = "black", size = 30, location = "+40+20") %>%
        image_write(tempfile(fileext = 'png'), format = 'png')
      
      list(src = tmp_file, contentType = "image/png", alt = "Fog Computing Model Diagram")
    }, deleteFile = TRUE)
    
    # Throughput Evolution Plot
    output$throughputPlot <- renderPlot({
      req(model())
      df <- data.frame(cust = 1:input$c, thro = model()$throughput)
      ggplot(df, aes(x = cust, y = thro)) + 
        geom_point(color = "#1f77b4") + 
        geom_smooth(method = "loess", se = TRUE, color = "#ff7f0e", fill = "#ffbb78") + 
        labs(title = "Throughput Evolution", x = "# Jobs", y = "Throughput (jobs/time unit)") + 
        theme(text = element_text(size = 18))
    })
    
    # Mean Time Evolution Plot
    output$responseTimePlot <- renderPlot({
      req(model())
      df <- data.frame(Jobs = 1:input$c, Mean_Time = model()$mean_time)
      ggplot(df, aes(x = Jobs, y = Mean_Time)) + 
        geom_point(color = "#1f77b4", size = 2) + 
        geom_smooth(method = "loess", se = TRUE, color = "#ff7f0e", fill = "#ffbb78") + 
        geom_hline(yintercept = model()$sla_time, linetype = "dashed", color = "#9467bd", size = 1) + 
        annotate("text", x = min(df$Jobs), y = model()$sla_time, label = "SLA Limit", 
                 vjust = -1, hjust = 0, color = "#9467bd", size = 5) + 
        labs(title = "Mean Time Evolution", x = "# Jobs", y = "Mean Time (time units)") + 
        theme(text = element_text(size = 18))
    })
    
    # Mean Customers Evolution Plot
    output$meanCustomersPlot <- renderPlot({
      req(model())
      df <- data.frame(cust = 1:length(model()$mean_customers), thro = model()$mean_customers)
      ggplot(df, aes(x = cust, y = thro)) + 
        geom_bar(stat = "identity", fill = "navyblue") + 
        labs(title = "Mean Customers Evolution", x = "# Jobs", y = "Mean Number of Customers") + 
        theme(text = element_text(size = 18))
    })
    
    # Node Usage Plot
    output$ROkPlot <- renderPlot({
      req(model())
      df <- data.frame(cust = c("ES", "PS", "DS", "OS", "CS", "FS"), thro = model()$rok)
      ggplot(df, aes(x = cust, y = thro, fill = "navyblue")) + 
        geom_bar(stat = "identity",fill = "navyblue") + 
        labs(title = "Node Usage", x = "Node", y = "Usage") + 
        theme(text = element_text(size = 18), legend.position = "none")
    })
    
    # Summary Output
    output$sum <- renderPrint({
      req(model())
      cat("---- Model Summary ----\n")
      print(model()$sum, digits = 2)
      
      cat("\n---- Cost Breakdown ----\n")
      cat(sprintf("Processing Cost: $%.2f\n", model()$cost$processing))
      cat(sprintf("Network Bandwidth Cost: $%.2f\n", model()$cost$network))
      cat(sprintf("Storage Cost: $%.2f\n", model()$cost$storage))
      cat(sprintf("Energy Cost: $%.2f\n", model()$cost$energy))
      cat(sprintf("SLA Penalty: $%.2f\n", model()$cost$sla_penalty))
      cat(sprintf("Total Cost: $%.2f\n", model()$cost$total))
    })
}

# Run the application 
shinyApp(ui = ui, server=server)

