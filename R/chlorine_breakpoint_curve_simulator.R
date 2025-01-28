#Required R packages for application to run
library(deSolve)
library(ggplot2)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinythemes)

#SERVER FUNCTION SECTION BEGINS HERE########################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#SERVER FUNCTION GENERAL SECTION############################################################################################
############################################################################################################################

# Set general simulation parameters
length_m <- 240
ratio_step <- 0.2
ratio_min <- 0.0
ratio_max <- 15.0

# Set parameter names for inputed conditions to pass to simulation function
sim_Names <- c("initial_chemical", "Free_mgL", "TOTNH_mgL", "pH", "Alk", "T_C", "time_m")

# Define colorblind friendly palette
cb_palette <- c("#000000", "#CC79A7", "#E69F00", "#56B4E9", "#009E73",
                "#0072B2", "#D55E00", "#999999")

# Define basic theme used for all plots
mytheme <-  theme(
  panel.background = element_rect(fill = "white", colour = NA),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_line(colour = "grey85", size = 0.5),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text = element_text(face = "bold", size = rel(1.25)),
  legend.position = "top",
  legend.direction = "horizontal",
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = rel (1.5)),
  axis.ticks = element_line(colour = "black", size = 1),
  axis.line = element_line(colour = "black", size = 1, lineend = "square"),
  axis.text.x = element_text(colour = "black", size = 12),
  axis.text.y = element_text(colour = "black", size = 12),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14))

# Copy initial conditions from one simulation to another
update_IC <- function(session, from, to, input) {

  # Update initial condition for slider inputs
  updateSliderInput(session, paste0(to, "_", "Free_mgL"), value = input[[paste0(from, "_", "Free_mgL")]])
  updateSliderInput(session, paste0(to, "_", "TOTNH_mgL"), value = input[[paste0(from, "_", "TOTNH_mgL")]])
  updateSliderInput(session, paste0(to, "_", "pH"), value = input[[paste0(from, "_", "pH")]])
  updateSliderInput(session, paste0(to, "_", "Alk"), value = input[[paste0(from, "_", "Alk")]])
  updateSliderInput(session, paste0(to, "_", "T_C"), value = input[[paste0(from, "_", "T_C")]])

  #Update initial conditions for select inputs
  updateSelectInput(session, paste0(to, "_", "initial_chemical"), selected = input[[paste0(from, "_", "initial_chemical")]])
}

# Define chloramine system simulation function
simulate_chloramine <- function(initial_chemical, Free_mgL, TOTNH_mgL, pH, Alk, T_C, time_m) {

  #Set time steps
  time <- seq(from = 0, to = length_m*60, by = 60)
  data_points <- length(time)

  #Get initial conditions based on various possible input scenarios

  #Calcualate initial total chlorine concentration
  #Free Chlorine
  if (initial_chemical == "chlorine") {
    #Set chlorine to nitrogen mass ratio number sequence
    CltoN_Mass <- seq(1, ratio_max, ratio_step)
    num_cond <- length(CltoN_Mass)

    #Calcualate initial total chlorine and total ammonia concentrations
    TOTCl_ini <- rep(Free_mgL/71000, num_cond)
    TOTNH_ini <- (Free_mgL/CltoN_Mass)/14000
    }

  #Free Ammonia
  if (initial_chemical == "ammonia") {
    #Set chlorine to nitrogen mass ratio number sequence
    CltoN_Mass <- seq(ratio_min, ratio_max, ratio_step)
    num_cond <- length(CltoN_Mass)

    #Calcualate initial total chlorine and total ammonia concentrations
    TOTCl_ini <- (CltoN_Mass*TOTNH_mgL)/71000
    TOTNH_ini <- rep(TOTNH_mgL/14000, num_cond)
    }

  #Calcualate initial concentrations
  NH2Cl_ini <- rep(0, num_cond)
  NHCl2_ini <- rep(0, num_cond)
  NCl3_ini <- rep(0, num_cond)
  I_ini <- rep(0, num_cond)

  # Convert temperature from Celsius to Kelvin
  T_K <- T_C + 273.15

  # Calculate equilibrium constants for chloramine system adjusted for temperature
  KHOCl <- 10^(-(1.18e-4 * T_K^2 - 7.86e-2 * T_K + 20.5))  #10^-7.6
  KNH4 <- 10^(-(1.03e-4 * T_K^2 - 9.21e-2 * T_K + 27.6))   #10^-9.25
  KH2CO3 <- 10^(-(1.48e-4 * T_K^2 - 9.39e-2 * T_K + 21.2)) #10^-6.35
  KHCO3 <- 10^(-(1.19e-4 * T_K^2 - 7.99e-2 * T_K + 23.6))  #10^-10.33
  KW <- 10^(-(1.5e-4 * T_K^2 - 1.23e-1 * T_K + 37.3))      #10^-14

  # Calculate water species concentrations (moles/L)
  H <- 10^-pH
  OH <- KW/H

  # Calculate alpha values
  alpha0TOTCl <- 1/(1 + KHOCl/H)
  alpha1TOTCl <- 1/(1 + H/KHOCl)

  alpha0TOTNH <- 1/(1 + KNH4/H)
  alpha1TOTNH <- 1/(1 + H/KNH4)

  alpha0TOTCO <- 1/(1 + KH2CO3/H + KH2CO3*KHCO3/H^2)
  alpha1TOTCO <- 1/(1 + H/KH2CO3 + KHCO3/H)
  alpha2TOTCO <- 1/(1 + H/KHCO3 + H^2/(KH2CO3*KHCO3))

  # Calculate total carbonate concentration (moles/L)
  TOTCO <- (Alk/50000 + H - OH)/(alpha1TOTCO + 2 * alpha2TOTCO)

  # Calculate carbonate species concentrations (moles/L)
  H2CO3 <- alpha0TOTCO*TOTCO
  HCO3 <- alpha1TOTCO*TOTCO
  CO3 <- alpha2TOTCO*TOTCO

  # Calculated rate constants (moles/L and seconds) adjusted for temperature
  k1 <- 6.6e8 * exp(-1510/T_K)                #4.2e6
  k2 <- 1.38e8 * exp(-8800/T_K)               #2.1e-5
  k3 <- 3.0e5 * exp(-2010/T_K)                #2.8e2        
  k4 <- 6.5e-7
  k5H <- 1.05e7 * exp(-2169/T_K)              #6.9e3        
  k5HCO3 <- 4.2e31 * exp(-22144/T_K)          #2.2e-1       
  k5H2CO3 <- 8.19e6 * exp(-4026/T_K)          #1.1e1
  k5 <- k5H*H + k5HCO3*HCO3 + k5H2CO3*H2CO3
  k6 <- 6.0e4
  k7 <- 1.1e2
  k8 <- 2.8e4
  k9 <- 8.3e3
  k10 <- 1.5e-2
  k11p <- 3.28e9*OH + 6.0e6*CO3               
  k11OCl <- 9e4
  k12 <- 5.56e10
  k13 <- 1.39e9
  k14 <- 2.31e2

  # Define function for chloramine system
  chloramine <- function(t, y, parms) {
    with(as.list(y), {

      dTOTNH <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H)
      dTOTCl <- (-k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH + k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 + k8*I*NHCl2 -
        (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 + 2*k12*NHCl2*NCl3*OH + k13*NH2Cl*NCl3*OH -
        2*k14*NHCl2*alpha1TOTCl*TOTCl)
      dNH2Cl <- (k1*alpha0TOTCl*TOTCl*alpha1TOTNH*TOTNH - k2*NH2Cl - k3*alpha0TOTCl*TOTCl*NH2Cl + k4*NHCl2 - 2*k5*NH2Cl^2 +
        2*k6*NHCl2*alpha1TOTNH*TOTNH*H - k9*I*NH2Cl - k10*NH2Cl*NHCl2 - k13*NH2Cl*NCl3*OH)
      dNHCl2 <- (k3*alpha0TOTCl*TOTCl*NH2Cl - k4*NHCl2 + k5*NH2Cl^2 - k6*NHCl2*alpha1TOTNH*TOTNH*H - k7*NHCl2*OH - k8*I*NHCl2 -
        k10*NH2Cl*NHCl2 - (k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH -
        k14*NHCl2*alpha1TOTCl*TOTCl)
      dNCl3 <- ((k11p + k11OCl*alpha1TOTCl*TOTCl)*alpha0TOTCl*TOTCl*NHCl2 - k12*NHCl2*NCl3*OH - k13*NH2Cl*NCl3*OH)
      dI <- (k7*NHCl2*OH - k8*I*NHCl2 - k9*I*NH2Cl)
      list(c(dTOTNH, dTOTCl, dNH2Cl, dNHCl2, dNCl3, dI))
    })
  }

  #Initialize blank data frame for simulation results
  sim_data <- data.frame(TOTNH = numeric(),
                         TOTCl = numeric(),
                         NH2Cl = numeric(),
                         NHCl2 = numeric(),
                         NCl3 = numeric(),
                         I = numeric(),
                         Mass_Ratio = numeric()
                         )

  for (i in 1:num_cond){
    #Set Initial Condition Variables
    yini <- c(TOTNH = TOTNH_ini[i],
              TOTCl = TOTCl_ini[i],
              NH2Cl = NH2Cl_ini[i],
              NHCl2 = NHCl2_ini[i],
              NCl3 = NCl3_ini[i],
              I = I_ini[i])

    #Solver of ODE System
    out <- cbind(as.data.frame(ode(func = chloramine,
                                   parms = NULL,
                                   y = yini,
                                   times = time,
                                   atol = 1e-12,
                                   rtol = 1e-12
                                   )
                               ),
                 Mass_Ratio = CltoN_Mass[i]
                 )

    sim_data <- rbind(sim_data, out)
  }

  # Extract concentrations (moles/L) and convert to typical units (e.g., mg Cl2/L or mg N/L)
  sim_data$Total_Chlorine <- (sim_data$NH2Cl + sim_data$NHCl2*2 + sim_data$NCl3*3 + sim_data$TOTCl)*71000
  sim_data$Monochloramine <- sim_data$NH2Cl*71000
  sim_data$Dichloramine <- sim_data$NHCl2*71000*2
  sim_data$Trichloramine <- sim_data$NCl3*71000*3
  sim_data$Free_Chlorine <- sim_data$TOTCl*71000
  sim_data$Free_Ammonia <- sim_data$TOTNH*14000
  sim_data$Total_Ammonia_N <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*14000
  sim_data$Total_Ammonia_NH3 <- (sim_data$TOTNH + sim_data$NH2Cl + sim_data$NHCl2 + sim_data$NCl3)*17000
  sim_data$Cl2N <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_N
  sim_data$Cl2NH3 <- sim_data$Total_Chlorine/sim_data$Total_Ammonia_NH3
  sim <- melt(sim_data, id.vars=c("time", "Mass_Ratio"), variable.name="chemical", value.name="concentration")
}

# Define function to plot simulation results in various formats
plot_sim <- function(sim, chem, time_m, initial_chemical) {

  # Extract concentrations
  sim_simple <- sim[sim$chemical %in% c("Total_Chlorine", "Monochloramine", "Dichloramine", "Trichloramine", "Free_Chlorine", "Free_Ammonia"),]
  sim_simple$chemical <- factor(sim_simple$chemical)
  levels(sim_simple$chemical) <- c("Total Chlorine", "Monochloramine", "Dichloramine", "Trichloramine", "Free Chlorine", "Free Ammonia")

  #Set time of breakpoint reaction to plot
  sim_plot <- sim_simple[sim_simple$time == time_m*60,]

  # Define chemical concentration composite plot versus time in days
  if (initial_chemical == "ammonia") {
    plot <- ggplot(subset(sim_plot, chemical %in% chem), aes(x=Mass_Ratio, y=concentration, colour=chemical)) +
      geom_line(size = 1) +
      xlab(expression(Initial~Chlorine~to~Nitrogen~Mass~Ratio~(mg~Cl[2]:mg~N))) +
      ylab(expression(Chlorine~(mg~Cl[2]~L^-1)~or~Free~Ammonia~(mg~N~L^-1)~Concentrations)) +
      scale_x_continuous(limits = c(ratio_min, ratio_max), breaks = seq(ratio_min, ratio_max, by = 1)) +
      guides(colour = guide_legend(ncol = 2)) +
      scale_colour_manual (values = cb_palette) +
      ggtitle(label = paste0("Breakpoint Curve (", time_m, " Minute Reaction Time)")) +
      mytheme +
      theme(plot.title = element_text(size = rel(1.75), face = "bold"))
    }

  if (initial_chemical == "chlorine") {
    plot <- ggplot(subset(sim_plot, chemical %in% chem), aes(x=Mass_Ratio, y=concentration, colour=chemical)) +
      geom_line(size = 1) +
      xlab(expression(Initial~Chlorine~to~Nitrogen~Mass~Ratio~(mg~Cl[2]:mg~N))) +
      ylab(expression(Chlorine~(mg~Cl[2]~L^-1)~or~Free~Ammonia~(mg~N~L^-1)~Concentrations)) +
      scale_x_reverse(limits = c(ratio_max, ratio_min), breaks = seq(ratio_max, ratio_min, by = -1)) +
      guides(colour = guide_legend(ncol = 2)) +
      scale_colour_manual (values = cb_palette) +
      ggtitle(label = paste0("Breakpoint Curve (", time_m, " Minute Reaction Time)")) +
      mytheme +
      theme(plot.title = element_text(size = rel(1.75), face = "bold"))
    }

  return(plot)
}

# Define download data function
export_data <- function(sim) {

  # Extract concentrations
  Time_minutes <- sim[sim$chemical == "Total_Chlorine", "time"]/60
  Initial_Mass_Ratio_mg_Cl2_mg_N <- sim[sim$chemical == "Total_Chlorine", "Mass_Ratio"]
  Total_Chlorine_mg_Cl2_L <- sim[sim$chemical == "Total_Chlorine", "concentration"]
  Monochloramine_mg_Cl2_L <- sim[sim$chemical == "Monochloramine", "concentration"]
  Dichloramine_mg_Cl2_L <- sim[sim$chemical == "Dichloramine", "concentration"]
  Trichloramine_mg_Cl2_L <- sim[sim$chemical == "Trichloramine", "concentration"]
  Free_Chlorine_mg_Cl2_L <- sim[sim$chemical == "Free_Chlorine", "concentration"]
  Free_Ammonia_mg_N_L <- sim[sim$chemical == "Free_Ammonia", "concentration"]
  data <- data.frame(Time_minutes,
                     Initial_Mass_Ratio_mg_Cl2_mg_N,
                     Total_Chlorine_mg_Cl2_L,
                     Monochloramine_mg_Cl2_L,
                     Dichloramine_mg_Cl2_L,
                     Trichloramine_mg_Cl2_L,
                     Free_Chlorine_mg_Cl2_L,
                     Free_Ammonia_mg_N_L)
}

#SERVER FUNCTION DEFINITION SECTION#########################################################################################
############################################################################################################################

# Define server logic required to run simulations and produce output
server <- function(input, output, session) {

  # Set option for sticky sessions
  options("Set-Cookie" = paste0("JSESSIONID=", session$token))

  #Copy Simulation A's inputs to Simulation B's inputs
  observe({
    #Take a dependency on input$AtoBIC
    if(input$A_to_B_IC == 0) return(NULL)

    isolate(update_IC(session, "A", "B", input))
    })

  #Copy Simulation B's inputs to Simulation A's inputs
  observe({
    #Take a dependency on input$BtoAIC
    if(input$B_to_A_IC == 0) return(NULL)

    isolate(update_IC(session, "B", "A", input))
  })

  # Define function to get inputted initial conditions and states based on prefix provided
  sim_Params <- function(prefix) {
    params <- lapply(sim_Names, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
  }

  # Run chloramine system simulation based on provided initial conditions
  simA <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateA == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_chloramine, sim_Params("A")))
    })

  simB <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateB == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_chloramine, sim_Params("B")))
    })

  # Produce desired reactive plots
  output$A <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    plot_sim(simA(), input$A_chemicals, input$A_time_m, input$A_initial_chemical)
    })

  output$B <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$plotupdate and input$simupdate selection
    plot_sim(simB(), input$B_chemicals, input$B_time_m, input$B_initial_chemical)
    })

  # Expression that gets data to be downloaded at User's request
  output$A_downloadData <- downloadHandler(
    filename = function() {
      paste('A', '_', substr(as.character(Sys.time()),1,10),
            '_', substr(as.character(Sys.time()),12,16), '.csv', sep='')
    },
    content = function(file) {write.csv(export_data(simA()), file, row.names=TRUE)
    }
  )

  output$B_downloadData <- downloadHandler(
    filename = function() {
      paste('B', '_', substr(as.character(Sys.time()),1,10),
            '_', substr(as.character(Sys.time()),12,16), '.csv', sep='')
    },
    content = function(file) {
      write.csv(export_data(simB()), file, row.names=TRUE)
    }
  )
}

#UI OBJECT SECTION BEGINS HERE##############################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#UI OBJECT GENERAL SECTION##################################################################################################
############################################################################################################################

# Define function to take inputs for initial conditions for simulations and plots
render_inputs <- function(prefix, prefix2) {
  wellPanel(
    h3(paste0("Simulation ", prefix, " Inputs")),

    # Call to display initial simulation notification
    conditionalPanel(condition = paste0("input.simupdate", prefix, "== 0"),
                     tags$div("Note: An initial simulation has not been run; therefore, no plot has been generated", id = "initialsim")
    ),
    br(),
    h4("Initial Conditions"),

    fluidRow(
      column(6,
             #Create input and tooltip for known chemical entry selection
             selectInput(paste0(prefix, "_", "initial_chemical"),
                         label = p(HTML("Select Chemical with Initial Fixed Concentration"), style = "font-size: 12px"),
                         choices = c("Free Chlorine" = "chlorine",
                                     "Free Ammonia" = "ammonia"),
                         selected = "ammonia",
                         selectize = TRUE),
             bsTooltip(id = paste0(prefix, "_", "initial_chemical"),
                       "Select whether the initial free chlorine or initial free ammonia concentration will be fixed to generate the breakpoint curve",
                       "right",
                       options = list(container = "body")
                       )
             )
      ),

    fluidRow(
      column(6,
             #Only show panel if free chlorine is selected as chemical
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "initial_chemical == 'chlorine'"),

               #Create input and tooltip for chemical selection of free chlorine
               sliderInput(paste0(prefix, "_", "Free_mgL"),
                           label = p(HTML("Initial Free Chlorine Concentration (mg Cl<sub>2</sub>/L)"), style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 1.00,
                           step = 0.05),
               br(),
               bsTooltip(id = paste0(prefix, "_", "Free_mgL"),
                         "Set slider to initial free chlorine concentration",
                         "right",
                         options = list(container = "body")
                         )
               ),

             #Only show panel if free ammonia is selected as chemical
             conditionalPanel(
               condition = paste0("input.", prefix, "_", "initial_chemical == 'ammonia'"),

               #Create input and tooltip for chemical selection of free ammonia
               sliderInput(paste0(prefix, "_", "TOTNH_mgL"),
                           label = p("Initial Free Ammonia Concentration (mg N/L)", style = "font-size: 12px"),
                           min = 0.00,
                           max = 10.00,
                           value = 1.00,
                           step = 0.05),
               br(),
               bsTooltip(id = paste0(prefix, "_", "TOTNH_mgL"),
                         "Set slider to initial free ammonia concentration",
                         "right",
                         options = list(container = "body")
                         )
               ),

             #Enter other conditions for breakpoint curve simulation
             sliderInput(paste0(prefix, "_", "pH"),
                         label = p("pH", style = "font-size: 12px"),
                         min = 6.00,
                         max = 9.00,
                         value = 8.00,
                         step = 0.05),
             br(),
             bsTooltip(id = paste0(prefix, "_", "pH"),
                       "Set slider to known pH (held constant during simulations)",
                       "right",
                       options = list(container = "body"))
             ),
      column(6,
             sliderInput(paste0(prefix, "_", "Alk"),
                         label = p(HTML("Total Alkalinity (mg/L as CaCO<sub>3</sub>)"), style = "font-size: 12px"),
                         min = 0,
                         max = 500,
                         value = 150,
                         step = 5),
             br(),
             bsTooltip(id = paste0(prefix, "_", "Alk"),
                       "Set slider to known alkalinity as mg/L of calcium carbonate (held constant during simulations)",
                       "left",
                       options = list(container = "body")),
             sliderInput(paste0(prefix, "_", "T_C"),
                         label = p(HTML("Water Temperature (&deg;C)"), style = "font-size: 12px"),
                         min = 5.0,
                         max = 35.0,
                         value = 25.0,
                         step = 0.5),
             br(),
             bsTooltip(id = paste0(prefix, "_", "T_C"),
                       "Set slider to known water temperature in degrees Celsius (held constant during simulations)",
                       "left",
                       options = list(container = "body")),
             br()
             )
      ),

    # Copy inputs from one simulation to the other
    actionButton(paste0(prefix, "_to_", prefix2, "_IC"),
                 paste0("Copy Simulation ", prefix, "'s Inputs to Simulation ", prefix2, "'s Inputs"),
                 icon("copy")
    ),
    bsTooltip(id = paste0(prefix, "_to_", prefix2, "_IC"),
              "Press button to copy current simulation inputs to other simulation",
              "top",
              options = list(container = "body")),
    br(),
    br(),

    # Update simulation
    actionButton(paste0("simupdate", prefix),
                 paste0("Update Simulation ", prefix, " (Press after Finished Changing Simulation Inputs)"), icon("refresh")),
    bsTooltip(id = paste0("simupdate", prefix),
              "Press button to update simulation and breakpoint curve using current input settings",
              "top",
              options = list(container = "body")),
    br(),
    br(),

    downloadButton(paste0(prefix, "_", "downloadData"),
                   paste0("Simulation ", prefix, " ", "Chemical Concentration Data Download (.csv file)")),
    bsTooltip(id = paste0(prefix, "_", "downloadData"),
              "Press button to download simulation data to a comma seperated variable (.csv) file for use in another program (e.g., Excel)",
              "bottom",
              options = list(container = "body"))
    )

}

render_plot_outputs <- function(prefix) {
  wellPanel(
    h3(paste0("Simulation ", prefix, " Breakpoint Curve Generation")),
    checkboxGroupInput(paste0(prefix, "_", "chemicals"),
                       label = h4("Chemicals to Plot"),
                       choices = c("Total Chlorine",
                                   "Monochloramine",
                                   "Dichloramine",
                                   "Trichloramine",
                                   "Free Chlorine",
                                   "Free Ammonia"),
                       selected = c("Total Chlorine",
                                    "Monochloramine",
                                    "Dichloramine",
                                    "Trichloramine",
                                    "Free Chlorine",
                                    "Free Ammonia"),
                       inline = TRUE
    ),
    br(),
    bsTooltip(id = paste0(prefix, "_", "chemicals"),
              "Use check boxes to select which chemicals to show on breakpoint curve",
              "bottom",
              options = list(container = "body")),
    h4("Reaction Time Selection"),
    sliderInput(paste0(prefix, "_", "time_m"),
                label = p("Select Reaction Time (minutes) for Breakpoint Curve (Hit play button in lower right to animate)", style = "font-size: 12px"),
                min = 0,
                max = 240,
                value = 0,
                step = 1,
                width = "100%",
                ticks = FALSE,
                animate = animationOptions(interval = 1000,
                                           loop = TRUE)
    ),
    bsTooltip(id = paste0(prefix, "_", "time_m"),
              "Select reaction time for generated breakpoint curve (hit play button in lower right to step through time)",
              "top",
              options = list(container = "body")),

    br(),
    plotOutput(prefix, height = "600px")
  )
}

#UI OBJECT DEFINITION SECTION#############################################################################################
############################################################################################################################

#Define UI layout
ui <- shinyUI(fluidPage(theme = shinytheme("flatly"),

  #Define header
  tags$head(tags$style(type = "text/css",

  #Define progress bar class
    "#loadmessage {
       position: fixed;
       width: 50%;
       top: 25%;
       right: 25%;
       text-align: center;
       font-weight: bold;
       font-size: 300%;
       color: black;
       padding: 10px;
       word-wrap: break-word;
       line-height: 40px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: #f5f5f5;
       opacity: 1;
       z-index: 105;}",

  #Define initial simulation not run warning style
    "#initialsim {
       width: 90%;
       top: 0%;
       left: 5%;
       text-align: center;
       font-weight: bold;
       font-size: 12px;
       color: black;
       padding: 7.5px;
       word-wrap: break-word;
       line-height: 15px;
       border-style: solid;
       border-width: large;
       border-color: black;
       border-radius: 15px;
       background-color: Yellow;
       opacity: 1;
       z-index: 105;}",

  #Define style for buttons
  ".btn {
       width: 100%;
       word-wrap: break-word;
       white-space: normal;}",

  #Define style to bold free chlorine, monochloramine and free ammonia in chemicals to plot checkbox group
    "input[value = 'Total Chlorine'] + span,
      input[value = 'Monochloramine'] + span,
      input[value = 'Free Ammonia'] + span {
      font-weight: 900;}"
    )
  ),

####Added from EPA template####################################################################################################################################
  tags$body(class = "html wide-template"),
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css", href = "style.css")),

  # Header
  HTML("<header class='masthead clearfix' role='banner'>
       <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov/' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'>"),

  # Search Form
  #$form(action='https://search.epa.gov/epasearch/epasearch', class='epa-search', method='get',
  #      tags$label(class='element-hidden'),
  #      tags$input(autocomplete='off', class='form-text ui-autocomplete-input', id='search-box', name='querytext', placeholder='Search EPA.gov', value=''),
  #  tags$span( class='ui-helper-hidden-accessible', role='status'),
  #  tags$button(class='epa-search-button', id='search-button', title='Search', type='submit'),
  #  tags$input(name='areaname', type='hidden', value=''),
  #  tags$input(name='areacontacts', type='hidden', value=''),
  #  tags$input(name='areasearchurl', type='hidden', value=''),
  #  tags$input(name='typeofsearch', type='hidden', value='epa'),
  #  tags$input(name='result_template', type='hidden', value='2col.ftl')
  #),

  HTML("</div>
       </div>
       </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <span class='related-info'><strong>Related Topics:</strong></span>
       <ul class='menu pipeline'>
       <li class='menu-item'><a href='https://www.epa.gov/environmental-topics'>Environmental Topics</a></li>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/water-research/forms/contact-us-about-water-research'>Contact Us</a></li>
       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
       <h1  class='page-title'>Chlorine Breakpoint Curve Simulator</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>"),
####Added from EPA template####################################################################################################################################

  # Call to display update in progress bar
  conditionalPanel(condition = "$('html').hasClass('shiny-busy') && !$('a').hasClass('slider-animate-button playing')",
                   tags$div("Update in progress...", id = "loadmessage")
                   ),

  # Application title
  h4("Version 0.25, Last Updated December 18, 2017"),

  h4("Created by David G. Wahman (wahman.david@epa.gov), United States Environmental Protection Agency"),

  p("Model implementation from Jafvert & Valentine",
    a(target = "_blank", href="http://pubs.acs.org/doi/abs/10.1021/es00027a022", "(Environ. Sci. Technol., 1992, 26 (3), pp 577-586)"),
    "and Vikesland et al.",
    a(target = "_blank", href="http://www.sciencedirect.com/science/article/pii/S0043135400004061", "(Water Res., 2001, 35 (7), pp 1766-1776).")),
  p("The provided application generates two side-by-side breakpoint curves (A and B) for comparison purposes with user defined conditions.
    Because several simulations are conducted to generate a breakpoint curve, simulation updates may take approximately a minute to complete."),

  p("To open a manuscript describing the application in a new window, click on the following link: ",

    a(target = "_blank", href = "manual.pdf", "Application Documentation")

  ),

  p("The application was developed by the United States Environmental Protection Agency (EPA). No warranty expressed or implied is made regarding the accuracy
    or utility of the system, nor shall the act of distribution constitute any such warranty. Any reference to specific commercial products, processes, or services by service mark,
    trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by
    EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity
    by EPA or the United States Government. This application has been reviewed in accordance with EPA policy
    and has been approved for external and free use. The views expressed in this application do not necessarily represent the views
    or policies of the Agency. Although a reasonable effort has been made to assure that the results obtained are correct,
    this application is experimental. Therefore, the author and the EPA are not responsible and assume no liability whatsoever
    for any results or any use made of the results obtained from this application, nor for any damages or litigation that result
    from the use of the application for any purpose."),
  hr(),

  # Layout for initial conditions, plot inputs, and plots
  fluidRow(
    column(6,
           render_inputs("A", "B"),
           hr(),
           render_plot_outputs("A")
    ),
    column(6,
           render_inputs("B", "A"),
           hr(),
           render_plot_outputs("B")
           )
    ),

####Additional required contact section########################################################################################################################
hr(),
p( a(href="https://www.epa.gov/water-research/forms/contact-us-about-water-research", "Contact Us"),
   " to ask a question, provide feedback, or report a problem."),

####Added from EPA template####################################################################################################################################
# Footer
HTML("</div>
     </div>
     </div>
     </div>
     </section>
     <footer class='main-footer clearfix' role='contentinfo'>
     <div class='main-footer__inner'>
     <div class='region-footer'>
     <div class='block-pane-epa-global-footer' id='block-pane-epa-global-footer'>
     <div class='row cols-3'>
     <div class='col size-1of3'>
     <div class='col__title'>
     Discover.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/accessibility'>Accessibility</a></li>
     <li><a href='https://www.epa.gov/aboutepa/administrator-gina-mccarthy'>EPA Administrator</a></li>
     <li><a href='https://www.epa.gov/planandbudget'>Budget &amp; Performance</a></li>
     <li><a href='https://www.epa.gov/contracts'>Contracting</a></li>
     <li><a href='https://www.epa.gov/home/grants-and-other-funding-opportunities'>Grants</a></li>
     <li><a href='https://19january2017snapshot.epa.gov'>January 19, 2017 Web Snapshot</a></li>
     <li><a href='https://www.epa.gov/ocr/whistleblower-protections-epa-and-how-they-relate-non-disclosure-agreements-signed-epa-employees'>No FEAR Act Data</a></li>
     <li><a href='https://www.epa.gov/privacy'>Privacy</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Connect.
     </div>
     <ul class='menu'>
     <li><a href='https://www.data.gov/'>Data.gov</a></li>
     <li><a href='https://www.epa.gov/office-inspector-general/about-epas-office-inspector-general'>Inspector General</a></li>
     <li><a href='https://www.epa.gov/careers'>Jobs</a></li>
     <li><a href='https://www.epa.gov/newsroom'>Newsroom</a></li>
     <li><a href='https://www.epa.gov/open'>Open Government</a></li>
     <li><a href='https://www.regulations.gov/'>Regulations.gov</a></li>
     <li><a href='https://www.epa.gov/newsroom/email-subscriptions'>Subscribe</a></li>
     <li><a href='https://www.usa.gov/'>USA.gov</a></li>
     <li><a href='https://www.whitehouse.gov/'>White House</a></li>
     </ul>
     </div>
     <div class='col size-1of3'>
     <div class='col__title'>
     Ask.
     </div>
     <ul class='menu'>
     <li><a href='https://www.epa.gov/home/forms/contact-epa'>Contact Us</a></li>
     <li><a href='https://www.epa.gov/home/epa-hotlines'>Hotlines</a></li>
     <li><a href='https://www.epa.gov/foia'>FOIA Requests</a></li>
     <li><a href='https://www.epa.gov/home/frequent-questions-specific-epa-programstopics'>Frequent Questions</a></li>
     </ul>
     <div class='col__title'>
     Follow.
     </div>
     <ul class='social-menu'>
     <li><a class='menu-link social-facebook' href='https://www.facebook.com/EPA'>Facebook</a></li>
     <li><a class='menu-link social-twitter' href='https://twitter.com/epa'>Twitter</a></li>
     <li><a class='menu-link social-youtube' href='https://www.youtube.com/user/USEPAgov'>YouTube</a></li>
     <li><a class='menu-link social-flickr' href='https://www.flickr.com/photos/usepagov'>Flickr</a></li>
     <li><a class='menu-link social-instagram' href='https://www.instagram.com/epagov'>Instagram</a></li>
     </ul>
     <p class='last-updated'>Last updated on March 20, 2019</p>
     </div>
     </div>
     </div>
     </div>
     </div>
     </footer>")
####Added from EPA template####################################################################################################################################

)
)

#APPLICATION FUNCTION CALL DEFINITION#######################################################################################
############################################################################################################################
shinyApp(ui = ui, server = server)