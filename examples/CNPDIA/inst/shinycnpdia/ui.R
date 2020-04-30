
# Panel definition alternative
slimSidebarPanel <- function (...) {  div(class = "span2", tags$form(class = "well", ...))  }
wideMainPanel    <- function (...) {  div(class = "span10", ...)}

shinyUI(pageWithSidebar(

  # Application title
  headerPanel(title=div(img(src="CNPDIA.png", height = 60), " The CNPDIA model"), 
              windowTitle = "karline soetaert"),

  # Sidebar with two slider inputs
  sidebarPanel(
    tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")),
   
   conditionalPanel(condition = "input.tabset  == 21 | input.tabset  == 14 | input.tabset == 11",
    wellPanel(   # wellPanel for 0D variable settings
     selectInput(inputId = "zeroD", label = "Select 0D variables",
           choices = c("fluxes","O2flux","NO3flux","NH3flux",
           "ODUflux","PO4flux","DICflux","FDETflux","SDETflux",
           "OrgCflux","OrgNflux", "OrgPflux","DINDIPflux","DINDIPmean","DINDIPdeep",
           "TotMin","TotOxic","TotDenit","TotAnoxic",
           "PartOxic","PartDenit","PartAnoxic",
           "TotNitri","TotODUoxid","TotFePprod", "TotCaPprod","TotFePdesorp","TotCaPdiss",
           "TotNprod", "TotPprod", "TotNH3ads","PartPremoved","PartNremoved",
           "O2deepflux","NO3deepflux","NH3deepflux","ODUdeepflux","PO4deepflux",
           "DICdeepflux","FDETdeepflux","SDETdeepflux","FePdeepflux","CaPdeepflux"),
            selected = "fluxes", multiple = TRUE, selectize=FALSE)
     )
   ),

   conditionalPanel(condition = "input.tabset  == 6 | input.tabset == 17 | input.tabset == 21 | input.tabset == 11",
    wellPanel(   # wellPanel for 1D variable settings
    selectInput(inputId = "oneD", label = "select 1D variables",
            choices = c("states","O2","NO3","NH3","PO4","ODU","FeP","CaP","DIC",
              "FDET","SDET","TOC","Cprod","Nprod","Pprod",
              "Oxicmin","Denitrific","Anoxicmin",
              "Nitri","Oduox","Odudepo","FePadsorp","FePdesorp","CaPprod","CaPdiss"),
            selected = "states",
            multiple = TRUE, selectize=FALSE),
     )            
   ),
   conditionalPanel(condition = "input.tabset  == 15 | input.tabset == 14 |  input.tabset == 17 ",
    wellPanel(   # wellPanel for time settings
   
    checkboxInput('season', "seasonal deposition", value = FALSE),
    checkboxInput('mixing', "mixing disturbance", value = FALSE),
    checkboxInput('erosion', "erosion disturbance", value = FALSE),
    checkboxInput('deposition', "deposition disturbance", value = FALSE),

    sliderInput("times", "simulation length, yr",              min = 1, max = 10, value = 1, step = 1),
    sliderInput("spinup", "spinup length, yr",                 min = 0, max = 10, value = 0, step = 1),

    conditionalPanel(condition = "input.season == true",
      sliderInput("pow", "peak intensity",                       min = 0, max = 10, value = 1)  
                ),
    conditionalPanel(condition = "input.mixing == true | input.erosion == true | input.deposition == true",
      sliderInput("numPert",   "number of perturbations/year",  min = 1, max = 20, value = 2)
                ),
    conditionalPanel(condition = "input.mixing == true",
      sliderInput("depthMixing", "depth of mixing perturbations, cm", min = 1, max = 20, value = 5)
                ),
    conditionalPanel(condition = "input.erosion == true",
      sliderInput("depthErosion", "depth of erosion perturbations, cm", min = 1, max = 20, value = 5)
                ),
    conditionalPanel(condition = "input.deposition == true",
      sliderInput("depthDeposition", "thickness of deposited layer, cm", min = 1, max = 20, value = 5)
                ),
    conditionalPanel(condition = "input.mixing == true | input.erosion == true | input.deposition == true",
      sliderInput("perttimes", "Duration of perturbations, yr", min = 1, max = 10, value = 10, step = 1)
    )
                ),
    actionButton("resetperturb", "Reset default")   
   ),
   
   conditionalPanel(condition = "input.tabset == 18",          # data tab
    wellPanel(   # wellPanel for file selection
    fileInput('datafile', 'Choose CSV File with data',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator', choices = c(Comma=',',  Semicolon=';', Tab='\t')),
    radioButtons('dec', 'Decimal', choices = c(Point='.',  Comma=',')),
    radioButtons('quote', 'Quote', choices = c('Double Quote'='"','Single Quote'="'",'None'='')),
    checkboxInput('first', 'Ignore first column', FALSE),
#    actionButton('addData', 'Add Data'),
    actionButton('clearData', 'Clear Data')
  )    
   ),

   conditionalPanel(condition = "input.tabset  < 9",
    wellPanel(   # wellPanel for parameter settings
    selectInput(inputId = "par", label = "Parameter type",
                choices = c("sediment", "organic", "bottomwater",
                "phosphorus", "oxygen"), selected = "sediment"),

     conditionalPanel(condition = "input.par == 'organic'",
      uiOutput("Cfluxslider") ,
      sliderInput("pFast",    "Part of flux in FDET",  min = 0.0,   max = 1.0,  value = 0.9,  step = 0.001),
      sliderInput("rFast",    "Decay rate FDET, /d",   min = 0.005, max = 0.25, value = 0.05, step = 0.005),
      sliderInput("rSlow",    "Decay rate SDET, /d",   min = 1e-6,  max = 2e-3, value = 1e-4, step = 1e-6),
      sliderInput("rMPBprod", "MPBprod., mmolO2/m3/d", min = 0.,    max = 7000, value = 0),
      actionButton("resetorganic", "Reset default")
     ),

     conditionalPanel(condition = "input.par == 'sediment'",
      sliderInput("D",        "Plotting depth, cm",    min = 5,      max = 100,   value = 20),
      sliderInput("biotdepth", "Mixed layer depth, cm", min = 0.01,  max = 10.0,  value = 5.0, step = 0.001),
      uiOutput("biotslider")    , #sliderInput("biot","Bioturbation, cm2/d", min = 0.0, max = 100.0, value = 1.0),
      sliderInput("irr",      "irrigation, /d",        min = 0.0,    max = 0.2,   value = 0.0, step = 0.001),
      uiOutput("wslider")    ,    #sliderInput("w","Advection, cm/d",min = 3e-7,max = 3e-3,  value = 3e-6),
      sliderInput("temperature", "temperature, dgC",      min = 0.0, max = 30,    value = 10.0, step = 0.001),
      actionButton("resetsediment", "Reset default")
     ),

     conditionalPanel(condition = "input.par == 'bottomwater'",
      sliderInput("bwO2",     "Oxygen, mmol/m3",       min = 0.01, max = 400.0, value = 300.0, step = 0.01),
      sliderInput("bwNO3",    "Nitrate, mmol/m3",      min = 0.01, max = 100.0, value = 10.0, step = 0.01),
      sliderInput("bwNH3",    "Ammonium, mmol/m3",     min = 0.0,  max = 100.0, value = 1.0, step = 0.01),
      sliderInput("bwPO4",    "Phosphate, mmol/m3",    min = 0.0,  max = 10.0,  value = 0.5, step = 0.01),
      sliderInput("bwDIC",    "DIC, mmol/m3",          min = 1000, max = 3000,  value = 2200.0, step = 0.01),
      sliderInput("bwODU",    "ODU, mmol O2/m3",       min = 0,    max = 1000,  value = 0.0, step = 0.01),
      actionButton("resetbottomwater", "Reset default")
     ),

     conditionalPanel(condition = "input.par == 'phosphorus'",
      sliderInput("rFePadsorp", "P adsorption to FeOxide, /d", min = 0.0, max = 1.0,   value = 0.3),
      sliderInput("rFePdesorp", "Desorption of FeP, /d",       min = 0.0, max = 0.2,   value = 0.02),
      sliderInput("rCaPprod", "Production Ca-bound P, /d",     min = 0.0, max = 0.5,   value = 0.15),
      sliderInput("rCaPdiss", "Dissolution Ca-bound P, /d",    min = 0.0, max = 0.001, value = 0.0001),
      sliderInput("CPrCaP", "Ca-P ratio in CaPprod",           min = 0.2869565, max = 30, value = 0.2869565),
      actionButton("resetphosphorus", "Reset default")
     ),

     conditionalPanel(condition = "input.par == 'oxygen'",
      sliderInput("rnit",       "nitrification rate, /d",                    min = 0.0, max = 100, value = 20),
      sliderInput("rODUox",     "ODU oxidation rate, /d",                    min = 0.0, max = 100, value = 20),
      sliderInput("ksO2nitri",  "ksO2 nitrification & odu oxidation, mM O2", min = 0.1, max = 10,  value = 1),
      sliderInput("ksNO3denit", "ksNO3 denitrification, mmol N/m3",          min = 0.1, max = 100, value = 30),
      sliderInput("kinO2",      "kinO2 denitrification, anoxic min, mM O2",  min = 0.1, max = 100, value = 1),
      sliderInput("kinNO3",     "kinNO3 anoxic mineralisation, mmol N/m3",   min = 0.1, max = 10,  value = 1),
      actionButton("resetoxygen", "Reset default")
      )
      )
     ),

   
   conditionalPanel(condition = "input.tabset  < 9 ",          # RUNS AND DATA  tabs
    wellPanel(   # wellPanel for model settings
    actionButton('addModel', 'Add Model'),
    actionButton('clearList', 'Clear'),
    uiOutput("modellist")
    )
   ),


   conditionalPanel(condition = "input.tabset == 31",           # settings tab
    radioButtons('grid', 'Grid type', choices = c(normal = 1,  radial = 2, spherical = 3)),
    wellPanel(   # wellPanel for grid and porosity settings
    numericInput("por0", "surface porosity [0,1]", value = 0.9, min = 0.0, max = 1.0, step = 0.01),
    numericInput("pordeep", "deep porosity [0,1]", value = 0.5, min = 0.0, max = 1.0, step = 0.01),
    numericInput("porcoeff", "decline coefficient [1,10], cm", value = 3, min = 1, max = 10, step = 0.1),
    radioButtons("formation", "squared tortuosity (theta^2)", choices = c('sand, por^-1' = 1,  'fine sand, por^-2' = 2, 'general, 1-ln(por^2)' = 3))

    ),

    wellPanel(   # wellPanel for maximum parameter settings
    h3("Set maximum values"),
    numericInput("maxDb", "Maximum bioturbation, cm2/d", value = 0.05, min = 0.01, max = 0.5),
    numericInput("maxCflux", "Maximum Carbon flux, nmolC/cm2/d", value = 1000, min = 100, max = 5000.0),
    numericInput("maxW",     "Maximum sedimentation rate, cm/d", value = 3e-3, min = 3e-6, max = 0.01)
    )
    ),
    
   conditionalPanel(condition = "input.tabset == 33",           # settings tab
    wellPanel(   # wellPanel for file settings
    fileInput('parameterfile', 'Choose CSV File with parameter settings',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('pheader', 'Header', TRUE),
    checkboxInput('pfirst', 'Ignore first column', FALSE),

    radioButtons('psep', 'Separator', choices = c(Comma=',',  Semicolon=';', Tab='\t', space = " ")),
    radioButtons('pdec', 'Decimal', choices = c(point ='.',Comma = ","))
    )
   ),
   
   conditionalPanel(condition = "input.tabset  == 20",               # sensitivity tab
#    sliderInput("nruns", "number of sensitivity runs",
#                min = 10, max = 50, value = 30, step = 1),
    wellPanel(   

    selectInput(inputId = "sp", label = "Sensitivity Parameter",
          choices = c("Carbon flux","pFast","rFast","rSlow",
          "biotdepth","biot","irr","w","temperature",
           "bwO2","bwNO3","bwNH3","bwPO4","bwDIC","bwODU",
           "rFePdesorp","rFePadsorp","rCaPprod","rCaPdiss",
           "rnit","rODUox"), selected = "Carbon flux"),


   conditionalPanel(condition = "input.sp == 'Carbon flux'", 
      sliderInput("Fs",          "Carbon flux, nMolC/cm2/d",   min = 0.1,  max = 5000, value = c(1, 1000))),
   conditionalPanel(condition = "input.sp == 'pFast'",       
      sliderInput("pfs",         "Part of flux in FDET",       min = 0.0,  max = 1.0,  value = c(0,1.0))),
   conditionalPanel(condition = "input.sp == 'rFast'",       
      sliderInput("rfs",         "Decay rate FDET, /d",        min = 2e-3, max = 0.5,  value = c(2e-3, 0.2))),
   conditionalPanel(condition = "input.sp == 'rSlow'",       
      sliderInput("rss",         "Decay rate SDET, /d",        min = 2e-6, max = 2e-3, value = c(2e-6,2e-3))),
   conditionalPanel(condition = "input.sp == 'biotdepth'",   
      sliderInput("mls",         "Mixed layer depth, cm",      min = 0.01, max = 10.0, value = c(0.01, 10))),
   conditionalPanel(condition = "input.sp == 'biot'",        
      sliderInput("dbs",         "Bioturbation, cm2/d",        min = 0.0,  max = 0.3,  value = c(3e-5, 10/365))),
   conditionalPanel(condition = "input.sp == 'irr'",         
      sliderInput("irrs",        "Irrigation, /d",             min = 0.0,  max = 0.2,  value = c(0.0, 0.15))),
   conditionalPanel(condition = "input.sp == 'temperature'", 
      sliderInput("tmps",        "temperature, dgC",           min = 0.0,  max = 30,   value = c(0.0, 30))),
   conditionalPanel(condition = "input.sp == 'w'",           
      sliderInput("ws",          "Advection, cm/d",            min = 3e-7, max = 3e-3, value = c(3e-7, 3e-3))),
   conditionalPanel(condition = "input.sp == 'bwO2'",        
      sliderInput("bwO2s",       "BW Oxygen, mmol/m3",         min = 0.01, max = 400,  value = c(0.01, 400))),
   conditionalPanel(condition = "input.sp == 'bwNO3'",       
      sliderInput("bwNO3s",      "BW Nitrate, mmol/m3",        min = 0.01, max = 100,  value = c(0.01, 100))),
   conditionalPanel(condition = "input.sp == 'bwNH3'",       
      sliderInput("bwNH3s",      "BW Ammonium, mmol/m3",       min = 0.0,  max = 100,  value = c(0,100.0))),
   conditionalPanel(condition = "input.sp == 'bwPO4'",       
      sliderInput("bwPO4s",      "BW Phosphate, mmol/m3",      min = 0.0,  max = 10.0, value = c(0, 10))),
   conditionalPanel(condition = "input.sp == 'bwDIC'",       
      sliderInput("bwDICs",      "BW DIC, mmol/m3",            min = 1000, max = 3000, value = c(1000,3000.0))),
   conditionalPanel(condition = "input.sp == 'bwODU'",       
      sliderInput("bwODUs",      "BW ODU, mmol O2/m3",         min = 0,    max = 1000, value = c(0,1000))),
   conditionalPanel(condition = "input.sp == 'rFePadsorp'",  
      sliderInput("rFePadsorps", "P adsorption to FeOx, /d",   min = 0.0,  max = 1.0,  value = c(0,1))),
   conditionalPanel(condition = "input.sp == 'rFePdesorp'",  
      sliderInput("rFePdesorps", "Desorption of FeP, /d",      min = 0.0,  max = 0.2,  value = c(0,0.2))),
   conditionalPanel(condition = "input.sp == 'rCaPprod'",    
      sliderInput("rCaPprods",   "Production Ca-bound P, /d",  min = 0.0,  max = 0.5,  value = c(0,0.5))),
   conditionalPanel(condition = "input.sp == 'rCaPdiss'",    
      sliderInput("rCaPdisss",   "Dissolution Ca-bound P, /d", min = 0.0,  max = 0.001,value = c(0,0.001))),
   conditionalPanel(condition = "input.sp == 'rnit'",        
      sliderInput("rnits",       "nitrification, /d",          min = 0.0,  max = 100,  value = c(1,100))),
   conditionalPanel(condition = "input.sp == 'rODUox'",      
      sliderInput("rODUoxs",     "ODU oxidation, /d",          min = 1.0,  max = 100,  value = c(1,100)))
     ) 
   ),
   
   conditionalPanel(condition = "input.tabset  == 20",
       wellPanel(   # wellPanel for model settings

     selectInput(inputId = "sv", label = "plotted variables",
           choices = c("all","O2flux","NO3flux","NH3flux",
              "ODUflux","PO4flux","DICflux",
              "PartDenit","PartAnoxic","PartOxic","DINDIPmean","DINDIPflux","DINDIPdeep",
              "TotFePprod","TotCaPprod","PartPremoved","PartNremoved"),
            selected = "all", multiple = TRUE, selectize=FALSE)
     )
   ),

   conditionalPanel(condition = "input.tabset == 25",
     img(src = "useR-logo.png", width = 150)
   ),

   conditionalPanel(condition = "input.tabset == 10",
     img(src = "useR-book.png", width = 150)
   )
 ),
  # Show plot 
  mainPanel(
   tabsetPanel(id = "tabset",

#    tabPanel("default steady",        value = 1,  plotOutput("CNPDIAPlot", height ="600px", width = "900px")),
    tabPanel("steady",        value = 6,  plotOutput("CNPDIAPlot2", height ="600px", width = "600px")),
    tabPanel("budget steady", value = 2,  HTML('<br><strong>Oxygen</strong>'), tableOutput("budgetO2f"), tableOutput("budgetO2r"), 
                                          HTML('<br><strong>Carbon</strong>'), tableOutput("budgetCf"), tableOutput("budgetCr"),
                                          HTML('<br><strong>Nitrogen</strong>'), tableOutput("budgetNf"), tableOutput("budgetNr"), 
                                          HTML('<br><strong>Phosphorus</strong>'), tableOutput("budgetPf"), tableOutput("budgetPr")),
    tabPanel("dynamic",       value = 15, plotOutput("dynaPlot", height ="600px", width = "900px")),
    tabPanel("select dyna 1D",value = 17, plotOutput("dynaPlot2", height ="600px", width = "600px")),
    tabPanel("select dyna 0D",value = 14, plotOutput("dynaPlot3", height ="600px", width = "600px")),
    tabPanel("budget dyna",   value = 3,  HTML('<br><strong>Oxygen</strong>'), tableOutput("budgetO2fd"), tableOutput("budgetO2rd"), 
                                          HTML('<br><strong>Carbon</strong>'), tableOutput("budgetCfd"), tableOutput("budgetCrd"),
                                          HTML('<br><strong>Nitrogen</strong>'), tableOutput("budgetNfd"), tableOutput("budgetNrd"), 
                                          HTML('<br><strong>Phosphorus</strong>'), tableOutput("budgetPfd"), tableOutput("budgetPrd")),
    tabPanel("data",          value = 18, verbatimTextOutput("dfile"), plotOutput("data", height ="600px", width = "600px")),
    tabPanel("parameters",    value = 32, downloadLink('downloadPars', label = "export parameters to csv"), tableOutput("pars")),
    tabPanel("outputs",       value = 21, downloadLink('downloadOut', label = "export steady-state solution  ."),
                                          downloadLink('downloadDyna', label = ".   export dynamic solution"), 
                                          tableOutput("out0D"), tableOutput("out1D")),
    tabPanel("sensitivity",   value = 20, plotOutput("sensitivity", height ="600px", width = "600px")),
    tabPanel("settings",      value = 31, plotOutput("settings", height ="600px", width = "600px")),
    tabPanel("filepars",      value = 33, tableOutput("filepars")),
    tabPanel("codes",         value = 25, htmlOutput("Rcode"), HTML('<a href="OMEXDIA_model.R" target="_blank">Download R code</a>'),
             htmlOutput("fortran"), HTML('<a href="omexdia.f" target="_blank">Download fortran code</a>'),
        	   HTML('<br><a href="OMEXDIA_dll.R" target="_blank">Download R code to call the fortran model</a>')),
    tabPanel("about",         value = 10, htmlOutput("about"))
   )
  )

))

