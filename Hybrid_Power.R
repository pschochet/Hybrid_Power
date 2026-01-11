# This OpenSource R program contains R code for the Hybrid_Power.R dashboard
# to conduct power calculations for multisite hybrid impact designs. It is described 
# in the package documentation pdf file. It was created by Peter Z. Schochet, Ph.D. 
# (pzschochet@gmail.com), and was posted to github in January 2026.

suppressWarnings(suppressPackageStartupMessages(library('shiny')))
suppressWarnings(suppressPackageStartupMessages(library('ggplot2')))
suppressWarnings(suppressPackageStartupMessages(library('ggtext')))
suppressWarnings(suppressPackageStartupMessages(library('shinydashboard')))
suppressWarnings(suppressPackageStartupMessages(library('kableExtra')))
suppressWarnings(suppressPackageStartupMessages(library('scales')))

#
# DEFINE THE UI
#

ui <- shinyUI(
  dashboardPage(skin='green',
    dashboardHeader(titleWidth = 333, title=HTML("<em><b><font size='3'>Hybrid_Power</font></b></em>")),
    dashboardSidebar(width=350,
      tags$style(HTML(".sidebar { height: 90vh; overflow-y: auto; }" )),
      tags$style(HTML(type='text/css', ".irs-min, .irs-max, .irs-grid-text, .irs-grid-pol { visibility: hidden !important;}")),
      sidebarMenu(

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),    # This adds te left margin
        helpText(
          div(strong(HTML("</p>This dashboard conducts power calculations </br> for a hybrid design with RCT-QED and QED-only </br> 
                          sites (see Schochet, Journal of Policy Analysis </br> and Management, 2026)
                          </p> After entering the inputs below, you can press </br> \"Submit\" at the bottom of the dashboard </br> to view
                          and save the results"))),
            style = "color: #27CF82;" )),  #27CF82;CAFF70

        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
        helpText(
          div(strong(HTML("Key design information"))),
          style = "color:#33C5FF"
        )),

        # CREATE THE SIDEBAR PANEL INPUTS
        
        radioButtons(
          inputId = "rct_design",
          label   = "RCT and QED design type",
          choices = c(
            "Nonclustered RCT and QED",
            "Clustered RCT and QED",
            "Nonclustered RCT and clustered QED",
            "Clustered RCT and Nonclustered QED"),
          selected = "Nonclustered RCT and QED",
          inline = FALSE
        ),
        
        radioButtons(
          inputId = "estimand",
          label   = "Causal estimand",
          choices = c(
            "Complier average causal effect (CACE)",
            "Intention-to-treat (ITT)"),
            #"RCT eligible applicant and QED participant samples (CACE)",
            #"Eligible applicant samples for both (ITT)"),
          inline = FALSE
        ),
        
        radioButtons(
          inputId = "var_bias",
          label   = "RCT and QED variance components",
          choices = c(
            "Include variance of QED bias corrections",
            "Exclude variance of QED bias corrections",
            "Include RCT variance only"),
          inline = FALSE
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Sample size information"))),
            style = "color:#33C5FF"
          )),
        
        numericInput(
          inputId = "se",
          label = "Number of (#) RCT-QED sites",
          min = 1,
          step = 1,
          value = 10
        ),
        
        numericInput(
          inputId = "sq",
          label = "# QED-only sites",
          min = 1,
          step = 1,
          value = 10
        ),
        
        conditionalPanel(
          'input.rct_design == "Nonclustered RCT and QED"
          | input.rct_design == "Nonclustered RCT and clustered QED"',
          numericInput(
            inputId = "nbar1e",
            label = "# RCT treatments per RCT-QED site",
            value = 100,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Nonclustered RCT and QED"
          | input.rct_design == "Nonclustered RCT and clustered QED"',
          numericInput(
            inputId = "nbar0e",
            label = "# RCT controls per RCT-QED site",
            value = 100,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Clustered RCT and QED"
          | input.rct_design == "Clustered RCT and Nonclustered QED"',
          numericInput(
            inputId = "mclus1e",
            label = "# RCT treatment clusters per RCT-QED site",
            value = 10,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Clustered RCT and QED"
          | input.rct_design == "Clustered RCT and Nonclustered QED"',
          numericInput(
            inputId = "mclus0e",
            label = "# RCT control clusters per RCT-QED site",
            value = 10,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Clustered RCT and QED"
          | input.rct_design == "Clustered RCT and Nonclustered QED"',
          numericInput(
            inputId = "nsampe",
            label = "# individuals per RCT cluster",
            value = 10,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Nonclustered RCT and QED"
          | input.rct_design == "Clustered RCT and Nonclustered QED"',
          numericInput(
            inputId = "nbar1q",
            label = "# QED treatments per site",
            value = 100,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Clustered RCT and QED"
          | input.rct_design == "Nonclustered RCT and clustered QED"',
          numericInput(
            inputId = "mclus1q",
            label = "# QED treatment clusters per site",
            value = 10,
            min = 1,
            step = 1
          )
        ),
        
        conditionalPanel(
          'input.rct_design == "Clustered RCT and QED"
          | input.rct_design == "Nonclustered RCT and clustered QED"',
          numericInput(
            inputId = "nsampq",
            label = "# individuals per QED treatment cluster",
            value = 10,
            min = 1,
            step = 1
          )
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Intraclass correlation coefficient (ICC) </br> for clustered
                            designs"))),
            style = "color:#33C5FF"
          )),
        
        conditionalPanel(
          'input.rct_design != "Nonclustered RCT and QED"',
          numericInput(
            inputId = "icc",
            label = "ICC for those in the same cluster (0 to 1)",
            min = 0,
            max = 1,
            step = .01,
            value = .05
          )
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("RCT program participation rates </br> 
                            for the CACE estimand"))),  
            style = "color:#33C5FF"
          )),
        
        conditionalPanel(
          'input.estimand == "Complier average causal effect (CACE)"',
          numericInput(
            inputId = "mu1",
            label = "Treatment group participation rate (0 to 1)",
            min = 0,
            max = 1,
            step = .01,
            value = .70
          )
        ),
        
        conditionalPanel(
          'input.estimand == "Complier average causal effect (CACE)"',
          numericInput(
            inputId = "mu0",
            label = "Control group participation rate (0 to 1)",
            min = 0,
            max = 1,
            step = .01,
            value = 0
          )
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Design effects"))),  
            style = "color:#33C5FF"
          )),
        
        numericInput(
          inputId = "deff_ipw",
          label = "Ratio of the effective # of QED comparisons 
          to the # of QED treatments accounting for design effects due to IPW weighting
          (for individuals and clusters if pertinent)",
          #label = "Design effect due to IPW weighting",
          min = .1,
          step = .1,
          value = 1
        ),
        
        numericInput(
          inputId = "deff_node",
          label = "Design effect from allocating the QED-only comparisons
          to the CART bias leaf nodes",
          min = .1,
          step = .1,
          value = 1.2
        ),
        
        radioButtons(
          inputId = "wgt_rct",
          label   = "Weighting approach to combine the RCT and QED impacts",
          choices = c(
            "Weight by inverse variances",
            "Weight by relative sample sizes"),
          inline = FALSE
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Precision gains from model covariates"))),
            style = "color:#33C5FF"
          )),
        
        numericInput(
          inputId = "r2_node",
          label = "R2 value for the CART bias function models (that reduces the within-leaf variances)",
          min = 0,
          max = .99,
          step = .01,
          value = .10
        ),
        
        numericInput(
          inputId = "r2yx",
          label = "Regression R2 value for the RCT impact models (0 to 1)",
          min = 0,
          max = .99,
          step = .01,
          value = .20
        ),
        
        conditionalPanel(
          'input.r2yx>0',
          numericInput(
            inputId = "nx",
            label = "Number of model covariates",
            value = 5,
            min = 1,
            step = 1
          )
        ),
        
        radioButtons(
          inputId = "r2yx_qed",
          label   = "Whether the R2 value also pertains to the QED impact models using the IPW weights",
          choices = c(
            "No",
            "Yes"),
          inline = FALSE
        ),
        
        fluidPage(
          tags$head(tags$style(HTML(".textInput {margin-left:5px}"))),
          helpText(
            div(strong(HTML("Hypothesis testing parameters"))),
            style = "color:#33C5FF"
          )),
        
        numericInput(
          inputId = "alpha",
          label = "Significance (alpha) level (0 to 1)",
          min = 0,
          max = .99,
          step = .01,
          value = 0.05
        ),
        
        radioButtons(
          inputId = "two_tailed",
          label   = "Two- or one-tailed test",
          choices = c(
            "Two-tailed",
            "One-tailed"),
          inline = TRUE
        ),
        
        sliderInput(
          inputId = "power",
          label = "Power level range: Calculations are conducted between min and max values",
          min = .50,
          max = .99,
          #step = 0.5,
          value = c(.60,.90)
        ),
        
        radioButtons(
          inputId = "power_by",
          label   = "Power level interval",
          choices = c(
            ".01",
            ".05",
            ".10"),
          selected = ".05",
          inline = TRUE
        ),

        div(actionButton("submit_button", "Submit"), class="form-group shiny-input-container")
       )
    ),


    # DEFINE THE MAIN PANEL ELEMENTS. A SplitLayout IS USED SO THAT THERE ARE
    # TWO PANELS FOR PRINTING THE TABLES AND GRAPHS

    dashboardBody(
     splitLayout(
       htmlOutput("tableset"),     # These are critical for printing - need html for kableExtra
       plotOutput("plotset"),      # This is for Panel 2 for the plots
       cellArgs = list(style='white-space: normal;')

       #tableOutput("table"),
       #plotOutput("plot"),
       #textOutput("vv"),
     ),

     hr(),
     textInput(
       inputId = "file_name",
       label   = "Name of file to save the graph (exclude the file path and extension)",
       value = "Hybrid_Power graph"),
     radioButtons(
       inputId = "png_pdf",
       label   = "Save graph as a png or pdf file",
       choices = c("png" = "png", "pdf" = "pdf"),
       inline = FALSE),
     downloadButton('down', 'Download Graph')
     )
    )
  )

server <- shinyServer(
  function(input, output, session) {

  # The following exits into R when exiting Shiny- this is nice!
    session$onSessionEnded(function() {
      stopApp(returnValue = invisible())
      })

    #
    # THIS IS THE MAIN PROGRAM THAT DOES ALL THE CALCULATIONS AND RETURNS THE RESULTS
    # INTO POW_RES. IT IS REACTIVE SO UPDATES WHEN SIDE BAR INPUTS ARE CHANGED
    #

    pow_res <- reactive({

      # Read in inputs and save them to variables which makes it easier to refer to them
      
      if (input$rct_design ==  "Nonclustered RCT and QED") {
        rct_design <- 1
      } else if (input$rct_design == "Clustered RCT and QED") {
        rct_design <- 2
      } else if (input$rct_design == "Nonclustered RCT and clustered QED") {
        rct_design <- 3
      } else if (input$rct_design == "Clustered RCT and Nonclustered QED") {
        rct_design <- 4
      }
      
      if (input$estimand ==  "Complier average causal effect (CACE)") {
        estimand <- 1
      } else if (input$estimand == "Intention-to-treat (ITT)") {
        estimand <- 2
      }

      if (input$var_bias ==  "Include variance of QED bias corrections") {
        var_bias <- 1
      } else if (input$var_bias == "Exclude variance of QED bias corrections") {
        var_bias <- 2
      } else if (input$var_bias == "Include RCT variance only") {
        var_bias <- 3
      }
      
      if (input$wgt_rct ==  "Weight by inverse variances") {
        wgt_rct <- 1
      } else if (input$wgt_rct == "Weight by relative sample sizes") {
        wgt_rct <- 2
      }
      
      if (input$r2yx_qed ==  "No") {
        r2yx_qed <- 1
      } else if (input$r2yx_qed == "Yes") {
        r2yx_qed <- 2
      }
      
      se      <- input$se
      sq      <- input$sq
      nbar1e  <- input$nbar1e
      nbar0e  <- input$nbar0e
      mclus1e <- input$mclus1e
      mclus0e <- input$mclus0e
      nsampe  <- input$nsampe
      nbar1q  <- input$nbar1q
      mclus1q <- input$mclus1q
      nsampq  <- input$nsampq
      icc     <- input$icc
      mu1     <- input$mu1
      mu0     <- input$mu0
      deff_ipw  <- input$deff_ipw
      deff_node <- input$deff_node
      
      alpha <- input$alpha
      
      #nbar0q  <- deff_ipw*nbar1q
      #mclus0q <- deff_ipw*mclus1q
      
      if (input$two_tailed=="Two-tailed") {
        two_tailed <- 1
      } else if (input$two_tailed=="One-tailed") {
        two_tailed <- 0
      }
      
      power   <- input$power
      power_s <- power[1]
      power_f <- power[2]
      
      if (input$power_by == ".05") {
        power_by <- .05
      } else if (input$power_by == ".01") {
        power_by <- .01
      } else if (input$power_by == ".10") {
        power_by <- .10
      }
      
      r2yx <- input$r2yx
      nx   <- input$nx
      r2_node <- input$r2_node
      
      #print(paste('se',se))
      
      # CHECK FOR ERRORS

      # Set ICC to 0 for Nonclustered design 

      if (rct_design==1) {
        icc <- 0
      } 

      # Write error messages and crash variable for key inputs
      # Test for missing values, decimal values for categories, and out of ranges

      crash <- 0
      nerr  <- 0
      err_mess <- matrix(ncol=100,nrow=1)
      
      # se and sq
      
      if ((!exists("se")) | (is.na(se)) | (se<1)
          | (se != round(se))) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid number of (#) RCT-QED sites")
      }
      
      #print(paste('err_mess',err_mess[1,1]))
      
      if ((!exists("sq")) | (is.na(sq)) | (sq<1)
          | (sq != round(sq))) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid # QED-only sites")
      }
      
      # nbar1e, nbar0e
      
      if ((rct_design==1) | (rct_design==3)) {
        if ((!exists("nbar1e")) | (is.na(nbar1e)) | (nbar1e<1)
            | (nbar1e != round(nbar1e))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # RCT treatments per RCT-QED site")
        }
        
        if ((!exists("nbar0e")) | (is.na(nbar0e)) | (nbar0e<1)
            | (nbar0e != round(nbar0e))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # RCT controls per RCT-QED site")
        }
      }
      
      # mclus1e, mclus0e, nsampe
      
      if ((rct_design==2) | (rct_design==4)) {
        if ((!exists("mclus1e")) | (is.na(mclus1e)) | (mclus1e<1)
            | (mclus1e != round(mclus1e))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # RCT treatment clusters per RCT-QED site")
        }
        
        if ((!exists("mclus0e")) | (is.na(mclus0e)) | (mclus0e<1)
            | (mclus0e != round(mclus0e))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # RCT control clusters per RCT-QED site")
        }
        
        if ((!exists("nsampe")) | (is.na(nsampe)) | (nsampe<1)
            | (nsampe != round(nsampe))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # individuals per RCT cluster")
        }
      }
      
      # nbar1q, nbar0e
      
      if ((rct_design==1) | (rct_design==4)) {
        if ((!exists("nbar1q")) | (is.na(nbar1q)) | (nbar1q<1)
            | (nbar1q != round(nbar1q))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # QED treatments per RCT-QED site")
        }
      }
      
      # mclus1q, nsampq
      
      if ((rct_design==2) | (rct_design==3)) {
        if ((!exists("mclus1q")) | (is.na(mclus1q)) | (mclus1q<1)
            | (mclus1q != round(mclus1q))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # QED treatment clusters per site")
        }
        
        if ((!exists("nsampq")) | (is.na(nsampq)) | (nsampq<1)
            | (nsampq != round(nsampq))) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid # individuals per QED cluster")
        }
      }
      
      # icc
      
      if (rct_design>1) {
        if ((!exists("icc")) | (length(icc) != 1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation coefficient")
        } else if ((is.na(icc)) | (icc > 1) | (icc < 0))
        {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid intraclass correlation coefficient")
        }
      }
      
      # mu1, mu0
      
      if (estimand==1) {
        badmu1 <- 0
        badmu0 <- 0
        
        if ((!exists("mu1")) | (length(mu1) != 1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid treatment group participation rate")
          badmu1 <- 1
        } else if ((is.na(mu1)) | (mu1 > 1) | (mu1 <= 0))
        {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid treatment group participation rate")
          badmu1 <- 1
        }
        
        if ((!exists("mu0")) | (length(mu0) != 1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid control group participation rate")
          badmu0 <- 1
        } else if ((is.na(mu0)) | (mu0 >= 1) | (mu0 < 0))
        {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Invalid control group participation rate")
          badmu0 <- 1
        }
        
        if ((badmu1==0) & (badmu0==0) & (mu0 >= mu1)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Participation rate must be larger for the treatment than control group")
        }
      }
      
      # deff_ipw
      
      if ((!exists("deff_ipw")) | (is.na(deff_ipw)) | (deff_ipw<=0)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid effective # of QED comparisons")
      }
      
      # deff_node
      
      if ((!exists("deff_node")) | (is.na(deff_node)) | (deff_node<=0)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid design effect from allocating the sample to the CART tree bias nodes")
      }
      
      # R2YX
      
      badr2 <- 0
      if ((!exists("r2yx")) | (length(r2yx) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid regression R2 value for the RCT impact models")
        badr2 <- 1
      } else if ((is.na(r2yx)) | (r2yx >= 1) | (r2yx < 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid regression R2 value from model covariates")
        badr2 <- 1
      }
      
      # NX
      
      # Set NX to 0 if R2YX=0
      
      if ((badr2==0) & (r2yx==0)) {
        nx <- 0
      }
      
      if ((!exists("nx")) | (is.na(nx)) | (nx<0)
          | (nx != round(nx))) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid number of model covariates")
      } else if ((badr2==0) & (r2yx>0) & (nx==0)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Number of model covariates must be greater than 0 if R2>0")
      }
      
      # r2_node
      
      if ((!exists("r2_node")) | (length(r2_node) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid R2 value for the CART bias function models")
      } else if ((is.na(r2_node)) | (r2_node >= 1) | (r2_node < 0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid R2 value for the CART bias function models")
        badr2 <- 1
      }
      
      # alpha

      if ((!exists("alpha")) | (length(alpha) != 1)) {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid alpha level")
      } else if ((is.na(alpha)) | (alpha>=1) | (alpha<=0))
      {
        crash <- 1
        nerr  <- nerr + 1
        err_mess[1,nerr] <- c("Invalid alpha level")
      }

      # NOW PERFORM THE VARIANCE CALCULATIONS IF crash==0
      
      if (crash==0) {

        # Calculate the t cutoff alpha depending on a two- or one-tailed test

        if (two_tailed==1) {
          alpha2 <- 1 - (alpha/2)
        } else if (two_tailed==0)
        {
          alpha2 <- 1 - alpha
        }
        
        jbar <- 10   # This j value is irrelevant since it is covered the deffnode
        
        # Do first for RCT_DESIGN==1: Both Nonclustered 
          
        if (rct_design==1) {
          
          # RCT Variance 1
          
          if (estimand == 1) {
            p10hat <- mu1-mu0
            t1_rct <- 1/(se*(p10hat^2))
            t2_rct <- (1-r2yx)*((1/nbar1e)+(1/nbar0e))
            t3_rct <- (mu1*(1-mu1)/nbar1e) + (mu0*(1-mu0)/nbar0e)
            
          } else if (estimand==2) {
            p10hat <- 1
            t1_rct <- 1/se
            t2_rct <- (1-r2yx)*((1/nbar1e)+(1/nbar0e))
            t3_rct <- 1
          }
          
          v1rct <- t1_rct*t2_rct
          
          # QED VARIANCE 1
            
          #vqed <- (1+deff_ipw)/(sq*nbar1q)
          
          nbar0q  <- deff_ipw*nbar1q
          vqed    <- (1/sq)*((1/nbar1q)+(1/nbar0q))
          
          if (r2yx_qed == 2) {
            vqed <- vqed*(1-r2yx)
          }
          
          # BIAS 1
          # RCT PART OF BIAS 1 
            
          #nbar_node0e <- nbar0e/jbar
          #nbar_node1e <- nbar1e/jbar
          
          nbar_node0e <- se*nbar0e/jbar
          nbar_node1e <- se*nbar1e/jbar
          
          if (estimand == 1) {
            pstar10 <- (mu1-mu0)/(1-mu0)
            t1_node <- 1/(nbar_node0e*(1-mu0))
            
            if (mu1<1){
              t2_node <- ((1-pstar10)^2)/(nbar_node1e*(1-mu1))
            } else if (mu1==1){
              t2_node <- 0
            }
            
            t3_node <- 1/(pstar10^2)
            
            vnode1 <- t3_node*(t1_node + t2_node)
            
          } else if (estimand == 2){
            pstar10 <- 1
            vnode1  <- 1/nbar_node0e
          } 
          
          # QED PART OF BIAS 1
            
          #nbar_node1q <- nbar1q/jbar
          #vnode2 <- deff_ipw/nbar_node1q
          
          #nbar_node1q <- sq*nbar1q/jbar
          #vnode2 <- deff_ipw/nbar_node1q
          
          nbar_node0q <- se*nbar0q/jbar
          vnode2      <- 1/nbar_node0q
          
        # NOW FOR RCT_DESIGN==2: Both Clustered
          
        } else if (rct_design == 2){
          
          deff_clusr <- 1 + icc*(nsampe-1)
          #deff_clusq <- 1 + icc*(nsampq-1)
          
          nsamp1q <- nsampq
          nsamp0q <- nsamp1q*deff_ipw
          
          deff_clus1q <- 1 + icc*(nsamp1q-1)
          deff_clus0q <- 1 + icc*(nsamp0q-1)
          
          mn1e <- nsampe*mclus1e
          mn0e <- nsampe*mclus0e
          
          #mn1q <- nsampq*mclus1q
          
          mclus0q <- deff_ipw*mclus1q
          mn1q <- nsamp1q*mclus1q
          mn0q <- nsamp0q*mclus0q
          
          # RCT VARIANCE 2
            
          if (estimand == 1){
            p10hat <- mu1-mu0
            t1_rct <- 1/(se*(p10hat^2))
            t2_rct <- (1-r2yx)*((1/mn1e)+(1/mn0e))
            t3_rct <- (mu1*(1-mu1)/mn1e) + (mu0*(1-mu0)/mn0e)
            
          } else if (estimand==2){
            p10hat <- 1
            t1_rct <- 1/se
            t2_rct <- (1-r2yx)*((1/mn1e)+(1/mn0e))
            t3_rct <- 1
          } 
          
          v1rct <- t1_rct*t2_rct
          v1rct <- v1rct*deff_clusr
          
          # QED VARIANCE 2
            
          #vqed <- (1+deff_ipw)/(sq*mn1q)
          #vqed <- vqed*deff_clusq
          
          vqed <- (1/sq)*((deff_clus1q/mn1q)+(deff_clus0q/mn0q))
          
          if (r2yx_qed == 2){
            vqed <- vqed*(1-r2yx)
          }
          
          # BIAS 2
          # RCT PART OF BIAS 2 
            
          #nbar_node0e <- mn0e/jbar
          #nbar_node1e <- mn1e/jbar
          
          nbar_node0e <- se*mn0e/jbar
          nbar_node1e <- se*mn1e/jbar
          
          if (estimand == 1){
            pstar10 <- (mu1-mu0)/(1-mu0)
            t1_node <- 1/(nbar_node0e*(1-mu0))
            
            if (mu1 < 1){
              t2_node <- ((1-pstar10)^2)/(nbar_node1e*(1-mu1))
            } else if (mu1 == 1){
              t2_node <- 0
            }  
            
            t3_node <- 1/(pstar10^2)
            
            vnode1 <- t3_node*(t1_node + t2_node)
            vnode1 <- vnode1*deff_clusr
            
          } else if (estimand == 2){
            pstar10 <- 1
            vnode1  <- 1/nbar_node0e
            vnode1  <- vnode1*deff_clusr
          } 
          
         # QED PART OF BIAS 2
            
          #nbar_node1q <- mn1q/jbar
          #nbar_node1q <- se*mn1q/jbar
          #vnode2 <- deff_ipw/nbar_node1q
          #vnode2 <- vnode2*deff_clusq
          
          nbar_node0q <- se*mn0q/jbar
          vnode2      <- deff_clus0q/nbar_node0q
          
        # NOW for RCT_DESIGN=3: RCT Nonclustered, QED Clustered 
        
        } else if (rct_design==3){
          #deff_clusq <- 1 + icc*(nsampq-1)
          
          #mn1q <- nsampq*mclus1q
          
          nsamp1q <- nsampq
          nsamp0q <- nsamp1q*deff_ipw
          
          deff_clus1q <- 1 + icc*(nsamp1q-1)
          deff_clus0q <- 1 + icc*(nsamp0q-1)
          
          mclus0q <- deff_ipw*mclus1q
          mn1q <- nsamp1q*mclus1q
          mn0q <- nsamp0q*mclus0q
          
          # RCT VARIANCE 3
            
          if (estimand == 1){
            p10hat <- mu1-mu0
            t1_rct <- 1/(se*(p10hat^2))
            t2_rct <- (1-r2yx)*((1/nbar1e)+(1/nbar0e))
            t3_rct <- (mu1*(1-mu1)/nbar1e) + (mu0*(1-mu0)/nbar0e)
            
          } else if (estimand==2){
            p10hat <- 1
            t1_rct <- 1/se
            t2_rct <- (1-r2yx)*((1/nbar1e)+(1/nbar0e))
            t3_rct <- 1
          } 
          
          v1rct <- t1_rct*t2_rct
          
          # QED VARIANCE 3
            
          #vqed <- (1+deff_ipw)/(sq*mn1q)
          #vqed <- vqed*deff_clusq
          
          vqed <- (1/sq)*((deff_clus1q/mn1q)+(deff_clus0q/mn0q))
          
          if (r2yx_qed == 2){
            vqed <- vqed*(1-r2yx)
          }  
          
          # BIAS 3
          # RCT PART OF BIAS 3 
            
          #nbar_node0e <- nbar0e/jbar
          #nbar_node1e <- nbar1e/jbar
          
          nbar_node0e <- se*nbar0e/jbar
          nbar_node1e <- se*nbar1e/jbar
          
          if (estimand == 1){
            pstar10 <- (mu1-mu0)/(1-mu0)
            t1_node <- 1/(nbar_node0e*(1-mu0))
            
            if (mu1 < 1){
              t2_node <- ((1-pstar10)^2)/(nbar_node1e*(1-mu1))
            } else if (mu1 == 1){
              t2_node <- 0
            }  
            
            t3_node <- 1/(pstar10^2)
            
            vnode1 <- t3_node*(t1_node + t2_node)
            
          } else if (estimand == 2){
            pstar10 <- 1
            vnode1  <- 1/nbar_node0e
          } 
          
          # QED PART OF BIAS 3
            
          #nbar_node1q <- mn1q/jbar
          #nbar_node1q <- se*mn1q/jbar
          
          #vnode2 <- deff_ipw/nbar_node1q
          #vnode2 <- vnode2*deff_clusq
          
          nbar_node0q <- se*mn0q/jbar
          vnode2      <- deff_clus0q/nbar_node0q
          
          #print(paste('rct_design',rct_design))
          
          # NOW FOR RCT_DESIGN=4: RCT Clustered QED Nonclustered
          
        } else if (rct_design==4){
          
          deff_clusr <- 1 + icc*(nsampe-1)
          
          mn1e <- nsampe*mclus1e
          mn0e <- nsampe*mclus0e
          
          # RCT VARIANCE 4
            
          if (estimand == 1){
            p10hat <- mu1-mu0
            t1_rct <- 1/(se*(p10hat^2))
            t2_rct <- (1-r2yx)*((1/mn1e)+(1/mn0e))
            t3_rct <- (mu1*(1-mu1)/mn1e) + (mu0*(1-mu0)/mn0e)
            
          } else if (estimand==2){
            p10hat <- 1
            t1_rct <- 1/se
            t2_rct <- (1-r2yx)*((1/mn1e)+(1/mn0e))
            t3_rct <- 1
          }
          
          v1rct <- t1_rct*t2_rct
          v1rct <- v1rct*deff_clusr
          
          # QED VARIANCE 4
            
          #vqed <- (1+deff_ipw)/(sq*nbar1q)
          
          nbar0q  <- deff_ipw*nbar1q
          vqed    <- (1/sq)*((1/nbar1q)+(1/nbar0q))
          
          if (r2yx_qed == 2){
            vqed <- vqed*(1-r2yx)
          }
          
          # BIAS 4
          # RCT PART OF BIAS 4
            
          #nbar_node0e <- mn0e/jbar
          #nbar_node1e <- mn1e/jbar
          
          nbar_node0e <- se*mn0e/jbar
          nbar_node1e <- se*mn1e/jbar
          
          if (estimand == 1){
            pstar10 <- (mu1-mu0)/(1-mu0)
            t1_node <- 1/(nbar_node0e*(1-mu0))
            
            if (mu1 < 1){
              t2_node <- ((1-pstar10)^2)/(nbar_node1e*(1-mu1))
            } else if (mu1 == 1){
              t2_node <- 0
            } 
            
            t3_node <- 1/(pstar10^2)
            
            vnode1 <- t3_node*(t1_node + t2_node)
            vnode1 <- vnode1*deff_clusr
            
          } else if (estimand == 2){
            pstar10 <- 1
            vnode1  <- 1/nbar_node0e
            vnode1  <- vnode1*deff_clusr
          } 
          
          # QED PART OF BIAS 4
            
          #nbar_node1q <- nbar1q/jbar
          #nbar_node1q <- se*nbar1q/jbar
          #vnode2 <- deff_ipw/nbar_node1q
          
          nbar_node0q <- se*nbar0q/jbar
          vnode2      <- 1/nbar_node0q
          
          #print(paste('rct_design',rct_design))
          
        } # End RCT_DESIGN==4
        
        # PUT TOGETHER RCT AND QED BIAS PARTS 
          
        vnode <- (1-r2_node)*(vnode1+vnode2)
        
        #vbias <- vnode*deff_node/(se*jbar)
        
        vbiasa <- vnode*deff_node/jbar
        
        # Add variance of u as in article to account for forecast model
        
        vbias <- vbiasa*(1+(se/sq))
        #vbias <- vbiasa
        
        # GET TOTAL QED VARIANCE USING VBIAS 
        
        vqed_b <- vqed + vbias
        
        if ((var_bias == 1) | (var_bias == 3)){
          vqed_bias <- vqed + vbias
        } else if (var_bias == 2){
          vqed_bias <- vqed
        } 
        
        #print(paste('vqed',vqed))
        
        # CALCULATE MDE formula in (D.14) 
          
        # First calculate RCT weight 
          
        if (rct_design == 1){
          nn_e <- nbar1e + nbar0e
          nn_q <- nbar1q+nbar0q
          #nn_q <- nbar1q+(nbar1q/deff_ipw)
          
        } else if (rct_design == 2){
          nn_e <- mn1e + mn0e
          nn_q <- mn1q + mn0q
          mm_e <- mclus1e + mclus0e
          mm_q <- mclus1q + mclus0q
          #nn_q <- mn1q + (mn1q/deff_ipw)
          #mm_q <- mclus1q + (mclus1q/deff_ipw)
          
        } else if (rct_design == 3){
          nn_e <- nbar1e + nbar0e
          nn_q <- mn1q + mn0q
          mm_q <- mclus1q + mclus0q
          #nn_q <- mn1q + (mn1q/deff_ipw)
          #mm_q <- mclus1q + (mclus1q/deff_ipw)
          
        } else if (rct_design == 4){
          nn_e <- mn1e + mn0e
          nn_q <- nbar1q+nbar0q
          mm_e <- mclus1e + mclus0e
          #nn_q <- nbar1q+(nbar1q/deff_ipw)
        } 
        
        if (wgt_rct == 1){
          wrct <- vqed_bias / (v1rct + vqed_bias)
          
        } else if (wgt_rct == 2){
          wrct <- (se*nn_e*p10hat)/((se*nn_e*p10hat) + (sq*nn_q))
        }  
        
        if (var_bias == 3){
          wrct <- 1
        }  
        
        if (estimand == 1){
          v2rct <- (wrct^2)*t1_rct*t3_rct
        } else if (estimand == 2){
          v2rct <- 0
        } 
        
        # Total QED + RCT variance 
        
        vterm <- (wrct**2)*v1rct + ((1-wrct)**2)*vqed_bias
        
        if (var_bias == 3){
          vterm <- v1rct
        } 
        
        seterm <- vterm^.5
        
        # Variance using the RCT sample only 
        
        vterm_rct <- v1rct
        seterm_rct <- vterm_rct^.5
        
        # Variance that excludes the variance from the bias 
        
        vterm_nob  <- (wrct^2)*v1rct + ((1-wrct)^2)*vqed
        seterm_nob <- vterm_nob^.5
        
        #print(paste('vterm',vterm))
        
        # Cut Bias 
        
        cut_bias <- (vqed_b-vqed)^.5
        
        #print(paste('cut_bias',cut_bias))
        
        # Calculate alpha level, number of xs, and df for later power formula
        
        if (two_tailed == 1){
          alpha2 <- 1 - (alpha/2)
        } else if (two_tailed == 0){
          alpha2 <- 1 - alpha
        }
        
        if (r2yx == 0){
          nxx <- 0
        } else if ((r2yx > 0) & (r2yx_qed == 1)){
          nxx <- nx
        } else if ((r2yx > 0) & (r2yx_qed == 2)){
          nxx <- 2*nx
        } 
        
        if (rct_design == 1){
          df  <- se*nn_e + sq*nn_q - 2*se - 2*sq - nxx
          dfr <- se*nn_e - 2*se - nx
          
          if (var_bias == 3){
            df  <- se*nn_e - 2*se - nx
            dfr <- df
          } 
        } else if (rct_design == 2){
          df  <- se*mm_e + sq*mm_q - 2*se - 2*sq - nxx
          dfr <- se*mm_e - 2*se - nx
          
          if (var_bias == 3){
            df  <- se*mm_e - 2*se - nx
            dfr <- df
          } 
        } else if (rct_design == 3){
          df  <- se*nn_e + sq*mm_q - 2*se - 2*sq - nxx
          dfr <- se*nn_e - 2*se - nx
          
          if (var_bias == 3){
            df  <- se*nn_e - 2*se - nx
            dfr <- df
          } 
        } else if (rct_design == 4){
          df  <- se*mm_e + sq*nn_q - 2*se - 2*sq - nxx
          dfr <- se*mm_e - 2*se - nx
          
          if (var_bias == 3){
            df  <- se*mm_e - 2*se - nx
            dfr <- df
          }
        } 
        
        #print(paste('df',df))
        
        # Check for negative degrees of freedom
        
        baddf <- 0
        
        df_chk <- df
        if ((df_chk <= 0) & (baddf == 0)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Degrees of freedom are negative: Increase sample sizes or reduce the number of covariates")
          baddf <- 1
        }
        
        if (df_chk <= 0) {
          df_chk <- 1
        }
        
        baddfr <- 0
        
        df_chkr <- dfr
        if ((df_chkr <= 0) & (baddfr == 0)) {
          crash <- 1
          nerr  <- nerr + 1
          err_mess[1,nerr] <- c("Degrees of freedom are negative: Increase RCT sample sizes or reduce the number of covariates")
          baddfr <- 1
        }
        
        if (df_chkr <= 0) {
          df_chkr <- 1
        }
        
        inv_alpha2 <- qt(alpha2,df_chk)
        inv_alpha2r <- qt(alpha2,df_chkr)
        
        #print(paste('inv_alpha2',inv_alpha2))
        
        # Do over power loop 
        
        ps <- round(power_s*100)
        pf <- round(power_f*100)
        pb <- round(power_by*100)
        
        countp <- 1
        for (powert in seq(ps,pf,pb)){
          
          power <- powert/100
          
          #print(paste('power',power))
  
          inv_power  <- qt(power,df_chk)
          
          factor <- inv_alpha2 + inv_power
          
          # Calculate MDE for full RCT and QED variance
          
          fterm <- (1-((factor^2)*v2rct))^.5
          
          mde_val <- factor*seterm/fterm
          
          #print(paste('power',power))
          #print(paste('mde_val',mde_val))
          
          # MDE using the RCT sample only 
          
          inv_powerr  <- qt(power,df_chkr)
          
          factorr <- inv_alpha2r + inv_powerr
          
          mde_rct <- factorr*seterm_rct/fterm
          
          # MDE that excludes the variance from the bias 
        
          mde_nob <- factor*seterm_nob/fterm
          
          # Write results to the data frame resgt and stack across power levels in resg
          
          power1    <- format(power, digits=2, nsmall=2)
          #mde_val1 <- format(mde_val, digits=2, nsmall = 2)
          mde_val1  <- format(round(mde_val,3), nsmall = 3)
          cut_bias1 <- format(round(cut_bias,3), nsmall = 3)
          resgt     <- data.frame(power,mde_val,power1,mde_val1,cut_bias1)
          
          if (countp==1) {
            resg <- resgt
          } else if (countp>1) {
            resg <- rbind(resg,resgt)
          }
          
          countp <- countp + 1
          
      }  # End of Power Level loop
        
        #print(paste('resg',resg))
        
    } # End of if Crash=0  
    
    #
    # WRITE RESULTS TO POW_RES WHICH IS A DATA FRAME
    #
    
    if (crash==0) {
      pow_res <- resg
      
    } else if (crash==1) {
      err_out <- data.frame(err_mess[1:nerr])
      colnames(err_out) <- c("Errors")
      pow_res <- err_out
    }
  }) # end pow_res reactive
          
    #
    # RETRIEVES THE CRASH VARIABLE NEEDED TO DEFINE THE TABLES AND PLOTS
    #

    crashv <- reactive({
      if (ncol(pow_res())==1) {
        crashv <- 1
      } else if (ncol(pow_res())>1)
      {
        crashv <- 0
      }
    })

    #
    # CREATES THE KABLEEXTRA CODE FOR THE TABLES TO RUN LATER
    # DEPENDING ON THE CRASH VARIABLE
    #

    # First get caption for the tables

    cap <- reactive({
      
      if (input$rct_design ==  "Nonclustered RCT and QED") {
        rct_designz <- 1
      } else if (input$rct_design == "Clustered RCT and QED") {
        rct_designz <- 2
      } else if (input$rct_design == "Nonclustered RCT and clustered QED") {
        rct_designz <- 3
      } else if (input$rct_design == "Clustered RCT and Nonclustered QED") {
        rct_designz <- 4
      }
      
      #print(paste('rct_designz',rct_designz))
      
      if (input$estimand ==  "Complier average causal effect (CACE)") {
        estimandz <- 1
      } else if (input$estimand == "Intention-to-treat (ITT)") {
        estimandz <- 2
      }
      
      if (input$var_bias ==  "Include variance of QED bias corrections") {
        var_biasz <- 1
      } else if (input$var_bias == "Exclude variance of QED bias corrections") {
        var_biasz <- 2
      } else if (input$var_bias == "Include RCT variance only") {
        var_biasz <- 3
      }
      
      if (input$wgt_rct ==  "Weight by inverse variances") {
        wgt_rctz <- 1
      } else if (input$wgt_rct == "Weight by relative sample sizes") {
        wgt_rctz <- 2
      }
      
      if (input$r2yx_qed ==  "No") {
        r2yx_qedz <- 1
      } else if (input$r2yx_qed == "Yes") {
        r2yx_qedz <- 2
      }
      
      sez      <- input$se
      sqz      <- input$sq
      
      if (rct_designz==1) {
        dsgn <- c("Nonclustered RCT and Nonclustered QED")
      } else if (rct_designz==2) {
        dsgn <- c("Clustered RCT and Clustered QED")
      } else if (rct_designz==3) {
        dsgn <- c("Nonclustered RCT and Clustered QED")
      } else if (rct_designz==4) {
        dsgn <- c("Clustered RCT and Nonclustered QED")
      }
      
      if (estimandz==1) {
        estm <- c("CACE")
      } else if (estimandz==2) {
        estm <- c("ITT")
      }
      
      if (var_biasz==1) {
        vb <- c("(Includes Variances for the QED Bias Corrections)")
      } else if (var_biasz==2) {
        vb <- c("(Excludes Variances for the QED Bias Corrections)")
      } else if (var_biasz==3) {
        vb <- c("Using only the RCT impacts")
      } 
      
      sez      <- format(sez,digits=1)
      sqz      <- format(sqz,digits=1)
      
      if (crashv()==0) {
        
        cap1 <- sprintf("MDE Results for the %s",dsgn)
        cap2 <- sprintf("for the %s Estimand with",estm)
        cap3 <- sprintf("%s RCT-QED and %s QED-only Sites",sez,sqz)
        cap4 <- sprintf("%s",vb)
        cap  <- data.frame(cap1,cap2,cap3,cap4)
        
        #print(paste('cap',cap))
        #print(paste('cap2',cap2))

      } else NULL
    }) # end cap reactive

    cap_tab <- reactive({
      if (crashv()==0) {
        cap_tab <- paste(cap()[1],cap()[2],cap()[3],cap()[4])
      } else NULL
    })

    cap_grph <- reactive({
      if (crashv()==0) {
        cap_grph1 <- paste(cap()[1],"\n",cap()[2])
        cap_grph2 <- paste(cap_grph1,"\n",cap()[3])
        cap_grph  <- paste(cap_grph2,"\n",cap()[4])
      } else NULL
    })

    # Now for column names

    cn <- reactive({
      
      if (input$rct_design ==  "Nonclustered RCT and QED") {
        rz <- 1
      } else if (input$rct_design == "Clustered RCT and QED") {
        rz <- 2
      } else if (input$rct_design == "Nonclustered RCT and clustered QED") {
        rz <- 3
      } else if (input$rct_design == "Clustered RCT and Nonclustered QED") {
        rz <- 4
      }
      
      #print(paste('rz',rz))
      
      if (input$estimand ==  "Complier average causal effect (CACE)") {
        ez <- 1
      } else if (input$estimand == "Intention-to-treat (ITT)") {
        ez <- 2
      }
      
      #print(paste('ez',ez))
      
      if (input$var_bias ==  "Include variance of QED bias corrections") {
        vbz <- 1
      } else if (input$var_bias == "Exclude variance of QED bias corrections") {
        vbz <- 2
      } else if (input$var_bias == "Include RCT variance only") {
        vbz <- 3
      }
      
      #print(paste('vbz',vbz))
      
      cn <- c("Power Level", "MDE Value") 
      
    })

    # Now for the footnote

    fn <- reactive({
      
      if (input$rct_design ==  "Nonclustered RCT and QED") {
        rz <- 1
      } else if (input$rct_design == "Clustered RCT and QED") {
        rz <- 2
      } else if (input$rct_design == "Nonclustered RCT and clustered QED") {
        rz <- 3
      } else if (input$rct_design == "Clustered RCT and Nonclustered QED") {
        rz <- 4
      }
      
      #print(paste('rz',rz))
      
      if (input$estimand ==  "Complier average causal effect (CACE)") {
        ez <- 1
      } else if (input$estimand == "Intention-to-treat (ITT)") {
        ez <- 2
      }
      
      if (input$var_bias ==  "Include variance of QED bias corrections") {
        vbz <- 1
      } else if (input$var_bias == "Exclude variance of QED bias corrections") {
        vbz <- 2
      } else if (input$var_bias == "Include RCT variance only") {
        vbz <- 3
      }
      
      if (input$r2yx_qed ==  "No") {
        r2qz <- 1
      } else if (input$r2yx_qed == "Yes") {
        r2qz <- 2
      }
      
      sez      <- input$se
      sqz      <- input$sq
      nbar1ez  <- input$nbar1e
      nbar0ez  <- input$nbar0e
      mclus1ez <- input$mclus1e
      mclus0ez <- input$mclus0e
      nsampez  <- input$nsampe
      nbar1qz  <- input$nbar1q
      mclus1qz <- input$mclus1q
      nsampqz  <- input$nsampq
      iccz     <- input$icc
      mu1z     <- input$mu1
      mu0z     <- input$mu0
      deff_ipwz  <- input$deff_ipw
      deff_nodez <- input$deff_node
      
      alphaz     <- input$alpha
      r2yxz      <- input$r2yx
      r2_nodez   <- input$r2_node
      nxz        <- input$nx
  
      if (input$two_tailed=="Two-tailed") {
        twotz <- c("Two-tailed")
      } else if (input$two_tailed=="One-tailed") {
        twotz <- c("One-tailed")
      }

      if (rz==1) {
        #icc <- 0
        iccz <- 0
      } 

      # Set NX and NXX to 0 if R2YX=0

      if (r2yxz==0) {
        nxz <- 0
      }
      
      if (r2yxz == 0){
        nxxz <- 0
      } else if ((r2yxz > 0) & (r2qz == 1)){
        nxxz <- nxz
      } else if ((r2yxz > 0) & (r2qz == 2)){
        nxxz <- 2*nxz
      } 

      # Calculate the design effect due to clustering

      if (rz==1) {
        deff_clusrz <- 1
        deff_clusqz <- 1
      } else if (rz==2) {
        deff_clusr <- 1 + iccz*(nsampez-1)
        deff_clusq <- 1 + iccz*(nsampqz-1)
      } else if (rz==3) {
        deff_clusr <- 1 
        deff_clusq <- 1 + iccz*(nsampqz-1)
      } else if (rz==4){
        deff_clusr <- 1 + iccz*(nsampez-1)
        deff_clusq <- 1
      }
      
      # Degrees of freedom

      if (rz == 1){
        nn_ez <- nbar1ez + nbar0ez
        nn_qz <- nbar1qz+(nbar1qz/deff_ipwz)
      
        dfz  <- sez*nn_ez + sqz*nn_qz - sez - sqz - nxxz
        dfrz <- sez*nn_ez - sez - nxz
      
        if (vbz == 3){
          dfz  <- sez*nn_ez - sez - nxz
          dfrz <- dfz
        }
      
      } else if (rz == 2){
        mm_ez <- mclus1ez + mclus0ez
        mm_qz <- mclus1qz + (mclus1qz/deff_ipwz)
        
        dfz  <- sez*mm_ez + sqz*mm_qz - sez - sqz - nxxz
        dfrz <- sez*mm_ez - sez - nxz
        
        if (vbz == 3){
          dfz  <- sez*mm_ez - sez - nxz
          dfrz <- dfz
        } 
        
      } else if (rz == 3){
        nn_ez <- nbar1ez + nbar0ez
        mm_qz <- mclus1qz + (mclus1qz/deff_ipwz)
        
        dfz  <- sez*nn_ez + sqz*mm_qz - sez - sqz - nxxz
        dfrz <- sez*nn_ez - sez - nxz
        
        if (vbz == 3){
          dfz  <- sez*nn_ez - sez - nxz
          dfrz <- dfz
        }
        
      } else if (rz == 4){
        nn_qz <- nbar1qz+(nbar1qz/deff_ipwz)
        mm_ez <- mclus1ez + mclus0ez
        
        dfz  <- sez*mm_ez + sqz*nn_qz - sez - sqz - nxxz
        dfrz <- sez*mm_ez - sez - nxz
        
        if (vbz == 3){
          dfz  <- sez*mm_ez - sez - nxz
          dfrz <- dfz
        }
      }
      
      #print(paste('dfz',dfz))
      
      sez      <- format(sez,digits=1)
      sqz      <- format(sqz,digits=1)
      nbar1ez  <- format(nbar1ez,digits=1)
      nbar0ez  <- format(nbar0ez,digits=1)
      mclus1ez <- format(mclus1ez,digits=1)
      mclus0ez <- format(mclus0ez,digits=1)
      nsampez  <- format(nsampez,digits=1)
      nbar1qz  <- format(nbar1qz,digits=1)
      mclus1qz <- format(mclus1qz,digits=1)
      nsampqz  <- format(nsampqz,digits=1)
      iccz     <- format(iccz,digits=2, nsmall=2)
      mu1z     <- format(mu1z,digits=2, nsmall=2)
      mu0z     <- format(mu0z,digits=2, nsmall=2)
      deff_ipwz  <- format(deff_ipwz,digits=2, nsmall=2)
      deff_nodez <- format(deff_nodez,digits=2, nsmall=2)
      r2yxz      <- format(r2yxz,digits=2, nsmall=2)
      r2_nodez   <- format(r2_nodez,digits=2, nsmall=2)
      alphaz     <- format(alphaz,digits=2, nsmall=2)
      df         <- format(dfz,digits=8)
      
      # Retrieve cut bias
      
      cbza <- pow_res()[5]
      
      cbz <- cbza[1,1]
      
      #print(paste('cbz',cbz))
      #print(paste('deff_ipwz',deff_ipwz))
      
      if (crashv()==0) {
        
        if ((rz == 1) & (ez == 1) & (vbz < 3)) {

          txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s, QED treatments per site = %s,")
          txt2 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
          txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s, ")
          #txt3 <- c("IPW design effect = %s, CART allocation design effect = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s, ")
          txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4)
          
          fn <- sprintf(txtg,nbar1ez,nbar0ez,nbar1qz,mu1z,mu0z,deff_ipwz,deff_nodez,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
          #print(paste('txtg',txtg))
          #print(paste('fn',fn))

        } else if ((rz == 1) & (ez == 2) & (vbz < 3)){
          
          txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s, QED treatments per site = %s,")
          txt2 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s, ")
          txt3 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3)
          
          fn <- sprintf(txtg,nbar1ez,nbar0ez,nbar1qz,deff_ipwz,deff_nodez,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 2) & (ez == 1) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
          txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
          txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
          txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4,txt5)
          
          fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,mclus1qz,nsampqz,mu1z,mu0z,
                        deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 2) & (ez == 2) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
          txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
          txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4)
          
          fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,mclus1qz,nsampqz,
                        deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 3) & (ez == 1) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s,")
          txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
          txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
          txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4,txt5)
          
          fn <- sprintf(txtg,nbar1ez,nbar0ez,mclus1qz,nsampqz,mu1z,mu0z,
                        deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 3) & (ez == 2) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s,")
          txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
          txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4)
          
          fn <- sprintf(txtg,nbar1ez,nbar0ez,mclus1qz,nsampqz,deff_ipwz,
                        deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 4) & (ez == 1) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
          txt2 <- c("QED treatments per site = %s,")  
          txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
          txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4,txt5)
          
          fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,nbar1qz,mu1z,mu0z,
                        deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 4) & (ez == 2) & (vbz < 3)) {
          
          txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
          txt2 <- c("QED treatments per site = %s,")  
          txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
          txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s, Bias cutoff to yield smaller MSE for bias-adjusted estimator = %s")
          
          txtg <- paste(txt1,txt2,txt3,txt4)
          
          fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,nbar1qz,deff_ipwz,
                        deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df,cbz)
          
        } else if ((rz == 1) & (ez == 1) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s, QED treatments per site = %s,")
            txt2 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
            txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s, ")
            txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4)
            
            fn <- sprintf(txtg,nbar1ez,nbar0ez,nbar1qz,mu1z,mu0z,deff_ipwz,deff_nodez,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
            #print(paste('txtg',txtg))
            #print(paste('fn',fn))
            
          } else if ((rz == 1) & (ez == 2) & (vbz = 3)){
            
            txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s, QED treatments per site = %s,")
            txt2 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s, ")
            txt3 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3)
            
            fn <- sprintf(txtg,nbar1ez,nbar0ez,nbar1qz,deff_ipwz,deff_nodez,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 2) & (ez == 1) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
            txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
            txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
            txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4,txt5)
            
            fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,mclus1qz,nsampqz,mu1z,mu0z,
                          deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 2) & (ez == 2) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
            txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
            txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4)
            
            fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,mclus1qz,nsampqz,
                          deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 3) & (ez == 1) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s,")
            txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
            txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
            txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4,txt5)
            
            fn <- sprintf(txtg,nbar1ez,nbar0ez,mclus1qz,nsampqz,mu1z,mu0z,
                          deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 3) & (ez == 2) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatments per site = %s, RCT controls per site = %s,")
            txt2 <- c("QED treatment clusters per site = %s, Individuals per QED cluster = %s,")  
            txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4)
            
            fn <- sprintf(txtg,nbar1ez,nbar0ez,mclus1qz,nsampqz,deff_ipwz,
                          deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 4) & (ez == 1) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
            txt2 <- c("QED treatments per site = %s,")  
            txt3 <- c("RCT treatment participation rate = %s, RCT control participation rate = %s,")
            txt4 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt5 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4,txt5)
            
            fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,nbar1qz,mu1z,mu0z,
                          deff_ipwz,deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
          } else if ((rz == 4) & (ez == 2) & (vbz = 3)) {
            
            txt1 <- c("Inputs: RCT treatment clusters per site = %s, RCT control clusters per site = %s, Individuals per RCT cluster = %s,")
            txt2 <- c("QED treatments per site = %s,")  
            txt3 <- c("IPW design effect ratio = %s, CART allocation design effect = %s, ICC = %s, R2 for CART = %s, R2 for impact model = %s, Covariates = %s,")
            txt4 <- c("Alpha = %s, %s test, Degrees of freedom = %s")
            
            txtg <- paste(txt1,txt2,txt3,txt4)
            
            fn <- sprintf(txtg,mclus1ez,mclus0ez,nsampez,nbar1qz,deff_ipwz,
                          deff_nodez,iccz,r2_nodez,r2yxz,nxxz,alphaz,twotz,df)
            
        } else NULL
      }
    })

    ktab <- reactive({

      if (crashv() == 0) {
      kbl(pow_res()[3:4],
          caption = paste("<center><strong>", cap_tab(), "<center><strong>"),
          align=c("c","c"),escape=FALSE,col.names=cn()) %>%
          kable_styling(full_width=FALSE,position="left",
          bootstrap_options = "striped") %>%
          column_spec(1, color ="#00aff5",bold=TRUE) %>%          #007bbc
          column_spec(2, color ="#00aff5",bold=TRUE) %>%
          footnote(general = fn(), fixed_small_size = TRUE)

      } else if (crashv() == 1)
      {
      kbl(pow_res()) %>%
          kable_styling(full_width=FALSE,position="left") %>%
          column_spec(1, color = "red", width = 5)
      }
    })

    #
    # CREATES THE GGPLOT2 CODE FOR THE PLOTS TO RUN LATER
    # DEPENDING ON THE CRASH AND SAMP_SIZE VARIABLE
    #

    grph <- reactive({

      if (crashv() == 0) {
        ggplot(
          pow_res()[1:2],                     # Reads the data
          aes(x = power,                      # aes sets the axes
              y = mde_val,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          labs(x = cn()[1], y = cn()[2]) +
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))
      } else NULL

    })

    # This one is for downloading

    grph1 <- reactive({

      if (crashv() == 0) {
        ggplot(
          pow_res()[1:2],
          aes(x = power,
              y = mde_val,
              group = 1)) +
          geom_line(color="#00aff5",size=1.2) +
          labs(title = cap_grph(), caption = fn(), x = cn()[1], y = cn()[2]) +
          # Allows the footnote to wrap and adds space before it
          theme(plot.caption=element_textbox_simple(hjust=0, size=10, margin=margin(10,0,0,0))) +
          theme(plot.title = element_text(hjust = 0.5)) +        # This centers the title
          theme(axis.title.x = element_text(face="bold")) +
          theme(axis.title.y = element_text(face="bold"))

      } else NULL

    })

  #
  # CREATE SUBMIT BUTTON AND MAKE TABLES IN PANEL 1 AND PLOTS IN PANEL 2
  #

    # Need to reset the output file name for those who leave it blank or the download produces an error
    filen <- reactive({
      ifelse(input$file_name != "", input$file_name, "Hybrid_Power graph")
    })

  observe({

    if (input$submit_button > 0) {
      #output$vv <- renderText({ tt() })
      #output$table <- renderTable({ crashv() })

      output$tableset <- renderText({ ktab() })

      if (is.null(grph())) {
        output$plotset <- renderPlot(NULL)
      } else
      {
        output$plotset <- renderPlot({ grph() })

        #Download table to file
        output$down <- downloadHandler(
          filename = function() {
            paste(filen(), input$png_pdf, sep=".")
          },
          content = function(file) {
            plot_function <- match.fun(input$png_pdf)

            plot_function(file)
            print( grph1() )
            #save_kable(ktab())
            dev.off()
          }
        )
      }
    }
  })

})  # End server

suppressMessages(
  suppressWarnings(
    runApp(shinyApp(ui = ui, server = server))
))




