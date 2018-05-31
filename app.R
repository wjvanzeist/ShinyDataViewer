#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above. http://shiny.rstudio.com/
#

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(tools)
library(reshape)
library(reshape2)
library(plyr)
library(dplyr)
library(data.table)
library(knitr)
library(tidyr)
library(colourpicker)
library(shiny)
library(shinyjs)

# library(Cairo)

source('./plot_build.R')
source('./WJlib.R')

# Initialzing global vars ---------------------------------------------
if(dir.exists("../data_files")){datapath="../data_files"}else{datapath="./"}
dir.create("../output_plots", showWarnings = FALSE)
csv_filenames <- list.files(path=datapath, pattern = "\\.csv$")
rda_filenames <- list.files(path=datapath, pattern = "\\.rda$")
ini_filenames <- list.files(path="./settings/", pattern = "\\.ini$")
color_ssp <- c("#00FF00","#0000FF","#FF0000","#FFB900","#FF00FF")
color_decomp <- c("#FFCC00", "#C0504D", "#9BBB59", "#8064A2", "#4BC0C6", "#F79646") #, "#797A7A", "#376092")
color_sspland1 <- c('#b2df8a','#33a02c','#6a3d9a','#fdbf6f','#ff7f00','#1f78b4','#e31a1c')
color_sspland2 <- c('#bebada','#fb8072','#8dd3c7','#80b1d3','#ffffb3')

### UI code  ============
# This main bit 'ui' contains all the frontend bits, defining menu items etc.
# ===

ui <- function(request) {
  
  useShinyjs()
  
  fluidPage(
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(id="tabs",
         tabPanel("Main", id="Main",
                  p('Selection will influence order of plots. Selection applies to all plots'),
                  uiOutput("flex_options"),
                  textAreaInput("plot_text_add", "Add code lines to plot build function:", value="#G1 = G1 + code to add to the plot")
                  
         ),
         tabPanel("Theme",
                  selectInput("themeopt", "select theme:", choices=c("theme_bw", "theme_classic", "theme_dark","theme_gray","theme_light","theme_minimal","theme_void", "Other"), selected="theme_classic"),
                  sliderInput('TextSize', 'Text size adjustment', min=-16, max=20, value=0, step=1, round=0),
                  colourpicker::colourInput("panel.background.color", "Panel background color:", value=""),
                  colourpicker::colourInput("panel.grid.color", "Grid color:", value="#E0E0E0"),
                  numericInput("panel.grid.major.x", "Major x grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.major.y", "Major y grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.minor.x", "Minor x grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.minor.y", "Minor y grid (if>0):", value=0, min=0, step=0.2)
         ),
         tabPanel("Labels & Legend",
                  textInput("title", "Title:", value = ""),
                  textInput("xlab", "Title x-axis", value = ""),
                  textInput("scale_x_discretelabels", "Overwrite x-axis labels", value =""),
                  checkboxInput('xlabrotate', 'Rotate x-axis 90 deg', value = FALSE),
                  textInput("ylab", "Label y-axis", value = ""),
                  checkboxInput("ylabpercent", "y-label as percentage?", value = FALSE),
                  textInput("colorlabels", "Overwrite color labels (use semicolon seperated list)", value=""),
                  textInput("filllabels", "Overwrite fill labels", value=""),
                  checkboxInput('dropfilllabels', 'If checked unused labels will be left out', value=TRUE),
                  textInput("linetypelabels", "Overwrite linetype labels", value=""),
                  textInput("shapelabels", "Overwrite shape labels", value=""),
                  checkboxInput("LegendReverse", label = "Reverse legend order (labels)", value = FALSE),
                  checkboxInput("Reverse_x_var", label = "Reverse data order x_var", value = FALSE),
                  selectInput("LegendPosition", label = "Select position of the legend", choices=c("left", "top", "right", "bottom"), selected = "right")
         ),
         tabPanel("Facet",
                  uiOutput("facet_options"),
                  p("Choose which columns to use for faces in the 'Main' tab."),
                  checkboxInput("facet_grid", "Use facet grid (2d) wrapping?", value = FALSE),
                  numericInput("ncol", label = "Nr. of colums", value = 2, min=1, step=1),
                  selectInput("scales", label = "Scales for multiple charts:", choices=c("free_y", "free_x","free", "fixed"), selected = "free")
         ),
        tabPanel("Size and scales",
                 #checkboxInput('summary', 'Summary in line chart?', value=FALSE),
                 checkboxInput("factor_xvar", "Make factors out of x axis, necessary for years & bar charts.", value = FALSE),
                 numericInput('ChartHeight', 'Chart height (pixels)', min=1, max=10000, value=400),
                 numericInput('ChartWidth', 'Chart widht (pixels)', min=1, max=10000, value=500),
                 numericInput('aspect_ratio', 'If >0: Aspect ratio (height/width)', min=0, max=5, value=0, step = 0.25),
                 checkboxInput('yman', 'Manually adjust y range?'),
                 numericInput("ymin", label = "Minimum y", value = 0),
                 numericInput("ymax", label = "Maximum y (if bigger then min)", value = 0),
                 checkboxInput('xman', 'Manually adjust x range?'),
                 numericInput("xmin", label = "Minimum x", value = 2010),
                 numericInput("xmax", label = "Maximum x", value = 2050),
                 selectInput("Scaling", label = "Scale by (affects all charts)", choices=c("None", "2005 = 0", "2010 = 0", "index 2010 = 1", "index 2005 = 1"))
                 ),
        tabPanel("Colors",
                 selectInput("colour_preset", label = "Select color preset", choices=c("None","SSP", "Decomp", "SSP land 1", "SSP land 2"), selected = "None"),
                 textInput("colour_list", label="Provide ; seperated list of colours to override below", value=""),
                 checkboxInput("colour_reverse", label = "Reverse", value = FALSE),
                 colourpicker::colourInput("c1","Pick colour 1", "#00FF00"),
                 colourpicker::colourInput("c2","Pick colour 2", value=color_ssp[2]),
                 colourpicker::colourInput("c3","Pick colour 3", value=color_ssp[3]),
                 colourpicker::colourInput("c4","Pick colour 4", value=color_ssp[4]),
                 colourpicker::colourInput("c5","Pick colour 5", value=color_ssp[5]),
                 colourpicker::colourInput("c6","Pick colour 6", value=rainbow(12)[6]),
                 colourpicker::colourInput("c7","Pick colour 7", value=rainbow(12)[7]),
                 colourpicker::colourInput("c8","Pick colour 8", value=rainbow(12)[8]),
                 colourpicker::colourInput("c9","Pick colour 9", value=rainbow(12)[9]),
                 colourpicker::colourInput("c10","Pick colour 10", value=rainbow(12)[10]),
                 colourpicker::colourInput("c11","Pick colour 11", value=rainbow(12)[11]),
                 colourpicker::colourInput("c12","Pick colour 12", value=rainbow(12)[12])
        ),
        tabPanel("Coordinates",
                 checkboxInput('flip', 'Flip coordinates'),
                 checkboxInput('polar', 'Use polar coordinate system')

        ),
        #https://shiny.rstudio.com/gallery/creating-a-ui-from-a-loop.html
        tabPanel("Chart1", uiOutput("flex_options_chart1")),
        tabPanel("Chart2", uiOutput("flex_options_chart2")),
        tabPanel("Chart3", uiOutput("flex_options_chart3")),
        tabPanel("Chart4", uiOutput("flex_options_chart4")),
        tabPanel("Chart5", uiOutput("flex_options_chart5")),
        tabPanel("Chart6", uiOutput("flex_options_chart6")),
        tabPanel("Chart7", uiOutput("flex_options_chart7")),
        tabPanel("Chart8", uiOutput("flex_options_chart8")),
        tabPanel("Chart9", uiOutput("flex_options_chart9")),
        tabPanel("Chart10", uiOutput("flex_options_chart10")),
        selected = "Chart1" #ACtivates main panel
      )
    ),
    mainPanel(
      tabsetPanel(id="settingstabs",
        tabPanel("Data & settings",
                 p("AgMIP Data Viewer. Author: Willem-Jan van Zeist, willemjan.vanzeist@pbl.nl."),
                 p("Use URL as bookmark (if running locally restart if port is not 5900)"),
                 fileInput('file1', 'Choose CSV File to upload (multiple files of same format are possible)',
                           accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.rda'), multiple = TRUE),
                 selectInput("dataset",label = "Choose Dataset", choices = c(rda_filenames, csv_filenames), selected = "Combined_yield_data.csv"),
                 selectInput("settings_opt",label = "Choose settings", choices = c("None", ini_filenames), selected = "None"),
                 actionButton('update_settings', 'Force update settings'),
                 downloadButton('saveInputs', 'Save settings for re-use later (put in settings folder)'),
                 numericInput('nr_of_plots', 'Number of plots (do not change)', min=1, max=100, value=10, step = 1)
                 
                 # Idea add waiting button https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
        ),
        tabPanel("Chart", 
                 downloadButton("download_png", "Download higher quality PNG"),
                 # bookmarkButton(label = "Bookmark to shiny_bookmarks folder and restore in browser."),
                 imageOutput("plot1")
        ),
        tabPanel("Table", 
                 downloadButton('downloadData', 'Download selected data'),
                 dataTableOutput("mytable")
        ),
        tabPanel("ggplot code",
                 p('Code below can be run standalone with plot_build.R'),
                 verbatimTextOutput("coderecycle")
        ),
        tabPanel("AutoPNG",
                 uiOutput("PNGoptions")
        ),
        selected="Chart"
      )
    )
  )) 
  }

### Server code =======================
# Here are al the backend things
# ===

server <- function(input, output, session) {
  # Reading csv ====
  options(shiny.maxRequestSize=1000*1024^2)
  session$onSessionEnded(stopApp)
  
  agmip_csv <- reactive({
    req(input)
    
    
    
    
    if (is.null(input$file1)) {
      fname = paste(datapath,input$dataset,sep="/")
      DATA <- my_dataread(fname)
      # if (file_ext(input$dataset)[1] == "rda") { 
      #   DATA <- readRDS(fname)
      # } else {
      #   DATA <- read.csv(fname, sep=",", dec=".")
      # }
      #Some things might get renamed here, just so you know! 
      #This general cleanup routine does a bunch of things
      # DATA <- data_cleaner(DATA)
    } else {
      #My_dataread can do multiple files
      print(input$file1)
      DATA <- my_dataread(input$file1[,4], input$file1[,1])
      
      
      # filecount = length(input$file1[,1])
      # 
      # for(i in 1:filecount){
      #   
      #   f <- input$file1[[i, 'datapath']]
      #   
      #   if (file_ext(input$file1[[i, 'name']])[1] == "rda") { 
      #     DATAtmp <- readRDS(f)
      #   } else {
      #     DATAtmp <- read.csv(f, sep=",", dec=".")
      #     if(ncol(DATAtmp)==1) {
      #       DATAtmp <- read.csv(f, sep=";", dec=".")
      #     }
      #   }
      #   
      #   DATAtmp <- data_cleaner(DATAtmp)
      #   
      #   if(filecount>1){
      #     DATAtmp$file_name <- input$file1[[i, 'name']]
      #     # Moving new filename column to front
      #     DATAtmp <- DATAtmp[,c(ncol(DATAtmp),1:(ncol(DATAtmp)-1))]
      #     if(i==1){
      #       DATA <- DATAtmp
      #     } else {
      #       DATA <- rbind(DATA, DATAtmp)
      #     }
      #   } else {
      #     DATA <- DATAtmp
      #   }
      # }
    }
    return(DATA)
  })
  
  plot_levels <- reactive({
    #returns lists of names which are factors or integers
 #   pl <- colnames(agmip_csv()[!sapply(agmip_csv(), is.numeric) | sapply(agmip_csv(), is.numeric)])
    pl <- colnames(agmip_csv())
    return(pl)
  })
  
  # Flexible UI options -----
  output$flex_options <- renderUI({
    
    dyn_taglist <- tagList()
    
    col_options = c(colnames(agmip_csv()),"None")
    def_cols = col_options[col_options != "Year" & col_options != "value"]
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('color', 'Color', choices = col_options, selected=def_cols[1]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('linetype', 'Linetype', choices = col_options, selected=def_cols[2]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('shape', 'Shape (for points)', choices = col_options, selected=def_cols[3]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('fill', 'Fill (for bar, ribbon, etc)', choices = col_options, selected=def_cols[4]))
    
    dyn_taglist <- tagAppendChildren(dyn_taglist,
                    selectInput("x_var", label = "Choose what to use for x axis:", choices = colnames(agmip_csv()),selected = "Year"),
                    selectInput("y_var", label = "Choose what to plot on y_axis:", choices = colnames(agmip_csv()), selected = if(!("value" %in% colnames(agmip_csv()))){tail(colnames(agmip_csv()),1)}else{c("value")}),
                    selectInput("Facet", label = "Facets: Mulptiple chart by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
                    selectInput("Facet2", label = "Facets: Grid by:", choices = c(colnames(agmip_csv()), "None"),selected = "None")
    )
    
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]
      if (in_name %in% plot_levels()) {
        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        in_selected <- "All" # Default is all
                
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        # input layout can be manipulated via css style tags. Handy way to figure out which is through inspect element in the browser.
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    } 
    
    dyn_taglist
  })
  
  flex_options <- function(suffix) {
    
    dyn_taglist <- tagList()
    
    sel = if(suffix==1){"Line"}else{"None"} # setting default
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste("chart", suffix, sep=""), label = "Choose chart type", choices = c("Line","Point","Bar","Ribbon","Area","Boxplot","geom_vline","geom_hline","linesummary","average_line","geom_smooth", "None"), selected = sel))

    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      if (colnames(agmip_csv())[i] %in% plot_levels()) {
        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        in_name <- paste(colnames(agmip_csv())[i], suffix, sep="")
     
        in_selected <- "All" # Default is all
        if ("WLD" %in% in_choices) {
          in_selected <- "WLD"
        } else {
          in_selected <- "World"
        }
        if (in_name %like% "Scenario"){in_selected <- levels(agmip_csv()[1,i])[1]}
        if (in_name %like% "Variable" & !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]} 
        if (in_name %like% "Item"& !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]}
        if ("Variable_AgMIP" %in% colnames(agmip_csv())){
          
        }
        
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    }
    
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste('size',suffix,sep=""), 'Size (line thickness etc.)', value=1.5, step=0.25))
    dyn_taglist <- tagAppendChild(dyn_taglist, checkboxInput(paste('singlecolorcheck',suffix,sep=""), 'Use single color', value=FALSE))
    dyn_taglist <- tagAppendChild(dyn_taglist, colourpicker::colourInput(paste('singlecolor',suffix,sep=""),"Pick single colour", value="#000000"))
    dyn_taglist <- tagAppendChild(dyn_taglist, sliderInput(paste('alpha',suffix,sep=""), 'Alpha', min=0, max=1, value=1, step=0.01))
  
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("barwidth", suffix, sep=""), label = "width of bars", value = 0.8,step=0.1))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("dodgewidth", suffix, sep=""), label = "Unstack width (activates if >0)", value = 0, step=0.1))
    
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste("scaling", suffix, sep=""), label = "Scale by", choices=c("None", "Relative", "Absolute")))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("scalingnr", suffix, sep=""), label = "Scale to number", value = 0))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("scalingyr", suffix, sep=""), label = "Scale relative to year", value = 2010, step = 1))
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput(paste("scalingby", suffix, sep=""), label = "Scale to selection (col;var)", value = ""))
    
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("shiftx", suffix, sep=""), label = "Shift on x-axis (usefull for summary)", value = 0))
    
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput(paste("manualdata", suffix, sep=""), label = "Manual data for vline etc (parsed as R code)", value = ""))
  }
  
  # for (i in 1:nr_of_plots) {
  #   chartopt = input[[paste("flex_options_chart", i, sep="")]]
  #   output[[chartopt]] <- renderUI({flex_options(i)})
  # }
  output$flex_options_chart1 <- renderUI({flex_options(1)})
  output$flex_options_chart2 <- renderUI({flex_options(2)})
  output$flex_options_chart3 <- renderUI({flex_options(3)})
  output$flex_options_chart4 <- renderUI({flex_options(4)})
  output$flex_options_chart5 <- renderUI({flex_options(5)})
  output$flex_options_chart6 <- renderUI({flex_options(6)})
  output$flex_options_chart7 <- renderUI({flex_options(7)})
  output$flex_options_chart8 <- renderUI({flex_options(8)})
  output$flex_options_chart9 <- renderUI({flex_options(9)})
  output$flex_options_chart10 <- renderUI({flex_options(10)})
  
  output$PNGoptions <-  renderUI({
    dyn_taglist <- tagList()
    dyn_taglist <- tagAppendChildren(dyn_taglist,
                   selectInput("pngloop", label = "Select item to loop for autopng (figures may look a bit different, test it out)", choices=plot_levels(), selected = "Variable"),
                   p("Make sure to set the selection of the item to 'All', both in Main and Chart#, it will then loop over all available items."),
                   actionButton("pngbutton", "Make PNGs")
    )
  })
  
  # Various reactive functions
  chartcount <- reactive({
    if (is.null(input$Facet)){return(1)}
    if (input$Facet == "None" & input$Facet2 == "None") { return(1) }
    else if ( input$Facet2 =="None" ) { chartcount <- nrow(unique(plot_data()[input$Facet])) }
    else if ( input$Facet =="None" ) { chartcount <- nrow(unique(plot_data()[input$Facet2])) }  
    else { chartcount <- nrow(unique(plot_data()[,c(input$Facet,input$Facet2)])) }
    return(chartcount)
  })
  
  heightSize <- reactive({
    input$ChartHeight
  })
  
  widthSize = reactive({
    input$ChartWidth
  })
  
  colorset <- reactive({
    cset <- c(input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8, input$c9, input$c10, input$c11, input$c12)
    df <- plot_data()
    
    if (input$color!="None") {
      if (length(levels(df[,input$color])) > 12) {
        cset <- c(cset, rainbow(length(levels(df[,input$fill]))-12))
      }
      return(cset)
    }
    if (input$fill!="None") {
      if (length(levels(df[,input$fill])) > length(cset)) {
        cset <- c(cset, rainbow(length(levels(df[,input$fill]))-12))
      }
      return(cset)
    }
  })
  
  # Various observers 
  # Observer for changes in color input
  observeEvent(input$colour_preset, {
    # Color presets -----
    preset <- input$colour_preset

    if(preset=="Decomp") {
      col_scale <- color_decomp
      if(input$colour_reverse){col_scale <- rev(col_scale)}
      updateSelectInput(session, "c1", selected=col_scale[1])
      updateSelectInput(session, "c2", selected=col_scale[2])
      updateSelectInput(session, "c3", selected=col_scale[3])
      updateSelectInput(session, "c4", selected=col_scale[4])
      updateSelectInput(session, "c5", selected=col_scale[5])
      updateSelectInput(session, "c6", selected=col_scale[6])
      updateSelectInput(session, "c7", selected=col_scale[7])
      updateSelectInput(session, "c8", selected=col_scale[8])
    }
    if(preset=="SSP") {
      col_scale <- color_ssp
      if(input$colour_reverse){col_scale <- rev(col_scale)}
      updateSelectInput(session, "c1", selected = col_scale[1])
      updateSelectInput(session, "c2", selected = col_scale[2])
      updateSelectInput(session, "c3", selected = col_scale[3])
      updateSelectInput(session, "c4", selected = col_scale[4])
      updateSelectInput(session, "c5", selected = col_scale[5])
    }
    if(preset=="SSP land 1") {
      col_scale <- color_sspland1
      if(input$colour_reverse){col_scale <- rev(col_scale)}
      updateSelectInput(session, "c1", selected = col_scale[1])
      updateSelectInput(session, "c2", selected = col_scale[2])
      updateSelectInput(session, "c3", selected = col_scale[3])
      updateSelectInput(session, "c4", selected = col_scale[4])
      updateSelectInput(session, "c5", selected = col_scale[5])
      updateSelectInput(session, "c6", selected = col_scale[6])
      updateSelectInput(session, "c7", selected = col_scale[7])
    }
    if(preset=="SSP land 2") {
      col_scale <- color_sspland2
      if(input$colour_reverse){col_scale <- rev(col_scale)}
      updateSelectInput(session, "c1", selected = col_scale[1])
      updateSelectInput(session, "c2", selected = col_scale[2])
      updateSelectInput(session, "c3", selected = col_scale[3])
      updateSelectInput(session, "c4", selected = col_scale[4])
      updateSelectInput(session, "c5", selected = col_scale[5])
    }
  })
  
  observeEvent(input$pngbutton, {
    lp <- unique(plot_data()[,input$pngloop])
    
    #updateTabsetPanel(session, "tabs", selected = "Main")
    #updateTabsetPanel(session, "settingstabs", selected = "Chart")
    
    for (i in 1:length(lp)) {
      
      # This doesn't work and its complicated..
      #updateSelectInput(session, input$pngloop, lp[i])
      
      ss <- plot_data()
      ss <- subset(ss, ss[,input$pngloop] == lp[i])
      
      G1 <- plot_build(ss)
      
      if(input$title==''){
        plotname <- paste(gsub('%', '',gsub('/', '_',lp[i])))
      } else {
        plotname <- stringconvert(input$title, ss, input)
      }
      
      res=72# default resolution shiny
      ggsave(paste(plotname, ".png"), G1, device = "png", width=input$ChartWidth/res, height=input$ChartHeight/res)
      
    }
  })
  # Plot data generation ====
  plot_data <- reactive({
    
    req(input)
    df <- agmip_csv()
    df <- plot_data_wj(df,input)
    return(df)
  
  })
  
  #Outputs and rendering --------
  output$plot1 <- renderPlot({
    
    req(input)
    if (nrow(plot_data())==0){
      #print("No plot data (yet)")
      return(NULL)
      }
    #G1 <- plot_build_wj(plot_data(), reactiveValuesToList(input))
    df <- plot_data()
    G1 <- plot_build_wj(df, input)
    plot(G1)
    #save(df, file="shinyplotdata.rda")

  }, height=heightSize, width=widthSize)
  
  output$mytable <- renderDataTable({
    tab <- input$tabs
    if(substr(tab, 1, 5) == "Chart") {
      suffix <- as.numeric(substr(tab, nchar(tab), nchar(tab)))
      plot_subset(plot_data(), suffix, reactiveValuesToList(input))
    } else {
      plot_data()
    }
  })
  
  output$coderecycle <- renderText({
    
    default_input <-list(themeopt='theme_classic',aspect_ratio=0,scale_x_discretelabels='',Model10='All',panel.grid.major.x=0,panel.grid.major.y=0,Scenario1='FAO baseline',Scenario2='FAO baseline',Scenario3='FAO baseline',fill='Item',Scenario4='FAO baseline',Region='All',Scenario5='FAO baseline',Scenario6='FAO baseline',Scenario7='FAO baseline',LegendPosition='right',Scenario8='FAO baseline',Scenario9='FAO baseline',scaling10='None',scalingnr10=0,flip=FALSE,dropfilllabels=TRUE,ylabpercent=FALSE,ymin=0,Scaling='None',Variable='All',Facet2='None',color='Model',barwidth1=0.8,barwidth2=0.8,barwidth3=0.8,LegendReverse=FALSE,barwidth4=0.8,barwidth5=0.8,barwidth6=0.8,barwidth7=0.8,barwidth8=0.8,barwidth9=0.8,x_var='Year',colour_preset='None',panel.grid.minor.x=0,panel.grid.minor.y=0,shiftx1=0,TextSize=0,shiftx2=0,shiftx3=0,shiftx4=0,Variable1='AREA',shiftx5=0,Variable2='AREA',shiftx6=0,Variable3='AREA',shiftx7=0,Variable4='AREA',shiftx8=0,Variable5='AREA',scaling1='None',shiftx9=0,Variable6='AREA',scaling2='None',xlab='',Variable7='AREA',scaling3='None',Variable8='AREA',scaling4='None',Variable9='AREA',scaling5='None',scaling6='None',colorlabels='',scaling7='None',scaling8='None',Year='All',manualdata1='',scaling9='None',manualdata2='',manualdata3='',manualdata4='',manualdata5='',Item='All',manualdata6='',manualdata7='',singlecolorcheck10=FALSE,manualdata8='',manualdata9='',Model1='All',Model2='All',Model3='All',filllabels='',Model4='All',settings_opt='None',Model5='All',Region1='World',Model6='All',plot_text_add='',Region2='World',Model7='All',Region3='World',Model8='All',Region4='World',Model9='All',title='',scalingnr1=0,Region5='World',xman=FALSE,Facet='None',scalingnr2=0,Region6='World',dodgewidth1=0,scalingnr3=0,Region7='World',dodgewidth2=0,scalingnr4=0,Region8='World',dodgewidth3=0,scalingnr5=0,Region9='World',dodgewidth4=0,scalingnr6=0,dodgewidth5=0,scalingnr7=0,dodgewidth6=0,scalingnr8=0,dodgewidth7=0,scales='free',scalingnr9=0,dodgewidth8=0,value='All',dodgewidth9=0,xmax=2050,Scenario='All',barwidth10=0.8,colour_list='',value10='All',ncol=2,ylab='',nr_of_plots=10,Year10='All',factor_xvar=FALSE,scalingby10='',shiftx10=0,scalingyr10=2010,size10=1.5,ChartHeight=400,colour_reverse=FALSE,Year1='All',Year2='All',Year3='All',Year4='All',Year5='All',Year6='All',y_var='value',Year7='All',Year8='All',Year9='All',pngbutton=0,ChartWidth=500,xlabrotate=FALSE,manualdata10='',yman=FALSE,panel.grid.color='#E0E0E0',ymax=0,shape='Scenario',scalingby1='',scalingby2='',scalingby3='',dodgewidth10=0,scalingby4='',scalingby5='',scalingby6='',scalingby7='',scalingby8='',scalingby9='',singlecolorcheck1=FALSE,singlecolorcheck2=FALSE,singlecolorcheck3=FALSE,singlecolorcheck4=FALSE,tabs='Main',singlecolorcheck5=FALSE,singlecolorcheck6=FALSE,singlecolorcheck7=FALSE,singlecolorcheck8=FALSE,singlecolorcheck9=FALSE,value1='All',Scenario10='FAO baseline',Reverse_x_var=FALSE,value2='All',value3='All',value4='All',value5='All',value6='All',value7='All',value8='All',value9='All',scalingyr1=2010,size1=1.5,Variable10='AREA',scalingyr2=2010,size2=1.5,scalingyr3=2010,size3=1.5,scalingyr4=2010,size4=1.5,scalingyr5=2010,size5=1.5,Region10='World',scalingyr6=2010,size6=1.5,c1='#00FF00',scalingyr7=2010,size7=1.5,pngloop='Variable',c2='#0000FF',scalingyr8=2010,size8=1.5,linetype='Region',c3='#FF0000',scalingyr9=2010,size9=1.5,c4='#FFB900',c5='#FF00FF',c6='#00FF80',c7='#00FFFF',c8='#0080FF',c9='#0000FF',singlecolor1='#000000',singlecolor2='#000000',singlecolor3='#000000',singlecolor4='#000000',singlecolor5='#000000',singlecolor6='#000000',Item1='CGR',singlecolor7='#000000',alpha10=1,Item10='CGR',chart10='None',facet_grid=FALSE,Item2='CGR',singlecolor8='#000000',update_settings=0,Item3='CGR',singlecolor9='#000000',shapelabels='',Item4='CGR',Item5='CGR',Item6='CGR',Item7='CGR',Item8='CGR',Model='All',linetypelabels='',Item9='CGR',alpha1=1,chart1='Line',polar=FALSE,alpha2=1,dataset='Combined_yield_data.csv',chart2='None',alpha3=1,chart3='None',alpha4=1,chart4='None',alpha5=1,chart5='None',alpha6=1,xmin=2010,chart6='None',alpha7=1,chart7='None',c10='#8000FF',singlecolor10='#000000',alpha8=1,chart8='None',c11='#FF00FF',alpha9=1,chart9='None',panel.background.color='#FFFFFF',c12='#FF0080')
    
    inp <- reactiveValuesToList(input)
    inp$file1 <- NULL # removing to avoid errors (is a null element)
    inp$tabs <- NULL
    inp$settingstabs <- NULL
   # inp <- deparse(inp)
    
    txt = "default_input <-list(themeopt='theme_classic',aspect_ratio=0,scale_x_discretelabels='',Model10='All',panel.grid.major.x=0,panel.grid.major.y=0,Scenario1='FAO baseline',Scenario2='FAO baseline',file1=NULL,Scenario3='FAO baseline',fill='Item',Scenario4='FAO baseline',Region='All',Scenario5='FAO baseline',Scenario6='FAO baseline',Scenario7='FAO baseline',LegendPosition='right',Scenario8='FAO baseline',Scenario9='FAO baseline',scaling10='None',scalingnr10=0,flip=FALSE,dropfilllabels=TRUE,ylabpercent=FALSE,ymin=0,Scaling='None',Variable='All',Facet2='None',color='Model',barwidth1=0.8,barwidth2=0.8,barwidth3=0.8,LegendReverse=FALSE,barwidth4=0.8,barwidth5=0.8,barwidth6=0.8,barwidth7=0.8,barwidth8=0.8,barwidth9=0.8,x_var='Year',colour_preset='None',panel.grid.minor.x=0,panel.grid.minor.y=0,shiftx1=0,TextSize=0,shiftx2=0,shiftx3=0,shiftx4=0,Variable1='AREA',shiftx5=0,Variable2='AREA',shiftx6=0,Variable3='AREA',shiftx7=0,Variable4='AREA',shiftx8=0,Variable5='AREA',scaling1='None',shiftx9=0,Variable6='AREA',scaling2='None',xlab='',Variable7='AREA',scaling3='None',Variable8='AREA',scaling4='None',Variable9='AREA',scaling5='None',scaling6='None',colorlabels='',scaling7='None',scaling8='None',Year='All',manualdata1='',scaling9='None',manualdata2='',manualdata3='',manualdata4='',manualdata5='',Item='All',manualdata6='',manualdata7='',singlecolorcheck10=FALSE,manualdata8='',manualdata9='',Model1='All',Model2='All',Model3='All',filllabels='',Model4='All',settings_opt='None',Model5='All',Region1='World',Model6='All',plot_text_add='',Region2='World',Model7='All',Region3='World',Model8='All',Region4='World',Model9='All',title='',scalingnr1=0,Region5='World',xman=FALSE,Facet='None',scalingnr2=0,Region6='World',dodgewidth1=0,scalingnr3=0,Region7='World',dodgewidth2=0,scalingnr4=0,Region8='World',dodgewidth3=0,scalingnr5=0,Region9='World',dodgewidth4=0,scalingnr6=0,dodgewidth5=0,scalingnr7=0,dodgewidth6=0,scalingnr8=0,dodgewidth7=0,scales='free',scalingnr9=0,dodgewidth8=0,value='All',dodgewidth9=0,xmax=2050,Scenario='All',barwidth10=0.8,colour_list='',value10='All',ncol=2,ylab='',nr_of_plots=10,Year10='All',factor_xvar=FALSE,scalingby10='',shiftx10=0,scalingyr10=2010,size10=1.5,ChartHeight=400,colour_reverse=FALSE,Year1='All',Year2='All',Year3='All',Year4='All',Year5='All',Year6='All',y_var='value',Year7='All',Year8='All',Year9='All',pngbutton=0,ChartWidth=500,xlabrotate=FALSE,manualdata10='',yman=FALSE,panel.grid.color='#E0E0E0',ymax=0,shape='Scenario',scalingby1='',scalingby2='',scalingby3='',dodgewidth10=0,scalingby4='',scalingby5='',scalingby6='',scalingby7='',scalingby8='',scalingby9='',singlecolorcheck1=FALSE,singlecolorcheck2=FALSE,singlecolorcheck3=FALSE,singlecolorcheck4=FALSE,singlecolorcheck5=FALSE,singlecolorcheck6=FALSE,singlecolorcheck7=FALSE,singlecolorcheck8=FALSE,singlecolorcheck9=FALSE,value1='All',Scenario10='FAO baseline',Reverse_x_var=FALSE,value2='All',value3='All',value4='All',value5='All',value6='All',value7='All',value8='All',value9='All',scalingyr1=2010,size1=1.5,Variable10='AREA',scalingyr2=2010,size2=1.5,scalingyr3=2010,size3=1.5,scalingyr4=2010,size4=1.5,scalingyr5=2010,size5=1.5,Region10='World',scalingyr6=2010,size6=1.5,c1='#00FF00',scalingyr7=2010,size7=1.5,pngloop='Variable',c2='#0000FF',scalingyr8=2010,size8=1.5,linetype='Region',c3='#FF0000',scalingyr9=2010,size9=1.5,c4='#FFB900',c5='#FF00FF',c6='#00FF80',c7='#00FFFF',c8='#0080FF',c9='#0000FF',singlecolor1='#000000',singlecolor2='#000000',singlecolor3='#000000',singlecolor4='#000000',singlecolor5='#000000',singlecolor6='#000000',Item1='CGR',singlecolor7='#000000',alpha10=1,Item10='CGR',chart10='None',facet_grid=FALSE,Item2='CGR',singlecolor8='#000000',update_settings=0,Item3='CGR',singlecolor9='#000000',shapelabels='',Item4='CGR',Item5='CGR',Item6='CGR',Item7='CGR',Item8='CGR',Model='All',linetypelabels='',Item9='CGR',alpha1=1,chart1='Line',polar=FALSE,alpha2=1,dataset='Combined_yield_data.csv',chart2='None',alpha3=1,chart3='None',alpha4=1,chart4='None',alpha5=1,chart5='None',alpha6=1,xmin=2010,chart6='None',alpha7=1,chart7='None',c10='#8000FF',singlecolor10='#000000',alpha8=1,chart8='None',c11='#FF00FF',alpha9=1,chart9='None',panel.background.color='#FFFFFF',c12='#FF0080')\n"
    
    txt = paste(txt,"input<-default_input\n",sep="")
    
    for (i in 1:length(inp)){
      #txt <- paste(txt, inp[i],sep="")
     name = names(inp)[i]
   
     check = FALSE
     if(is.null(default_input[[name]])) {
       check = TRUE
     } else if (inp[[name]]!=default_input[[name]]){
       check = TRUE 
     }
     if (check) {
       if(is.character(unlist(inp[i])) & length(unlist(inp[i]))==1){
         s <- "'"}
        else{
          s<-""
        }
        txt <- paste(txt, "input$",names(inp)[i], "<-",s,inp[i],s,"\n",sep="")
      }
    }
    wd <- getwd()
    fname = paste(wd,datapath,input$dataset,sep='/')
    txt <- paste(txt, "\nsetwd('",wd, "')
source('./plot_build.R')
fname='",fname,"'
DATA <- read.csv(fname, sep=',', dec='.')
DATA <- data_cleaner(DATA)
df <- plot_data_wj(DATA, input)
shiny_plot <- plot_build_wj(df,input)


res=72 #Defult resolution used to calculate proper height of graph output.
ggsave('plot.png', plot = shiny_plot, width=input$ChartWidth/res, height=input$ChartHeight/res)
namepref = '' # adds prefix to filename
#input$pngloop <- 'Item'
lp <- unique(df[,input$pngloop])
for (i in 1:length(lp)) {
   ss <- subset(df, df[,input$pngloop] == lp[i])
   shiny_plot <- plot_build_wj(ss,input)
   if(input$title==''){
    plotname <- paste(namepref,gsub('%', '',gsub('/', '_',lp[i])),sep='')
  } else {
   plotname <- paste(namepref, stringconvert(input$title, ss, input),sep='')
   }
   ggsave(paste(plotname, '.png'), shiny_plot, , width=input$ChartWidth/res, height=input$ChartHeight/res)
}
", sep="")
      
    # txt <- "input <- list("
    # for (i in 1:length(inp)) {
    #    txt <- paste(txt, names(inp)[i], "=", inp[i], sep="")
    #    if (i != length(inp)) {txt <- paste(txt, ", ", sep="")}
    # }
    # 
    # txt <- paste(txt,")",sep="")
 
    return(txt)
  })
  
  output$downloadData <- downloadHandler(
    filename =  'Selected_values.csv',
    content = function(file) {
      write.csv(plot_data(), file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_png <- downloadHandler(
    
    filename = function(){
      if(input$title==''){
        plotname <- "plot.png"
      } else {
        plotname <- stringconvert(input$title, ss, input) 
      }
    },

    content = function(file) {
      shiny_plot <- plot_build_wj(plot_data(), reactiveValuesToList(input))
      ggsave(file, shiny_plot, width=input$ChartWidth/72, height=input$ChartHeight/72, device="png")
      
      #file.copy("../output_plots/plot.png", file)
    },
    contentType = "image/png" 
  )
  
  # Related to saving of settings ------
  AllInputs <- reactive({

    myvalues <- c(0,0)
    for(i in 1:length(names(input))){

      if (names(input)[i]!="settings_file" &
          names(input)[i]!="file_ggplot" &
          names(input)[i]!="dataset" &
          names(input)[i]!="file1" &
          names(input)[i]!="Preset" &
          names(input)[i]!="settings_opt") {
        myvalues <- as.data.frame(rbind(myvalues,
                        (cbind(names(input)[i],
                               if(is.null(input[[names(input)[i]]])){
                                 "NA"
                               }else{
                                 input[[names(input)[i]]]}))))

      }
    }

    names(myvalues) <- c("User Input","Last Value")

    myvalues
  })

  output$saveInputs <- downloadHandler(
    filename = function() {'Settings.ini'},
    content = function(filename) {
      write.csv(AllInputs(), filename, row.names=FALSE)
    }
  )
  
  settings_list <- reactive({
    if (exists("input")) {
      if (input$settings_opt != "None") {
        settings_list <- as.data.frame(read.csv(paste("./settings/",input$settings_opt, sep=""), sep=",", dec="."))
      }
    }
  })
  
  observeEvent({
    input$update_settings
    }, {

    if (input$settings_opt!="None"){
      ids = as.vector(droplevels(unique(settings_list()[,1])))
  
      for (i in 1:length(ids)) {
  
        if (ids[i] %in% AllInputs()[,1]) {
          us <- as.vector(droplevels(unique(subset(settings_list(), settings_list()[,1] == ids[i])[,2])))
          updateSelectInput(session, ids[i], selected = us)
          updateSelectizeInput(session, ids[i], selected = us)
          if(is.numeric(us[1])) {
            updateNumericInput(session, ids[i], value = us)
            updateSliderInput(session, ids[i], value =  us)
          } 
          if(us[1]=="FALSE" | us[1]=="TRUE") {
            updateCheckboxInput(session, ids[i], value = if(us=="FALSE"){FALSE}else{TRUE})
          }
        }
      }
    }
  })
  
  setBookmarkExclude(c("update_settings", "settings_opt"))
  
  onRestored(function(state) {
    updateTabsetPanel(session, "tabs", selected="Chart1")
    updateTabsetPanel(session, "settingstabs", selected="Chart")
    
  })
   observe({
     # Trigger this observer every time an input changes

     req(input$chart1)
     reactiveValuesToList(input)
     
     #print(deparse(scenario_range))

     #if(input$settingstabs == "Data & settings"){
       session$doBookmark()
      #}
    })
    onBookmarked(function(url) {
      updateQueryString(url)
    })
}
# options(shiny.port=5903)
# Run the application  ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")

