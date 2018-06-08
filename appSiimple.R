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

source('./plot_build_simple.R')
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

# Plot presets
plotpresets <- list(None="",
                    tornado_decomp="G1 = G1 + geom_point()"
                    )

plotpresetsdefault <- names(plotpresets)[1]

### UI code  ============
# This main bit 'ui' contains all the frontend bits, defining menu items etc.
# ===

ui <- function(request) {
  fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
      tabsetPanel(id="tabs",
         tabPanel("Main", id="Main",
                  uiOutput("flex_options_chart"),
                  selectInput("themeopt", "select theme:", choices=c("theme_bw", "theme_classic", "theme_dark","theme_gray","theme_light","theme_minimal","theme_void", "Other"), selected="theme_classic"),
                  sliderInput('TextSize', 'Text size adjustment', min=-16, max=20, value=0, step=1, round=0)
                  
          ),
         tabPanel("Titles & Legend",
                  textInput("title", "Title:", value = ""),
                  textInput("xlab", "Title x-axis", value = ""),
                  checkboxInput('xlabrotate', 'Rotate x-axis 90 deg', value = FALSE),
                  textInput("ylab", "Label y-axis", value = ""),
                  checkboxInput("ylabpercent", "y-label as percentage?", value = FALSE),
                  selectInput("LegendPosition", label = "Select position of the legend", choices=c("left", "top", "right", "bottom"), selected = "right")
         ),
        tabPanel("Size and scales",
                 #checkboxInput('summary', 'Summary in line chart?', value=FALSE),
                 numericInput('ChartHeight', 'Chart height (pixels)', min=1, max=10000, value=400),
                 numericInput('ChartWidth', 'Chart widht (pixels)', min=1, max=10000, value=500),
                 checkboxInput('yman', 'Manual y range?', value=FALSE),
                 numericInput("ymin", label = "Minimum y", value = 0),
                 numericInput("ymax", label = "Maximum y", value = 0),
                 checkboxInput('xman', 'Manual x range?', value=FALSE),
                 numericInput("xmin", label = "Minimum x", value = 2010),
                 numericInput("xmax", label = "Maximum x", value = 2050),
                 selectInput("scaling", label = "Scale by", choices=c("None", "Relative", "Absolute")),
                 numericInput("scalingyr", label = "Scale relative to year", value = 2010, step = 1)
        ),
        tabPanel("Colors",
                 selectInput("colour_preset", label = "Select color preset", choices=c("None","SSP", "Decomp", "SSP land 1", "SSP land 2"), selected = "Decomp"),
                 colourpicker::colourInput("c1","Pick colour 1", value=color_decomp[1]),
                 colourpicker::colourInput("c2","Pick colour 2", value=color_decomp[2]),
                 colourpicker::colourInput("c3","Pick colour 3", value=color_decomp[3]),
                 colourpicker::colourInput("c4","Pick colour 4", value=color_decomp[4]),
                 colourpicker::colourInput("c5","Pick colour 5", value=color_decomp[5]),
                 colourpicker::colourInput("c6","Pick colour 6", value=color_decomp[6]),
                 colourpicker::colourInput("c7","Pick colour 7", value=rainbow(12)[7]),
                 colourpicker::colourInput("c8","Pick colour 8", value=rainbow(12)[8]),
                 colourpicker::colourInput("c9","Pick colour 9", value=rainbow(12)[9]),
                 colourpicker::colourInput("c10","Pick colour 10", value=rainbow(12)[10]),
                 colourpicker::colourInput("c11","Pick colour 11", value=rainbow(12)[11]),
                 colourpicker::colourInput("c12","Pick colour 12", value=rainbow(12)[12])
        )
    )),
    mainPanel(
         downloadButton("download_png", "Download higher quality PNG"),
         # bookmarkButton(label = "Bookmark to shiny_bookmarks folder and restore in browser."),
         imageOutput("plot1"),
         tabsetPanel(id="settingstabs",
           tabPanel('Chart',
             column(4, 
                uiOutput("flex_options")
             ),
             column(4,
                uiOutput("flex_options2"),
                checkboxInput("facet_grid", "Facet grid (2d)", value = FALSE),
                numericInput("ncol", label = "Nr. of colums", value = 2, min=1, step=1),
                selectInput("scales", label = "Facet scales", choices=c("free_y", "free_x","free", "fixed"), selected = "free")
             ),
              column(4, 
                     uiOutput("flex_options3")
            )
           ),
           tabPanel("Data & settings",
                    p("AgMIP Data Viewer. Author: Willem-Jan van Zeist, willemjan.vanzeist@pbl.nl."),
                    
                    fileInput('file1', 'Choose CSV File to upload (multiple files of same format are possible)',
                              accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.rda'), multiple = TRUE),
                    selectInput("dataset",label = "Choose Dataset", choices = c(rda_filenames, csv_filenames), selected = "Combined_yield_data.csv"),
                    selectInput("settings_opt",label = "Choose settings", choices = c("None", ini_filenames), selected = "None"),
                    actionButton('update_settings', 'Force update settings'),
                    downloadButton('saveInputs', 'Save settings for re-use later (put in settings folder)')
          ),
          tabPanel("Table", 
                   downloadButton('downloadData', 'Download selected data'),
                   dataTableOutput("mytable")
          ),
          tabPanel("ggplot code",
                   p(''),
                   selectInput("preset_code",label = "Preset plot", choices = names(plotpresets), selected = plotpresetsdefault),
                   textAreaInput("plot_text_add", "Add code lines to plot build function:", value="#G1 = G1 + code to add to the plot\n", width='600px', height ='200px'),
                   verbatimTextOutput("coderecycle")
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
    } else {
      print(input$file1)
      DATA <- my_dataread(input$file1[,4], input$file1[,1])
    }
    return(DATA)
  })
  
  plot_levels <- reactive({
    pl <- colnames(agmip_csv())
    return(pl)
  })
  
  # Flexible UI options -----
  output$flex_options <- renderUI({
    
    dyn_taglist <- tagList()
    
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]

        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        
        in_selected <- "All" # Default is all
        if (in_name %like% "Region"){
          if ("WLD" %in% in_choices) {
            in_selected <- "WLD"
          } else {
            in_selected <- "World"
          }
        }
        if (in_name %like% "Scenario"){in_selected <- levels(agmip_csv()[1,i])[1]}
        if (in_name %like% "Variable" & !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]} 
        if (in_name %like% "Item"& !("Variable_AgMIP" %in% colnames(agmip_csv()))){in_selected <- levels(agmip_csv()[1,i])[1]}
        
        dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
    }
    
    dyn_taglist
  })
  
  output$flex_options2 <- renderUI({
    
    dyn_taglist <- tagList()
    
    
    dyn_taglist <- tagAppendChildren(dyn_taglist,
                       selectInput("x_var", label = "x_axis:", choices = colnames(agmip_csv()),selected = "Year"),
                       selectInput("y_var", label = "y_axis:", choices = colnames(agmip_csv()), selected = if(!("value" %in% colnames(agmip_csv()))){tail(colnames(agmip_csv()),1)}else{c("value")}),
                       selectInput("facet", label = "Facet by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
                       selectInput("facet2", label = "Facets nr2 by:", choices = c(colnames(agmip_csv()), "None"),selected = "None")
    )
       
    dyn_taglist
  })
  
  output$flex_options3 <- renderUI({
    
    dyn_taglist <- tagList()
    col_options = c("None", colnames(agmip_csv()))
    def_cols = col_options[col_options != "Year" & col_options != "value"]
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('color', 'Color', choices = col_options, selected=def_cols[2]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('linetype', 'Linetype', choices = col_options, selected=def_cols[3]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('shape', 'Shape (for points)', choices = col_options, selected=def_cols[4]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('fill', 'Fill (for bar, etc)', choices = col_options, selected=def_cols[5]))
    
    dyn_taglist
  })
  
  output$flex_options_chart <- renderUI({
    dyn_taglist <- tagList()
    
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput("chart", label = "Choose chart type", choices = c("Line","Point","Bar","Ribbon","Area","Boxplot","linesummary","average_line","geom_smooth", "None"), selected = "Line"))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput('size', label = 'Size/thickness', value=1.5, step=0.25))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput("dodgewidth", label = "Unstack width bars", value = 0, step=0.1))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput('alpha', 'Alpha', min=0, max=1, value=1, step=0.05))
    
    dyn_taglist
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
      updateSelectInput(session, "c1", selected = col_scale[1])
      updateSelectInput(session, "c2", selected = col_scale[2])
      updateSelectInput(session, "c3", selected = col_scale[3])
      updateSelectInput(session, "c4", selected = col_scale[4])
      updateSelectInput(session, "c5", selected = col_scale[5])
    }
    if(preset=="SSP land 1") {
      col_scale <- color_sspland1
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
      updateSelectInput(session, "c1", selected = col_scale[1])
      updateSelectInput(session, "c2", selected = col_scale[2])
      updateSelectInput(session, "c3", selected = col_scale[3])
      updateSelectInput(session, "c4", selected = col_scale[4])
      updateSelectInput(session, "c5", selected = col_scale[5])
    }
  })
  
  
  # Plot data generation ====
  input_selection <- reactive({
    input_sel <- list()
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]
      input_sel[[in_name]] <- input[[in_name]]
    }
    return(input_sel)
  })
  
  plot_data <- reactive({
    req(input)
    df <- agmip_csv()
    df <- plot_data_wj(df,input_selection(),input$scaling, input$scalingyr)
    return(df)
  })
  
  #Outputs and rendering --------
  output$plot1 <- renderPlot({
    
    req(input)
    if (nrow(plot_data())==0){
      return(NULL)
      }

    df <- plot_data()
    G1_text <- plot_build_wj(df, input)
    eval(parse(text=G1_text))
    eval(parse(text=plotpresets[[input$preset_code]]))
    eval(parse(text=input$plot_text_add))
    
    plot(G1)

  }, height=heightSize, width=widthSize)
  
  output$mytable <- renderDataTable({
    plot_data()
  })
  
  output$coderecycle <- renderText({
    codetext <- ""
    codetext <- paste(codetext,"source('./WJlib.R')\n",sep="")
    codetext <- paste(codetext,"source('./plot_build_simple.R')\n",sep="")
    codetext <- paste(codetext,"df <- my_dataread('",datapath,input$dataset,"')\n",sep="")
    codetext <- paste(codetext,"input_sel <- ", paste(deparse(input_selection()), sep="", collapse=""), "\n", sep="")
    codetext <- paste(codetext,"df <- plot_data_wj(df,input_sel,'", input$scaling,"',", input$scalingyr,")\n",sep="")
    codetext <- paste(codetext, plot_build_wj(plot_data(), input), sep="\n")
    codetext <- paste(codetext, plotpresets[[input$preset_code]],
                      gsub("#G1 \\= G1 \\+ code to add to the plot","",input$plot_text_add), sep="\n")
    
    if(input$title==''){
      plotname <- "plot.png"
    } else {
      plotname <- paste(stringconvert(input$title, ss, input), ".png", sep="")
    }
    
    codetext <- paste(codetext, "ggsave('",plotname,"', G1, width=",input$ChartWidth/72,", height=",input$ChartHeight/72,", device='png')\n")
    
    return(codetext)
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
      G1_text <- plot_build_wj(plot_data(), reactiveValuesToList(input))
      eval(parse(text=G1_text))
      eval(parse(text=plotpresets[[input$preset_code]]))
      eval(parse(text=input$plot_text_add))
      ggsave(file, G1, width=input$ChartWidth/72, height=input$ChartHeight/72, device="png")
      
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
  
   observe({
     # Trigger this observer every time an input changes

     req(input$chart)
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

