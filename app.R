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

# ---- Functions
scenario_range <- function(df, yr) {  
  
  scens <- levels(df$Scenario)
  
  yr_data <- subset(df, Year==yr)
  
  yr_data1 <- subset(yr_data, Scenario==scens[1])
  yr_data1$Year <- yr_data1$Year + 2
  yr_data2 <- subset(yr_data, Scenario==scens[2])
  yr_data2$Year <- yr_data2$Year + 4
  yr_data3 <- subset(yr_data, Scenario==scens[3])
  yr_data3$Year <- yr_data3$Year + 6
  yr_data4 <- subset(yr_data, Scenario==scens[4])
  yr_data4$Year <- yr_data4$Year + 8
  yr_data5 <- subset(yr_data, Scenario==scens[5])
  yr_data5$Year <- yr_data5$Year + 10
  
  yr_data <- rbind(yr_data1, yr_data2, yr_data3, yr_data4, yr_data5)
  
  
  return(yr_data)
}

my_dataread <- function(input) {
  
  filecount = length(input$file1[,1])
  
  for(i in 1:filecount){
    
    f <- input$file1[[i, 'datapath']]
    
    if (file_ext(input$file1[[i, 'name']])[1] == "rda") { 
      DATAtmp <- readRDS(f)
    } else {
      DATAtmp <- read.csv(f, sep=",", dec=".")
      if(ncol(DATAtmp)==1) {
        DATAtmp <- read.csv(f, sep=";", dec=".")
      }
    }
    
    DATAtmp <- data_cleaner(DATAtmp)
    
    if(filecount>1){
      DATAtmp$file_name <- input$file1[[i, 'name']]
      # Moving new filename column to front
      DATAtmp <- DATAtmp[,c(ncol(DATAtmp),1:(ncol(DATAtmp)-1))]
      if(i==1){
        DATA <- DATAtmp
      } else {
        DATA <- rbind(DATA, DATAtmp)
      }
    } else {
      DATA <- DATAtmp
    }
    
  }
  return(DATA)
}

data_cleaner <- function(df){
  
  #Multiple claenup of names/headers units, etc.
  #Also tries to detect years in column names to convert to long format
  #Creates AgMIP item & variable columns if detected
  
  #Renaming some columns
  df <- plyr::rename(df, c("region"="Region", "model"="Model", "variable"="Variable", "scenario"="Scenario", "year"="Year", "unit"="Unit", "item"="Item"), warn_missing=FALSE)
  df <- plyr::rename(df, c("Index"="index", "Value"="value"), warn_missing=FALSE)
  df <- plyr::rename(df, c("REGION"="Region", "MODEL"="Model", "VARIABLE"="Variable", "SCENARIO"="Scenario", "YEAR"="Year", "UNIT"="Unit", "ITEM"="Item"), warn_missing=FALSE)
  df <- plyr::rename(df, c("INDEX"="index", "INDEX"="value"), warn_missing=FALSE)
  
  #converting years if found
  if (!("Year" %in% colnames(df))){
    if ("X2010" %in% colnames(df)|"X2005" %in% colnames(df)|"X2015" %in% colnames(df)|"X2000" %in% colnames(df)) {
      for (i in 1970:2100) { # finding first year occurrence.
        clnr = match(paste("X", i, sep=""), colnames(df), nomatch = 0)
        if (clnr != 0) {break}
      }
      df <- melt(df, id=1:clnr-1, variable_name = "Year")
      df$Year = as.numeric(substr(df$Year, 2, stop=100))      
    }
  }
  
  # Just to be sure, not sure if all of the below is necessary.
  df$value <- as.numeric(substr(df$value, 1, stop=100))
  if("index" %in% colnames(df)){
    df$index <- as.numeric(df$index)
  }
  df$Year = as.integer(as.character(df$Year))
  
  #Making agmip columsn ig "AGMIP| etc is detected
  if (!("Item" %in% colnames(df)) & substr(df$Variable[1],1,5) == "AGMIP") {
    df$Variable <- gsub("AGMIP|", "", df$Variable, fixed=TRUE)
    df$Item <- gsub("\\|.*", "", df$Variable) # Everything after | removed
    df$Variable <- gsub(".*\\|", "", df$Variable)
  }
  if ("Unit" %in% colnames(df) & "Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
    df$Variable_AgMIP <- factor(paste(df$Variable ,"_", df$Item, "_", df$Unit, sep=""))
  }
  
  
  
  #some general renaming
  if("Region" %in% colnames(df)){
    df$Region = gsub("R5.2", "", df$Region)
    df$Region = gsub("Global", "World", df$Region)
    df$Region = gsub("WLD", "World", df$Region)
    df$Region = gsub("Global", "World", df$Region)
    df$Region = gsub("Total", "World", df$Region)
  }
  if("Unit" %in% colnames(df)){
    df$Unit = gsub("Mt CO2e", "MtCO2e",df$Unit)
    df$Unit = gsub("kcal/cap/day", "kcal/cap/d",df$Unit)
  }
  if("Item" %in% colnames(df)){
    df$Item = gsub("wheat", "Wheat", df$Item)
    df$Item = gsub("barley", "Barley", df$Item)
    df$Item = gsub("maize", "Maize", df$Item)
    df$Item = gsub("oats", "Oats", df$Item)
    df$Item = gsub("rice", "Rice", df$Item)
    df$Item = gsub("rye", "Rye", df$Item)
    df$Item = gsub("millet", "Millet", df$Item)
    df$Item = gsub("sorghum", "Sorghum", df$Item)
    df$Item = gsub("Cereals", "Cereal", df$Item)
  }
  
  df = na.omit(df)
  
  #This makes sure that all non-numeric columns are factorized.
  df <- df %>% mutate_if(is.character,as.factor)
  df$value <- as.numeric(df$value)
  
  return(df)
}


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

debug_opt <- FALSE

nr_of_plots <- 5 #Increased the number of plots available, might slow down stuff

### UI code  ============
# This main bit 'ui' contains all the frontend bits, defining menu items etc.
# ===

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput('mycharttabs'),
      tabsetPanel(id="tabs",
        tabPanel("Presets",
                 p("AgMIP Data Viewer. Author: Willem-Jan van Zeist, willemjan.vanzeist@pbl.nl."),
                 fileInput('file1', 'Choose CSV File to upload (multiple files of same format are possible)',
                           accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.rda'), multiple = TRUE),
                 selectInput("dataset",label = "Choose Dataset", choices = c(rda_filenames, csv_filenames), selected = "Combined_yield_data.csv"),
                 selectInput("settings_opt",label = "Choose settings", choices = c("None", ini_filenames), selected = "None"),
                 actionButton('update_settings', 'Force update settings')
        ),
         tabPanel("Main",
                  p('Selection will influence order of plots. Selection applies to all plots'),
                  uiOutput("flex_options"),
                  checkboxInput("factor_xvar", "Make factors out of x axis, necessary for years & bar charts.", value = FALSE)
                  
         ),
         tabPanel("Scales",
                  uiOutput("scale_options")
         ),
         
         # tabPanel("Line",
         #  checkboxInput("line_chart_active", "Activate line chart:", value=TRUE),
         #  uiOutput("flex_options_line"),
         #  textInput("label_line", "Overwrite labels (use ; seperated list", value="")
         # ),
         

         # tabPanel("Chart1", uiOutput("flex_options_chart1")),
         # tabPanel("Chart2", uiOutput("flex_options_chart2")),
         # tabPanel("Chart3", uiOutput("flex_options_chart3")),
         # tabPanel("Chart4", uiOutput("flex_options_chart4")),
         # tabPanel("Chart5", uiOutput("flex_options_chart5")),
         # 
         # tabPanel("Point",
         #  checkboxInput("point_chart_active", "Activate point chart:", value=FALSE),
         #  uiOutput('flex_options_point')
         # ),
         # tabPanel("Bar",
         #  checkboxInput("bar_chart_active", "Activate bar chart:", value=FALSE),
         #  uiOutput('flex_options_bar'),
         #  
         #  
         # ),
         # tabPanel("Ribbon",
         #  checkboxInput("ribbon_chart_active", "Activate ribbon chart:", value=FALSE),
         #  p("ribbon based on stat_summary function for data seleced below"),
         #  uiOutput('flex_options_ribbon')
         # ),
        
        tabPanel("More",
                 textInput("title", "Title:", value = ""),
                 textInput("xlab", "Label x-axis", value = ""),
                 textInput("ylab", "Label y-axis", value = ""),
                 checkboxInput('summary', 'Summary in line chart?', value=FALSE),
                 checkboxInput('ribbon', 'Ribbon in line chart?', value=FALSE),
                 selectInput("scales", label = "Scales for multiple charts:", choices=c("free_y", "free_x","free", "fixed"), selected = "free"),
                 numericInput('ChartHeight', 'Chart height (pixels)', min=1, max=10000, value=400),
                 numericInput('ChartWidth', 'Chart widht (pixels)', min=1, max=10000, value=500),
                 checkboxInput('aspect_opt', "force aspect ratio?", value = FALSE),
                 numericInput('aspect_ratio', 'If selected: Aspect ratio (height/width)', min=0.1, max=5, value=4/5, step = 0.1),
                 sliderInput('TextSize', 'Text size adjustment', min=-10, max=10, value=-2, step=1, round=0),
                 numericInput("ncol", label = "Nr. of colums", value = 2, min=1, step=1),
                 selectInput("Scaling", label = "Scale by", choices=c("None", "2005 = 0", "2010 = 0", "index 2010 = 1", "index 2005 = 1")),
                 checkboxInput('yman', 'Manually adjust y range?'),
                 numericInput("ymin", label = "Minimum y", value = 0),
                 numericInput("ymax", label = "Maximum y (if bigger then min)", value = 0),
                 checkboxInput("grid_y_opt", "Add y grid", value = FALSE),
                 checkboxInput('xman', 'Manually adjust x range?'),
                 numericInput("xmin", label = "Minimum x", value = 2010),
                 numericInput("xmax", label = "Maximum x", value = 2050),
                 checkboxInput("grid_x_opt", "Add x grid", value = FALSE),
                 checkboxInput("LegendReverse", label = "Reverse legend order (labels)", value = FALSE),
                 checkboxInput("LegendReverse2", label = "Reverse color order (data)", value = FALSE),
                 checkboxInput("Reverse_x_var", label = "Reverse data order x_var", value = FALSE),
                 selectInput("LegendPosition", label = "Select position of the legend", choices=c("left", "top", "right", "bottom"), selected = "right")
        ),
        tabPanel("Colors",
                 selectInput("colour_preset", label = "Select color preset", choices=c("SSP", "Decomp", "SSP land 1", "SSP land 2"), selected = "SSP"),
                 checkboxInput("colour_reverse", label = "Reverse", value = FALSE),
                 colourInput("c1","Pick colour 1", value=color_ssp[1]),
                 colourInput("c2","Pick colour 2", value=color_ssp[2]),
                 colourInput("c3","Pick colour 3", value=color_ssp[3]),
                 colourInput("c4","Pick colour 4", value=color_ssp[4]),
                 colourInput("c5","Pick colour 5", value=color_ssp[5]),
                 colourInput("c6","Pick colour 6", value=rainbow(12)[6]),
                 colourInput("c7","Pick colour 7", value=rainbow(12)[7]),
                 colourInput("c8","Pick colour 8", value=rainbow(12)[8]),
                 colourInput("c9","Pick colour 9", value=rainbow(12)[9]),
                 colourInput("c10","Pick colour 10", value=rainbow(12)[10]),
                 colourInput("c11","Pick colour 11", value=rainbow(12)[11]),
                 colourInput("c12","Pick colour 12", value=rainbow(12)[12])
        ),
        tabPanel("Coordinates",
                 checkboxInput('flip', 'Flip coordinates'),
                 checkboxInput('polar', 'Use polar coordinate system')

        ),
        selected = "Main" #ACtivates main panel
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", 
                 downloadLink("download_png", "Download higher quality PNG"),
                 imageOutput("plot1")
        ),
        tabPanel("Table", 
                 downloadButton('downloadData', 'Download selected data'),
                 dataTableOutput("mytable")
        ),
        tabPanel("ggplot code",
                 textAreaInput("plot_text_add", "Add code lines to plot build function:", value="
                  ##The dataframe is 'df' in the code and use G1 = G1 + code to add to the plot, for example add continuous scale details
                  #G1 = G1 + scale_y_continuous(breaks=seq(-1000,1000,200),limits=c(-1100,1100))", width="800px", height="400px")
        ),
        tabPanel("Settings",
                 downloadButton('saveInputs', 'Save settings for re-use later (put in settings folder)'),
                 tableOutput('show_inputs')
        ),
        tabPanel("AutoPNG",
                 uiOutput("PNGoptions")
        )
      )
    )
  ))

### Server code =======================
# Here are al the backend things
# ===

server <- function(input, output, session) {
  # Reading csv ====
  options(shiny.maxRequestSize=1000*1024^2)
  
  agmip_csv <- reactive({
    
    if(debug_opt){print("Updating agmip_csv function")}
    
    if (exists("input")) {
      
      fname = paste(datapath,input$dataset,sep="/")
      
      if (is.null(input$file1)) {
        if (file_ext(input$dataset)[1] == "rda") { 
          DATA <- readRDS(fname)
        } else {
          DATA <- read.csv(fname, sep=",", dec=".")
        }
        #Some things might get renamed here, just so you know! 
        #This general cleanup routine does a bunch of things
        DATA <- data_cleaner(DATA)
      } else {
        #My_dataread can do multiple files
        DATA <- my_dataread(input)
      } 
    } 
   
    
    return(DATA)
  })
  
  plot_levels <- reactive({
    #returns lists of names which are factors or integers
    colnames(agmip_csv()[!sapply(agmip_csv(), is.numeric) | sapply(agmip_csv(), is.numeric)])
  })
  
  output$mycharttabs = renderUI({
    # myTabs = lapply(paste('Chart', 1:nr_of_plots), tabPanel)
    # do.call(tabPanel, myTabs)
    for (i in nr_of_plots:1) {
      insertTab(inputId="tabs", 
                tabPanel(paste("Chart", i, sep=""), uiOutput(paste("flex_options_chart", i, sep=""))),
                target="Main", position="after")
    }
  })
  

  
  # Flexible UI options -----
  output$flex_options <- renderUI({
    
    if(debug_opt){print("Updating flex_optionss renderUI")}
    
    dyn_taglist <- tagList()
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      in_name <- colnames(agmip_csv())[i]
      if (in_name %in% plot_levels()) {
        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        in_selected <- "All" # Default is all
        if (in_name == "Region"){in_selected <- "World"}
        if (in_name == "Scenario"){in_selected <- levels(agmip_csv()[1,i])[1]}
        if (in_name == "Variable"){in_selected <- levels(agmip_csv()[1,i])[1]} 
        if (in_name == "Item"){in_selected <- levels(agmip_csv()[1,i])[1]}
        
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        # input layout can be manipulated via css style tags. Handy way to figure out which is through inspect element in the browser.
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    } 
    
    tornado_selected = "Variable"
    chart_selected = "Line"
    x_var_selected = "Year"
    fill_selected = "Scenario"
    facet_selected = "None"
    
    dyn_taglist <- tagAppendChildren(dyn_taglist,
              selectInput("x_var", label = "Choose what to use for x axis:", choices = colnames(agmip_csv()),selected = x_var_selected),
              selectInput("y_var", label = "Choose what to plot on y_axis:", choices = if("index" %in% colnames(agmip_csv())){c("value","index")}else{c("value")}, selected = "value"),
              selectInput("fill", label = "Choose colouring by:", choices = colnames(agmip_csv()), selected = fill_selected),

               # This inputs steers the facet wrap option of ggplot.
              selectInput("Facet", label = "Mulptiple chart by:", choices = c(colnames(agmip_csv()), "None"),selected = facet_selected),
              selectInput("Facet2", label = "Grid by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
              checkboxInput("facet_grid", "Use facet grid (2d) wrapping?", value = FALSE)
    )
    dyn_taglist
  })
  
  flex_options <- function(suffix) {
    
    dyn_taglist <- tagList()
    
    sel = if(suffix==1){"Line"}else{"None"} # setting default
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste("chart", suffix, sep=""), label = "Choose chart type", choices = c("Line","Point","Bar","Stacked bar","Ribbon","Area","Tornado","Boxplot", "None"), selected = sel))

    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      if (colnames(agmip_csv())[i] %in% plot_levels()) {
        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        in_name <- paste(colnames(agmip_csv())[i], suffix, sep="")
        in_selected <- "All" # Default is all
        
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    }
    
  #  dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste("choice",suffix,sep=""), label= "Choose chart type by:", choices = colnames(agmip_csv()), "Model"))
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput(paste("label",suffix,sep=""), "Overwrite labels (use semicolon seperated list", value=""))
    
    col_options = c(colnames(agmip_csv()),"None")
    
    
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste('color',suffix,sep=""), 'Color', choices = col_options))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste('fill',suffix,sep=""), 'Fill', choices = col_options))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste('linetype',suffix,sep=""), 'Linetype', choices = col_options))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste('shape',suffix,sep=""), 'Shape', choices = col_options))
    
    dyn_taglist <- tagAppendChild(dyn_taglist, sliderInput(paste('alpha',suffix,sep=""), 'Alpha', min=0, max=1, value=1, step=0.01))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste('size',suffix,sep=""), 'Size', value=1))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste('weight',suffix,sep=""), 'Weight',value=1))
    
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("barwidth", suffix, sep=""), label = "width of bars", value = 0.8))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("dodgewidth", suffix, sep=""), label = "dodge width of bars", value = 0.9))
    
    dyn_taglist
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
  
  output$scale_options <- renderUI({
    
    dyn_taglist <- tagList()

    dyn_taglist <- tagAppendChild(dyn_taglist, textInput("colorlabels", "Overwrite color labels (use semicolon seperated list)", value=""))
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput("filllabels", "Overwrite fill labels", value=""))
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput("linetypelabels", "Overwrite linetype labels", value=""))
    dyn_taglist <- tagAppendChild(dyn_taglist, textInput("shapelabels", "Overwrite shape labels", value=""))

    dyn_taglist
  })
  
  output$flex_options_ribbon <- renderUI({
    
    if(debug_opt){print("Updating flex_optionss renderUI")}
    prefix = "ribbon_"
    dyn_taglist <- tagList()
    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      if (colnames(agmip_csv())[i] %in% plot_levels()) {
        if (is.integer(agmip_csv()[,i])) { #
          in_choices <- c("All",unique(agmip_csv()[,i]))
        } else {
          in_choices <- c("All",levels(agmip_csv()[,i]))
        }
        in_name <- paste(prefix, colnames(agmip_csv())[i], sep="")
        in_selected <- "All" # Default is all
        
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    }
    dyn_taglist <- tagAppendChild(dyn_taglist, sliderInput(paste(prefix, 'alpha', sep=""), 'Alpha', min=0, max=1, value=0.15, step=0.01))
    
    dyn_taglist
    
  })
  
  output$PNGoptions <-  renderUI({
    dyn_taglist <- tagList()
    dyn_taglist <- tagAppendChildren(dyn_taglist,
                   selectInput("pngloop", label = "Select item to loop for autopng (figures may look a bit different)", choices=plot_levels(), selected = "Variable"),
                   p("Make sure to set the selction of the item to 'All', it will wil then loop over all available items."),
                   actionButton("pngbutton", "Make PNGs")
    )
  })
  
  # Various reactive functions
  chartcount <- reactive({
    if (input$Facet == "None" & input$Facet2 == "None") { return(1) }
    else if ( input$Facet2 =="None" ) { chartcount <- nrow(unique(plot_data()[input$Facet])) }
    else if ( input$Facet =="None" ) { chartcount <- nrow(unique(plot_data()[input$Facet2])) }  
    else { chartcount <- nrow(unique(plot_data()[,c(input$Facet,input$Facet2)])) }
    return(chartcount)
  })
  
  heightSize <- reactive({
    input$ChartHeight
    # if(is.null(input$Facet)){input$ChartHeight}
    # else { input$ChartHeight * ceiling(chartcount()/input$ncol) }
  })
  
  widthSize = reactive({
    input$ChartWidth
    # if(is.null(input$Facet)){input$ChartWidth}
    # else {input$ChartWidth * min(chartcount(),input$ncol)}
  })
  colorset <- reactive({
    cset <- c(input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8, input$c9, input$c10, input$c11, input$c12)
    df <- plot_data()
    if (length(levels(df[,input$fill])) > 12) {
      cset <- c(cset, rainbow(length(levels(df[,input$fill]))-12))
    }
    return(cset)
  })
  
  # Various observers 
  # Observer for changes in color input
  observe({
    if(debug_opt){print("Updating color presets")}
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
    
    for (i in 1:length(lp)) {
      ss <- plot_data()
      ss <- subset(ss, ss[,input$pngloop] == lp[i])
      
      G1 <- plot_build(ss)
      plot(G1)
      plotname <- paste(gsub("%", "",gsub("/", "_",lp[i])))
      ggsave(paste(plotname, ".png"), G1, device = "png")
      
    }
  })
  # Plot data generation ====
  plot_data <- reactive({
    
    req(input$Facet,input$Region, input$Variable, input$Year, input$fill)
    
    if(debug_opt){
      print("Updating plot_data reactive value. Summary of data at start:")
      print(summary(agmip_csv()))
      print(plot_levels())
    }
    
    ss <- agmip_csv()
    colcount <- ncol(ss) - 2
    cln <- colcount + 1
    
    for (i in 1:length(plot_levels())) {
      # subsets for all plot levels used for input control. 
      # Double brackets for input needed because it is a reactivevalues class
      if(!("All" %in% input[[plot_levels()[i]]])){ss <- subset(ss, ss[,plot_levels()[i]] %in% input[[plot_levels()[i]]])}
      
      if(!(plot_levels()[i] == "Year")) { # 
        if(!("All" %in% input[[plot_levels()[i]]])){ss[,plot_levels()[i]] <- factor(ss[,plot_levels()[i]], levels=input[[plot_levels()[i]]])}
      }   
    }
    
    ss <- droplevels(ss) # necessary because empty levels might be left over after subsetting.
    ss$Year = as.integer(as.character(ss$Year))
    
    if(input$Scaling == "2005 = 0"){
      ss <- spread(ss, Year, value)
      ss[,cln:ncol(ss)] <- ss[,cln:ncol(ss)] - ss$'2005'
      ss <- melt(ss, id.vars=1:colcount, variable_name="Year")
      ss <- na.omit(ss)
      ss$Year <- as.numeric(substr(ss$Year, 1, stop=100))
    }
    if(input$Scaling == "2010 = 0"){
      ss <- spread(ss, Year, value)
      ss[,cln:ncol(ss)] <- ss[,cln:ncol(ss)] - ss$'2010'
      ss <- melt(ss, id.vars=1:colcount, variable_name="Year")
      ss <- na.omit(ss)
      ss$Year <- as.numeric(substr(ss$Year, 1, stop=100))
    }
    if(input$Scaling == "index 2010 = 1"){
      ss <- spread(ss, Year, value)
      ss[,cln:ncol(ss)] <- ss[,cln:ncol(ss)]/ss$'2010'
      ss <- melt(ss, id.vars=1:colcount, variable_name="Year")
      ss <- na.omit(ss)
      ss$Year <- as.numeric(substr(ss$Year, 1, stop=100))
    }
    if(input$Scaling == "index 2005 = 1"){
      ss <- spread(ss, Year, value)
      ss[,cln:ncol(ss)] <- ss[,cln:ncol(ss)]/ss$'2005'
      ss <- melt(ss, id.vars=1:colcount, variable_name="Year")
      ss <- na.omit(ss)
      ss$Year <- as.numeric(substr(ss$Year, 1, stop=100))
    }
    
    if(input$LegendReverse) {
      ss[,input$fill] <- factor(ss[,input$fill], rev(levels(ss[,input$fill])))
    }
    if(input$Reverse_x_var) {
      ss[,input$x_var] <- factor(ss[,input$x_var], rev(levels(ss[,input$x_var])))
    }
    
    if(debug_opt){
      print("Summary of data returned:")
      print(summary(ss))
    }
    
    return(ss)
    
  })
  
  # Plot building ====
  
  # plot_build_point <- function(df) {
  #   
  #   ss <- df
  # 
  #   for (i in 1:length(plot_levels())) {
  #     # subsets for all plot levels used for input control. 
  #     # Double brackets for input needed because it is a reactivevalues class
  #     in_name <- paste("point_", plot_levels()[i], sep="")
  #     col_name <-plot_levels()[i]
  # 
  #     if(!("All" %in% input[[in_name]])){ss <- subset(ss, ss[,col_name] %in% input[[in_name]])}
  #     
  #     if(!(in_name == "point_Year")) { # 
  #       if(!("All" %in% input[[in_name]])){ss[,col_name] <- factor(ss[,col_name], levels=input[[in_name]])}
  #     }   
  #   }
  #   
  #   return(G1)
  # }
  # 
  # plot_build_bar <- function(df) {
  #   
  #   ss <- df
  #   
  #   for (i in 1:length(plot_levels())) {
  #     # subsets for all plot levels used for input control. 
  #     # Double brackets for input needed because it is a reactivevalues class
  #     in_name <- paste("bar_", plot_levels()[i], sep="")
  #     col_name <-plot_levels()[i]
  #     if(!("All" %in% input[[in_name]])){ss <- subset(ss, ss[,col_name] %in% input[[in_name]])}
  #     
  #     if(!(in_name == "bar_Year")) { # 
  #       if(!("All" %in% input[[in_name]])){ss[,col_name] <- factor(ss[,col_name], levels=input[[in_name]])}
  #     }   
  #   }
  #   
  #   ss[,input$x_var] <-factor(ss[,input$x_var])
  # 
  #   return(G1)
  # }
  # 
  # plot_build_ribbon <- function(df) {
  #   
  #   ss <- df
  #   
  #   for (i in 1:length(plot_levels())) {
  #     # subsets for all plot levels used for input control. 
  #     # Double brackets for input needed because it is a reactivevalues class
  #     in_name <- paste("ribbon_", plot_levels()[i], sep="")
  #     col_name <-plot_levels()[i]
  #     
  #     if(!("All" %in% input[[in_name]])){ss <- subset(ss, ss[,col_name] %in% input[[in_name]])}
  #     
  #     if(!(in_name == "ribbon_Year")) { # 
  #       if(!("All" %in% input[[in_name]])){ss[,col_name] <- factor(ss[,col_name], levels=input[[in_name]])}
  #     }   
  #   }
  #   
  #   G1 = list(stat_summary(data=ss,geom="ribbon", fun.ymin="min", fun.ymax="max", aes_string(fill=input$fill), alpha=input$ribbon_alpha,show.legend = FALSE))
  #   return(G1)
  # }
  
  plot_subset <- function(df, suffix){
    
    for (i in 1:length(plot_levels())) {
      # subsets for all plot levels used for input control. 
      # Double brackets for input needed because it is a reactivevalues class
      in_name <- paste(plot_levels()[i],suffix, sep="")
      col_name <- plot_levels()[i]
      if(!("All" %in% input[[in_name]])){df <- subset(df, df[,col_name] %in% input[[in_name]])}
    }

    return(df) 
  }
  
  plot_build_suf <- function(G1, df, suffix, chartopt) {
    
    alpha = input[[paste("alpha", suffix, sep="")]]
    color = input[[paste("color", suffix, sep="")]]
    fill = input[[paste("fill", suffix, sep="")]]
    linetype = input[[paste("linetype", suffix, sep="")]]
    shape = input[[paste("shape", suffix, sep="")]]
    size = input[[paste("size", suffix, sep="")]]
    weight = input[[paste("weight", suffix, sep="")]]
    
    col_breaks <- levels(df[,color])
    col_labels <- col_breaks

    lbls <- input[[paste("label", suffix, sep="")]]
    if (lbls!="" ){
      lbls <- unlist(strsplit(lbls, ";"))
      if (length(lbls) == length(col_breaks)){
        col_labels <- lbls
      }
    }
    
    print(col_breaks)
    print(col_labels)
    # G1 = G1 + scale_color_manual(values=colorset(), breaks = breaks2, labels=labels_opt) 
    # G1 = G1 + scale_fill_manual(values=colorset(), breaks = breaks2, labels=labels_opt)
    # G1 = G1 + scale_color_manual(values=colorset()) 
    #G1 = G1 + scale_fill_manual(values=colorset())
    print(color)
    print(linetype)

    if(chartopt=="Line"){
      print("Making line")
      G1 = G1 + geom_line(data=df, aes_string(x=input$x_var, y=input$y_var, color=color, linetype=linetype),size=size, alpha=alpha)
      #G1 = G1 + scale_linetype(breaks = col_breaks, labels=col_labels)
      #G1 = G1 + scale_color_manual(values=colorset(),breaks = col_breaks, labels=col_labels)
    } else if(chartopt=="Point") {
      print("Making poitn")
      G1 = G1 + geom_point(data=df, aes_string(x=input$x_var, y=input$y_var, fill=fill, color=color, shape=shape), size=size, alpha=alpha)
      G1 = G1 + scale_shape(solid = FALSE) 
      #G1 = G1 + scale_shape(breaks = breaks2, labels=labels_opt)
    } else if (chartopt=="Bar" | chartopt=="Stacked bar"){
      print("Making bar chart")
      G1 = G1 + geom_bar(data=df, aes_string(x=input$x_var, y=input$y_var,fill=fill,color=color),
                         width=input[[paste("barwidth", suffix, sep="")]],
                          position=if(chartopt=="Stacked bar"){"stack"}else{position_dodge((width=input[[paste("dodgewidth", suffix, sep="")]]))}, 
                          stat="identity")
    } else if(chartopt=="Area") {
      G1 = G1 + geom_area(data=df, aes_string(x=input$x_var, y=input$y_var, colour=input$fill))
      #G1 = G1 + scale_color_(breaks = breaks2, labels=labels_opt)
    }
    
    return(G1)
  }
  
  
  plot_build_scales <- function() {
    
    
    G1 = ggplot()
    col_labels <- input$colorlabels
   
    if (col_labels!=""){
      col_labels <- unlist(strsplit(col_labels, ";"))
      G1 = G1 + scale_color_manual(values=colorset(), labels=col_labels) 
    } else {
      G1 = G1 + scale_color_manual(values=colorset()) 
    }
    
    labels <- input$linetypelabels
    if (labels!=""){
      labels <- unlist(strsplit(labels, ";"))
      G1 = G1 + scale_linetype(labels=labels) 
    } 
    
    labels <- input$filllabels
    if (labels!=""){
      labels <- unlist(strsplit(labels, ";"))
      G1 = G1 + scale_fill_manual(values=colorset(), labels=labels) 
    } else {
      G1 = G1 + scale_fill_manual(values=colorset())
    }
    
    labels <- input$shapelabels
    if (labels!=""){
      labels <- unlist(strsplit(labels, ";"))
      G1 = G1 + scale_shape(labels=labels) 
    } 
    
    # G1 = G1 + scale_color_manual(values=colorset(), breaks = breaks2, labels=labels_opt) 
    # G1 = G1 + scale_fill_manual(values=colorset(), breaks = breaks2, labels=labels_opt)
   

    #G1 = G1 + scale_fill_manual(values=colorset())
    #G1 = G1 + scale_linetype(breaks = col_breaks, labels=col_labels)
    #G1 = G1 + scale_shape(solid=FALSE, breaks = breaks2, labels=labels_opt)

    return(G1)
  }
  
  plot_build <- function(df){
    
    if(debug_opt){print("Updating plot_build function")}
    
    if(input$factor_xvar){df[,input$x_var] <-factor(df[,input$x_var])}
    
    G1 =  plot_build_scales()
    #G1 = ggplot(df, aes_string(x = input$x_var, y=input$y_var, fill = input$fill)) 
    for (i in 1:nr_of_plots) {
      chartopt = input[[paste("chart", i, sep="")]]
      if(!is.null(chartopt)) {
        if(chartopt!="None") {
          G1 = G1 %>% plot_build_suf(plot_subset(df, i), i, chartopt)
        }
      }
    }
    

    # if (input$Chart == "Tornado"){
    #   tornadata <- spread_(df, input$Tornado, "value")
    # 
    #   G1 = ggplot(tornadata, aes_string(x = input$x_var, y="FstOrdEff", fill = input$fill)) +
    #     geom_bar(data=tornadata, width=.8, alpha = 0.5, stat="identity", position=position_dodge(width=0.9))+
    #     geom_bar(data=tornadata, width=0.45, alpha = 1, aes_string(x="Var", y="TotOrdEff"),stat="identity", position=position_dodge(width=0.9)) +
    #     geom_bar(data=tornadata, width=0.8, alpha = 0.2, aes_string(x="Var", y="IntEff"), stat="identity", position=position_dodge(width=0.9)) +
    #     geom_vline(xintercept=seq(1.5, 7.5, 1), colour='black')
    #   G1 <- G1 + scale_y_continuous(labels=percent) 
    # } 
    # if (input$Chart == "Boxplot") {
    #   G1 = G1 + geom_boxplot(outlier.shape=46)
    #   G1 = G1 + geom_point(data=df,aes_string(x=input$x_var, y=input$y_var, colour=input$fill, pch=input$point), size=3) + scale_shape(solid = FALSE) 
    #   G1 = G1 + geom_hline(yintercept=0, colour='grey20')
    #   G1 <- G1 + scale_y_continuous(labels=percent)
    #   } 
      
    # if(input$summary & "Scenario" %in% plot_levels()) {
    #   scen_range <- scenario_range(df, 2050)
    #   G1 = G1 + stat_summary(data = scen_range,geom="linerange", fun.ymax=max, fun.ymin=min, aes_string(colour=input$fill),alpha=0.12,show.legend=FALSE,size=2)
    #   G1 = G1 + geom_point(data=scen_range,aes_string(x=input$x_var, y=input$y_var, colour=input$fill, pch=input$point), size=3) + scale_shape(solid = FALSE) 
    # }
    
    if(input$title!=""){
      titlestring <- unlist(strsplit(input$title, "CO2"))
      #N20 toevoetgen.
      if(titlestring!=input$title){
        G1 = G1 + ggtitle(bquote(.(titlestring[1])*CO[2]*.(if(length(titlestring)>1){titlestring[2]})))
      } else {
        G1 = G1 + ggtitle(input$title)
      }
    } else {
      if("Variable_AgMIP" %in% plot_levels()) {
        G1 = G1 +ggtitle(unique(df$Variable_AgMIP)[1])
      } else{
        G1 = G1 +ggtitle(unique(df$Variable)[1])
      }
    }
    
    if(input$xlab!=""){
      titlestring <- unlist(strsplit(input$xlab, "CO2"))
      if(titlestring!=input$xlab){
        G1 = G1 + xlab(bquote(.(titlestring[1])*CO[2]*.(if(length(titlestring)>1){titlestring[2]})))
      } else {
        G1 = G1 + xlab(input$xlab)
      }
    } else {
        if("Unit" %in% plot_levels()) {
          G1 = G1 + xlab(unique(df$Variable_AgMIP)[1])
        }
    }
    
    if(input$ylab!=""){
      titlestring <- unlist(strsplit(input$ylab, "CO2"))
      if(titlestring!=input$ylab){
        G1 = G1 + ylab(bquote(.(titlestring[1])*CO[2]*.(if(length(titlestring)>1){titlestring[2]})))
      } else {
        G1 = G1 + ylab(input$ylab)
      }
    } 
    
    G1 = G1 +
      theme(
        panel.background = element_rect(fill = 'white', colour = 'black'),
        strip.text = element_text(size=18+input$TextSize),
        plot.title = element_text(size=18+input$TextSize),
        axis.text.y=element_text(size=20+input$TextSize),
        axis.text.x=element_text(size=20+input$TextSize,angle = 90),
        #axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size=18+input$TextSize),
        
        if(input$aspect_opt) {aspect.ratio = input$aspect_ratio},
        
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank())
       
    if (input$grid_y_opt) {
      G1 = G1 + theme(panel.grid.major.y = element_line(size=.1, color="grey20"))
    }
    if (input$grid_x_opt) {
      G1 = G1 + theme(panel.grid.major.x = element_line(size=.1, color="grey20"))
    }


    # G1 = G1 + theme(legend.position=input$LegendPosition,legend.text = element_text(size=16+input$TextSize),legend.title = element_text(size=16+input$TextSize),legend.box="vertical",legend.box.just="left")
    G1 = G1 + theme(legend.position=input$LegendPosition, legend.box="vertical",legend.box.just="left",
                    legend.text = element_text(size=16+input$TextSize),
                    legend.title = element_text(size=16+input$TextSize))
    
    if (input$xman){G1 <- G1 + xlim(input$xmin, input$xmax)}  
    if (input$yman){G1 <- G1 + ylim(input$ymin, if(input$ymin<input$ymax){input$ymax}else{NA})}
    
    if (input$Facet!="None" & input$Facet2=="None"){
      G1 <- G1 + facet_wrap(as.formula(paste("~", input$Facet)),
                            ncol=input$ncol, 
                            scales = input$scales)
    } else if (input$Facet!="None" & input$Facet2!="None"){
      if (input$facet_grid) {
        G1 <- G1 + facet_grid(as.formula(paste(input$Facet2, "~", input$Facet)),
                              scales = input$scales)
      }else{
        G1 <- G1 + facet_wrap(as.formula(paste(input$Facet2, "~", input$Facet)),
                              ncol=input$ncol,
                              scales = input$scales)
      }
    }
    if(input$flip){G1 <- G1 + coord_flip()}
    if(input$polar){G1 <- G1 + coord_polar()}
    
    #This adds anything in input$plot_text_add interpreted as parsed formulas
    eval(parse(text=input$plot_text_add))

    return(G1)
  }
  
  #Outputs and rendering --------
  output$plot1 <- renderPlot({
    if(debug_opt){print("Rendering plot")}
    
    if (nrow(plot_data())==0) {
      if(debug_opt){print("No plot data (yet)")}
       return(NULL)
    }
    
    G1 <- plot_build(plot_data())
    plot(G1)
    #ggsave("../output_plots/plot.png")
    
  }, height=heightSize, width=widthSize)
  
  output$mytable <- renderDataTable({
    plot_data()
  })
  
  output$downloadData <- downloadHandler(
    filename =  'Selected_values.csv',
    content = function(file) {
      write.csv(plot_data(), file, row.names=FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_png <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      file.copy("../output_plots/plot.png", file)
    },
    contentType = "image/png" 
  )
  
  # Related to saving of settings ------
  AllInputs <- reactive({

    if(debug_opt){print("updating AllInputs reactive value")}

    myvalues <- c(0,0)
    for(i in 1:length(names(input))){

      if (names(input)[i]!="settings_file" &
          names(input)[i]!="file_ggplot" &
          names(input)[i]!="dataset" &
          names(input)[i]!="file1" &
          names(input)[i]!="Preset" &
          names(input)[i]!="settings_opt") 

           {
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

  output$show_inputs <- renderTable({
    req(input)
    AllInputs()
  })

  output$saveInputs <- downloadHandler(
    filename = function() {'Settings.ini'},
    content = function(filename) {
      write.csv(AllInputs(), filename, row.names=FALSE)
    }
  )
  
  settings_list <- reactive({
    if (exists("input")) {
      settings_list <- as.data.frame(read.csv(paste("./settings/",input$settings_opt, sep=""), sep=",", dec="."))
    }
  })
  
  observeEvent({
    input$update_settings
    }, {

    if(debug_opt){print("updating AllInputs reactive value")}

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
  })
}

# Run the application  ----
shinyApp(ui = ui, server = server)



