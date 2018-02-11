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

nr_of_plots <- 3 #Increased the number of plots available, might slow down stuff

### UI code  ============
# This main bit 'ui' contains all the frontend bits, defining menu items etc.
# ===

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput('mycharttabs'),
      tabsetPanel(id="tabs",
         tabPanel("Main",
                  p('Selection will influence order of plots. Selection applies to all plots'),
                  uiOutput("flex_options"),
                  checkboxInput("factor_xvar", "Make factors out of x axis, necessary for years & bar charts.", value = FALSE)
         ),
         tabPanel("Theme",
                  selectInput("themeopt", "select theme:", choices=c("theme_bw", "theme_classic", "theme_dark","theme_gray","theme_light","theme_minimal","theme_void", "Other"), selected="theme_classic"),
                  sliderInput('TextSize', 'Text size adjustment', min=-16, max=20, value=0, step=1, round=0),
                  colourInput("panel.background.color", "Panel background color:", value=""),
                  colourInput("panel.grid.color", "Grid color:", value="#E0E0E0"),
                  numericInput("panel.grid.major.x", "Major x grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.major.y", "Major y grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.minor.x", "Minor x grid (if>0):", value=0, min=0, step=0.2),
                  numericInput("panel.grid.minor.y", "Minor y grid (if>0):", value=0, min=0, step=0.2)
         ),
         tabPanel("Labels",
                  textInput("title", "Title:", value = ""),
                  textInput("xlab", "Label x-axis", value = ""),
                  textInput("ylab", "Label y-axis", value = ""),
                  textInput("colorlabels", "Overwrite color labels (use semicolon seperated list)", value=""),
                  textInput("filllabels", "Overwrite fill labels", value=""),
                  textInput("linetypelabels", "Overwrite linetype labels", value=""),
                  textInput("shapelabels", "Overwrite shape labels", value="")
         ),
         tabPanel("Facet",
                  uiOutput("facet_options"),
                  numericInput("ncol", label = "Nr. of colums", value = 2, min=1, step=1),
                  selectInput("scales", label = "Scales for multiple charts:", choices=c("free_y", "free_x","free", "fixed"), selected = "free")
         ),
        tabPanel("More",
                 #checkboxInput('summary', 'Summary in line chart?', value=FALSE),
                 numericInput('ChartHeight', 'Chart height (pixels)', min=1, max=10000, value=400),
                 numericInput('ChartWidth', 'Chart widht (pixels)', min=1, max=10000, value=500),
                 numericInput('aspect_ratio', 'If >0: Aspect ratio (height/width)', min=0, max=5, value=0, step = 0.25),
                
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
        tabPanel("Data & settings",
                 p("AgMIP Data Viewer. Author: Willem-Jan van Zeist, willemjan.vanzeist@pbl.nl."),
                 fileInput('file1', 'Choose CSV File to upload (multiple files of same format are possible)',
                           accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv', '.rda'), multiple = TRUE),
                 selectInput("dataset",label = "Choose Dataset", choices = c(rda_filenames, csv_filenames), selected = "Combined_yield_data.csv"),
                 selectInput("settings_opt",label = "Choose settings", choices = c("None", ini_filenames), selected = "None"),
                 actionButton('update_settings', 'Force update settings'),
                 downloadButton('saveInputs', 'Save settings for re-use later (put in settings folder)')
        ),
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
        tabPanel("AutoPNG",
                 uiOutput("PNGoptions")
        ),
        selected="Chart"
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
    for (i in nr_of_plots:1) {
      insertTab(inputId="tabs", 
                tabPanel(paste("Chart", i, sep=""), uiOutput(paste("flex_options_chart", i, sep=""))),
                target="Main", position="after", select=TRUE)
      }
    })
  
  # Flexible UI options -----
  output$flex_options <- renderUI({
    
    if(debug_opt){print("Updating flex_optionss renderUI")}
    
    dyn_taglist <- tagList()
    
    col_options = c(colnames(agmip_csv()),"None")
    def_cols = col_options[col_options != "Year" & col_options != "value"]
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('color', 'Color', choices = col_options, selected=def_cols[1]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('linetype', 'Linetype', choices = col_options, selected=def_cols[2]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('shape', 'Shape (for points)', choices = col_options, selected=def_cols[3]))
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput('fill', 'Fill (for bar, ribbon, etc)', choices = col_options, selected=def_cols[4]))
    
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
    
    dyn_taglist <- tagAppendChildren(dyn_taglist,
              selectInput("x_var", label = "Choose what to use for x axis:", choices = colnames(agmip_csv()),selected = "Year"),
              selectInput("y_var", label = "Choose what to plot on y_axis:", choices = if("index" %in% colnames(agmip_csv())){c("value","index")}else{c("value")}, selected = "value")
    )
    dyn_taglist
  })
  
  flex_options <- function(suffix) {
    
    dyn_taglist <- tagList()
    
    sel = if(suffix==1){"Line"}else{"None"} # setting default
    dyn_taglist <- tagAppendChild(dyn_taglist, selectInput(paste("chart", suffix, sep=""), label = "Choose chart type", choices = c("Line","Point","Bar","Ribbon","Area","Tornado","Boxplot", "None"), selected = sel))

    for(i in 1:dim(agmip_csv())[2]) { # looping over column names
      if (colnames(agmip_csv())[i] %in% plot_levels()) {
        in_choices <- c("All",levels(agmip_csv()[,i]))
        in_name <- paste(colnames(agmip_csv())[i], suffix, sep="")
      
        in_selected <- "All" # Default is all
        if (in_name %like% "Region"){in_selected <- "World"}
        if (in_name %like% "Scenario"){in_selected <- levels(agmip_csv()[1,i])[1]}
        if (in_name %like% "Variable"){in_selected <- levels(agmip_csv()[1,i])[1]} 
        if (in_name %like% "Item"){in_selected <- levels(agmip_csv()[1,i])[1]}
      
        new_input <- div(style="line-height: 0.4;", selectInput(in_name, label=in_name, choices = in_choices, selected = in_selected, multiple = TRUE, selectize = TRUE))
        dyn_taglist <- tagAppendChild(dyn_taglist, new_input)
      }
    }
    
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste('size',suffix,sep=""), 'Size (line thickness etc.)', value=1.5, step=0.25))
    dyn_taglist <- tagAppendChild(dyn_taglist, checkboxInput(paste('singlecolorcheck',suffix,sep=""), 'Use single color', value=FALSE))
    dyn_taglist <- tagAppendChild(dyn_taglist, colourInput(paste('singlecolor',suffix,sep=""),"Pick single colour", value="#000000"))
    dyn_taglist <- tagAppendChild(dyn_taglist, sliderInput(paste('alpha',suffix,sep=""), 'Alpha', min=0, max=1, value=1, step=0.01))
  
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("barwidth", suffix, sep=""), label = "width of bars", value = 0.8,step=0.1))
    dyn_taglist <- tagAppendChild(dyn_taglist, numericInput(paste("dodgewidth", suffix, sep=""), label = "Unstack width (activates if >0)", value = 0, step=0.1))
    
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
  
  
  output$facet_options <-  renderUI({
    dyn_taglist <- tagList()
    
    # This inputs steers the facet wrap option of ggplot.
    dyn_taglist <- tagAppendChildren(dyn_taglist,
    selectInput("Facet", label = "Mulptiple chart by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
    selectInput("Facet2", label = "Grid by:", choices = c(colnames(agmip_csv()), "None"),selected = "None"),
    checkboxInput("facet_grid", "Use facet grid (2d) wrapping?", value = FALSE)    )
    
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
      ggsave(paste(plotname, ".png"), last_plot(), device = "png")
      
    }
  })
  # Plot data generation ====
  plot_data <- reactive({
    
    req(input)
    
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
  
  plot_subset <- function(df, suffix){
    
    for (i in 1:length(plot_levels())) {
      # subsets for all plot levels used for input control. 
      # Double brackets for input needed because it is a reactivevalues class
      in_name <- paste(plot_levels()[i],suffix, sep="")
      col_name <- plot_levels()[i]
      if(!("All" %in% input[[in_name]])){df <- subset(df, df[,col_name] %in% input[[in_name]])}
      
      if(!(plot_levels()[i] == "Year")) { # 
        if(!("All" %in% input[[in_name]])){df[,col_name] <- factor(df[,col_name], levels=input[[in_name]])}
      } 
    }

    return(df) 
  }
  
  plot_build_suf <- function(G1, df, suffix, chartopt) {
    
    alpha = input[[paste("alpha", suffix, sep="")]]
    color = input[["color"]]
    fill = input[["fill"]]
    linetype = input[["linetype"]]
    shape = input[["shape"]]
    size = input[[paste("size", suffix, sep="")]]
    width = input[[paste("barwidth", suffix, sep="")]]
    dodgewidth=input[[paste("dodgewidth", suffix, sep="")]]
    
    
    singlecolorcheck = input[[paste("singlecolorcheck", suffix, sep="")]]
    singlecolor = input[[paste("singlecolor", suffix, sep="")]]
    
    G1 = G1 + aes_string(x=input$x_var, y=input$y_var)
    if(linetype!="None"){G1 = G1 + aes_string(linetype=linetype)}
    if(color!="None"){G1 = G1 + aes_string(color=color)}
    if(shape!="None"){G1 = G1 + aes_string(shape=shape)}
    if(fill!="None"){G1 = G1 + aes_string(fill=fill)}
    
    if(chartopt=="Line"){
      if(singlecolorcheck) {
        G1 = G1 + geom_line(data=df,size=size, alpha=alpha, color=singlecolor)
      } else {
        G1 = G1 + geom_line(data=df,size=size, alpha=alpha)
      }
    } else if(chartopt=="Point"){
      if(singlecolorcheck) {
        G1 = G1 + geom_point(data=df, size=size, alpha=alpha, color=singelcolor)
      } else {
        G1 = G1 + geom_point(data=df, size=size, alpha=alpha)
      }
    } else if (chartopt=="Bar" | chartopt=="Stacked bar"){
      if(singlecolorcheck) {
        G1 = G1 + geom_bar(data=df,alpha=alpha, width=width,stat="identity", color=singlecolor, size=size,
                           position=if(dodgewidth==0){"stack"}else{position_dodge(dodgewidth)})
      } else {
        G1 = G1 + geom_bar(data=df,alpha=alpha,width=width,stat="identity",size=size,
                           position=if(dodgewidth==0){"stack"}else{position_dodge(dodgewidth)})
      }
    } else if(chartopt=="Area") {
      if(singlecolorcheck) {
        G1 = G1 + geom_area(data=df, alpha=alpha, color=singlecolor)
      } else {
        G1 = G1 + geom_area(data=df, alpha=alpha)
      }
    } else if(chartopt=="Ribbon") {
      if(singlecolorcheck) {
        G1 = G1 + list(stat_summary(data=df,geom="ribbon", fun.ymin="min", fun.ymax="max", alpha=alpha, color=singlecolor))
      } else {
        G1 = G1 + list(stat_summary(data=df,geom="ribbon", fun.ymin="min", fun.ymax="max", alpha=alpha))
      }
    }
    
    return(G1)
  }
  
  
  plot_build_labels <- function() {
    
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
      G1 = G1 + scale_shape(labels=labels, solid=FALSE) 
    } else {
      G1 = G1 + scale_shape(solid=FALSE)
    }

    return(G1)
  }
  
  plot_build_theme <- function(G1) {
    
  
    if(input$themeopt=="theme_bw"){G1 = G1 + theme_bw(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_classic"){G1 = G1 + theme_classic(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_dark"){G1 = G1 + theme_dark(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_gray"){G1 = G1 + theme_gray(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_light"){G1 = G1 + theme_light(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_linedraw"){G1 = G1 + theme_linedraw(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_minimal"){G1 = G1 + theme_minimal(base_size=14+input$TextSize)}
    if(input$themeopt=="theme_void"){G1 = G1 + theme_void(base_size=14+input$TextSize)}
    
    
    
    if(input$aspect_ratio > 0) {G1 = G1 + theme(aspect.ratio = input$aspect_ratio)}
    # G1 = G1 +
    #   theme(
    #     #panel.background = element_rect(fill = 'white', color = 'black'),
    #     #axis.ticks.x = element_blank(),
    #     axis.ticks.y = element_blank(),
    # 
    #     panel.grid.major.x=element_blank(),
    #     panel.grid.minor.x=element_blank(),
    #     panel.grid.minor.y=element_blank())

    # if (input$grid_y_opt) {
    #   G1 = G1 + theme(panel.grid.major.y = element_line(size=.1, color="grey20"))
    # }
    # if (input$grid_x_opt) {
    #   G1 = G1 + theme(panel.grid.major.x = element_line(size=.1, color="grey20"))
    # }
    # 
    # G1 = G1 + theme(legend.position=input$LegendPosition, legend.box="vertical",legend.box.just="left",
    #                 legend.text = element_text(size=16+input$TextSize),
    #                 legend.title = element_text(size=16+input$TextSize))
    
 
   # axis.line =         theme_blank(),
  #  axis.text.x =       theme_text(size = base_size * 0.8 , lineheight = 0.9, colour = "grey50", vjust = 1),
  #  axis.text.y =       theme_text(size = base_size * 0.8, lineheight = 0.9, colour = "grey50", hjust = 1),
    # axis.ticks =        theme_segment(colour = "grey50"),
    # axis.title.x =      theme_text(size = base_size, vjust = 0.5),
    # axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    # axis.ticks.length = unit(0.15, "cm"),
    # axis.ticks.margin = unit(0.1, "cm"),
    
    # legend.background = theme_rect(colour="white"), 
    # legend.key =        theme_rect(fill = "grey95", colour = "white"),
    # legend.key.size =   unit(1.2, "lines"),
    # legend.text =       theme_text(size = base_size * 0.8),
    # legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
    # legend.position =   "right",
    
    G1=G1+theme(panel.background = element_rect(fill=input$panel.background.color))    
    # panel.background =  theme_rect(fill = "grey90", colour = NA), 
    # panel.border =      theme_blank(), 
    # panel.grid.major.x =  theme_line(colour = "white"),
    # panel.grid.minor.x =  theme_line(colour = "grey95", size = 0.25),
    if(input$panel.grid.major.x>0){G1=G1+theme(panel.grid.major.x = element_line(size=input$panel.grid.major.x, color=input$panel.grid.color))}
    if(input$panel.grid.major.y>0){G1=G1+theme(panel.grid.major.y = element_line(size=input$panel.grid.major.y, color=input$panel.grid.color))}
    if(input$panel.grid.major.y>0 & input$panel.grid.major.x>0){G1=G1+theme(panel.grid.major = element_line(size=max(input$panel.grid.major.y,input$panel.grid.major.x, color=input$panel.grid.color)))}
    if(input$panel.grid.minor.x>0){G1=G1+theme(panel.grid.minor.x = element_line(size=input$panel.grid.minor.x, color=input$panel.grid.color))}
    if(input$panel.grid.minor.y>0){G1=G1+theme(panel.grid.minor.y = element_line(size=input$panel.grid.minor.y, color=input$panel.grid.color))}
    if(input$panel.grid.minor.y>0 & input$panel.grid.minor.x>0){G1=G1+theme(panel.grid.minor = element_line(size=max(input$panel.grid.minor.y,input$panel.grid.minor.x, color=input$panel.grid.color)))}
    # panel.margin =      unit(0.25, "lines"),
    # 
    # strip.background =  theme_rect(fill = "grey80", colour = NA), 
    # strip.text.x =      theme_text(size = base_size * 0.8),
    # strip.text.y =      theme_text(size = base_size * 0.8, angle = -90),
    # 
    #plot.background =   theme_rect(colour = NA, fill = "white"),
    #plot.title =        theme_text(size = base_size * 1.2),
    #plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
    
    return(G1)
    
  }  
    
  plot_build <- function(df){
    
    if(debug_opt){print("Updating plot_build function")}
    
    if(input$factor_xvar){df[,input$x_var] <-factor(df[,input$x_var])}
    
    G1 =  plot_build_labels()
    for (i in 1:nr_of_plots) {
      chartopt = input[[paste("chart", i, sep="")]]
      if(!is.null(chartopt)) {
        if(chartopt!="None") {
          G1 = G1 %>% plot_build_suf(plot_subset(df, i), i, chartopt)
        }
      }
    }
    G1 =  G1 %>% plot_build_theme()

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
        G1 = G1 + ggtitle(unique(df$Variable_AgMIP)[1])
      } else{
        G1 = G1 + ggtitle(unique(df$Variable)[1])
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
    
       
    if (input$xman){G1 <- G1 + xlim(input$xmin, input$xmax)}  
    if (input$yman){G1 <- G1 + ylim(input$ymin, if(input$ymin<input$ymax){input$ymax}else{NA})}
    
    if (!is.null(input$Facet)) {
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



