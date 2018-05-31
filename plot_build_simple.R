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
# library(Cairo)

source('./WJlib.R')

plot_data_wj <- function(df,input){
  
  
  colcount <- ncol(df) - 2
  cln <- colcount + 1
  plot_levels <- plot_levels(df)
  
  for (i in 1:length(plot_levels)) {
    # subsets for all plot levels used for input control. 
    # Double brackets for input needed because it is a reactivevalues cladf
    
    #if(input$pngloop!=plot_levels[i]){
    if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
    #}
    if(!(plot_levels[i] == "Year")) { # 
      if(!("All" %in% input[[plot_levels[i]]])){df[,plot_levels[i]] <- factor(df[,plot_levels[i]], levels=input[[plot_levels[i]]])}
    }   
  }
  
  df <- droplevels(df) # necedfary because empty levels might be left over after subsetting.
  df$Year = as.integer(as.character(df$Year))
  
  
  return(df)
}

# data_cleaner <- function(df){
#   
#   #Multiple claenup of names/headers units, etc.
#   #Also tries to detect years in column names to convert to long format
#   #Creates AgMIP item & variable columns if detected
#   
#   #Renaming some columns
#   df <- plyr::rename(df, c("region"="Region", "model"="Model", "variable"="Variable", "scenario"="Scenario", "year"="Year", "unit"="Unit", "item"="Item"), warn_missing=FALSE)
#   df <- plyr::rename(df, c("Index"="index", "Value"="value"), warn_missing=FALSE)
#   df <- plyr::rename(df, c("REGION"="Region", "MODEL"="Model", "VARIABLE"="Variable", "SCENARIO"="Scenario", "YEAR"="Year", "UNIT"="Unit", "ITEM"="Item"), warn_missing=FALSE)
#   df <- plyr::rename(df, c("INDEX"="index", "INDEX"="value"), warn_missing=FALSE)
#   
#   #converting years if found
#   if (!("Year" %in% colnames(df))){
#     if ("X2010" %in% colnames(df)|"X2005" %in% colnames(df)|"X2015" %in% colnames(df)|"X2000" %in% colnames(df)) {
#       for (i in 1970:2100) { # finding first year occurrence.
#         clnr = match(paste("X", i, sep=""), colnames(df), nomatch = 0)
#         if (clnr != 0) {break}
#       }
#       df <- melt(df, id=1:clnr-1, variable_name = "Year")
#       df$Year = as.numeric(substr(df$Year, 2, stop=100))      
#     }
#   }
# 
#   # Just to be sure, not sure if all of the below is necessary.
#   if("value" %in% colnames(df)) {
#     df$value <- as.numeric(substr(df$value, 1, stop=100))
#   }
#   if("index" %in% colnames(df)){
#     df$index <- as.numeric(df$index)
#   }
#   if("Year" %in% colnames(df)) {
#     df$Year = as.integer(as.character(df$Year))
#   }
#   
#   #Making agmip columsn ig "AGMIP| etc is detected
#   if("Variable" %in% colnames(df)) {
#     if (!("Item" %in% colnames(df)) & substr(df$Variable[1],1,5) == "AGMIP") {
#       df$Variable <- gsub("AGMIP|", "", df$Variable, fixed=TRUE)
#       df$Item <- gsub("\\|.*", "", df$Variable) # Everything after | removed
#       df$Variable <- gsub(".*\\|", "", df$Variable)
#     }
#   }
#   
#   if ("Unit" %in% colnames(df) & "Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
#     df$Variable_AgMIP <- factor(paste(df$Variable ,"_", df$Item, "_", df$Unit, sep=""))
#   }
#   if ("Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
#     # To do, make uppercase
#     # df$Variable = gsub("Area", "AREA", df$Region)
#     # df$Variable = gsub("Area", "AREA", df$Region)
#     # df$Variable = gsub("Area", "AREA", df$Region)
#   }
#   
#   #some general renaming
#   if("Region" %in% colnames(df)){
#     df$Region = gsub("R5.2", "", df$Region)
#     df$Region = gsub("Global", "World", df$Region)
#     df$Region = gsub("WLD", "World", df$Region)
#     df$Region = gsub("Global", "World", df$Region)
#     df$Region = gsub("Total", "World", df$Region)
#   }
#   if("Unit" %in% colnames(df)){
#     df$Unit = gsub("Mt CO2e", "MtCO2e",df$Unit)
#     df$Unit = gsub("kcal/cap/day", "kcal/cap/d",df$Unit)
#   }
#   if("Item" %in% colnames(df)){
#     df$Item = gsub("wheat", "Wheat", df$Item)
#     df$Item = gsub("barley", "Barley", df$Item)
#     df$Item = gsub("maize", "Maize", df$Item)
#     df$Item = gsub("oats", "Oats", df$Item)
#     df$Item = gsub("rice", "Rice", df$Item)
#     df$Item = gsub("rye", "Rye", df$Item)
#     df$Item = gsub("millet", "Millet", df$Item)
#     df$Item = gsub("sorghum", "Sorghum", df$Item)
#   #  df$Item = gsub("Cereals", "Cereal", df$Item)
#   }
#   
#   df = na.omit(df)
#   
#   #This makes sure that all non-numeric columns are factorized.
#   df <- df %>% mutate_if(is.character,as.factor)
#   if("value" %in% colnames(df)) {
#     df$value <- as.numeric(df$value)
#     col_idx <- grep("value", names(df))
#     df <- df[, c((1:ncol(df))[-col_idx],col_idx)]
#   }
#   
#   df <- df[!duplicated(df),]
#   #value needs to be in the end
#   
#   return(df)
# }
#   
plot_levels <- function(df) {
  #returns lists of names which are factors or integers
  colnames(df)
}


colorset <- function(df,input) {
  cset <- c(input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8, input$c9, input$c10, input$c11, input$c12)
  
  if (input$color!="None") {
    if (length(levels(df[,input$color])) > length(cset)) {
      cset <- c(cset, rainbow(length(levels(df[,input$color]))-length(cset)))
    }
    return(cset)
  }
  if (input$fill!="None") {
    if (length(levels(df[,input$fill])) > length(cset)) {
      cset <- c(cset, rainbow(length(levels(df[,input$fill]))-length(cset)))
    }
    return(cset)
  }
  
  
}

stringconvert <- function(convstr,df,input) {
  
  text_list <- unlist(strsplit(convstr, "%"))
  if (length(text_list)>1) {
    convstr = ""
    for (i in 1:length(text_list)){
      # Should add that this works for all subsets just in case
      if (text_list[i] %in% colnames(plot_subset(df,1,input))){
        uname = unique(plot_subset(df,1,input)[,text_list[i]])
        convstr = paste(convstr, uname, sep ="", collapse = ', ')  
      } else {
        convstr = paste(convstr, text_list[i], sep ="")
      }
    }
  } 
  
  str2 <- unlist(strsplit(convstr, "CO2"))
  #N20 toevoetgen.
  if(!(convstr=="" | identical(str2, character(0)))){
    if(str2!=convstr){
      convstr = bquote(.(str2[1])*CO[2]*.(if(length(str2)>1){str2[2]}))
    } 
  }
  
  return(convstr)
}

plot_build_suf <- function(G1, df, suffix, chartopt, input) {
  
  color = input[["color"]]
  fill = input[["fill"]]
  linetype = input[["linetype"]]
  shape = input[["shape"]]
  alpha = input[[paste("alpha", suffix, sep="")]]
  size = input[[paste("size", suffix, sep="")]]
  width = input[[paste("size", suffix, sep="")]]
  dodgewidth=input[[paste("dodgewidth", suffix, sep="")]]
  
  params <- list()
  if (chartopt!="geom_vline"){
    G1 = G1 + aes_string(x=input$x_var, y=input$y_var)
    if(linetype!="None"){G1 = G1 + aes_string(linetype=linetype)}
    if(color!="None"){G1 = G1 + aes_string(color=color)}
    if(shape!="None"){G1 = G1 + aes_string(shape=shape)}
    if(fill!="None"){G1 = G1 + aes_string(fill=fill)}
    params <- list(size=size, alpha=alpha)
  }
  
  
  stat = "identity"
  position = "identity"
  show.legend=NA
  inherit.aes=TRUE
  check.aes=TRUE
  
  if(chartopt=="Line"){
    geom="line"
  } else if(chartopt=="Point"){
    geom="point"
  } else if (chartopt=="Bar" | chartopt=="Stacked bar"){
    geom='bar'
    params$width = width
    if(dodgewidth==0){position="stack"}else{position=position_dodge(dodgewidth)}
  } else if(chartopt=="Area") {
    geom="area"
  } else if(chartopt=="geom_smooth") {
    geom="line"
    stat="smooth"
  } else if(chartopt=="Ribbon") {
    stat="summary"
    geom="ribbon"
    params$fun.ymin="min"
    params$fun.ymax="max"
    params$colour=NA
  } else if(chartopt=="average_line") {
    geom="line"
    params$fun.y="mean"
    stat="summary"
  } else if(chartopt=="Boxplot") {
    geom="boxplot"
    params$outlier.shape=46
  } else if(chartopt=="linesummary") {
    geom="linerange"
    params$fun.yxmax = "max"
    params$fun.ymin = "min"
    show.legend = FALSE
  }
  
  G1 = G1 + layer(geom=geom, stat=stat, data=df, mapping=NULL, position=position, params=params, show.legend=show.legend, inherit.aes=inherit.aes, check.aes=check.aes)
  
  
  
  return(G1)
}


plot_build_init <- function(df, input) {
  # Labels and stuff
  #functext = "G1 = ggplot(data=df)"
  G1 = ggplot(data=df)
  
  G1 = G1 + scale_color_manual(values=colorset(df, input)) 
  G1 = G1 + scale_fill_manual(values=colorset(df, input))
  G1 = G1 + scale_shape(solid=FALSE)
  
  return(G1)
}

plot_build_theme <- function(G1, input) {

  if(input$themeopt=="theme_bw"){G1 = G1 + theme_bw(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_classic"){G1 = G1 + theme_classic(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_dark"){G1 = G1 + theme_dark(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_gray"){G1 = G1 + theme_gray(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_light"){G1 = G1 + theme_light(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_linedraw"){G1 = G1 + theme_linedraw(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_minimal"){G1 = G1 + theme_minimal(base_size=14+input$TextSize)}
  if(input$themeopt=="theme_void"){G1 = G1 + theme_void(base_size=14+input$TextSize)}
  
  G1 = G1 + theme(legend.position=input$LegendPosition, legend.box="vertical",legend.box.just="left")

  return(G1)
  
}  
  
plot_build_wj <- function(df, input){
  
  if(nrow(df)==0){return(NULL)}
  
  #if(input$factor_xvar){df[,input$x_var] <-factor(df[,input$x_var])}
  
  
  #if(input$factor_xvar){df[,input$x_var] <-factor(df[,input$x_var])}
  
  
  G1 = plot_build_init(df,input)
  i = 1
  chartopt = input[[paste("chart", i, sep="")]]
  if(chartopt == "Bar") {df[,input$x_var] <-factor(df[,input$x_var])}
  
  if(!is.null(chartopt)) {
    if(chartopt!="None") {
      G1 = G1 %>% plot_build_suf(df, i, chartopt, input)
    }
  }
  
  G1 =  G1 %>% plot_build_theme(input)
  
  if(input$title!=""){G1 = G1 + ggtitle(stringconvert(convstr = input$title, df, input))} 
  if(input$xlab!=""){G1 = G1 + xlab(stringconvert(convstr = input$xlab, df, input))} 
  if(input$ylab!=""){G1 = G1 + ylab(stringconvert(convstr = input$ylab, df, input))} 
  
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
  
  #This adds anything in input$plot_text_add interpreted as parsed formulas
  eval(parse(text=input$plot_text_add))

  return(G1)
}
  
  


