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

plot_data_wj <- function(df,input,scaling="None", yr=2010){
  
  colcount <- ncol(df) - 2
  cln <- colcount + 1
  plot_levels <- colnames(df)
  
  for (i in 1:length(plot_levels)) {
    # Double brackets for input needed because it is a reactivevalues cladf
    # Doing year after scaling
    if(!(plot_levels[i] == "Year")) { # 
      if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
      if(!("All" %in% input[[plot_levels[i]]])){df[,plot_levels[i]] <- factor(df[,plot_levels[i]], levels=input[[plot_levels[i]]])}
    }   
  }
  
  df <- droplevels(df) # necedfary because empty levels might be left over after subsetting.
  df$Year = as.integer(as.character(df$Year))
  
  if(scaling == "Absolute"){
    if ("index" %in% colnames(df) & "value" %in% colnames(df)){
      df$index <- NULL
      cln = cln - 1
      colcount = colcount - 1
    }
    df <- spread(df, Year, value)
  
    df[,cln:ncol(df)] <- df[,cln:ncol(df)] - df[,as.character(yr)]

    df <- melt(df, id.vars=1:colcount, variable_name="Year")
    df <- na.omit(df)
    df$Year <- as.numeric(substr(df$Year, 1, stop=100))
  }
  if(scaling == "Relative"){
    if ("index" %in% colnames(df) & "value" %in% colnames(df)){
      df$index <- NULL
      cln = cln - 1
      colcount = colcount - 1
    }
    df <- spread(df, Year, value)
    df[,cln:ncol(df)] <- df[,cln:ncol(df)]/df[,as.character(yr)]
    df <- melt(df, id.vars=1:colcount, variable_name="Year")
    df <- na.omit(df)
    df$value <- df$value
    df$Year <- as.numeric(substr(df$Year, 1, stop=100))
  }
  
  for (i in 1:length(plot_levels)) {
    # Double brackets for input needed because it is a reactivevalues cladf
    # Doing year after scaling
    if(plot_levels[i] == "Year") { # 
      if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
    }   
  }
  
  return(df)
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
      if (text_list[i] %in% colnames(df)){
        uname = unique(df[,text_list[i]])
        convstr = paste(convstr, uname, sep ="", collapse = ', ')  
      } else {
        convstr = paste(convstr, text_list[i], sep ="")
      }
    }
  } 
  
  return(convstr)
}

plot_build_wj <- function(df, input){
  
  if(nrow(df)==0){return(NULL)}
  chartopt = input$chart
  if(chartopt == "Bar") {df[,input$x_var] <-factor(df[,input$x_var])}

  colset = deparse(colorset(df, input), width.cutoff = 500)

  G1_text = "G1 = ggplot(data=df)\n"
  
  G1_text = paste(G1_text, "G1 = G1 + aes(x=",input$x_var,", y=",input$y_var,")\n", sep="")
  if(input$ylabpercent){G1_text = paste(G1_text, "G1 = G1 + scale_y_continuous(labels=scales::percent)\n", sep="")}
  
  scalecolor = paste("G1 = G1 + scale_color_manual(values=",colset,")\n", sep="")
  scalefill = paste("G1 = G1 + scale_fill_manual(values=",colset,")\n", sep="")
  scaleshape = paste("G1 = G1 + scale_shape(solid=FALSE)\n", sep="")
  
  aesline = if(input$linetype!="None"){paste("G1 = G1 + aes(linetype=",input$linetype,")\n", sep="")}else{""}
  aesfill = if(input$fill!="None"){paste("G1 = G1 + aes(fill=",input$fill,")\n", sep="")}else{""}
  aesshape = if(input$shape!="None"){paste("G1 = G1 + aes(shape=",input$shape,")\n", sep="")}else{""}
  aescolor = if(input$color!="None"){paste("G1 = G1 + aes(color=",input$color,")\n", sep="")}else{""}
  plttext <- switch(chartopt,
         Line = paste(aesline,aescolor,scalecolor, 
                      "G1 = G1 + geom_line(size=",input$size,", alpha=",input$alpha,")\n", sep=""),
         Point = paste(aesshape,scaleshape, aescolor, 
                       "G1 = G1 + geom_point(size=",input$size,", alpha=",input$alpha,")\n", sep=""),
         Bar = paste(aesfill,scalefill, aescolor, scalecolor,
                       "G1 = G1 + geom_bar(stat='identity',size=",input$size,", alpha=",input$alpha,", width=",input$size,",",
                       "position=", if(input$dodgewidth==0){"'stack'"}else{paste("position_dodge(",input$dodgewidth,")", sep="")},")\n", sep="")
          )
  G1_text = paste(G1_text,plttext, sep="")
  
  G1_text = paste(G1_text, "G1 = G1 + ", input$themeopt,"(base_size=14+", input$TextSize,")\n", sep="")
  G1_text = paste(G1_text, "G1 = G1 + theme(legend.position='",input$LegendPosition,"',legend.box='vertical',legend.box.just='left')\n", sep="")  
  if(input$xlabrotate){
    G1_text = paste(G1_text, "G1 = G1 + theme(axis.text.x = element_text(angle=90, hjust=1, vjust =0.5))\n")
  }
  
  if(input$title!=""){G1_text <- paste(G1_text, "G1 = G1 + ggtitle('",stringconvert(convstr = input$title, df, input) ,"')\n", sep="")} 
  if(input$xlab!=""){G1_text <- paste(G1_text, "G1 = G1 + xlab('",stringconvert(convstr = input$xlab, df, input) ,"')\n", sep="")} 
  if(input$ylab!=""){G1_text <- paste(G1_text, "G1 = G1 + ylab('",stringconvert(convstr = input$ylab, df, input) ,"')\n", sep="")} 
  if(input$xman){G1_text <- paste(G1_text, "G1 = G1 + xlim(", input$xmin, ",",input$xmax,")\n", sep="")}
  if(input$yman){G1_text <- paste(G1_text, "G1 = G1 + ylim(", input$ymin, ",",if(input$ymin<input$ymax){input$ymax}else{NA},")\n", sep="")}
  
  if (!is.null(input$facet)) {
    if(input$facet!="None"){
      coltxt = paste("ncol=", input$ncol, sep="")
      scltxt = paste("scales='", input$scales, "'",sep="")
      frmtxt = paste("'",if(input$facet2=="None"){""}else{input$facet2}," ~ ",input$facet,"'", sep="")  
      if (input$facet_grid) {
        G1_text <- paste(G1_text, "G1 = G1 + facet_grid(as.formula(",frmtxt,"),",scltxt,")\n", sep="") 
      }else{
        G1_text <- paste(G1_text, "G1 = G1 + facet_wrap(as.formula(",frmtxt,"),",coltxt,",",scltxt,")\n", sep="")
      }
    }
  }
  
  return(G1_text)
}
  
  


