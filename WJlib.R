#library(reshape);
library(reshape2);
library(ggplot2);
library(plyr);
# Required packages to run script
library(plyr)
library(dplyr)
library(rmarkdown)
library(data.table)
library(knitr)
#library(tidyr)
# Plotting packages
library(ggplot2)
library(grid)

find_col_year <- function(df, yr) {
  # Finds 
  clnr=c(0,0)
  for (i in 1970:2100) { # finding first year occurrence.
    clnr[2] = match(paste("X", yr, sep=""), colnames(df), nomatch = 0)
    clnr[2] = match(yr, colnames(df), nomatch = 0)
    if (clnr != 0) {break}
  }
  
  return(clnr)
}

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
  
  print(summary(df))
  
  return(df)
}

my_dataread <- function(file_list, name_list = FALSE) {
  
  filecount = length(file_list)
  
  for(i in 1:filecount){
   
    f <- file_list[i]
    fext <- tolower(file_ext(file_list[i])[1])

    if (fext == "rda") { 
      DATAtmp <- readRDS(f)
    } else if (fext == "out" |fext == "scn") {
      DATAtmp <- read.mym2r(f)
    } else {
      DATAtmp <- read.csv(f, sep=",", dec=".")
      if(ncol(DATAtmp)==1) {
        DATAtmp <- read.csv(f, sep=";", dec=".")
      }
    }
    
    DATAtmp <- data_cleaner(DATAtmp)
    
    if(filecount>1){
      if (name_list){
        DATAtmp$file_name <- name_list[i]
      } else {
        DATAtmp$file_name <- f
      }
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

plot_data_wj <- function(df,input,scaling="None", yr=2010, scalingby="", Facet="None"){
  
  colcount <- ncol(df) - 2
  cln <- colcount + 1
  plot_levels <- colnames(df)
  
  for (i in 1:length(plot_levels)) {
    # Double brackets for input needed because it is a reactivevalues class
    # Doing year after scaling
    if(!(plot_levels[i] == "Year")) { # 
      if(!("All" %in% input[[plot_levels[i]]])){df <- subset(df, df[,plot_levels[i]] %in% input[[plot_levels[i]]])}
      if(!("All" %in% input[[plot_levels[i]]])){df[,plot_levels[i]] <- factor(df[,plot_levels[i]], levels=input[[plot_levels[i]]])}
    }   
  }
  
  df <- droplevels(df) # necedfary because empty levels might be left over after subsetting.
  df$Year = as.integer(as.character(df$Year))
  
  if (scalingby != "" & scaling == "Absolute") {
    rowvar <- unlist(strsplit(scalingby, ";"))[2]
    col <- unlist(strsplit(scalingby, ";"))[1]
    scale <- df[df[,col]==rowvar & df$Year == yr,]
    scale = scale$value # will be multiple values in case of facets (or wrong input)
  }
  
  if(scaling == "Absolute"){
    if ("index" %in% colnames(df) & "value" %in% colnames(df)){
      df$index <- NULL
      cln = cln - 1
      colcount = colcount - 1
    }
    df <- spread(df, Year, value)
    if(scalingby != "" & Facet != "None") {
      rows <- unique(df[,Facet])
      for (i in 1:length(rows)) {
        dfsub = subset(df, df[,Facet]==rows[i])
        scale <- dfsub[dfsub[,col]==rowvar,]
        scale = scale[,as.character(yr)]
        dfsub[,cln:ncol(dfsub)] <- dfsub[,cln:ncol(dfsub)] - dfsub[,as.character(yr)] + scale
        df <- rbind(dfsub, subset(df, df[,Facet]!=rows[i]))
      }
    } else {
      df[,cln:ncol(df)] <- df[,cln:ncol(df)] - df[,as.character(yr)] + scale
    }
    
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
  if("value" %in% colnames(df)) {
    df$value <- as.numeric(substr(df$value, 1, stop=100))
  }
  if("index" %in% colnames(df)){
    df$index <- as.numeric(df$index)
  }
  if("Year" %in% colnames(df)) {
    df$Year = as.integer(as.character(df$Year))
  }
  
  #Making agmip columsn ig "AGMIP| etc is detected
  if("Variable" %in% colnames(df)) {
    if (!("Item" %in% colnames(df)) & substr(df$Variable[1],1,5) == "AGMIP") {
      df$Variable <- gsub("AGMIP|", "", df$Variable, fixed=TRUE)
      df$Item <- gsub("\\|.*", "", df$Variable) # Everything after | removed
      df$Variable <- gsub(".*\\|", "", df$Variable)
    }
  }
  if ("Unit" %in% colnames(df) & "Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
    df$Variable_AgMIP <- factor(paste(df$Variable ,"_", df$Item, "_", df$Unit, sep=""))
  }
  if ("Variable" %in% colnames(df) & "Item" %in% colnames(df) ) {
    # To do, make uppercase
    df$Variable = gsub("^Area$", "AREA", df$Variable)
    df$Variable = gsub("^Prod$", "PROD", df$Variable)
    df$Variable = gsub("^Feed$", "FEED", df$Variable)
    df$Variable = gsub("^food$", "FOOD", df$Variable)
    df$Variable = gsub("^Food$", "FOOD", df$Variable)
    # df$Variable = gsub("Area", "AREA", df$Region)
    # df$Variable = gsub("Area", "AREA", df$Region)
  }
  
  #some general renaming
  # if("Region" %in% colnames(df)){
  #   df$Region = gsub("R5.2", "", df$Region)
  #   df$Region = gsub("Global", "WLD", df$Region)
  #   df$Region = gsub("^World$", "WLD", df$Region)
  #   df$Region = gsub("Total", "WLD", df$Region)
  # }
  if("Unit" %in% colnames(df)){
    df$Unit = gsub("Mt CO2e", "MtCO2e",df$Unit)
    df$Unit = gsub("kcal/cap/day", "kcal/cap/d",df$Unit)
    df$Unit = gsub("1000 Ha", "1000 ha",df$Unit)
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
    df$Item = gsub("Soyabeans", "Soybeans", df$Item)
    df$Item = gsub("^Soybean$", "Soybeans", df$Item)
    df$Item = gsub("^soybean$", "Soybeans", df$Item)
    df$Item = gsub("Palm oil  /", "Palm oil", df$Item)
    df$Item = gsub("groundnut$", "Groundnuts", df$Item)
    df$Item = gsub("^cotton", "Seed cotton", df$Item)
    df$Item = gsub("oilpalm", "Palm oil", df$Item)
    df$Item = gsub("^potato", "Potatoes", df$Item)
    df$Item = gsub("rapeseed", "Rape seed", df$Item)
    df$Item = gsub("sunflower", "Sunflower seed", df$Item)
    df$Item = gsub("cassava", "Cassava", df$Item)
    df$Item = gsub("sugarbeet", "Sugar beet", df$Item)
    df$Item = gsub("sugarcane", "Sugar cane", df$Item)
    
    #df$Item = gsub("Cereals", "Cereal", df$Item)
  }
  
  df = na.omit(df)
  
  #This makes sure that all non-numeric columns are factorized.
  df <- df %>% mutate_if(is.character,as.factor)
  
  if("value" %in% colnames(df)) {
    df$value <- as.numeric(df$value)
    col_idx <- grep("value", names(df))
    #moves value to the back
    df <- df[, c((1:ncol(df))[-col_idx],col_idx)]
  }
  
  df <- df[!duplicated(df),]
  
  return(df)
}

## Some functions for mapping that didn't work out too well

mapping_aggregation <- function(DATA){
  
#   df <- "
# Variable,PerVariable
# YILD_CRP_fm t/ha,AREA_CRP_ha
# YILD_WHT_fm t/ha,AREA_WHT_ha
# YIRF_WHT_fm t/ha,ARRF_WHT_ha
# YIIR_WHT_fm t/ha,ARIR_WHT_ha
# YILD_RIC_fm t/ha,AREA_RIC_ha
# YIRF_RIC_fm t/ha,ARRF_RIC_ha
# YIIR_RIC_fm t/ha,ARIR_RIC_ha
# YILD_CGR_fm t/ha,AREA_CGR_ha
# YIRF_CGR_fm t/ha,ARRF_CGR_ha
# YIIR_CGR_fm t/ha,ARIR_CGR_ha
# YILD_Cereals_fm t/ha,AREA_Cereals_ha
# YIRF_Cereals_fm t/ha,ARRF_Cereals_ha
# YIIR_Cereals_fm t/ha,ARIR_Cereals_ha
# YILD_ATT_WHT_fm t/ha,AREA_WHT_ha
# YILD_ATT_RIC_fm t/ha,AREA_RIC_ha
# YILD_ATT_CGR_fm t/ha,AREA_CGR_ha
# YILD_ATT_Cereals_fm t/ha,AREA_Cereals_ha"

  df <- "
Variable,PerVariable
YILD,AREA
YIRF,ARRF
YIIR,ARIR
YEXO,AREA
YENDO,AREA
YSTRUC,AREA
YENDOSTRUC,AREA
CropInt,LAND
IrrPerc,LAND
YILD_ATT,AREA
YILD_POT,AREA
YIIR_ATT,ARIR
YIIR_POT,ARIR
YIRF_ATT,ARRF
YIRF_POT,ARRF"
 
  map_ag <- read.delim(textConnection(df), header = TRUE, sep = ",")
  
  #currently works for AgMIP only.
  
  df_ex <- DATA

  # First loop calculate totals per variable
  for (i in 1:dim(map_ag)[1]) {
    mapvar <- toString(map_ag$Variable[i])
    pervar <- toString(map_ag$PerVariable[i])
    
    if (mapvar %in% DATA$Variable) {
      
      ss <- subset(DATA, Variable == mapvar | Variable == pervar)
      ncol = length(colnames(ss))-2
      ss <- spread(ss, Variable, value)
      
      ss <- mapping_crops(ss, "AgMIP","Crop")
      sscer <- subset(ss, Item=="WHT" | Item=="CGR" | Item=="RIC")
      if(nrow(sscer)>0){
        sscer$Item <- "Cereals"
        ss <- rbind(sscer,ss)
      }
      sscrp <- subset(ss, Item=="OCR" |Item=="OSD" |Item=="SGC" |Item=="VFN" |Item=="WHT" | Item=="CGR" | Item=="RIC")
      if(nrow(sscrp)>0){
        sscrp$Item <- "CRP"
        ss <- rbind(sscrp,ss)
      }
      ss <- mapping_regions(ss, "AgMIP.13","Country")
      
      ss[,mapvar] <- ss[,mapvar] * ss[,pervar]
      ss <- aggregate(as.formula(paste("cbind(", mapvar,",",pervar,")~.", sep="")), data=ss, sum)
      ss[,mapvar] <- ss[,mapvar] / ss[,pervar]
      
      ss[,pervar] <- NULL

      ss <- melt(ss, id=1:ncol, variable_name = "Variable")  
      
      df_ex <- subset(df_ex, Variable != mapvar)
      
      # print(summary(ss))
      # ss_mapvar <- subset(DATA, Variable == mapvar)
      # ss_pervar <- subset(DATA, Variable == toString(map_ag$PerVariable[i]))
      # ss_mapvar$value <- ss_mapvar$value * ss_pervar$value
      # DATA <- rbind(subset(DATA, Variable != mapvar), ss)
      
      if (i==1) {
        df_ag = ss
      } else {
        df_ag = rbind(df_ag, ss)
      }
    }
  }
  
  # set divdes by zero to zero (assuming there are no nas comming in)
  df_ag[is.na(df_ag)] <- 0
  # df_ag <- rbind(df_ag, subset(DATA, !(Variable %in% map_ag$Variable | Variable %in% map_ag$PerVariable)))
  
  df_ex <- mapping_crops(df_ex, "AgMIP","Crop")
  df_excer <- subset(df_ex, Item=="WHT" | Item=="CGR" | Item=="RIC")
  if(nrow(df_excer)>0){
    df_excer$Item <- "Cereals"
    df_ex <- rbind(df_excer,df_ex)
  }
  df_excrp <- subset(df_ex, Item=="OCR"|Item=="OSD"|Item=="SGC"|Item=="VFN" |Item=="WHT"|Item=="CGR"|Item=="RIC")
  if(nrow(df_excrp)>0){
    df_excrp$Item <- "CRP"
    df_ex <- rbind(df_excrp,df_ex)
  }
  df_ex <- mapping_regions(df_ex, "AgMIP.13", "Country")
  df_ex <- aggregate(value~., data=df_ex, sum)
  
  # DATA <- aggregate(value~., data=DATA, sum)
  #  # summing similar data
  # 
  # # Seccond for calculation after summing
  # for (i in 1:dim(map_ag)[2]) {
  #   mapvar <- toString(map_ag$Variable[i])
  #   pervar <- toString(map_ag$PerVariable[i])
  #   ss_mapvar <- subset(DATA, Variable == mapvar)
  #   ss_pervar <- subset(DATA, Variable == pervar)
  #   ss_mapvar$value <- ss_mapvar$value / ss_pervar$value
  #   
  #   ss <- subset(DATA, Variable == mapvar | Variable == pervar)
  #   # 
  #   ss <- spread(ss, Variable, value)
  #   DATA[,mapvar] <- DATA[,mapvar] / DATA[,pervar]
  #   ncol = length(colnames(ss))-2
  #   ss <- melt(ss, id=1:ncol, "Variable")  
  #   # 
  #   DATA <- rbind(subset(DATA, Variable != mapvar), ss)
  # }
  # 
  # return(DATA)
  return(rbind(df_ag, df_ex))
  
}

mapping_crops <- function(df, to, from){
  
  map_table <- "
Crop,Image,AgMIP
CRP,CRP,CRP
Rice,Rice,RIC
Maize,Maize,CGR
Wheat,Temperate cereals,WHT
Barley,Temperate cereals,CGR
Oats,Temperate cereals,CGR
Rye,Temperate cereals,CGR
Sorgum,Tropical cereals,CGR
Sorghum,Tropical cereals,CGR
Millet,Tropical cereals,CGR
Beans,Pulses,VFN
Potatoes,Roots & tubers,VFN
Oilcrops,Oil crops,OSD
Soybeans,Oil crops,OSD
Other cereals, ,CGR
Sweet potatoes, ,VFN
Cassava, ,VFN
Other roots, ,VFN
Plantains, ,VFN
Sugar beet, ,SGC
Sugar cane, ,SGC
Pulses, ,VFN
Vegetables, ,VFN
Bananas, ,VFN
Citrus fruit, ,VFN
Other fruit, ,VFN
Oilcrops nes, ,OSD
Rape seed, ,OSD
Palm oil, ,OSD
Groundnuts, ,OSD
Sunflower seed, ,OSD
Sesame seed, ,OSD
Coconuts, ,OSD
Cocoa beans, ,OCR
Coffee green, ,OCR
Tea, ,OCR
Tobacco, ,OCR
Seed cotton, ,PFB
Hard fibres, ,PFB
  

"
  map_table <- read.delim(textConnection(map_table), header = TRUE, sep = ",")
  
  df_map <- subset(df, Item %in% map_table$Crop)
  df <- subset(df, !(Item %in% map_table$Crop))
  
  df_map$Item <- map_table$AgMIP[match(df_map$Item, map_table$Crop)]
  
  # for (i in 1:dim(map_table)[1]) {
  #   print(i)
  #   df$Region <- gsub(toString(map_table[i,from]), toString(map_table[i,to]), df$Region)
  # }
  
  return(rbind(df, df_map))
}


mapping_regions <- function(df, to, from, jd = TRUE) {
  
  map_table <- "
ISO code,Country name,IMAGE region Index-26,IMAGE region-26,IMAGE region-24,IMAGE region Code-26,MAGNET region Index-26,MAGNET region-26,GDAM L-0 Country Index,SSP Database Region,AgMIP-13,AgMIP-4,FSMIP Region-7,FSMIP Region Code-7,Comment
AFG,Afghanistan,25,Rest South Asia,South-East Asia,RSAS,20,India +,1,ASIA,OAS,SAS,South Asia,SASIA,
ALB,Albania,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,3,OECD,EUR,,Rest World,ROW,
DZA,Algeria,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,4,MAF,MEN,AME,Middle East and North Africa,MENA,
ASM,American Samoa,24,Oceania,Oceania,OCE,26,Oceania,5,OECD,ANZ,,Rest World,ROW,
AND,Andorra,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,6,OECD,EUR,,Rest World,ROW,
AGO,Angola,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,7,MAF,SSA,AME,Sub-Saharan Africa,SSA,
AIA,Anguilla,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,8,LAM,OSA,OAM,Latin America and Caribbean,LAC,
ATG,Antigua and Barbuda,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,10,LAM,OSA,OAM,Latin America and Caribbean,LAC,
ARG,Argentina,6,Rest South America,Rest South America,RSAM,6,Rest South America,11,LAM,OSA,OAM,Latin America and Caribbean,LAC,
ARM,Armenia,16,Russia +,Russia +,RUS,18,Russia +,12,REF,FSU,,Rest World,ROW,
ABW,Aruba,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,13,LAM,OSA,OAM,Latin America and Caribbean,LAC,
AUS,Australia,24,Oceania,Oceania,OCE,26,Oceania,14,OECD,ANZ,,Rest World,ROW,
AUT,Austria,11,OECD Europe,OECD Europe,WEU,11,EU16,15,OECD,EUR,,European Union,EU,
AZE,Azerbaijan,16,Russia +,Russia +,RUS,18,Russia +,16,REF,FSU,,Rest World,ROW,
BHS,Bahamas,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,17,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BHR,Bahrain,17,Middle East,Middle East,ME,19,Middle East,18,MAF,MEN,AME,Middle East and North Africa,MENA,
BGD,Bangladesh,25,Rest South Asia,South-East Asia,RSAS,20,India +,19,ASIA,OAS,SAS,South Asia,SASIA,
BRB,Barbados,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,20,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BLR,Belarus,14,Ukraine +,Ukraine +,UKR,16,Ukraine +,21,REF,FSU,,Rest World,ROW,
BEL,Belgium,11,OECD Europe,OECD Europe,WEU,11,EU16,22,OECD,EUR,,European Union,EU,
BLZ,Belize,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,23,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BEN,Benin,8,Western Africa,Western Africa,WAF,8,Western Africa,24,MAF,SSA,AME,Sub-Saharan Africa,SSA,
BMU,Bermuda,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,25,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BTN,Bhutan,25,Rest South Asia,South-East Asia,RSAS,20,India +,26,ASIA,OAS,SAS,South Asia,SASIA,
BOL,Bolivia,6,Rest South America,Rest South America,RSAM,6,Rest South America,27,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BES,\"Bonaire, Saint Eustatius and Saba\",4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,28,LAM,OSA,OAM,
BIH,Bosnia and Herzegovina,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,29,OECD,EUR,,Rest World,ROW,
BWA,Botswana,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,30,MAF,SSA,AME,Sub-Saharan Africa,SSA,
BVT,Bouvet Island,6,Rest South America,Rest South America,RSAM,6,Rest South America,31,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BRA,Brazil,5,Brazil,Brazil,BRA,5,Brazil,32,LAM,BRA,OAM,Latin America and Caribbean,LAC,
IOT,British Indian Ocean Territory,25,Rest South Asia,South-East Asia,RSAS,20,India +,33,ASIA,OAS,SAS,South Asia,SASIA,
VGB,British Virgin Islands,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,34,LAM,OSA,OAM,Latin America and Caribbean,LAC,
BRN,Brunei,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,35,ASIA,SEA,SAS,East Asia,EASIA,
BGR,Bulgaria,12,Eastern Europe,Eastern Europe,CEU,13,EU12,36,OECD,EUR,,European Union,EU,
BFA,Burkina Faso,8,Western Africa,Western Africa,WAF,8,Western Africa,37,MAF,SSA,AME,Sub-Saharan Africa,SSA,
BDI,Burundi,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,38,MAF,SSA,AME,Sub-Saharan Africa,SSA,
KHM,Cambodia,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,39,ASIA,SEA,SAS,East Asia,EASIA,
CMR,Cameroon,8,Western Africa,Western Africa,WAF,8,Western Africa,40,MAF,SSA,AME,Sub-Saharan Africa,SSA,
CAN,Canada,1,Canada,Canada,CAN,1,Canada,41,OECD,CAN,NAM,Rest World,ROW,
CPV,Cape Verde,8,Western Africa,Western Africa,WAF,8,Western Africa,42,MAF,SSA,AME,Sub-Saharan Africa,SSA,
CA-,Caspian Sea,16,Russia +,Russia +,RUS,18,Russia +,43,REF,FSU,,,,Added by WJvZ based on GADM0 dataset
CYM,Cayman Islands,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,44,LAM,OSA,OAM,Latin America and Caribbean,LAC,
CAF,Central African Republic,8,Western Africa,Western Africa,WAF,8,Western Africa,45,MAF,SSA,AME,Sub-Saharan Africa,SSA,
TCD,Chad,8,Western Africa,Western Africa,WAF,8,Western Africa,46,MAF,SSA,AME,Sub-Saharan Africa,SSA,
CHL,Chile,6,Rest South America,Rest South America,RSAM,6,Rest South America,47,LAM,OSA,OAM,Latin America and Caribbean,LAC,
CHN,China,20,China +,China +,CHN,22,China +,48,ASIA,CHN,SAS,East Asia,EASIA,
CXR,Christmas Island,24,Oceania,Oceania,OCE,26,Oceania,49,OECD,ANZ,,Rest World,ROW,
CCK,Cocos Islands,24,Oceania,Oceania,OCE,26,Oceania,51,OECD,ANZ,,Rest World,ROW,Added by WJvZ based on GADM0 dataset
COL,Colombia,6,Rest South America,Rest South America,RSAM,6,Rest South America,52,LAM,OSA,OAM,Latin America and Caribbean,LAC,
COM,Comoros,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,53,MAF,SSA,AME,Sub-Saharan Africa,SSA,
COK,Cook Islands,24,Oceania,Oceania,OCE,26,Oceania,54,OECD,OAS,,Rest World,ROW,
CRI,Costa Rica,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,55,LAM,OSA,OAM,Latin America and Caribbean,LAC,
CIV,Cote d'Ivoire,8,Western Africa,Western Africa,WAF,8,Western Africa,56,MAF,SSA,AME,Sub-Saharan Africa,SSA,
HRV,Croatia,12,Eastern Europe,Eastern Europe,CEU,13,EU12,57,OECD,EUR,,European Union,EU,
CUB,Cuba,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,58,LAM,OSA,OAM,Latin America and Caribbean,LAC,
CUW,Cura?ao,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,59,LAM,OSA,OAM,t
CYP,Cyprus,12,Eastern Europe,Eastern Europe,CEU,13,EU12,60,OECD,EUR,,European Union,EU,
CZE,Czech Republic,12,Eastern Europe,Eastern Europe,CEU,13,EU12,61,OECD,EUR,,European Union,EU,
COD,Democratic Republic of the Congo,8,Western Africa,Western Africa,WAF,8,Western Africa,62,MAF,SSA,AME,Sub-Saharan Africa,SSA,
DNK,Denmark,11,OECD Europe,OECD Europe,WEU,11,EU16,63,OECD,EUR,,European Union,EU,
DJI,Djibouti,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,64,MAF,SSA,AME,Sub-Saharan Africa,SSA,
DMA,Dominica,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,65,LAM,OSA,OAM,Latin America and Caribbean,LAC,
DOM,Dominican Republic,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,66,LAM,OSA,OAM,Latin America and Caribbean,LAC,
TLS,East Timor,22,Indonesia +,Indonesia +,INDO,24,Indonesia +,67,ASIA,SEA,SAS,East Asia,EASIA,
ECU,Ecuador,6,Rest South America,Rest South America,RSAM,6,Rest South America,68,LAM,OSA,OAM,Latin America and Caribbean,LAC,
EGY,Egypt,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,69,MAF,MEN,AME,Middle East and North Africa,MENA,
SLV,El Salvador,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,70,LAM,OSA,OAM,Latin America and Caribbean,LAC,
GNQ,Equatorial Guinea,8,Western Africa,Western Africa,WAF,8,Western Africa,71,MAF,SSA,AME,Sub-Saharan Africa,SSA,
ERI,Eritrea,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,72,MAF,SSA,AME,Sub-Saharan Africa,SSA,
EST,Estonia,12,Eastern Europe,Eastern Europe,CEU,13,EU12,73,OECD,EUR,,European Union,EU,
ETH,Ethiopia,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,74,MAF,SSA,AME,Sub-Saharan Africa,SSA,
FLK,Falkland Islands,6,Rest South America,Rest South America,RSAM,6,Rest South America,75,LAM,OSA,OAM,Latin America and Caribbean,LAC,
FRO,Faroe Islands,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,76,OECD,EUR,,Rest World,ROW,
FJI,Fiji,24,Oceania,Oceania,OCE,26,Oceania,77,OECD,OAS,,Rest World,ROW,
FIN,Finland,11,OECD Europe,OECD Europe,WEU,11,EU16,78,OECD,EUR,,European Union,EU,
FRA,France,11,OECD Europe,OECD Europe,WEU,11,EU16,79,OECD,EUR,,European Union,EU,
GUF,French Guiana,6,Rest South America,Rest South America,RSAM,6,Rest South America,80,LAM,OSA,OAM,Latin America and Caribbean,LAC,
PYF,French Polynesia,24,Oceania,Oceania,OCE,26,Oceania,81,OECD,OAS,,Rest World,ROW,
ATF,French Southern Territories,24,Oceania,Oceania,OCE,26,Oceania,82,OECD,ANZ,,Rest World,ROW,
GAB,Gabon,8,Western Africa,Western Africa,WAF,8,Western Africa,83,MAF,SSA,AME,Sub-Saharan Africa,SSA,
GMB,Gambia,8,Western Africa,Western Africa,WAF,8,Western Africa,84,MAF,SSA,AME,Sub-Saharan Africa,SSA,
GEO,Georgia,16,Russia +,Russia +,RUS,18,Russia +,85,REF,FSU,,Rest World,ROW,
DEU,Germany,11,OECD Europe,OECD Europe,WEU,11,EU16,86,OECD,EUR,,European Union,EU,
GHA,Ghana,8,Western Africa,Western Africa,WAF,8,Western Africa,87,MAF,SSA,AME,Sub-Saharan Africa,SSA,
GIB,Gibraltar,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,88,OECD,EUR,,Rest World,ROW,
GRC,Greece,11,OECD Europe,OECD Europe,WEU,11,EU16,89,OECD,EUR,,European Union,EU,
GRD,Grenada,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,91,LAM,OSA,OAM,Latin America and Caribbean,LAC,
GLP,Guadeloupe,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,92,LAM,OSA,OAM,Latin America and Caribbean,LAC,
GUM,Guam,24,Oceania,Oceania,OCE,26,Oceania,93,OECD,ANZ,,Rest World,ROW,
GTM,Guatemala,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,94,LAM,OSA,OAM,Latin America and Caribbean,LAC,
GGY,Guernsey,11,OECD Europe,OECD Europe,WEU,11,EU16,95,OECD,EUR,
GIN,Guinea,8,Western Africa,Western Africa,WAF,8,Western Africa,96,MAF,SSA,AME,Sub-Saharan Africa,SSA,
GNB,Guinea-Bissau,8,Western Africa,Western Africa,WAF,8,Western Africa,97,MAF,SSA,AME,Sub-Saharan Africa,SSA,
GUY,Guyana,6,Rest South America,Rest South America,RSAM,6,Rest South America,98,LAM,OSA,OAM,Latin America and Caribbean,LAC,
HTI,Haiti,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,99,LAM,OSA,OAM,Latin America and Caribbean,LAC,
HMD,Heard Island and McDonald Islands,24,Oceania,Oceania,OCE,26,Oceania,100,OECD,ANZ,,Rest World,ROW,
HND,Honduras,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,101,LAM,OSA,OAM,Latin America and Caribbean,LAC,
HKG,Hong Kong,20,China +,China +,CHN,22,China +,102,ASIA,CHN,SAS,Rest World,ROW,
HUN,Hungary,12,Eastern Europe,Eastern Europe,CEU,13,EU12,103,OECD,EUR,,European Union,EU,
ISL,Iceland,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,104,OECD,EUR,,Rest World,ROW,
IND,India,18,India +,India +,INDIA,20,India +,105,ASIA,IND,SAS,South Asia,SASIA,
IDN,Indonesia,22,Indonesia +,Indonesia +,INDO,24,Indonesia +,106,ASIA,SEA,SAS,East Asia,EASIA,
IRN,Iran,17,Middle East,Middle East,ME,19,Middle East,107,MAF,MEN,AME,Middle East and North Africa,MENA,
IRQ,Iraq,17,Middle East,Middle East,ME,19,Middle East,108,MAF,MEN,AME,Middle East and North Africa,MENA,
IRL,Ireland,11,OECD Europe,OECD Europe,WEU,11,EU16,109,OECD,EUR,,European Union,EU,
IMN,Isle of Man,11,OECD Europe,OECD Europe,WEU,11,EU16,110,OECD,EUR,,,,Added by WJvZ based on GADM0 dataset
ISR,Israel,17,Middle East,Middle East,ME,19,Middle East,111,MAF,MEN,AME,Middle East and North Africa,MENA,
ITA,Italy,11,OECD Europe,OECD Europe,WEU,11,EU16,112,OECD,EUR,,European Union,EU,
JAM,Jamaica,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,113,LAM,OSA,OAM,Latin America and Caribbean,LAC,
JPN,Japan,23,Japan,Japan,JAP,25,Japan,114,OECD,SEA,SAS,Rest World,ROW,
JEY,Jersey,11,OECD Europe,OECD Europe,WEU,11,EU16,115,OECD,EUR,
JOR,Jordan,17,Middle East,Middle East,ME,19,Middle East,116,MAF,MEN,AME,Middle East and North Africa,MENA,
KAZ,Kazakhstan,15,Asia-Stan,Asia-Stan,STAN,17,Asia-Stan,117,REF,FSU,,Rest World,ROW,
KEN,Kenya,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,118,MAF,SSA,AME,Sub-Saharan Africa,SSA,
KIR,Kiribati,24,Oceania,Oceania,OCE,26,Oceania,119,OECD,OAS,,Rest World,ROW,
KO-,Kosovo,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,120,OECD,EUR,
KWT,Kuwait,17,Middle East,Middle East,ME,19,Middle East,121,MAF,MEN,AME,Middle East and North Africa,MENA,
KGZ,Kyrgyzstan,15,Asia-Stan,Asia-Stan,STAN,17,Asia-Stan,122,REF,FSU,,Rest World,ROW,
LAO,Laos,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,123,ASIA,SEA,SAS,East Asia,EASIA,
LVA,Latvia,12,Eastern Europe,Eastern Europe,CEU,13,EU12,124,OECD,EUR,,European Union,EU,
LBN,Lebanon,17,Middle East,Middle East,ME,19,Middle East,125,MAF,MEN,AME,Middle East and North Africa,MENA,
LSO,Lesotho,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,126,MAF,SSA,AME,Sub-Saharan Africa,SSA,
LBR,Liberia,8,Western Africa,Western Africa,WAF,8,Western Africa,127,MAF,SSA,AME,Sub-Saharan Africa,SSA,
LBY,Libya,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,128,MAF,MEN,AME,Middle East and North Africa,MENA,
LIE,Liechtenstein,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,129,OECD,EUR,,Rest World,ROW,
LTU,Lithuania,12,Eastern Europe,Eastern Europe,CEU,13,EU12,130,OECD,EUR,,European Union,EU,
LUX,Luxembourg,11,OECD Europe,OECD Europe,WEU,11,EU16,131,OECD,EUR,,European Union,EU,
MAC,Macau,20,China +,China +,CHN,22,China +,132,ASIA,CHN,SAS,Rest World,ROW,
MKD,Macedonia,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,133,OECD,EUR,,Rest World,ROW,
MDG,Madagascar,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,134,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MWI,Malawi,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,135,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MYS,Malaysia,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,136,ASIA,SEA,SAS,East Asia,EASIA,
MDV,Maldives,25,Rest South Asia,South-East Asia,RSAS,20,India +,137,ASIA,OAS,SAS,South Asia,SASIA,
MLI,Mali,8,Western Africa,Western Africa,WAF,8,Western Africa,138,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MLT,Malta,12,Eastern Europe,Eastern Europe,CEU,11,EU16,139,OECD,EUR,,European Union,EU,
MHL,Marshall Islands,24,Oceania,Oceania,OCE,26,Oceania,140,OECD,OAS,,Rest World,ROW,
MTQ,Martinique,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,141,LAM,OSA,OAM,Latin America and Caribbean,LAC,
MRT,Mauritania,8,Western Africa,Western Africa,WAF,8,Western Africa,142,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MUS,Mauritius,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,143,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MYT,Mayotte,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,144,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MEX,Mexico,3,Mexico,Mexico,MEX,3,Mexico,145,LAM,OSA,OAM,Rest World,ROW,
FSM,Micronesia,24,Oceania,Oceania,OCE,26,Oceania,146,OECD,OAS,,Rest World,ROW,
MDA,Moldova,14,Ukraine +,Ukraine +,UKR,16,Ukraine +,147,REF,FSU,,Rest World,ROW,
MCO,Monaco,11,OECD Europe,OECD Europe,WEU,11,EU16,148,OECD,EUR,,Rest World,ROW,
MNG,Mongolia,20,China +,China +,CHN,22,China +,149,ASIA,OAS,SAS,Rest World,ROW,
MNE,Montenegro,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,150,OECD,EUR,,Rest World,ROW,
MSR,Montserrat,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,151,LAM,OSA,OAM,Latin America and Caribbean,LAC,
MAR,Morocco,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,152,MAF,MEN,AME,Middle East and North Africa,MENA,
MOZ,Mozambique,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,153,MAF,SSA,AME,Sub-Saharan Africa,SSA,
MMR,Myanmar,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,154,ASIA,SEA,SAS,East Asia,EASIA,
NAM,Namibia,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,155,MAF,SSA,AME,Sub-Saharan Africa,SSA,
NRU,Nauru,24,Oceania,Oceania,OCE,26,Oceania,156,OECD,OAS,,Rest World,ROW,
NPL,Nepal,25,Rest South Asia,South-East Asia,RSAS,20,India +,157,ASIA,OAS,SAS,South Asia,SASIA,
NLD,Netherlands,11,OECD Europe,OECD Europe,WEU,11,EU16,158,OECD,EUR,,European Union,EU,
NCL,New Caledonia,24,Oceania,Oceania,OCE,26,Oceania,159,OECD,OAS,,Rest World,ROW,
NZL,New Zealand,24,Oceania,Oceania,OCE,26,Oceania,160,OECD,ANZ,,Rest World,ROW,
NIC,Nicaragua,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,161,LAM,OSA,OAM,Latin America and Caribbean,LAC,
NER,Niger,8,Western Africa,Western Africa,WAF,8,Western Africa,162,MAF,SSA,AME,Sub-Saharan Africa,SSA,
NGA,Nigeria,8,Western Africa,Western Africa,WAF,8,Western Africa,163,MAF,SSA,AME,Sub-Saharan Africa,SSA,
NIU,Niue,24,Oceania,Oceania,OCE,26,Oceania,164,OECD,OAS,,Rest World,ROW,
NFK,Norfolk Island,24,Oceania,Oceania,OCE,26,Oceania,165,OECD,ANZ,,Rest World,ROW,
PRK,North Korea,19,Korea,Korea,KOR,21,Korea,166,ASIA,SEA,SAS,Rest World,ROW,
MNP,Northern Mariana Islands,24,Oceania,Oceania,OCE,26,Oceania,167,OECD,ANZ,,Rest World,ROW,
NOR,Norway,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,168,OECD,EUR,,Rest World,ROW,
OMN,Oman,17,Middle East,Middle East,ME,19,Middle East,169,MAF,MEN,AME,Middle East and North Africa,MENA,
PAK,Pakistan,25,Rest South Asia,South-East Asia,RSAS,20,India +,170,ASIA,OAS,SAS,South Asia,SASIA,
PLW,Palau,24,Oceania,Oceania,OCE,26,Oceania,171,OECD,OAS,,Rest World,ROW,
PSE,Palestina,17,Middle East,Middle East,ME,19,Middle East,172,MAF,MEN,AME,Middle East and North Africa,MENA,Added by WJvZ based on GADM0 dataset
PAN,Panama,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,173,LAM,OSA,OAM,Latin America and Caribbean,LAC,
PNG,Papua New Guinea,22,Indonesia +,Indonesia +,INDO,24,Indonesia +,174,ASIA,OAS,SAS,East Asia,EASIA,
PRY,Paraguay,6,Rest South America,Rest South America,RSAM,6,Rest South America,175,LAM,OSA,OAM,Latin America and Caribbean,LAC,
PER,Peru,6,Rest South America,Rest South America,RSAM,6,Rest South America,176,LAM,OSA,OAM,Latin America and Caribbean,LAC,
PHL,Philippines,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,177,ASIA,SEA,SAS,East Asia,EASIA,
PCN,Pitcairn Islands,24,Oceania,Oceania,OCE,26,Oceania,178,OECD,ANZ,,Rest World,ROW,
POL,Poland,12,Eastern Europe,Eastern Europe,CEU,13,EU12,179,OECD,EUR,,European Union,EU,
PRT,Portugal,11,OECD Europe,OECD Europe,WEU,11,EU16,180,OECD,EUR,,European Union,EU,
PRI,Puerto Rico,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,181,LAM,OSA,OAM,Latin America and Caribbean,LAC,
QAT,Qatar,17,Middle East,Middle East,ME,19,Middle East,182,MAF,MEN,AME,Middle East and North Africa,MENA,
COG,Republic of Congo,8,Western Africa,Western Africa,WAF,8,Western Africa,183,MAF,SSA,AME,Sub-Saharan Africa,SSA,
REU,Reunion,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,184,MAF,SSA,AME,Sub-Saharan Africa,SSA,
ROU,Romania,12,Eastern Europe,Eastern Europe,CEU,13,EU12,185,OECD,EUR,,European Union,EU,
RUS,Russia,16,Russia +,Russia +,RUS,18,Russia +,186,REF,FSU,,Rest World,ROW,
RWA,Rwanda,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,187,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SHN,Saint Helena,8,Western Africa,Western Africa,WAF,8,Western Africa,188,MAF,SSA,AME,Sub-Saharan Africa,SSA,
KNA,Saint Kitts and Nevis,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,189,LAM,OSA,OAM,Latin America and Caribbean,LAC,
LCA,Saint Lucia,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,190,LAM,OSA,OAM,Latin America and Caribbean,LAC,
SPM,Saint Pierre and Miquelon,2,USA,USA,USA,2,USA,191,OECD,USA,NAM,Rest World,ROW,
VCT,Saint Vincent and the Grenadines,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,192,LAM,OSA,OAM,Latin America and Caribbean,LAC,
MAF,Saint-Martin,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,194,LAM,OSA,OAM,
WSM,Samoa,24,Oceania,Oceania,OCE,26,Oceania,195,OECD,OAS,,Rest World,ROW,
SMR,San Marino,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,196,OECD,EUR,,Rest World,ROW,
STP,Sao Tome and Principe,8,Western Africa,Western Africa,WAF,8,Western Africa,197,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SAU,Saudi Arabia,17,Middle East,Middle East,ME,19,Middle East,198,MAF,MEN,AME,Middle East and North Africa,MENA,
SEN,Senegal,8,Western Africa,Western Africa,WAF,8,Western Africa,199,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SRB,Serbia,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,200,OECD,EUR,,Rest World,ROW,
SYC,Seychelles,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,201,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SLE,Sierra Leone,8,Western Africa,Western Africa,WAF,8,Western Africa,202,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SGP,Singapore,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,203,ASIA,SEA,SAS,East Asia,EASIA,
SVK,Slovakia,12,Eastern Europe,Eastern Europe,CEU,13,EU12,205,OECD,EUR,,European Union,EU,
SVN,Slovenia,12,Eastern Europe,Eastern Europe,CEU,13,EU12,206,OECD,EUR,,European Union,EU,
SLB,Solomon Islands,24,Oceania,Oceania,OCE,26,Oceania,207,OECD,OAS,,Rest World,ROW,
SOM,Somalia,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,208,MAF,SSA,AME,Sub-Saharan Africa,SSA,
ZAF,South Africa,10,South Africa,Southern Africa,SAF,10,Southern Africa,209,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SGS,South Georgia and the South Sandwich Islands,6,Rest South America,Rest South America,RSAM,6,Rest South America,210,LAM,OSA,OAM,Latin America and Caribbean,LAC,
KOR,South Korea,19,Korea,Korea,KOR,21,Korea,211,ASIA,SEA,SAS,Rest World,ROW,
SSD,South Sudan,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,212,MAF,SSA,AME,Sub-Saharan Africa,SSA,
ESP,Spain,11,OECD Europe,OECD Europe,WEU,11,EU16,213,OECD,EUR,,European Union,EU,
LKA,Sri Lanka,25,Rest South Asia,South-East Asia,RSAS,20,India +,215,ASIA,OAS,SAS,South Asia,SASIA,
SDN,Sudan,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,216,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SUR,Suriname,6,Rest South America,Rest South America,RSAM,6,Rest South America,217,LAM,OSA,OAM,Latin America and Caribbean,LAC,
SJM,Svalbard and Jan Mayen,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,218,OECD,EUR,,Rest World,ROW,
SWZ,Swaziland,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,219,MAF,SSA,AME,Sub-Saharan Africa,SSA,
SWE,Sweden,11,OECD Europe,OECD Europe,WEU,11,EU16,220,OECD,EUR,,European Union,EU,
CHE,Switzerland,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,221,OECD,EUR,,Rest World,ROW,
SYR,Syria,17,Middle East,Middle East,ME,19,Middle East,222,MAF,MEN,AME,Middle East and North Africa,MENA,
TWN,Taiwan,20,China +,China +,CHN,22,China +,223,ASIA,SEA,SAS,Rest World,ROW,
TJK,Tajikistan,15,Asia-Stan,Asia-Stan,STAN,17,Asia-Stan,224,REF,FSU,,Rest World,ROW,
TZA,Tanzania,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,225,MAF,SSA,AME,Sub-Saharan Africa,SSA,
THA,Thailand,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,226,ASIA,SEA,SAS,East Asia,EASIA,
TGO,Togo,8,Western Africa,Western Africa,WAF,8,Western Africa,227,MAF,SSA,AME,Sub-Saharan Africa,SSA,
TKL,Tokelau,24,Oceania,Oceania,OCE,26,Oceania,228,OECD,OAS,,,,
TON,Tonga,24,Oceania,Oceania,OCE,26,Oceania,229,OECD,OAS,,Rest World,ROW,
TTO,Trinidad and Tobago,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,230,LAM,OSA,OAM,Latin America and Caribbean,LAC,
TUN,Tunisia,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,231,MAF,MEN,AME,Middle East and North Africa,MENA,
TUR,Turkey,13,Turkey,Turkey,TUR,15,Turkey,232,OECD,MEN,AME,Rest World,ROW,
TKM,Turkmenistan,15,Asia-Stan,Asia-Stan,STAN,17,Asia-Stan,233,REF,FSU,,Rest World,ROW,
TCA,Turks and Caicos Islands,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,234,LAM,OSA,OAM,Latin America and Caribbean,LAC,
TUV,Tuvalu,24,Oceania,Oceania,OCE,26,Oceania,235,OECD,OAS,,Rest World,ROW,
UGA,Uganda,9,Eastern Africa,Eastern Africa,EAF,9,Eastern Africa,236,MAF,SSA,AME,Sub-Saharan Africa,SSA,
UKR,Ukraine,14,Ukraine +,Ukraine +,UKR,16,Ukraine +,237,REF,FSU,,Rest World,ROW,
ARE,United Arab Emirates,17,Middle East,Middle East,ME,19,Middle East,238,MAF,MEN,AME,Middle East and North Africa,MENA,
GBR,United Kingdom,11,OECD Europe,OECD Europe,WEU,11,EU16,239,OECD,EUR,,European Union,EU,
USA,United States,2,USA,USA,USA,2,USA,240,OECD,USA,NAM,Rest World,ROW,
USA,United States,2,USA,USA,USA,2,USA,240,OECD,USA,NAM,Rest World,ROW,
UMI,United States Minor Outlying Islands,2,USA,USA,USA,2,USA,241,OECD,USA,NAM,
URY,Uruguay,6,Rest South America,Rest South America,RSAM,6,Rest South America,242,LAM,OSA,OAM,Latin America and Caribbean,LAC,
UZB,Uzbekistan,15,Asia-Stan,Asia-Stan,STAN,17,Asia-Stan,243,REF,FSU,,Rest World,ROW,
VUT,Vanuatu,24,Oceania,Oceania,OCE,26,Oceania,244,OECD,OAS,,Rest World,ROW,
VEN,Venezuela,6,Rest South America,Rest South America,RSAM,6,Rest South America,246,LAM,OSA,OAM,Latin America and Caribbean,LAC,
VNM,Vietnam,21,South-East Asia,South-East Asia,SEAS,23,South-East Asia,247,ASIA,SEA,SAS,East Asia,EASIA,
VIR,\"Virgin Islands, U.S.\",4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,248,LAM,OSA,OAM,Latin America and Caribbean,LAC,
WLF,Wallis and Futuna,24,Oceania,Oceania,OCE,26,Oceania,249,OECD,ANZ,,Rest World,ROW,
ESH,Western Sahara,7,Northern Africa,Northern Africa,NAF,7,Northern Africa,250,MAF,MEN,AME,Middle East and North Africa,MENA,
YEM,Yemen,17,Middle East,Middle East,ME,19,Middle East,251,MAF,MEN,AME,Middle East and North Africa,MENA,
ZMB,Zambia,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,252,MAF,SSA,AME,Sub-Saharan Africa,SSA,
ZWE,Zimbabwe,26,Rest Southern Africa,Southern Africa,RSAF,10,Southern Africa,253,MAF,SSA,AME,Sub-Saharan Africa,SSA,
VAT,Holy See (Vatican C,11,OECD Europe,OECD Europe,WEU,12,RWeEurope,NA,OECD,EUR,,Rest World,ROW,
ANT,Netherlands Antille,4,Rest Central America,Rest Central America,RCAM,4,Rest Centr America,NA,LAM,OSA,OAM,Latin America and Caribbean,LAC,
YUG,Yugoslavia,12,Eastern Europe,Eastern Europe,CEU,14,REaEurope,NA,OECD,EUR,
,World,,World,World,World,,,,World,World,,,World,"
  
  
  map_table <- read.delim(textConnection(map_table), header = TRUE, sep = ",")
  
  map_table <- read.csv("/mnt/public/zeistvw/ontwapps/IMAGE/users/zeistvw/R Data Processing Scripts/All regions mapping.csv",sep=",", dec=".")
  
  df_map <- subset(df, Region %in% region_map$Country)
  df <- subset(df, !(Region %in% region_map$Country))
  df_map$Region <- map_table$AgMIP.13[match(df_map$Region, region_map$Country)]
  df <- rbind(df_map, df)
  
  # incase of image regions map those as well
  df_map <- subset(df, Region %in% region_map$IMAGE.region.26)
  df <- subset(df, !(Region %in% region_map$IMAGE.region.26))
  df_map$Region <- map_table$AgMIP.13[match(df_map$Region, region_map$IMAGE.region.26)]
  df <- rbind(df_map, df)
  
  # For mueller o.a. regions map those as well
  df_map <- subset(df, Region %in% region_map$Other)
  df <- subset(df, !(Region %in% region_map$Other))
  df_map$Region <- map_table$AgMIP.13[match(df_map$Region, region_map$Other)]
  df <- rbind(df_map, df)
  
  if (jd) {
    df_map <- subset(df, Region %in% region_map$AgMIP.13)
    df <- subset(df, !(Region %in% region_map$AgMIP.13))
    df_map$Region <- map_table$Doelman2018[match(df_map$Region, region_map$AgMIP.13)]
    df <- rbind(df_map, df)
  }
  
   # for (i in 1:dim(map_table)[1]) {
  #   print(i)
  #   df$Region <- gsub(toString(map_table[i,from]), toString(map_table[i,to]), df$Region)
  # }
  return(df)
}

## mym read functions stolen from David Gernaat:
read.mym2r.dims = function(path.to.mym.file){
  # utility function to extract only the dimensions from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  # extract the dimensions
  dims = numeric(0)
  if (grepl(pattern='[[]+.+[]]+',oneline)){
    dimstring = substr(x=oneline, start=regexpr('[',oneline,fixed=TRUE)[1]+1, stop=regexpr(']',oneline,fixed=TRUE)[1]-1)
    dims      = as.numeric(strsplit(dimstring,',')[[1]])
  }
  return(dims)
}

read.mym2r.varname = function(path.to.mym.file){
  # utility function to extract only the varname from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1)
  while (grepl(pattern='[:blank:]*[!]',oneline))
    oneline = readLines(con, n = 1)  # read next line
  close(con)
  
  varname = NA
  # extract variable name used in MyM
  if (grepl(pattern='\\s.+[[(]',oneline)) 
    varname = substr(x=oneline, start=regexpr('\\s\\S+[[(]',oneline)[1]+1, stop=regexpr('[[(]',oneline)[1]-1)
  varname <- gsub('real ','',varname)
  return(varname)
}

read.mym2r.dimnames = function(path.to.mym.file){
  # utility function to extract only the dimensions from the header in a mym file
  # input:  path to MyM output file
  # output: vector of dimensions as listed in the header / first line of the mym file
  
  # skip any commented lines at the start
  con = file(path.to.mym.file, open = 'r')
  oneline = readLines(con, n = 1) # they should be only on the first line after a comment field
  close(con)
  
  # extract the dimensions
  dims = ""
  
  if(!grepl('!', oneline)) {
    dims = paste('Dim', read.mym2r.dims(path.to.mym.file), sep="")
    return(dims)
  }
  
  if (grepl(pattern='[[]+.+[]]+',oneline)){
    dimstring = substr(x=oneline, start=regexpr('[',oneline,fixed=TRUE)[1]+1, stop=regexpr(']',oneline,fixed=TRUE)[1]-1)
    dims      = as.character(strsplit(dimstring,',')[[1]])
  }
  return(dims)
}

read.mym2r = function(path.to.mym.file, yearheader='year', yearsrun=NULL){
  # input:  path to MyM output file
  # output: R dataframe in 'long' format (many rows, each dimension as a 'factor' in its column, and only one column with actual values)
  
  varname = read.mym2r.varname(path.to.mym.file)
  dims    = read.mym2r.dims(path.to.mym.file)
  dimnames    = read.mym2r.dimnames(path.to.mym.file)
  
  # read all the data
  line      = readLines(path.to.mym.file, warn = FALSE)
  
  # skip any commented lines at the start
  while (1 < length(line)){
    if (grepl(pattern='[:blank:]*[!]',line[1])){
      line = line[2:length(line)]   # this might not be very efficient but it is clear: we discard the first line if it is only a comment
    } else {
      break
    }
  }
  
  # extract whether there is a time dimension or not
  has.time.dim = grepl(pattern='(t)',line[1],fixed=TRUE)
  
  # convert all lines to one long vector of numbers, including years
  string = paste0(line, collapse = ',')
  string = strsplit(string, split='=', fixed=TRUE) # now a list of 1 header and 1 content string
  content = string[[1]][2]
  # remove [ ] ;
  content = gsub('[','',content, fixed=TRUE)  
  content = gsub(']','',content, fixed=TRUE)  
  content = gsub(';','',content, fixed=TRUE)  
  # convert to vector of values
  content = strsplit(content, '[[:space:],]')[[1]]
  content = content[content != '']  # remove empty strings
  content = as.numeric(content)
  
  # create a dataframe that exactly matches the format of the MyM file
     # note: last MyM iterator comes first with rev(dims)
  if (length(dims)>=1) {
    dim.dummies = rev(dimnames)
  }
  
  ret = expand.grid( lapply(rev(dims), FUN=function(x){return(1:x)}) )
  
  if (has.time.dim && length(dims)>=1){
    
    # add one column of numbers for each year
    ncols  = length(content)/(nrow(ret)+1)   # +1 for the year numbers themselves
    m      = matrix(content, nrow=nrow(ret)+1, ncol=ncols)
    years  = m[1,]
    values = m[2:nrow(m),]
    ret    = cbind(ret, values)
    
    colnames(ret) = c(dim.dummies,years)
    
    ret = reshape2::melt(ret, id.vars=dim.dummies, value_name = 'value', variable_name = yearheader)      # not needed if you wanted wide format output, but default is long format
    ret = ret[c((length(dims)+1):1, ncol(ret))]                                                           # rearrange columns to match the order in MyM
    
  } else if (has.time.dim && length(dims)==0){
    
    m   = matrix(content, ncol = 2, byrow = TRUE)
    ret = data.frame(m)
    colnames(ret) = c(yearheader,'value')
    
  } else {  # timeless
    ret$value     = content                                                # join the values from the MyM file to the prepared dataframe
    colnames(ret) = c(dim.dummies,'value')            
    ret           = ret[c(length(dims):1, ncol(ret))]                  # rearrange columns to match the order in MyM
  }
  
  # additional info:
  ret$Varname = varname
  if (has.time.dim) ret[yearheader] = as.numeric(as.character(ret[[yearheader]]))  # make sure years are numeric
  return(ret)
}

# PBL color definitions

hemelblauw3010 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw3011 = rgb(24, 165, 227, maxColorValue = 255)
hemelblauw3012 = rgb(74, 175, 232, maxColorValue = 255)
hemelblauw3013 = rgb(105, 185, 236, maxColorValue = 255)
hemelblauw3014 = rgb(132, 196, 240, maxColorValue = 255)
hemelblauw3015 = rgb(156, 206, 243, maxColorValue = 255)
hemelblauw3016 = rgb(178, 216, 246, maxColorValue = 255)
hemelblauw3017 = rgb(199, 227, 249, maxColorValue = 255)
hemelblauw3018 = rgb(218, 236, 251, maxColorValue = 255)
hemelblauw3019 = rgb(237, 246, 254, maxColorValue = 255)
mosgroen3020 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen3021 = rgb(152, 154, 52, maxColorValue = 255)
mosgroen3022 = rgb(162, 164, 72, maxColorValue = 255)
mosgroen3023 = rgb(173, 175, 92, maxColorValue = 255)
mosgroen3024 = rgb(184, 185, 114, maxColorValue = 255)
mosgroen3025 = rgb(196, 196, 136, maxColorValue = 255)
mosgroen3026 = rgb(207, 207, 159, maxColorValue = 255)
mosgroen3027 = rgb(218, 219, 182, maxColorValue = 255)
mosgroen3028 = rgb(230, 230, 206, maxColorValue = 255)
mosgroen3029 = rgb(243, 243, 230, maxColorValue = 255)
violet3030 = rgb(157, 0, 100, maxColorValue = 255)
violet3031 = rgb(164, 47, 112, maxColorValue = 255)
violet3032 = rgb(172, 73, 126, maxColorValue = 255)
violet3033 = rgb(180, 97, 141, maxColorValue = 255)
violet3034 = rgb(190, 120, 158, maxColorValue = 255)
violet3035 = rgb(199, 144, 175, maxColorValue = 255)
violet3036 = rgb(210, 167, 192, maxColorValue = 255)
violet3037 = rgb(221, 189, 208, maxColorValue = 255)
violet3038 = rgb(232, 211, 224, maxColorValue = 255)
violet3039 = rgb(243, 233, 240, maxColorValue = 255)
donkergeel3040 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel3041 = rgb(233, 181, 58, maxColorValue = 255)
donkergeel3042 = rgb(236, 189, 82, maxColorValue = 255)
donkergeel3043 = rgb(238, 198, 105, maxColorValue = 255)
donkergeel3044 = rgb(241, 206, 128, maxColorValue = 255)
donkergeel3045 = rgb(243, 214, 150, maxColorValue = 255)
donkergeel3046 = rgb(246, 222, 171, maxColorValue = 255)
donkergeel3047 = rgb(248, 230, 193, maxColorValue = 255)
donkergeel3048 = rgb(250, 239, 214, maxColorValue = 255)
donkergeel3049 = rgb(253, 247, 234, maxColorValue = 255)
paars3050 = rgb(61, 29, 92, maxColorValue = 255)
paars3051 = rgb(73, 44, 102, maxColorValue = 255)
paars3052 = rgb(87, 60, 115, maxColorValue = 255)
paars3053 = rgb(102, 78, 129, maxColorValue = 255)
paars3054 = rgb(120, 98, 144, maxColorValue = 255)
paars3055 = rgb(138, 120, 161, maxColorValue = 255)
paars3056 = rgb(158, 145, 180, maxColorValue = 255)
paars3057 = rgb(180, 170, 198, maxColorValue = 255)
paars3058 = rgb(203, 196, 217, maxColorValue = 255)
paars3059 = rgb(228, 225, 236, maxColorValue = 255)
lichtblauw3060 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw3061 = rgb(175, 213, 243, maxColorValue = 255)
lichtblauw3062 = rgb(185, 218, 245, maxColorValue = 255)
lichtblauw3063 = rgb(195, 223, 247, maxColorValue = 255)
lichtblauw3064 = rgb(204, 228, 248, maxColorValue = 255)
lichtblauw3065 = rgb(213, 232, 249, maxColorValue = 255)
lichtblauw3066 = rgb(221, 237, 251, maxColorValue = 255)
lichtblauw3067 = rgb(230, 242, 252, maxColorValue = 255)
lichtblauw3068 = rgb(238, 246, 253, maxColorValue = 255)
lichtblauw3069 = rgb(247, 251, 254, maxColorValue = 255)
roze3070 = rgb(220, 156, 193, maxColorValue = 255)
roze3071 = rgb(224, 167, 201, maxColorValue = 255)
roze3072 = rgb(227, 178, 208, maxColorValue = 255)
roze3073 = rgb(231, 189, 215, maxColorValue = 255)
roze3074 = rgb(234, 199, 221, maxColorValue = 255)
roze3075 = rgb(238, 209, 228, maxColorValue = 255)
roze3076 = rgb(241, 218, 234, maxColorValue = 255)
roze3077 = rgb(245, 228, 240, maxColorValue = 255)
roze3078 = rgb(248, 237, 245, maxColorValue = 255)
roze3079 = rgb(251, 246, 250, maxColorValue = 255)
groen3080 = rgb(110, 169, 64, maxColorValue = 255)
groen3081 = rgb(126, 178, 78, maxColorValue = 255)
groen3082 = rgb(142, 185, 95, maxColorValue = 255)
groen3083 = rgb(158, 194, 114, maxColorValue = 255)
groen3084 = rgb(173, 203, 135, maxColorValue = 255)
groen3085 = rgb(188, 212, 155, maxColorValue = 255)
groen3086 = rgb(202, 221, 176, maxColorValue = 255)
groen3087 = rgb(216, 230, 197, maxColorValue = 255)
groen3088 = rgb(229, 238, 217, maxColorValue = 255)
groen3089 = rgb(242, 247, 236, maxColorValue = 255)
rood3090 = rgb(195, 33, 23, maxColorValue = 255)
rood3091 = rgb(200, 62, 40, maxColorValue = 255)
rood3092 = rgb(205, 87, 58, maxColorValue = 255)
rood3093 = rgb(210, 111, 78, maxColorValue = 255)
rood3094 = rgb(216, 134, 101, maxColorValue = 255)
rood3095 = rgb(223, 155, 124, maxColorValue = 255)
rood3096 = rgb(229, 177, 150, maxColorValue = 255)
rood3097 = rgb(236, 197, 176, maxColorValue = 255)
rood3098 = rgb(242, 217, 202, maxColorValue = 255)
rood3099 = rgb(249, 236, 228, maxColorValue = 255)
donkergroen3100 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen3101 = rgb(43, 78, 47, maxColorValue = 255)
donkergroen3102 = rgb(61, 92, 60, maxColorValue = 255)
donkergroen3103 = rgb(79, 107, 77, maxColorValue = 255)
donkergroen3104 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen3105 = rgb(121, 142, 116, maxColorValue = 255)
donkergroen3106 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen3107 = rgb(171, 183, 166, maxColorValue = 255)
donkergroen3108 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen3109 = rgb(225, 229, 223, maxColorValue = 255)
oranje3110 = rgb(209, 106, 25, maxColorValue = 255)
oranje3111 = rgb(213, 122, 48, maxColorValue = 255)
oranje3112 = rgb(218, 137, 68, maxColorValue = 255)
oranje3113 = rgb(223, 153, 90, maxColorValue = 255)
oranje3114 = rgb(227, 169, 113, maxColorValue = 255)
oranje3115 = rgb(232, 184, 136, maxColorValue = 255)
oranje3116 = rgb(237, 199, 160, maxColorValue = 255)
oranje3117 = rgb(242, 213, 183, maxColorValue = 255)
oranje3118 = rgb(246, 228, 207, maxColorValue = 255)
oranje3119 = rgb(250, 241, 231, maxColorValue = 255)
donkerbruin3120 = rgb(76, 37, 21, maxColorValue = 255)
donkerbruin3121 = rgb(90, 51, 36, maxColorValue = 255)
donkerbruin3122 = rgb(105, 66, 51, maxColorValue = 255)
donkerbruin3123 = rgb(120, 83, 67, maxColorValue = 255)
donkerbruin3124 = rgb(136, 102, 87, maxColorValue = 255)
donkerbruin3125 = rgb(154, 124, 109, maxColorValue = 255)
donkerbruin3126 = rgb(172, 147, 134, maxColorValue = 255)
donkerbruin3127 = rgb(192, 172, 161, maxColorValue = 255)
donkerbruin3128 = rgb(212, 198, 189, maxColorValue = 255)
donkerbruin3129 = rgb(233, 225, 221, maxColorValue = 255)
robijnrood3130 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood3131 = rgb(195, 51, 118, maxColorValue = 255)
robijnrood3132 = rgb(200, 80, 132, maxColorValue = 255)
robijnrood3133 = rgb(206, 107, 148, maxColorValue = 255)
robijnrood3134 = rgb(212, 131, 164, maxColorValue = 255)
robijnrood3135 = rgb(219, 154, 180, maxColorValue = 255)
robijnrood3136 = rgb(226, 176, 196, maxColorValue = 255)
robijnrood3137 = rgb(233, 197, 212, maxColorValue = 255)
robijnrood3138 = rgb(241, 217, 227, maxColorValue = 255)
robijnrood3139 = rgb(248, 236, 241, maxColorValue = 255)
bruin3140 = rgb(178, 142, 23, maxColorValue = 255)
bruin3141 = rgb(186, 152, 48, maxColorValue = 255)
bruin3142 = rgb(194, 162, 70, maxColorValue = 255)
bruin3143 = rgb(201, 172, 91, maxColorValue = 255)
bruin3144 = rgb(209, 183, 113, maxColorValue = 255)
bruin3145 = rgb(216, 194, 136, maxColorValue = 255)
bruin3146 = rgb(224, 205, 159, maxColorValue = 255)
bruin3147 = rgb(231, 217, 182, maxColorValue = 255)
bruin3148 = rgb(239, 230, 206, maxColorValue = 255)
bruin3149 = rgb(247, 242, 230, maxColorValue = 255)
mintgroen3150 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen3151 = rgb(167, 207, 198, maxColorValue = 255)
mintgroen3152 = rgb(178, 212, 204, maxColorValue = 255)
mintgroen3153 = rgb(188, 218, 211, maxColorValue = 255)
mintgroen3154 = rgb(199, 223, 217, maxColorValue = 255)
mintgroen3155 = rgb(209, 229, 224, maxColorValue = 255)
mintgroen3156 = rgb(218, 234, 230, maxColorValue = 255)
mintgroen3157 = rgb(228, 239, 237, maxColorValue = 255)
mintgroen3158 = rgb(237, 245, 243, maxColorValue = 255)
mintgroen3159 = rgb(246, 250, 249, maxColorValue = 255)
geel3160 = rgb(255, 238, 54, maxColorValue = 255)
geel3161 = rgb(255, 239, 81, maxColorValue = 255)
geel3162 = rgb(255, 241, 105, maxColorValue = 255)
geel3163 = rgb(255, 243, 128, maxColorValue = 255)
geel3164 = rgb(255, 244, 149, maxColorValue = 255)
geel3165 = rgb(255, 246, 169, maxColorValue = 255)
geel3166 = rgb(255, 248, 188, maxColorValue = 255)
geel3167 = rgb(255, 250, 206, maxColorValue = 255)
geel3168 = rgb(255, 251, 223, maxColorValue = 255)
geel3169 = rgb(255, 253, 239, maxColorValue = 255)
donkerblauw3170 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw3171 = rgb(56, 115, 184, maxColorValue = 255)
donkerblauw3172 = rgb(80, 127, 192, maxColorValue = 255)
donkerblauw3173 = rgb(103, 141, 200, maxColorValue = 255)
donkerblauw3174 = rgb(127, 156, 208, maxColorValue = 255)
donkerblauw3175 = rgb(149, 171, 217, maxColorValue = 255)
donkerblauw3176 = rgb(171, 187, 225, maxColorValue = 255)
donkerblauw3177 = rgb(192, 203, 233, maxColorValue = 255)
donkerblauw3178 = rgb(213, 220, 240, maxColorValue = 255)
donkerblauw3179 = rgb(234, 237, 248, maxColorValue = 255)
roodgeelgroen3200 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen3201 = rgb(203, 86, 29, maxColorValue = 255)
roodgeelgroen3202 = rgb(215, 129, 36, maxColorValue = 255)
roodgeelgroen3203 = rgb(228, 168, 43, maxColorValue = 255)
roodgeelgroen3204 = rgb(242, 205, 50, maxColorValue = 255)
roodgeelgroen3205 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen3206 = rgb(223, 221, 56, maxColorValue = 255)
roodgeelgroen3207 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen3208 = rgb(151, 187, 61, maxColorValue = 255)
roodgeelgroen3209 = rgb(110, 169, 64, maxColorValue = 255)
roodhemelblauw3300 = rgb(195, 33, 23, maxColorValue = 255)
roodhemelblauw3301 = rgb(205, 87, 58, maxColorValue = 255)
roodhemelblauw3302 = rgb(216, 134, 101, maxColorValue = 255)
roodhemelblauw3303 = rgb(229, 177, 150, maxColorValue = 255)
roodhemelblauw3304 = rgb(242, 217, 202, maxColorValue = 255)
roodhemelblauw3305 = rgb(218, 236, 251, maxColorValue = 255)
roodhemelblauw3306 = rgb(178, 216, 246, maxColorValue = 255)
roodhemelblauw3307 = rgb(132, 196, 240, maxColorValue = 255)
roodhemelblauw3308 = rgb(74, 175, 232, maxColorValue = 255)
roodhemelblauw3309 = rgb(0, 156, 223, maxColorValue = 255)
zwart3400 = rgb(0, 0, 0, maxColorValue = 255)
zwart3401 = rgb(38, 38, 38, maxColorValue = 255)
zwart3402 = rgb(77, 77, 77, maxColorValue = 255)
zwart3403 = rgb(102, 102, 102, maxColorValue = 255)
zwart3404 = rgb(128, 128, 128, maxColorValue = 255)
zwart3405 = rgb(153, 153, 153, maxColorValue = 255)
zwart3406 = rgb(191, 191, 191, maxColorValue = 255)
zwart3407 = rgb(204, 204, 204, maxColorValue = 255)
zwart3408 = rgb(230, 230, 230, maxColorValue = 255)
zwart3409 = rgb(242, 242, 242, maxColorValue = 255)
wit = rgb(255, 255, 255, maxColorValue = 255)
hemelblauwdonker3610 = rgb(0, 110, 154, maxColorValue = 255)
hemelblauwdonker3611 = rgb(0, 135, 190, maxColorValue = 255)
mosgroendonker3620 = rgb(100, 104, 22, maxColorValue = 255)
mosgroendonker3621 = rgb(121, 125, 28, maxColorValue = 255)
violetdonker3630 = rgb(111, 0, 70, maxColorValue = 255)
violetdonker3631 = rgb(134, 0, 85, maxColorValue = 255)
geeldonker3640 = rgb(202, 133, 24, maxColorValue = 255)
geeldonker3641 = rgb(215, 152, 24, maxColorValue = 255)
groendonker3680 = rgb(33, 128, 62, maxColorValue = 255)
groendonker3681 = rgb(69, 146, 64, maxColorValue = 255)
rooddonker3690 = rgb(134, 30, 4, maxColorValue = 255)
rooddonker3691 = rgb(165, 33, 18, maxColorValue = 255)
hemelblauw10_1 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw10_2 = rgb(24, 165, 227, maxColorValue = 255)
hemelblauw10_3 = rgb(74, 175, 232, maxColorValue = 255)
hemelblauw10_4 = rgb(105, 185, 236, maxColorValue = 255)
hemelblauw10_5 = rgb(132, 196, 240, maxColorValue = 255)
hemelblauw10_6 = rgb(156, 206, 243, maxColorValue = 255)
hemelblauw10_7 = rgb(178, 216, 246, maxColorValue = 255)
hemelblauw10_8 = rgb(199, 227, 249, maxColorValue = 255)
hemelblauw10_9 = rgb(218, 236, 251, maxColorValue = 255)
hemelblauw10_10 = rgb(237, 246, 254, maxColorValue = 255)
hemelblauw5_1 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw5_2 = rgb(74, 175, 232, maxColorValue = 255)
hemelblauw5_3 = rgb(132, 196, 240, maxColorValue = 255)
hemelblauw5_4 = rgb(178, 216, 246, maxColorValue = 255)
hemelblauw5_5 = rgb(218, 236, 251, maxColorValue = 255)
hemelblauw4_1 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw4_2 = rgb(105, 185, 236, maxColorValue = 255)
hemelblauw4_3 = rgb(178, 216, 246, maxColorValue = 255)
hemelblauw4_4 = rgb(218, 236, 251, maxColorValue = 255)
hemelblauw3_1 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw3_2 = rgb(132, 196, 240, maxColorValue = 255)
hemelblauw3_3 = rgb(218, 236, 251, maxColorValue = 255)
hemelblauw2_1 = rgb(0, 156, 223, maxColorValue = 255)
hemelblauw2_2 = rgb(156, 206, 243, maxColorValue = 255)
hemelblauw1_1 = rgb(0, 156, 223, maxColorValue = 255)
mosgroen10_1 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen10_2 = rgb(152, 154, 52, maxColorValue = 255)
mosgroen10_3 = rgb(162, 164, 72, maxColorValue = 255)
mosgroen10_4 = rgb(173, 175, 92, maxColorValue = 255)
mosgroen10_5 = rgb(184, 185, 114, maxColorValue = 255)
mosgroen10_6 = rgb(196, 196, 136, maxColorValue = 255)
mosgroen10_7 = rgb(207, 207, 159, maxColorValue = 255)
mosgroen10_8 = rgb(218, 219, 182, maxColorValue = 255)
mosgroen10_9 = rgb(230, 230, 206, maxColorValue = 255)
mosgroen10_10 = rgb(243, 243, 230, maxColorValue = 255)
mosgroen5_1 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen5_2 = rgb(162, 164, 72, maxColorValue = 255)
mosgroen5_3 = rgb(184, 185, 114, maxColorValue = 255)
mosgroen5_4 = rgb(207, 207, 159, maxColorValue = 255)
mosgroen5_5 = rgb(230, 230, 206, maxColorValue = 255)
mosgroen4_1 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen4_2 = rgb(173, 175, 92, maxColorValue = 255)
mosgroen4_3 = rgb(207, 207, 159, maxColorValue = 255)
mosgroen4_4 = rgb(230, 230, 206, maxColorValue = 255)
mosgroen3_1 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen3_2 = rgb(184, 185, 114, maxColorValue = 255)
mosgroen3_3 = rgb(230, 230, 206, maxColorValue = 255)
mosgroen2_1 = rgb(141, 145, 31, maxColorValue = 255)
mosgroen2_2 = rgb(196, 196, 136, maxColorValue = 255)
mosgroen1_1 = rgb(141, 145, 31, maxColorValue = 255)
violet10_1 = rgb(157, 0, 100, maxColorValue = 255)
violet10_2 = rgb(164, 47, 112, maxColorValue = 255)
violet10_3 = rgb(172, 73, 126, maxColorValue = 255)
violet10_4 = rgb(180, 97, 141, maxColorValue = 255)
violet10_5 = rgb(190, 120, 158, maxColorValue = 255)
violet10_6 = rgb(199, 144, 175, maxColorValue = 255)
violet10_7 = rgb(210, 167, 192, maxColorValue = 255)
violet10_8 = rgb(221, 189, 208, maxColorValue = 255)
violet10_9 = rgb(232, 211, 224, maxColorValue = 255)
violet10_10 = rgb(243, 233, 240, maxColorValue = 255)
violet5_1 = rgb(157, 0, 100, maxColorValue = 255)
violet5_2 = rgb(172, 73, 126, maxColorValue = 255)
violet5_3 = rgb(190, 120, 158, maxColorValue = 255)
violet5_4 = rgb(210, 167, 192, maxColorValue = 255)
violet5_5 = rgb(232, 211, 224, maxColorValue = 255)
violet4_1 = rgb(157, 0, 100, maxColorValue = 255)
violet4_2 = rgb(180, 97, 141, maxColorValue = 255)
violet4_3 = rgb(210, 167, 192, maxColorValue = 255)
violet4_4 = rgb(232, 211, 224, maxColorValue = 255)
violet3_1 = rgb(157, 0, 100, maxColorValue = 255)
violet3_2 = rgb(190, 120, 158, maxColorValue = 255)
violet3_3 = rgb(232, 211, 224, maxColorValue = 255)
violet2_1 = rgb(157, 0, 100, maxColorValue = 255)
violet2_2 = rgb(199, 144, 175, maxColorValue = 255)
violet1_1 = rgb(157, 0, 100, maxColorValue = 255)
donkergeel10_1 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel10_2 = rgb(233, 181, 58, maxColorValue = 255)
donkergeel10_3 = rgb(236, 189, 82, maxColorValue = 255)
donkergeel10_4 = rgb(238, 198, 105, maxColorValue = 255)
donkergeel10_5 = rgb(241, 206, 128, maxColorValue = 255)
donkergeel10_6 = rgb(243, 214, 150, maxColorValue = 255)
donkergeel10_7 = rgb(246, 222, 171, maxColorValue = 255)
donkergeel10_8 = rgb(248, 230, 193, maxColorValue = 255)
donkergeel10_9 = rgb(250, 239, 214, maxColorValue = 255)
donkergeel10_10 = rgb(253, 247, 234, maxColorValue = 255)
donkergeel5_1 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel5_2 = rgb(236, 189, 82, maxColorValue = 255)
donkergeel5_3 = rgb(241, 206, 128, maxColorValue = 255)
donkergeel5_4 = rgb(246, 222, 171, maxColorValue = 255)
donkergeel5_5 = rgb(250, 239, 214, maxColorValue = 255)
donkergeel4_1 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel4_2 = rgb(238, 198, 105, maxColorValue = 255)
donkergeel4_3 = rgb(246, 222, 171, maxColorValue = 255)
donkergeel4_4 = rgb(250, 239, 214, maxColorValue = 255)
donkergeel3_1 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel3_2 = rgb(241, 206, 128, maxColorValue = 255)
donkergeel3_3 = rgb(250, 239, 214, maxColorValue = 255)
donkergeel2_1 = rgb(230, 173, 31, maxColorValue = 255)
donkergeel2_2 = rgb(243, 214, 150, maxColorValue = 255)
donkergeel1_1 = rgb(230, 173, 31, maxColorValue = 255)
paars10_1 = rgb(61, 29, 92, maxColorValue = 255)
paars10_2 = rgb(73, 44, 102, maxColorValue = 255)
paars10_3 = rgb(87, 60, 115, maxColorValue = 255)
paars10_4 = rgb(102, 78, 129, maxColorValue = 255)
paars10_5 = rgb(120, 98, 144, maxColorValue = 255)
paars10_6 = rgb(138, 120, 161, maxColorValue = 255)
paars10_7 = rgb(158, 145, 180, maxColorValue = 255)
paars10_8 = rgb(180, 170, 198, maxColorValue = 255)
paars10_9 = rgb(203, 196, 217, maxColorValue = 255)
paars10_10 = rgb(228, 225, 236, maxColorValue = 255)
paars5_1 = rgb(61, 29, 92, maxColorValue = 255)
paars5_2 = rgb(87, 60, 115, maxColorValue = 255)
paars5_3 = rgb(120, 98, 144, maxColorValue = 255)
paars5_4 = rgb(158, 145, 180, maxColorValue = 255)
paars5_5 = rgb(203, 196, 217, maxColorValue = 255)
paars4_1 = rgb(61, 29, 92, maxColorValue = 255)
paars4_2 = rgb(102, 78, 129, maxColorValue = 255)
paars4_3 = rgb(158, 145, 180, maxColorValue = 255)
paars4_4 = rgb(203, 196, 217, maxColorValue = 255)
paars3_1 = rgb(61, 29, 92, maxColorValue = 255)
paars3_2 = rgb(120, 98, 144, maxColorValue = 255)
paars3_3 = rgb(203, 196, 217, maxColorValue = 255)
paars2_1 = rgb(61, 29, 92, maxColorValue = 255)
paars2_2 = rgb(138, 120, 161, maxColorValue = 255)
paars1_1 = rgb(61, 29, 92, maxColorValue = 255)
lichtblauw10_1 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw10_2 = rgb(175, 213, 243, maxColorValue = 255)
lichtblauw10_3 = rgb(185, 218, 245, maxColorValue = 255)
lichtblauw10_4 = rgb(195, 223, 247, maxColorValue = 255)
lichtblauw10_5 = rgb(204, 228, 248, maxColorValue = 255)
lichtblauw10_6 = rgb(213, 232, 249, maxColorValue = 255)
lichtblauw10_7 = rgb(221, 237, 251, maxColorValue = 255)
lichtblauw10_8 = rgb(230, 242, 252, maxColorValue = 255)
lichtblauw10_9 = rgb(238, 246, 253, maxColorValue = 255)
lichtblauw10_10 = rgb(247, 251, 254, maxColorValue = 255)
lichtblauw5_1 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw5_2 = rgb(185, 218, 245, maxColorValue = 255)
lichtblauw5_3 = rgb(204, 228, 248, maxColorValue = 255)
lichtblauw5_4 = rgb(221, 237, 251, maxColorValue = 255)
lichtblauw5_5 = rgb(238, 246, 253, maxColorValue = 255)
lichtblauw4_1 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw4_2 = rgb(195, 223, 247, maxColorValue = 255)
lichtblauw4_3 = rgb(221, 237, 251, maxColorValue = 255)
lichtblauw4_4 = rgb(238, 246, 253, maxColorValue = 255)
lichtblauw3_1 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw3_2 = rgb(204, 228, 248, maxColorValue = 255)
lichtblauw3_3 = rgb(238, 246, 253, maxColorValue = 255)
lichtblauw2_1 = rgb(165, 209, 242, maxColorValue = 255)
lichtblauw2_2 = rgb(213, 232, 249, maxColorValue = 255)
lichtblauw1_1 = rgb(165, 209, 242, maxColorValue = 255)
roze10_1 = rgb(220, 156, 193, maxColorValue = 255)
roze10_2 = rgb(224, 167, 201, maxColorValue = 255)
roze10_3 = rgb(227, 178, 208, maxColorValue = 255)
roze10_4 = rgb(231, 189, 215, maxColorValue = 255)
roze10_5 = rgb(234, 199, 221, maxColorValue = 255)
roze10_6 = rgb(238, 209, 228, maxColorValue = 255)
roze10_7 = rgb(241, 218, 234, maxColorValue = 255)
roze10_8 = rgb(245, 228, 240, maxColorValue = 255)
roze10_9 = rgb(248, 237, 245, maxColorValue = 255)
roze10_10 = rgb(251, 246, 250, maxColorValue = 255)
roze5_1 = rgb(220, 156, 193, maxColorValue = 255)
roze5_2 = rgb(227, 178, 208, maxColorValue = 255)
roze5_3 = rgb(234, 199, 221, maxColorValue = 255)
roze5_4 = rgb(241, 218, 234, maxColorValue = 255)
roze5_5 = rgb(248, 237, 245, maxColorValue = 255)
roze4_1 = rgb(220, 156, 193, maxColorValue = 255)
roze4_2 = rgb(231, 189, 215, maxColorValue = 255)
roze4_3 = rgb(241, 218, 234, maxColorValue = 255)
roze4_4 = rgb(248, 237, 245, maxColorValue = 255)
roze3_1 = rgb(220, 156, 193, maxColorValue = 255)
roze3_2 = rgb(234, 199, 221, maxColorValue = 255)
roze3_3 = rgb(248, 237, 245, maxColorValue = 255)
roze2_1 = rgb(220, 156, 193, maxColorValue = 255)
roze2_2 = rgb(238, 209, 228, maxColorValue = 255)
roze1_1 = rgb(220, 156, 193, maxColorValue = 255)
groen10_1 = rgb(110, 169, 64, maxColorValue = 255)
groen10_2 = rgb(126, 178, 78, maxColorValue = 255)
groen10_3 = rgb(142, 185, 95, maxColorValue = 255)
groen10_4 = rgb(158, 194, 114, maxColorValue = 255)
groen10_5 = rgb(173, 203, 135, maxColorValue = 255)
groen10_6 = rgb(188, 212, 155, maxColorValue = 255)
groen10_7 = rgb(202, 221, 176, maxColorValue = 255)
groen10_8 = rgb(216, 230, 197, maxColorValue = 255)
groen10_9 = rgb(229, 238, 217, maxColorValue = 255)
groen10_10 = rgb(242, 247, 236, maxColorValue = 255)
groen5_1 = rgb(110, 169, 64, maxColorValue = 255)
groen5_2 = rgb(142, 185, 95, maxColorValue = 255)
groen5_3 = rgb(173, 203, 135, maxColorValue = 255)
groen5_4 = rgb(202, 221, 176, maxColorValue = 255)
groen5_5 = rgb(229, 238, 217, maxColorValue = 255)
groen4_1 = rgb(110, 169, 64, maxColorValue = 255)
groen4_2 = rgb(158, 194, 114, maxColorValue = 255)
groen4_3 = rgb(202, 221, 176, maxColorValue = 255)
groen4_4 = rgb(229, 238, 217, maxColorValue = 255)
groen3_1 = rgb(110, 169, 64, maxColorValue = 255)
groen3_2 = rgb(173, 203, 135, maxColorValue = 255)
groen3_3 = rgb(229, 238, 217, maxColorValue = 255)
groen2_1 = rgb(110, 169, 64, maxColorValue = 255)
groen2_2 = rgb(188, 212, 155, maxColorValue = 255)
groen1_1 = rgb(110, 169, 64, maxColorValue = 255)
rood10_1 = rgb(195, 33, 23, maxColorValue = 255)
rood10_2 = rgb(200, 62, 40, maxColorValue = 255)
rood10_3 = rgb(205, 87, 58, maxColorValue = 255)
rood10_4 = rgb(210, 111, 78, maxColorValue = 255)
rood10_5 = rgb(216, 134, 101, maxColorValue = 255)
rood10_6 = rgb(223, 155, 124, maxColorValue = 255)
rood10_7 = rgb(229, 177, 150, maxColorValue = 255)
rood10_8 = rgb(236, 197, 176, maxColorValue = 255)
rood10_9 = rgb(242, 217, 202, maxColorValue = 255)
rood10_10 = rgb(249, 236, 228, maxColorValue = 255)
rood5_1 = rgb(195, 33, 23, maxColorValue = 255)
rood5_2 = rgb(205, 87, 58, maxColorValue = 255)
rood5_3 = rgb(216, 134, 101, maxColorValue = 255)
rood5_4 = rgb(229, 177, 150, maxColorValue = 255)
rood5_5 = rgb(242, 217, 202, maxColorValue = 255)
rood4_1 = rgb(195, 33, 23, maxColorValue = 255)
rood4_2 = rgb(210, 111, 78, maxColorValue = 255)
rood4_3 = rgb(229, 177, 150, maxColorValue = 255)
rood4_4 = rgb(242, 217, 202, maxColorValue = 255)
rood3_1 = rgb(195, 33, 23, maxColorValue = 255)
rood3_2 = rgb(216, 134, 101, maxColorValue = 255)
rood3_3 = rgb(242, 217, 202, maxColorValue = 255)
rood2_1 = rgb(195, 33, 23, maxColorValue = 255)
rood2_2 = rgb(223, 155, 124, maxColorValue = 255)
rood1_1 = rgb(195, 33, 23, maxColorValue = 255)
donkergroen10_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen10_2 = rgb(43, 78, 47, maxColorValue = 255)
donkergroen10_3 = rgb(61, 92, 60, maxColorValue = 255)
donkergroen10_4 = rgb(79, 107, 77, maxColorValue = 255)
donkergroen10_5 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen10_6 = rgb(121, 142, 116, maxColorValue = 255)
donkergroen10_7 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen10_8 = rgb(171, 183, 166, maxColorValue = 255)
donkergroen10_9 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen10_10 = rgb(225, 229, 223, maxColorValue = 255)
donkergroen5_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen5_2 = rgb(61, 92, 60, maxColorValue = 255)
donkergroen5_3 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen5_4 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen5_5 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen4_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen4_2 = rgb(79, 107, 77, maxColorValue = 255)
donkergroen4_3 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen4_4 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen3_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen3_2 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen3_3 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen2_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen2_2 = rgb(121, 142, 116, maxColorValue = 255)
donkergroen1_1 = rgb(26, 66, 34, maxColorValue = 255)
oranje10_1 = rgb(209, 106, 25, maxColorValue = 255)
oranje10_2 = rgb(213, 122, 48, maxColorValue = 255)
oranje10_3 = rgb(218, 137, 68, maxColorValue = 255)
oranje10_4 = rgb(223, 153, 90, maxColorValue = 255)
oranje10_5 = rgb(227, 169, 113, maxColorValue = 255)
oranje10_6 = rgb(232, 184, 136, maxColorValue = 255)
oranje10_7 = rgb(237, 199, 160, maxColorValue = 255)
oranje10_8 = rgb(242, 213, 183, maxColorValue = 255)
oranje10_9 = rgb(246, 228, 207, maxColorValue = 255)
oranje10_10 = rgb(250, 241, 231, maxColorValue = 255)
oranje5_1 = rgb(209, 106, 25, maxColorValue = 255)
oranje5_2 = rgb(218, 137, 68, maxColorValue = 255)
oranje5_3 = rgb(227, 169, 113, maxColorValue = 255)
oranje5_4 = rgb(237, 199, 160, maxColorValue = 255)
oranje5_5 = rgb(246, 228, 207, maxColorValue = 255)
oranje4_1 = rgb(209, 106, 25, maxColorValue = 255)
oranje4_2 = rgb(223, 153, 90, maxColorValue = 255)
oranje4_3 = rgb(237, 199, 160, maxColorValue = 255)
oranje4_4 = rgb(246, 228, 207, maxColorValue = 255)
oranje3_1 = rgb(209, 106, 25, maxColorValue = 255)
oranje3_2 = rgb(227, 169, 113, maxColorValue = 255)
oranje3_3 = rgb(246, 228, 207, maxColorValue = 255)
oranje2_1 = rgb(209, 106, 25, maxColorValue = 255)
oranje2_2 = rgb(232, 184, 136, maxColorValue = 255)
oranje1_1 = rgb(209, 106, 25, maxColorValue = 255)
donkergroen10_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen10_2 = rgb(43, 78, 47, maxColorValue = 255)
donkergroen10_3 = rgb(61, 92, 60, maxColorValue = 255)
donkergroen10_4 = rgb(79, 107, 77, maxColorValue = 255)
donkergroen10_5 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen10_6 = rgb(121, 142, 116, maxColorValue = 255)
donkergroen10_7 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen10_8 = rgb(171, 183, 166, maxColorValue = 255)
donkergroen10_9 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen10_10 = rgb(225, 229, 223, maxColorValue = 255)
donkergroen5_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen5_2 = rgb(61, 92, 60, maxColorValue = 255)
donkergroen5_3 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen5_4 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen5_5 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen4_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen4_2 = rgb(79, 107, 77, maxColorValue = 255)
donkergroen4_3 = rgb(145, 162, 140, maxColorValue = 255)
donkergroen4_4 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen3_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen3_2 = rgb(99, 124, 95, maxColorValue = 255)
donkergroen3_3 = rgb(197, 205, 193, maxColorValue = 255)
donkergroen2_1 = rgb(26, 66, 34, maxColorValue = 255)
donkergroen2_2 = rgb(121, 142, 116, maxColorValue = 255)
donkergroen1_1 = rgb(26, 66, 34, maxColorValue = 255)
robijnrood10_1 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood10_2 = rgb(195, 51, 118, maxColorValue = 255)
robijnrood10_3 = rgb(200, 80, 132, maxColorValue = 255)
robijnrood10_4 = rgb(206, 107, 148, maxColorValue = 255)
robijnrood10_5 = rgb(212, 131, 164, maxColorValue = 255)
robijnrood10_6 = rgb(219, 154, 180, maxColorValue = 255)
robijnrood10_7 = rgb(226, 176, 196, maxColorValue = 255)
robijnrood10_8 = rgb(233, 197, 212, maxColorValue = 255)
robijnrood10_9 = rgb(241, 217, 227, maxColorValue = 255)
robijnrood10_10 = rgb(248, 236, 241, maxColorValue = 255)
robijnrood5_1 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood5_2 = rgb(200, 80, 132, maxColorValue = 255)
robijnrood5_3 = rgb(212, 131, 164, maxColorValue = 255)
robijnrood5_4 = rgb(226, 176, 196, maxColorValue = 255)
robijnrood5_5 = rgb(241, 217, 227, maxColorValue = 255)
robijnrood4_1 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood4_2 = rgb(206, 107, 148, maxColorValue = 255)
robijnrood4_3 = rgb(226, 176, 196, maxColorValue = 255)
robijnrood4_4 = rgb(241, 217, 227, maxColorValue = 255)
robijnrood3_1 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood3_2 = rgb(212, 131, 164, maxColorValue = 255)
robijnrood3_3 = rgb(241, 217, 227, maxColorValue = 255)
robijnrood2_1 = rgb(191, 0, 105, maxColorValue = 255)
robijnrood2_2 = rgb(219, 154, 180, maxColorValue = 255)
robijnrood1_1 = rgb(191, 0, 105, maxColorValue = 255)
bruin10_1 = rgb(178, 142, 23, maxColorValue = 255)
bruin10_2 = rgb(186, 152, 48, maxColorValue = 255)
bruin10_3 = rgb(194, 162, 70, maxColorValue = 255)
bruin10_4 = rgb(201, 172, 91, maxColorValue = 255)
bruin10_5 = rgb(209, 183, 113, maxColorValue = 255)
bruin10_6 = rgb(216, 194, 136, maxColorValue = 255)
bruin10_7 = rgb(224, 205, 159, maxColorValue = 255)
bruin10_8 = rgb(231, 217, 182, maxColorValue = 255)
bruin10_9 = rgb(239, 230, 206, maxColorValue = 255)
bruin10_10 = rgb(247, 242, 230, maxColorValue = 255)
bruin5_1 = rgb(178, 142, 23, maxColorValue = 255)
bruin5_2 = rgb(194, 162, 70, maxColorValue = 255)
bruin5_3 = rgb(209, 183, 113, maxColorValue = 255)
bruin5_4 = rgb(224, 205, 159, maxColorValue = 255)
bruin5_5 = rgb(239, 230, 206, maxColorValue = 255)
bruin4_1 = rgb(178, 142, 23, maxColorValue = 255)
bruin4_2 = rgb(201, 172, 91, maxColorValue = 255)
bruin4_3 = rgb(224, 205, 159, maxColorValue = 255)
bruin4_4 = rgb(239, 230, 206, maxColorValue = 255)
bruin3_1 = rgb(178, 142, 23, maxColorValue = 255)
bruin3_2 = rgb(209, 183, 113, maxColorValue = 255)
bruin3_3 = rgb(239, 230, 206, maxColorValue = 255)
bruin2_1 = rgb(178, 142, 23, maxColorValue = 255)
bruin2_2 = rgb(216, 194, 136, maxColorValue = 255)
bruin1_1 = rgb(178, 142, 23, maxColorValue = 255)
mintgroen10_1 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen10_2 = rgb(167, 207, 198, maxColorValue = 255)
mintgroen10_3 = rgb(178, 212, 204, maxColorValue = 255)
mintgroen10_4 = rgb(188, 218, 211, maxColorValue = 255)
mintgroen10_5 = rgb(199, 223, 217, maxColorValue = 255)
mintgroen10_6 = rgb(209, 229, 224, maxColorValue = 255)
mintgroen10_7 = rgb(218, 234, 230, maxColorValue = 255)
mintgroen10_8 = rgb(228, 239, 237, maxColorValue = 255)
mintgroen10_9 = rgb(237, 245, 243, maxColorValue = 255)
mintgroen10_10 = rgb(246, 250, 249, maxColorValue = 255)
mintgroen5_1 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen5_2 = rgb(178, 212, 204, maxColorValue = 255)
mintgroen5_3 = rgb(199, 223, 217, maxColorValue = 255)
mintgroen5_4 = rgb(218, 234, 230, maxColorValue = 255)
mintgroen5_5 = rgb(237, 245, 243, maxColorValue = 255)
mintgroen4_1 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen4_2 = rgb(188, 218, 211, maxColorValue = 255)
mintgroen4_3 = rgb(218, 234, 230, maxColorValue = 255)
mintgroen4_4 = rgb(237, 245, 243, maxColorValue = 255)
mintgroen3_1 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen3_2 = rgb(199, 223, 217, maxColorValue = 255)
mintgroen3_3 = rgb(237, 245, 243, maxColorValue = 255)
mintgroen2_1 = rgb(156, 201, 191, maxColorValue = 255)
mintgroen2_2 = rgb(209, 229, 224, maxColorValue = 255)
mintgroen1_1 = rgb(156, 201, 191, maxColorValue = 255)
geel10_1 = rgb(255, 238, 54, maxColorValue = 255)
geel10_2 = rgb(255, 239, 81, maxColorValue = 255)
geel10_3 = rgb(255, 241, 105, maxColorValue = 255)
geel10_4 = rgb(255, 243, 128, maxColorValue = 255)
geel10_5 = rgb(255, 244, 149, maxColorValue = 255)
geel10_6 = rgb(255, 246, 169, maxColorValue = 255)
geel10_7 = rgb(255, 248, 188, maxColorValue = 255)
geel10_8 = rgb(255, 250, 206, maxColorValue = 255)
geel10_9 = rgb(255, 251, 223, maxColorValue = 255)
geel10_10 = rgb(255, 253, 239, maxColorValue = 255)
geel5_1 = rgb(255, 238, 54, maxColorValue = 255)
geel5_2 = rgb(255, 241, 105, maxColorValue = 255)
geel5_3 = rgb(255, 244, 149, maxColorValue = 255)
geel5_4 = rgb(255, 248, 188, maxColorValue = 255)
geel5_5 = rgb(255, 251, 223, maxColorValue = 255)
geel4_1 = rgb(255, 238, 54, maxColorValue = 255)
geel4_2 = rgb(255, 243, 128, maxColorValue = 255)
geel4_3 = rgb(255, 248, 188, maxColorValue = 255)
geel4_4 = rgb(255, 251, 223, maxColorValue = 255)
geel3_1 = rgb(255, 238, 54, maxColorValue = 255)
geel3_2 = rgb(255, 244, 149, maxColorValue = 255)
geel3_3 = rgb(255, 251, 223, maxColorValue = 255)
geel2_1 = rgb(255, 238, 54, maxColorValue = 255)
geel2_2 = rgb(255, 246, 169, maxColorValue = 255)
geel1_1 = rgb(255, 238, 54, maxColorValue = 255)
donkerblauw10_1 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw10_2 = rgb(56, 115, 184, maxColorValue = 255)
donkerblauw10_3 = rgb(80, 127, 192, maxColorValue = 255)
donkerblauw10_4 = rgb(103, 141, 200, maxColorValue = 255)
donkerblauw10_5 = rgb(127, 156, 208, maxColorValue = 255)
donkerblauw10_6 = rgb(149, 171, 217, maxColorValue = 255)
donkerblauw10_7 = rgb(171, 187, 225, maxColorValue = 255)
donkerblauw10_8 = rgb(192, 203, 233, maxColorValue = 255)
donkerblauw10_9 = rgb(213, 220, 240, maxColorValue = 255)
donkerblauw10_10 = rgb(234, 237, 248, maxColorValue = 255)
donkerblauw5_1 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw5_2 = rgb(80, 127, 192, maxColorValue = 255)
donkerblauw5_3 = rgb(127, 156, 208, maxColorValue = 255)
donkerblauw5_4 = rgb(171, 187, 225, maxColorValue = 255)
donkerblauw5_5 = rgb(213, 220, 240, maxColorValue = 255)
donkerblauw4_1 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw4_2 = rgb(103, 141, 200, maxColorValue = 255)
donkerblauw4_3 = rgb(171, 187, 225, maxColorValue = 255)
donkerblauw4_4 = rgb(213, 220, 240, maxColorValue = 255)
donkerblauw3_1 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw3_2 = rgb(127, 156, 208, maxColorValue = 255)
donkerblauw3_3 = rgb(213, 220, 240, maxColorValue = 255)
donkerblauw2_1 = rgb(23, 103, 175, maxColorValue = 255)
donkerblauw2_2 = rgb(149, 171, 217, maxColorValue = 255)
donkerblauw1_1 = rgb(23, 103, 175, maxColorValue = 255)
roodgeelgroen10_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen10_2 = rgb(203, 86, 29, maxColorValue = 255)
roodgeelgroen10_3 = rgb(215, 129, 36, maxColorValue = 255)
roodgeelgroen10_4 = rgb(228, 168, 43, maxColorValue = 255)
roodgeelgroen10_5 = rgb(242, 205, 50, maxColorValue = 255)
roodgeelgroen10_6 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen10_7 = rgb(223, 221, 56, maxColorValue = 255)
roodgeelgroen10_8 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen10_9 = rgb(151, 187, 61, maxColorValue = 255)
roodgeelgroen10_10 = rgb(110, 169, 64, maxColorValue = 255)
roodgeelgroen9_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen9_2 = rgb(203, 86, 29, maxColorValue = 255)
roodgeelgroen9_3 = rgb(215, 129, 36, maxColorValue = 255)
roodgeelgroen9_4 = rgb(228, 168, 43, maxColorValue = 255)
roodgeelgroen9_5 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen9_6 = rgb(223, 221, 56, maxColorValue = 255)
roodgeelgroen9_7 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen9_8 = rgb(151, 187, 61, maxColorValue = 255)
roodgeelgroen9_9 = rgb(110, 169, 64, maxColorValue = 255)
roodgeelgroen7_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen7_2 = rgb(203, 86, 29, maxColorValue = 255)
roodgeelgroen7_3 = rgb(228, 168, 43, maxColorValue = 255)
roodgeelgroen7_4 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen7_5 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen7_6 = rgb(151, 187, 61, maxColorValue = 255)
roodgeelgroen7_7 = rgb(110, 169, 64, maxColorValue = 255)
roodgeelgroen5_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen5_2 = rgb(215, 129, 36, maxColorValue = 255)
roodgeelgroen5_3 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen5_4 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen5_5 = rgb(110, 169, 64, maxColorValue = 255)
roodgeelgroen4_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen4_2 = rgb(215, 129, 36, maxColorValue = 255)
roodgeelgroen4_3 = rgb(189, 205, 59, maxColorValue = 255)
roodgeelgroen4_4 = rgb(110, 169, 64, maxColorValue = 255)
roodgeelgroen3_1 = rgb(195, 33, 23, maxColorValue = 255)
roodgeelgroen3_2 = rgb(255, 238, 54, maxColorValue = 255)
roodgeelgroen3_3 = rgb(110, 169, 64, maxColorValue = 255)
roodhemelblauw10_1 = rgb(195, 33, 23, maxColorValue = 255)
roodhemelblauw10_2 = rgb(205, 87, 58, maxColorValue = 255)
roodhemelblauw10_3 = rgb(216, 134, 101, maxColorValue = 255)
roodhemelblauw10_4 = rgb(229, 177, 150, maxColorValue = 255)
roodhemelblauw10_5 = rgb(242, 217, 202, maxColorValue = 255)
roodhemelblauw10_6 = rgb(218, 236, 251, maxColorValue = 255)
roodhemelblauw10_7 = rgb(178, 216, 246, maxColorValue = 255)
roodhemelblauw10_8 = rgb(132, 196, 240, maxColorValue = 255)
roodhemelblauw10_9 = rgb(74, 175, 232, maxColorValue = 255)
roodhemelblauw10_10 = rgb(0, 156, 223, maxColorValue = 255)
roodhemelblauw6_1 = rgb(195, 33, 23, maxColorValue = 255)
roodhemelblauw6_2 = rgb(205, 87, 58, maxColorValue = 255)
roodhemelblauw6_3 = rgb(229, 177, 150, maxColorValue = 255)
roodhemelblauw6_4 = rgb(178, 216, 246, maxColorValue = 255)
roodhemelblauw6_5 = rgb(74, 175, 232, maxColorValue = 255)
roodhemelblauw6_6 = rgb(0, 156, 223, maxColorValue = 255)
roodhemelblauw4_1 = rgb(195, 33, 23, maxColorValue = 255)
roodhemelblauw4_2 = rgb(229, 177, 150, maxColorValue = 255)
roodhemelblauw4_3 = rgb(178, 216, 246, maxColorValue = 255)
roodhemelblauw4_4 = rgb(0, 156, 223, maxColorValue = 255)
roodhemelblauw2_1 = rgb(195, 33, 23, maxColorValue = 255)
roodhemelblauw2_2 = rgb(0, 156, 223, maxColorValue = 255)
zwart10_1 = rgb(0, 0, 0, maxColorValue = 255)
zwart10_2 = rgb(38, 38, 38, maxColorValue = 255)
zwart10_3 = rgb(77, 77, 77, maxColorValue = 255)
zwart10_4 = rgb(102, 102, 102, maxColorValue = 255)
zwart10_5 = rgb(128, 128, 128, maxColorValue = 255)
zwart10_6 = rgb(153, 153, 153, maxColorValue = 255)
zwart10_7 = rgb(191, 191, 191, maxColorValue = 255)
zwart10_8 = rgb(204, 204, 204, maxColorValue = 255)
zwart10_9 = rgb(230, 230, 230, maxColorValue = 255)
zwart10_10 = rgb(242, 242, 242, maxColorValue = 255)
zwart5_1 = rgb(0, 0, 0, maxColorValue = 255)
zwart5_2 = rgb(77, 77, 77, maxColorValue = 255)
zwart5_3 = rgb(128, 128, 128, maxColorValue = 255)
zwart5_4 = rgb(191, 191, 191, maxColorValue = 255)
zwart5_5 = rgb(230, 230, 230, maxColorValue = 255)
zwart4_1 = rgb(77, 77, 77, maxColorValue = 255)
zwart4_2 = rgb(128, 128, 128, maxColorValue = 255)
zwart4_3 = rgb(191, 191, 191, maxColorValue = 255)
zwart4_4 = rgb(230, 230, 230, maxColorValue = 255)
zwart3_1 = rgb(77, 77, 77, maxColorValue = 255)
zwart3_2 = rgb(128, 128, 128, maxColorValue = 255)
zwart3_3 = rgb(191, 191, 191, maxColorValue = 255)
zwart2_1 = rgb(77, 77, 77, maxColorValue = 255)
zwart2_2 = rgb(191, 191, 191, maxColorValue = 255)
zwart1_1 = rgb(77, 77, 77, maxColorValue = 255)


