#
#
#   Utilities (functions) for program "disease deaths vs PM2_5.R"
#
#   Author: Hannah Chang
#   Date: 10-20-2022 created
#         12-28-2022 Created 3 variables and 15 types of accumulations, a total of 18 new columns; rebuild the analytic file
#
#
#
library(dplyr)
library(rgdal)
library(stringr)
library(leaflet) 

#============================ Global Variables ===============================
StartYear = 2010
EndYear = 2019
N = EndYear - StartYear + 2
# 
# N is a global variable for the number of elements in the missing list
# the first element is FIPS, followed by the all the years in the list, 
# thus total number of year is N- 1 ranging from 2010 to 2019 in the missing list
# N+1 is the total number of columns in the missing list, including FIPS, years 2010-2019, and total missing
# 
MaxMissingYear = 3
#
#=============================================
#
#  Function: general linear regression
#
#=============================================


run_glm_and_compute_residuals <- function(DF, equation, distribution) {

  model1 <- glm(formula=equation, family=distribution, data=DF)
  round(cbind(model1$coefficients, confint(model1)),4)
  DF$residuals <- residuals(model1)[row.names(DF)]
  
  return (DF)
}

#=============================================
#
#  Function: Display area and density of selected counties
#
#=============================================
draw_map <- function(loc_info, color_domain, color_value, map_title) {
  colours <- colorNumeric(palette = "YlOrBr", domain = color_domain, reverse=FALSE)
  leaflet(data=loc_info) %>%
    addTiles() %>%
    addPolygons(fillColor =  colours(color_value), color="", fillOpacity = 0.7,
                weight = 1, smoothFactor = 0.5, opacity = 1.0) %>%
    addLegend(pal = colours, values = variable, opacity = 1, title=map_title) %>%
    addScaleBar(position="bottomleft")
}
#=============================================
#
#  Function: Combine county daily PM2.5 data from 2010-2019 into yearly data for west coast states 
#            Create a CSV file as the base for building other data files
#
#=============================================

consolidate_daily_PM25_files <- function(geo_area, State_List, PM25Threshold) {
  
  csv_fl_name =  paste(geo_area, '-PM25-2010-2019-raw.csv', sep = '')  # the original PM2.5 data file before processing for missing data
  #================================================================ 
  
  
  print(paste("PM2.5 Threshold: ",  as.character(PM25Threshold), sep=''))
  #================================================================
  
  merge_DF = data.frame()
  for (state in State_List){
    folder <- paste("./State PM25 Data/", state, "-PM25-2010-2021/", sep='')
    file_list <- c(
      #paste(state,'-2021-PM25.csv',sep=''),
      #paste(state,'-2020-PM25.csv',sep=''),
      paste(state,'-2019-PM25.csv',sep=''),
      paste(state,'-2018-PM25.csv',sep=''),
      paste(state,'-2017-PM25.csv',sep=''),
      paste(state,'-2016-PM25.csv',sep=''),
      paste(state,'-2015-PM25.csv',sep=''),
      paste(state,'-2014-PM25.csv',sep=''),
      paste(state,'-2013-PM25.csv',sep=''),
      paste(state,'-2012-PM25.csv',sep=''),
      paste(state,'-2011-PM25.csv',sep=''),
      paste(state,'-2010-PM25.csv',sep='')
    )
    
    for (file_name in file_list) {

      old_DF <- read.csv(paste(folder, file_name , sep=''), check.names=FALSE, header=T)
      new_DF <- old_DF[c('Daily Mean PM2.5 Concentration','UNITS','POC','Site ID','Date', 'STATE_CODE','STATE','COUNTY_CODE','COUNTY')]
      state_days_PM25_higher_than_threshold = 0
      
      new_DF['FIPS'] <- 0
      new_DF['Year'] <- strtoi(substr(file_name, 4, 7))
      new_DF['Days PM25 >= Threshold'] <- 0
      new_DF['PM25 >= Threshold Flag'] <- FALSE
      new_DF['Multi Measurements'] <- FALSE
      new_DF['DAILY AVG PM25'] <- 0
      new_DF['Days >= Threshold & <= 50'] <- 0
      new_DF['Days > 50'] <- 0
      
      
      #================================================================
      new_DF$FIPS <- paste(str_pad(new_DF$STATE_CODE, 2, pad = "0"), str_pad(new_DF$COUNTY_CODE, 3, pad = "0"), sep='')
      
      #===========================================================================
      #
      #   Calculate daily average PM2.5 and determine if that value is greater than the threshold
      #   Note 1: some counties has more than one measuring sites and each site may have 
      #           more than one measurements a day
      #   Note 2: The "for" loops can be time consuming. 
      #  
      #=========================================================================== 
      
      for (fips in unique(new_DF$FIPS)) {
        county_days_PM25_higher_than_threshold = 0
        for (date in unique(new_DF$Date)) {
          row_list2 <- strtoi(rownames(new_DF[which(new_DF$FIPS == fips & new_DF$Date == date),0]))
          #
          #  strtoi: string to int func, used to calculate how many rows are there for this FIPS and this date 
          #     (each date for a specific county can have multi measurements of PM2.5)
          #
          #     
          
          if (length(row_list2) > 1) {
            new_DF[row_list2,'Multi Measurements'] <- TRUE
          } # if (length(row_list2) > 1)
          
          if (length(row_list2) >= 1) {
            daily_ave_PM25 = mean(new_DF[row_list2,'Daily Mean PM2.5 Concentration'])
            new_DF[row_list2,'DAILY AVG PM25'] <- daily_ave_PM25
            
            if (daily_ave_PM25 < PM25Threshold) {
              new_DF[row_list2,'PM25 >= Threshold Flag'] <- FALSE
            } else {
              new_DF[row_list2,'PM25 >= Threshold Flag'] <- TRUE
              new_DF[row_list2,'Days PM25 >= Threshold'] <- 1
              
              new_DF[row_list2,'Days >= Threshold & <= 50'] <- if (daily_ave_PM25 <=50) 1 else 0 
              new_DF[row_list2,'Days > 50'] <- if (daily_ave_PM25 > 50) 1 else 0
            }
            
          } # if (length(row_list2) >= 1)
          
        } # end for (date in new_DF$Date)
        
      }  #for (fips in unique(new_DF$FIPS))
      
      
      new_DF_unique_daily <- unique(new_DF[, c('Year','Date', 'FIPS','STATE','COUNTY','DAILY AVG PM25','UNITS','Days PM25 >= Threshold','Days >= Threshold & <= 50', 'Days > 50')])
      
      GT_Threshold <- summarize(group_by(new_DF_unique_daily, FIPS), PM2_5GE_Threshold = sum(`Days PM25 >= Threshold`, na.rm=T))
      GT50 <- summarize(group_by(new_DF_unique_daily, FIPS), PM2_5GT50 = sum(`Days > 50`, na.rm=T))
      GT35LT50 <- summarize(group_by(new_DF_unique_daily, FIPS), PM2_5GT35LE50 = sum(`Days >= Threshold & <= 50`, na.rm=T))
      new_DF_yearly <- unique(new_DF_unique_daily[, c('Year','FIPS', 'STATE','COUNTY')])
      
      new_DF_yearly <- merge(x=new_DF_yearly, y=GT_Threshold, by = c('FIPS'), all.y = TRUE)
      new_DF_yearly <- merge(x=new_DF_yearly, y=GT35LT50, by = c('FIPS'), all.y = TRUE)
      new_DF_yearly <- merge(x=new_DF_yearly, y=GT50, by = c('FIPS'), all.y = TRUE)
      
      
      days_PM25_higher_than_threshold_state = sum(GT_Threshold$PM2_5GE_Threshold)
      
      print(paste(file_name, ": ", as.character(days_PM25_higher_than_threshold_state), " County-Days PM2.5 >= ", as.character(PM25Threshold),  sep=''))
      
      merge_DF <- rbind(merge_DF, new_DF_yearly)
      
    } # for (file_name in file_list)
    
    write.csv(merge_DF,csv_fl_name, row.names = FALSE)  
    
  } #end for (state in State_List)
  #==================================================================================
  #
  #
  #   Create 'Westcoast-PM25-2000-2009.csv'
  #
  #   Counties missing 3 years or less are retains and the missing data is filled in 
  #   Counties missing more than 3 years are deleted
  #   Note: delete Coos(OR) Yamhill(OR) 2 records; Garfield(WA) 3 records; San Juan(WA) 1 records; Pend Orille(WA) 2 records; Alpine(CA) 6 records
  #
  #=================================================================================
  merge_DF <- read.csv(csv_fl_name, check.names=FALSE, header=T)
  missing_list <- identify_counties_with_missing_years(merge_DF, StartYear, EndYear)
  listOfDF  <- delete_counties_with_too_many_missing_years (merge_DF, missing_list, MaxMissingYear)  # Maximum allowed for missing 3 years out of 10
  merge_DF <- listOfDF[[1]]
  missing_list <- listOfDF[[2]]
  
  #================ check counties listed below for deleted counties ===================
  
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 53051)) # Pend Oreille Juan, WA
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 53055)) # San Juan, WA
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 53023)) # Garfield, WA
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 41071)) # Yamhill, OR
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 41011)) # Coor, OR
  #merge_DF <- subset(merge_DF <- subset(merge_DF, FIPS != 6003))  # Alpine, CA
  
  #==================================================================================
  #  Interpolate missing PM2.5 Value with adding one record
  #==================================================================================
  if ("OR" %in% State_List) { 
    print("Interploate the 2016 PM2.5 for Jefferson, OR")
    merge_DF[nrow(merge_DF) + 1,] <- c(41031, 2016, 'Oregon',	'Jefferson', 7.5, 0, 0)  # interpolate
  }
  #==================================================================================
  # Sort and re-index the data-frame after appending new records
  #==================================================================================
  merge_DF <- merge_DF[order(merge_DF$Year, merge_DF$FIPS, decreasing = FALSE),]
  rownames(merge_DF) = seq(length=nrow(merge_DF))
  #==================================================================================
  #  Create CSV file for PM2.5 data
  #==================================================================================
  fl_name =  paste(geo_area, '-PM25-2010-2019-yearly.csv', sep = '')
  write.csv(merge_DF,fl_name, row.names = FALSE)   # use this file to check if PM2.5 accumulations are calculated correctly in later steps
  #=================================================================================
} # end colsolidate_pm25_files()

#=======================================================
#
# Function: Find those counties with missing years
# Return: missing_list in which TRUE means in that year the county data is missing, and FALSE means not missing. 
# 
#
#=======================================================
identify_counties_with_missing_years <- function(DF, start_year, end_year) {
  
  missing_DF <- data.frame(matrix(ncol = end_year-start_year+3, nrow = 0))   # total number of col is N+1
  ideal_year_list <- seq(from = start_year, to = end_year, by=1)
  colnames(missing_DF) <- c("FIPS",ideal_year_list, "Total Missing Years") # now the total number of col is N+2
  missing = 0
  
  for (fips in unique(DF$FIPS)) { 
    
    actual_year_list <- DF[DF$FIPS == fips, 'Year']
    
    if (length(ideal_year_list) != length(actual_year_list)) {
    
      missing_years = !(ideal_year_list %in% actual_year_list)  # TRUE: there is a missing year 
      
      print(paste(fips, " (FIPS): the year(s) missing between ", start_year, " and ", end_year,  sep='')) 
      #print(actual_year_list)
      print(missing_years)
      
      missing = missing + 1
      missing_DF[nrow(missing_DF) + 1,] = c(fips, missing_years, 0) # # append one row to missing_list: county FIPS and TRUE/FALSE from start_year to end_year
      
      
    } # if (length(ideal_year_list) != length(actual_year_list))
  } #   for (fips in unique(PM25_DF$FIPS)) 
  
  print(paste(missing, " counties with missing years",  sep=''))
  print("..............")


  missing_DF$'Total Missing Years' <- rowSums(missing_DF[,c(2:N)]) # Note: rowSums() requires the entire row to be numeric
  
  print(paste(sum(missing_DF[,c('Total Missing Years')]), "  records missing",  sep=''))

  return (missing_DF)
  
}  # end identify_counties_with_missing_years

#=======================================================
#
# Function: Find those counties with NA's in data
# Return: missing_list in which 'NA' means in that year the county data is missing.
#
#=======================================================
identify_counties_with_NAs <- function(DF, start_year, end_year, DF_col) {
  
  missing_DF <- data.frame(matrix(ncol = end_year-start_year+3, nrow = 0))  # total number of col is N+1
  ideal_year_list <- seq(from = start_year, to = end_year, by=1)
  colnames(missing_DF) <- c("FIPS",ideal_year_list, "Total Missing Years") # now the total number of col is N+2
  missing = 0
  
 for (fips in unique(DF$FIPS)) { 
    
    actual_year_list <- DF[DF$FIPS == fips, DF_col]
    
    if (NA %in%  actual_year_list) { 
      
      missing_years = is.na(actual_year_list)
      
      print(paste(fips, " (FIPS): the year(s) of NA's between ", start_year, " and ", end_year ,  sep='')) 
      #print(actual_year_list)
      print(missing_years)
    
      missing_DF[nrow(missing_DF) + 1,] = c(fips, missing_years, 0) # append to one row to missing_list:  county FIPS and TRUE/FALSE from start_year to end_year
      
      missing = missing + 1
    } # if (sum(is.na(actual_year_list)) > 0)
  } #   for (fips in unique(PM25_DF$FIPS)) 
  
  print(paste(missing, " counties with missing years",  sep=''))
  print("..................")
  
  missing_DF$'Total Missing Years' <- rowSums(missing_DF[,c(2:N)]) # Note: rowSums() requires the entire row to be numeric
  
  print(paste(sum(missing_DF[,c('Total Missing Years')]), "  records missing",  sep=''))
  
  return (missing_DF)
  
}  # end identify_counties_with_missing_NAs
#=======================================================
#
#     Function: For the input data-frame, delete those counties with yearly data missing for more than the the maximum allowed years
#               
#     Return: a revised data-frame after deleting the above mentioned counties
#     Parameter:  missing_DF: a data frame containing FIPS and the status of missing for year 2010 thru 2019 
#
#=======================================================  
delete_counties_with_too_many_missing_years <- function(DF, missing_list, years_of_missing_allowed) { 
  temp_list <- missing_list
  count = 0
  for (fips in missing_list[missing_list$'Total Missing Years' > years_of_missing_allowed,]$FIPS){
    print(paste(fips, " (FIPS) has too many missing years",  sep='')) 

    DF <- subset(DF, FIPS != fips) 
    temp_list <- subset(temp_list, FIPS != fips)
    count = count + 1
  }  
  listOfDataframe = list(DF, temp_list)
  print(paste(count, " counties deleted", sep=''))
  return(listOfDataframe)
}  

#==============================================================================
#
#   Function: fill NA by recursively copying data from the last year
#   Parameters: 
#               row_NAList: used to help translate the years of missing data into the rows in data frame
#               DF_col: the column number of some data with NA, e.g., COPD Deaths, Population, etc.
#
#==============================================================================
copy_from_last_year <- function(DF, NAlist, row_NAlist, year, DF_col) {

  #=====================================================================
  # Note: The next two lines have no functional use in this program, only for self-learning
  # Note: row name in DF is not the row number.
  # Note: DF_row1 and DF_row2 do not match with 'row name' in DF, but they point to the correct location in DF
  # Note: rownames() matches with the row name in DF, but point to the wrong location in DF
  #===================================================================== 
  DF_row1Name = rownames(DF[DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year,])
  DF_row2Name = rownames(DF[DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year-1,])

  #=====================================================================
  
  DF_row1 = which(DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year)
  DF_row2 = which(DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year-1)
  
  if (NAlist[row_NAlist, year-2009] == TRUE ){ # if second to the last year is not missing; TRUE means having data
    #
    # For years are ranging from 2010-2019, the missing_list has 12 columns total, where col 11 is for year 2019.
    # If data of year 2019 is missing (TRUE), we check the year 2018 (col 10: 2019-2009 = 10), 
    # and if 2018 is not missing, we will copy values from year 2018, else to check 2017...,etc.
    # 
    year = year - 1
    
    DF <- copy_from_last_year(DF, NAlist,row_NAlist, year, DF_col)
    print(paste("replace ", DF[DF_row1,DF_col], " (row ",DF_row1,")"," with ", DF[DF_row2, DF_col], " (row ", DF_row2, ")",  sep='')) 
    DF[DF_row1,DF_col] <- DF[DF_row2, DF_col]
    assign("NAremained", NAremained - 1, envir = .GlobalEnv)
    #NAremained <<- NAremained - 1       # reduce global NA count by 1
    
  }
  else {
    
    print(paste("replace ", DF[DF_row1,DF_col], " (row ",DF_row1,")"," with ", DF[DF_row2, DF_col], " (row ", DF_row2, ")",  sep='')) 
    DF[DF_row1,DF_col] <- DF[DF_row2, DF_col]

    assign("NAremained", NAremained - 1, envir = .GlobalEnv)
    #NAremained <<- NAremained - 1     # reduce global NA count by 1

  }
  return (DF)
}

#==============================================================================
#
#   Function: fill NA by recursively copying data from the next year
#   Parameters: 
#               row_NAList: used to help translate the years of missing data into the rows in data frame 
#               DF_col: the column number of some data with NA, e.g., COPD Deaths, Population, etc.
#
#========================================================================
copy_from_next_year <- function(DF, NAlist, row_NAlist, year, DF_col) {
  
  #=====================================================================
  # Note: The next two lines have no functional use in this program, only for self-learning
  # Note: the row name in DF is not the row number.
  # Note: DF_row1 and DF_row2 do not match with 'row name' in DF, but they point to the correct location in DF
  # Note: rownames() matches with the 'row name' in DF, but point to the wrong location in DF
  # Note: Each DF_row corresponds to a year
  # Note: "1" represents the column number of FIPS in NAlist
  # ====================================================================
  DF_row1Name = rownames(DF[DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year,])
  DF_row2Name = rownames(DF[DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year + 1,])
  
  #=====================================================================
  
  DF_row1 = which(DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year)
  DF_row2 = which(DF$FIPS == NAlist[row_NAlist, 1] & DF$Year == year + 1)
  
  assign("head_ptr", DF_row2, envir = .GlobalEnv) 
  
  if (NAlist[row_NAlist, year - 2007] == TRUE ){ # if the year 2011 is missing; TRUE means missing data
    #
    # The missing_list has 13 columns (N=13), col 1 is for FIPS, col 2 is year 2010, then 2010-2007 = 3, which means that col 3 is 2011
    # For year 2010, we check col 3 (year 2011), and if 2011 is not missing, we will copy values from year 2011, else check 2012..., etc.
    #
    
    year = year + 1
    
    DF <- copy_from_next_year(DF, NAlist,row_NAlist, year, DF_col)
    print(paste("replace ", DF[DF_row1,DF_col], " (row ",DF_row1,")"," with ", DF[DF_row2, DF_col], " (row ", DF_row2, ")",  sep='')) 
    
    DF[DF_row1,DF_col] <- DF[DF_row2, DF_col]
    assign("NAremained", NAremained - 1, envir = .GlobalEnv)
    assign("NA_list", NA_list[-1], envir = .GlobalEnv)
    #NAremained <<- NAremained - 1    # reduce global NA count by 1

    }
  else {
    
    print(paste("replace ", DF[DF_row1,DF_col], " (row ",DF_row1,")"," with ", DF[DF_row2, DF_col], " (row ", DF_row2, ")",  sep=''))  
    
    DF[DF_row1,DF_col] <- DF[DF_row2, DF_col]
    assign("NAremained", NAremained - 1, envir = .GlobalEnv)
    assign("NA_list", NA_list[-1], envir = .GlobalEnv)
    #NAremained <<- NAremained - 1    # reduce global NA count by 1

  }
  return (DF)
}
#==============================================================================
#
#   Function: Determine the basic missing patterns, excluding those missing data starting at year 2010 or ending at year 2019;
#             
#========================================================================

find_next_NA_type <- function() {
  
  if (length(NA_list) == 1) {return (1)}
  else {
        if(NA_list[2]-NA_list[1] > 1) {return(1)}   # missing one year of data
        else {  
                if ((length(NA_list) == 2) & (NA_list[2]-NA_list[1] == 1)) {return(2)}   #missing two consecutive years
                else  {if ((length(NA_list) == 3) & (NA_list[2]-NA_list[1] == 1) & (NA_list[3]-NA_list[2] == 1)){ return (3) }} # missing three consecutive years
        }
  }
  return(0)
  }
#==============================================================================
#
#   Function: Interpolate one missing data by taking average from two adjacent years
#
#========================================================================

interpolate <- function(DF, fips, year, DF_col){
  DF_col2 = DF_col + 1
  DF_row_minus1 = which(DF$FIPS == fips & DF$Year == year - 1)
  DF_row = which(DF$FIPS == fips & DF$Year == year)
  DF_row_plus1 = which(DF$FIPS == fips & DF$Year == year + 1)
  
  DF[DF_row,DF_col] <- (DF[DF_row_minus1, DF_col] + DF[DF_row_plus1, DF_col])/2

  print("interpolate")
  print( rownames( DF[DF$FIPS == fips & DF$Year == year, ]  ))
  assign("NAremained", NAremained - 1, envir = .GlobalEnv)
  assign("NA_list", NA_list[-1], envir = .GlobalEnv)
  print(paste(NAremained, " NA's remaining",  sep=''))
  
  return(DF)}
#==============================================================================
#
#   Function: copy from two adjacent years
#
#========================================================================

fill_twosome <- function(DF, fips, year, DF_col){
  
  DF_col2 = DF_col + 1
  #==============================================
  DF_row_minus1 = which(DF$FIPS == fips & DF$Year == year - 1)
  DF_row = which(DF$FIPS == fips & DF$Year == year)
  
  DF[DF_row,DF_col] <- DF[DF_row_minus1, DF_col] 
  #==============================================
  DF_row_plus1 = which(DF$FIPS == fips & DF$Year == year + 1)
  DF_row_plus2 = which(DF$FIPS == fips & DF$Year == year + 2)
  
  DF[DF_row_plus1,DF_col] <- DF[DF_row_plus2, DF_col] 
  #==============================================
  print("fill in two")
  print( rownames( DF[DF$FIPS == fips & DF$Year == year, ]  ))
  assign("NAremained", NAremained - 2, envir = .GlobalEnv)
  assign("NA_list", NA_list[-2], envir = .GlobalEnv)
  
  print(paste(NAremained, " NA's remaining",  sep=''))

  return(DF)}

#==============================================================================
#
#   Function: copy from two adjacent years then take average for the missing year in the middle
#
#==============================================================================

fill_threesome <- function(DF, fips, year, DF_col){

  DF_col2 = DF_col + 1
  
  DF_row_minus1 = which(DF$FIPS == fips & DF$Year == year - 1)
  DF_row = which(DF$FIPS == fips & DF$Year == year)
  
  DF[DF_row,DF_col] <- DF[DF_row_minus1, DF_col] 
  
  DF_row_plus2 = which(DF$FIPS == fips & DF$Year == year + 2)
  DF_row_plus3 = which(DF$FIPS == fips & DF$Year == year + 3)
  
  DF[DF_row_plus2,DF_col] <- DF[DF_row_plus3, DF_col] 
  
  year = year + 1

  DF <- interpolate(DF, fips, year, DF_col)
  
  assign("NAremained", NAremained - 3, envir = .GlobalEnv)

  print("fill in threesome rows:")
  print( rownames( DF[DF$FIPS == fips & DF$Year == year, ]  ))
  print(paste(NAremained, " NA's remaining",  sep=''))
  
  return(DF)}

#==============================================================================
#
#   Function: fill missing data; can fill in missing data for a county up to 3 years.
#
#========================================================================

fill_missing_data <- function(missing_list, FinalDFSubset, DF_col) {

print("fill in missing data")
for (i in 1:nrow(missing_list)) { 
  #
  #   Declare global variable NAremained for number of NAs remaining after filling in data
  #=======================================================
  NAremained <<- missing_list[i, c('Total Missing Years')] # Note: " <<- " refers to a global variable
  head_ptr <<- 1
  #=======================================================
  NA_list <<- NULL
  # 
  # N is a global variable for the number of elements in the missing list
  # the first element is FIPS, followed by the all the years in the list, 
  # thus total number of year is N- 1 ranging from 2010 to 2019 in the missing list
  # 
  for(c in 2:N){   # the 1st year on the missing_list is column #2
    ifelse(missing_list[i,c]==TRUE,NA_list <<- append(NA_list, c),1 )
  } 
  
  #======================================================
  fips = missing_list[i,"FIPS"]
  print(paste("FIPS: ", fips,  sep=''))
  #======================================================
  
  
  if (missing_list[i, 2] == TRUE) {  # if the 1st year is missing; 'TRUE' means missing data
    
    FinalDFSubset <- copy_from_next_year(FinalDFSubset, missing_list, i, StartYear, DF_col ) # this fills in missing data in consecutive years starting 2010
  }
  
  if (missing_list[i, N] == TRUE) {  # if the last year on the list is missing; 'TRUE' means missing data

    FinalDFSubset <- copy_from_last_year(FinalDFSubset, missing_list, i, EndYear, DF_col ) # this fills in missing data in consecutive years ending 2019
  }
  print(paste(NAremained, " NA's remaining",  sep=''))
  #
  # Fill in missing data, but not for NA's at the beginning year (2010) or at the ending year (2019)
  #
  while (NAremained > 0) {
 
    switch (find_next_NA_type(),  
            FinalDFSubset <- interpolate(FinalDFSubset, fips, NA_list[1] + 2008, DF_col) ,   # case 1; fill in one missing data 
            FinalDFSubset <- fill_twosome(FinalDFSubset, fips, NA_list[1] + 2008, DF_col),   # case 2: fill in missing data of two consecutive years  
            FinalDFSubset <- fill_threesome(FinalDFSubset, fips, NA_list[1] + 2008, DF_col)) # case 3: fill in missing data of three consecutive years
  } # end while (NAremained > 0)
} # end for (i in 1:nrow(missing_list))

return (FinalDFSubset)
}
