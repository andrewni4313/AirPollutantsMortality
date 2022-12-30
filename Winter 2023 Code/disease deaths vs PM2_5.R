#
#
#
#   Main program: "disease deaths vs PM 2_5.R"
#
#   Author: Hannah Chang
#   Date: 10-20-2022 created  
#         12-28-2022 Created 3 variables and 15 types of accumulations, a total of total 18 new columns; rebuild the analytic file 
#
#=============================================================================================================

library(dplyr)
library(rgdal)
library(stringr)
library(leaflet)
#======================================================
#
#                 DATA MANAGEMENT SECTION
#
#=====================================================
#===============import utilities==============-
source("utilities-disease deaths vs PM2_5.R")
#==============================================
#
#   Step 1: Select a Disease Deaths to study
#
#==============================================
disease_deaths <<- 'Alzheimers Deaths'
#disease_deaths <<- 'All Causes of Death Deaths'
#==============================================
#
#   Step 2: Configure the analytical file 
#
#==============================================
#==============================================
#   2.1 Note: Adjustment flag is 'TRUE' by default
#==============================================
Adjustment <<- TRUE # do not adjust for social economics
#Adjustment <<- FALSE # adjust for social economics
#==============================================
#   2.2 Prepare a list of social-demographic-economics factors for adjusting_factors
#==============================================
#===== the next line is a complete list of social economics factors ==========
adjusting_factors <<- c('Poverty Percent, All Ages','% Smokers','% Excessive Drinking','% African American','% Asian','% Hispanic','% Age < 18','% Age 65 and over','% Obese','% Unemployed','% Uninsured','Median Household Income','% Rural')
#===== example of picking any 2 factors =============
#adjusting_factors = c('Poverty Percent, All Ages','% Age 65 and over')
#===== example of picking a factor =========
#adjusting_factors = c('% Age 65 and over')
#====================================================================
#   2,3: Create PM2.5 file from PM2.5 daily data of three west coast states.
#=============================================
State_List <- c(
  'OR',
  'WA',
  'CA'
)

#====================================================================
#   Step 3.
#
# 1. Compile PM2.5 data from west coast states according to the State_List
# 2. Generate 'Westcoast-PM25-2010-2019-yearly.csv' as the base of the analytic file
# 3. Import Disease Deaths data and social-demographic-economics data
#
#=================================================================

disease_abbreviation_list <- c("AZ","ALL")
PM25Threshold = 35
consolidate_daily_PM25_files("Westcoast", State_List, PM25Threshold) 

#=============================================
disease_deaths_list <<-  c('Alzheimers Deaths', 'All Causes of Death Deaths')
disease_deaths_per_100K_list <<- c('AZ Deaths Per 100K','All Deaths Per 100K')
disease_deaths_per_100K <<- disease_deaths_per_100K_list[which(disease_deaths_list == disease_deaths)]
#==================================================================================
#   Import PM2.5 data and convert FIPS from Integer type to Character type to match 
#   with the FIPS type in county_map
#==================================================================================

PM25_DF <- read.csv('Westcoast-PM25-2010-2019-yearly.csv', check.names=FALSE, header=T)
 
if (disease_deaths == 'Alzheimers Deaths') {
  FinalDF <- read.csv('AD Deaths by County 2010-2020.csv', check.names=FALSE, header=T)
  FinalDF <- FinalDF[c('County and state','FIPS','Year','Alzheimers Deaths','Population')]
  maplegend_Deaths = "Yearly AZ Deaths/100K (2015-2019)"
  maplegend_Risks = "AZ Deaths Risk (2015)"
} else if (disease_deaths == 'All Causes of Death Deaths') {

  FinalDF <- read.csv('AllData_8-15.csv', check.names=FALSE, header=T, row.names=1)
  FinalDF <- FinalDF[c('County','FIPS','Year','All Causes of Death Deaths','Population')]
  maplegend_Deaths = "Yearly All Deaths/100K (2015-2019)"
  maplegend_Risks = "All Deaths Risk (2015)"
}
  
FinalDF <- FinalDF[,2:5] 

#===================================================================
FinalDF <- subset(FinalDF, Year >= 2010 & Year <= 2019)

#=======================================================
# 1. Merge disease data with PM2.5 Data
# 2. Convert data type for "Population" and disease deaths
#=======================================================

FinalDFSubset <- merge(x=FinalDF, y=PM25_DF, by = c('FIPS', 'Year'), all.y = TRUE) 
FinalDFSubset$`Population` <- as.integer(FinalDFSubset$`Population`)
FinalDFSubset[,which(colnames(FinalDFSubset) == disease_deaths)] <- as.integer(FinalDFSubset[,which(colnames(FinalDFSubset) == disease_deaths)])

#=======================================================
# 1. Identify counties which data has NA's
# 2. Delete those counties having more than 3 years of missing data
# 3. Fill missing data
#=======================================================
missing_list <- identify_counties_with_NAs(FinalDFSubset, 2010, 2019, which( colnames(FinalDFSubset)== disease_deaths ))

listOfDF  <- delete_counties_with_too_many_missing_years (FinalDFSubset, missing_list, 3)
FinalDFSubset <- listOfDF[[1]]
missing_list <- listOfDF[[2]] 
if (nrow(missing_list) > 0) { 
  FinalDFSubset <- fill_missing_data(missing_list, FinalDFSubset, which(colnames(FinalDFSubset)== disease_deaths))
  }

missing_list <- identify_counties_with_NAs(FinalDFSubset, 2010, 2019, which( colnames(df)=="Population" ))
listOfDF  <- delete_counties_with_too_many_missing_years (FinalDFSubset, missing_list, 3)
FinalDFSubset <- listOfDF[[1]]
missing_list <- listOfDF[[2]] 
if (nrow(missing_list) > 0) { 
  FinalDFSubset <- fill_missing_data(missing_list, FinalDFSubset, which(colnames(FinalDFSubset)=="Population"))
}

#======================================================
# Re-index 
#======================================================
rownames(FinalDFSubset) = seq(length=nrow(FinalDFSubset))

#====================== Check again for Missing Data ======================================
check_DF <- FinalDFSubset
check_DF$FIPS <- as.integer(check_DF$FIPS)
missing_list <- identify_counties_with_missing_years(check_DF, 2010, 2019)
#===============================================================================
#
# Import social-demographic-economics data 
#
#===============================================================================

if (Adjustment == TRUE) {

AllData_DF <- read.csv('AllData_8-15.csv', check.names=FALSE, header=T)
 
  temp_DF <- AllData_DF[AllData_DF$Year <= 2019 & AllData_DF$Year >= 2010,]
  temp_DF <- temp_DF[c('FIPS','Year','Poverty Percent, All Ages','% Smokers','% Excessive Drinking','% African American','% Asian','% Hispanic','% Age < 18','% Age 65 and over','% Obese','% Unemployed','% Uninsured','Median Household Income','% Rural')]

  for(year in FinalDFSubset$Year) {
    for (fips in unique(FinalDFSubset$FIPS)) {
      for (col in adjusting_factors) {
        FinalDFSubset[FinalDFSubset$FIPS == fips & FinalDFSubset$Year == year,col] <- temp_DF[temp_DF$FIPS==fips & temp_DF$Year==year,col]
       } # end for (col in adjusting_factors)
      } # end for(year in FinalDFSubset$Year)
    } # end (fips in unique(FinalDFSubset$FIPS))

  #=============================================
  #
  #  Fill in for missing social-demographic-economics data
  #
  #=============================================
  for (col in adjusting_factors) {
    missing_list <- identify_counties_with_NAs(FinalDFSubset, 2010, 2019, which( colnames(FinalDFSubset)== col ))
    if(nrow(missing_list) > 0){
      print(paste("In '", col, "' ", nrow(missing_list), " counties have NAs", sep='')) 
      FinalDFSubset <- fill_missing_data (missing_list, FinalDFSubset, which( colnames(FinalDFSubset)== col ))
    } # end if (nrow(missing_list) > 0)
  } # end for (col in col_list)
} # end if (Adjustment == TRUE)
#======================================================
#   Re-index 
#======================================================
  rownames(FinalDFSubset) = seq(length=nrow(FinalDFSubset))

#=============================================
#
# Step 4: Calculate 5 types of PM2.5 accumulations for each of the three variables: 'PM2_5GE_Threshold', 'PM2_5GT35LE50' , 'PM2_5GT50'
#
#=============================================

AccEndYear = 2019     # do not change this value!
AccStartYear = 2015   # do not change this value!
Acc_N = 5 # the number of years in which 'days PM2.5>=35' is accumulated; e.g., 1: last year only; 2: accumulation of the last 2 year; 5: accumulation of the last 5 years
AccYears = 1:Acc_N # the range of years in which 'days PM2.5>=35' is accumulated, from (current_year - 1) to (current_year - N)

#====================================
var_List <- c('PM2_5GE_Threshold', 'PM2_5GT35LE50' , 'PM2_5GT50' )

temp_DF <- FinalDFSubset[FinalDFSubset$Year <= AccEndYear & FinalDFSubset$Year >= AccStartYear, ]
for (variable in var_List){

for (NewCol in AccYears) {
  
  if (NewCol == 1) {yr = 'Last'}
  if (NewCol >=2 & Acc_N >=5 )  {yr = paste('Last', NewCol,  sep='')}

  accGT_Threshold = paste(variable, yr, 'Year',  sep='')

  temp_DF[accGT_Threshold] <- 0

  for (fips in unique(temp_DF$FIPS)) {
    for (year in unique(temp_DF$Year)) {
      if (NewCol > 1) {
          for (acc in 1:NewCol) {
        
          temp_DF[temp_DF$Year == year & temp_DF$FIPS == fips,accGT_Threshold] =
          temp_DF[temp_DF$Year == year & temp_DF$FIPS == fips,accGT_Threshold] +
          FinalDFSubset[FinalDFSubset$FIPS == fips & FinalDFSubset$Year == (year - acc), variable]
        } # for (acc in NewCol)
      } else {
          temp_DF[temp_DF$Year == year & temp_DF$FIPS == fips,accGT_Threshold] =
          FinalDFSubset[FinalDFSubset$FIPS == fips & FinalDFSubset$Year == (year - 1), variable]
      } #if (NewCol > 1)  

  } #  for (year in unique(temp_DF$Year))
}  # for (fips in unique(temp_DF$FIPS))  
}  
} # end for (variable in var_List)
FinalDFSubset <- temp_DF  # End calculating accumulations

#=======================================================
# Create analytic file for Disease Deaths vs PM2.5.
# Note: 1. This is the master data file for subsequent analyses. 
#       2. Keep a copy for future reference.
#       
#=======================================================
write.csv(FinalDFSubset,paste("Westcoast-", disease_abbreviation_list[which(disease_deaths_list == disease_deaths)], "-Deaths-vs-PM25-2015-2019.csv",  sep=''),row.names = FALSE)

#============================== END DATA MANAGEMENT SECTION ========================================

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

#===================================================================================================
#
#                                 ANALYSIS SECTION
#
#===================================================================================================
Accumulation <- TRUE
Adjustmnet <- TRUE
#=====================================================
#   Note 1: No need to run the above DATA MANAGEMENT SECTION code if "Westcoast-XX-Deaths-vs-PM25-2015-2019.csv" is in the directory
#   Note 2: Must Keep the following three lines consistent with those in the DATA MANAGEMENT SECTION
#=====================================================

disease_deaths_list <<-  c('Alzheimers Deaths', 'All Causes of Death Deaths')
disease_deaths_per_100K_list <<- c('AZ Deaths Per 100K','All Deaths Per 100K')
disease_deaths_per_100K <<- disease_deaths_per_100K_list[which(disease_deaths_list == disease_deaths)]

#====================================================
#   Note: 'adjusting_factors' must be consistent with the Equation
#====================================================
adjusting_factors <<- c('Poverty Percent, All Ages','% Smokers','% Excessive Drinking','% African American','% Asian','% Hispanic','% Age < 18','% Age 65 and over','% Obese','% Unemployed','% Uninsured','Median Household Income','% Rural')
#====================================================
#   Import Data
#====================================================
FinalDFSubset <- read.csv(paste("Westcoast-", disease_abbreviation_list[which(disease_deaths_list == disease_deaths)], "-Deaths-vs-PM25-2015-2019.csv",  sep=''), check.names=FALSE, header=T)

#=================================================================================
#     Visualize the average yearly rate of disease deaths for West-coast counties
#=================================================================================

#============================================
#     Create county map data-frame
#=============================================
county_map <- readOGR(dsn = "./county_data/cb_2018_us_county_5m/cb_2018_us_county_5m.shp")
county_map$FIPS <- as.integer(paste0(county_map$STATEFP, county_map$COUNTYFP))
#==================================================================================
#   1. Calculate death counts in 100K population 
#   2. Calculate the average deaths count for each counties
#==================================================================================
if  (disease_deaths == 'All Causes of Death Deaths'){
  FinalDFSubset$`All Deaths Per 100K` <- as.integer((FinalDFSubset$`All Causes of Death Deaths`*100000/ FinalDFSubset$Population))
    # covert death rate to natural log for log-normal distribution
    FinalDFSubset$`Log(Death per 100K)` <- log(FinalDFSubset$`All Deaths Per 100K`   )
  averageDeath <- summarize(group_by(FinalDFSubset, FIPS), Death = mean(`All Deaths Per 100K`, na.rm=T))
} else if (disease_deaths == 'Alzheimers Deaths') {
  FinalDFSubset$'AZ Deaths Per 100K' <- as.integer((FinalDFSubset$`Alzheimers Deaths`*100000/ FinalDFSubset$Population))
    # convert death rate to natural log for log-normal distribution
    FinalDFSubset$`Log(Death per 100K)` <- log(FinalDFSubset$`Alzheimers Deaths`   )
  averageDeath <- summarize(group_by(FinalDFSubset, FIPS), Death = mean(`AZ Deaths Per 100K`, na.rm=T))
}
averageDeath.County <- merge(x=county_map, y=averageDeath, by ="FIPS", all.x=FALSE)
#=================================================================================
#   Add location information to each counties
#=================================================================================
averageDeath.County.ll <- spTransform(averageDeath.County, CRS("+proj=longlat +datum=WGS84 +no_defs"))
variable <- averageDeath.County.ll@data$Death

print("draw map")
draw_map(averageDeath.County.ll, variable, variable, maplegend_Deaths)

#====================================================
#
#                 Specify Equation 
#
#====================================================
if (Adjustment == TRUE) {

  #======================= (CONFIGURATION BLOCK 2 of 2 - START) ====================
   #formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population))  + `PM2_5GE_Threshold`  # THE CURENT YEAR
  #formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population))  + `PM2_5GT35LE50` + `PM2_5GT50`  # THE CURENT YEAR
  formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population))  + `PM2_5GT35LE50Last5Year` + `PM2_5GT50Last5Year`  #  Last 5 YEARS
                                        
                     + scale(`Poverty Percent, All Ages`) 
                     + scale(`% Smokers`)
                     + scale(`% Excessive Drinking`)
                     + scale(`% African American`)
                     + scale(`% Asian`) 
                     + scale(`% Hispanic`)
                     + scale(`% Age < 18`)
                     + scale(`% Age 65 and over`)
                     + scale(`% Obese`)
                     + scale(`% Unemployed`)
                     + scale(`% Uninsured`)
                     + scale(`Median Household Income`)
                     + scale(`% Rural`)
                     )
  
    } else {
  #    formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population))  + `PM2_5GE_Threshold`)
  #  formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population)) + `PM2_5GT35LE50` + `PM2_5GT50`) # THE CURENT YEAR
  formula1 = formula(`Log(Death per 100K)` ~ offset(log(Population)) + `PM2_5GT35LE50Last5Year` + `PM2_5GT50Last5Year`) # Last 5 YEARS
#======================= (CONFIGURATION BLOCK 2 of 2 -  END) ====================
    } # end if (Adjustment == TRUE) 
  
#=============================================================
#
#                   Regressions
#
#=============================================================

model_distribution = "gaussian"
#model_distribution = "poisson"

#=====================================================
#           General Linear Model Regression
#=====================================================
FinalDFSubset <- run_glm_and_compute_residuals (FinalDFSubset, formula1, model_distribution)

#==========================================================
#
#               Spacial Tempro Regression
#
#==========================================================

#=====================================================
#   Note: Use 2019 data to construct Adjacency Matrix
#=====================================================
DFSubset2015 <- filter(FinalDFSubset, Year==2019)
#====================================================
residuals2015 <- DFSubset2015

residuals2015.county <- merge(x=county_map, y=residuals2015, by.x ="FIPS", by.y="FIPS", all.x=FALSE, all.y=FALSE, duplicateGeoms = TRUE )

library(spdep)

W.nb <- poly2nb(residuals2015.county, row.names = residuals2015.county@data$FIPS)
W <- nb2mat(W.nb, style = "B", zero.policy = TRUE) 
#========================================================
#
# Note: the "for" loops change 0's to 0.1 diagonally in the adjacency matrix to cheat CARBayesST into accepting an "island" in the geological area
# An "island" represents a county with no data, which results in an entire row of 0's in the adjacency matrix; CARBayesST does not accept such an adjacency matrix
#
#========================================================
for (r in 1:nrow(W))
  for (c in 1:ncol(W))
    
    if (paste(W[r,c]) == '0') {
      W[r,c] <- 0.1
      
      if (r==c) {
        W[r,c] <- 0
      }
    }

W.list <- nb2listw(W.nb, style = "B", zero.policy = TRUE)

moran.mc(x = residuals2015.county$residuals, listw = W.list, nsim = 10000, zero.policy = TRUE,  na.action=na.omit)

lookup <- data.frame(FIPS=residuals2015.county@data$FIPS, spatialorder=1:nrow(residuals2015.county@data))
FinalDFSubset.temp <- merge(x=FinalDFSubset, y=lookup, by="FIPS")
FinalDFSubset.ordered <- arrange(FinalDFSubset.temp, Year, spatialorder)

library(CARBayesST)


chain1 <- ST.CARar(formula=formula1, family=model_distribution,
                   data=FinalDFSubset.ordered, W=W, burnin=200000, n.sample=2200000, thin=1000, verbose=TRUE, rho.S=NULL, rho.T=NULL,  AR=1)
chain2 <- ST.CARar(formula=formula1, family=model_distribution,
                   data=FinalDFSubset.ordered, W=W, burnin=200000, n.sample=2200000, thin=1000, verbose=TRUE, rho.S=NULL, rho.T=NULL,  AR=1)
chain3 <- ST.CARar(formula=formula1, family=model_distribution,
                   data=FinalDFSubset.ordered, W=W, burnin=200000, n.sample=2200000, thin=1000, verbose=TRUE, rho.S=NULL, rho.T=NULL,  AR=1)

#==========================================
#        Results: collect beta values
#==========================================
if (Accumulation == TRUE) {  # use PM2.5 accumulation data
print(paste(" number of days PM2.5 level >= threshold accumulated during the last ", Acc_N, " years PM2.5 level exceeds threshold",  sep=''))
} else {
  print(paste(" number of days PM2.5 level >= threshold  in the current year",  sep=''))
}
library(coda)
beta.samples <- mcmc.list(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta)


par(mar = c(1, 1, 1, 1))
plot(beta.samples)

gelman.diag(beta.samples)

print("Chain1:")
print(chain1)

#================================================================
#
#                     Inference
#
#================================================================    
sprintf("PM2.5 Threshold:  %d", PM25Threshold)
#================================================================
print(paste("SD for Log('", disease_deaths_per_100K, "'): ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == "Log(Death per 100K)")]), sep=''))
print(paste("SD for '", disease_deaths_per_100K, "': ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == disease_deaths_per_100K)]), sep=''))
#print(paste("SD for 'Acc Days >= Threshold': ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == 'PM2_5GE_Threshold')]), sep=''))
print(paste("SD for 'Acc Days >= Threshold': ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == 'PM2_5GT35LE50Last5Year')]), sep=''))
print(paste("SD for 'Acc Days >= Threshold': ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == 'PM2_5GT50Last5Year')]), sep=''))

if (Adjustment == TRUE) {
sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == col)])
  for (col in adjusting_factors){
    print(paste("SD for '", col, "': ", sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == col)]), sep=''))
  } # end 
}

beta.samples.combined <- rbind(chain1$samples$beta, chain2$samples$beta, chain3$samples$beta)

round(quantile(exp(sd(FinalDFSubset.ordered$`PM2_5GE_Threshold`) * beta.samples.combined[ ,2]), c(0.5, 0.025, 0.975)),3)
if (Adjustment == TRUE) {
for (col in adjusting_factors){  
  print(paste("quantile:",col, sep=''))
  print(round(quantile(exp(sd(FinalDFSubset.ordered[,which(colnames(FinalDFSubset.ordered) == col)]) * beta.samples.combined[ ,which(adjusting_factors == col)+2]), c(0.5, 0.025, 0.975)),3))
} # end for (col in adjusting_factors)
} # end (Adjustment == TRUE)

#====================================================
#
#     Compute risk distribution
#
#====================================================

fitted.samples.combined <- rbind(chain1$samples$fitted, chain2$samples$fitted, chain3$samples$fitted)
n.samples <- nrow(fitted.samples.combined)
n.all <- ncol(fitted.samples.combined)
risk.samples.combined <- fitted.samples.combined / matrix(rep(FinalDFSubset.ordered$Population, n.samples), nrow=n.samples, ncol=n.all, byrow=TRUE)

#### Compute the areal unit average risk for each year

N <- length(table(FinalDFSubset.ordered$Year))
risk.trends <- array(NA, c(n.samples, N))
for(i in 1:n.samples) {
  risk.trends[i, ] <- tapply(risk.samples.combined[i, ], FinalDFSubset.ordered$Year, mean)
}

time.trends <- as.data.frame(t(apply(risk.trends, 2, quantile, c(0.5, 0.025, 0.975))))
time.trends <- time.trends %>% mutate(Year=names(table(FinalDFSubset.ordered$Year)))
colnames(time.trends)[1:3] <- c("Median","LCI", "UCI")

#### Scatterplot
library(GGally)

ggplot(time.trends, aes(x = factor(Year), y = Median, group=1)) +
  geom_line(col="red") +
  geom_line(aes(x=factor(Year), y=LCI), col="red", lty=2) +
  geom_line(aes(x=factor(Year), y=UCI), col="red", lty=2) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Risk") +
  theme(text=element_text(size=16), plot.title=element_text(size=18, face="bold"))

#=================================================================================
#   Draw Risk Map for 2015
#
#   Note: the scale of legend needs some adjustment
#=================================================================================

risk.samples.2015 <- risk.samples.combined[ ,FinalDFSubset.ordered$Year==2015]
risk.2015 <- apply(risk.samples.2015, 2, median)
pep.2015 <- apply(risk.samples.2015 > 1, 2, mean)

residuals2015.county$risk.2015 <- risk.2015
residuals2015.county$pep.2015 <- pep.2015
residuals2015.county.ll <- spTransform(residuals2015.county, CRS("+proj=longlat +datum=WGS84 +no_defs"))

#draw_map(residuals2015.county.ll, risk.2015, risk.2015, "Risk 2015")

colours <- colorNumeric(palette = "YlOrBr", domain = residuals2015.county.ll@data$risk.2015, reverse=FALSE)
leaflet(data=residuals2015.county.ll) %>% 
  addTiles() %>% 
  addPolygons(fillColor = ~colours(risk.2015), 
              color="",
              fillOpacity = 0.7, weight = 1, smoothFactor = 0.5,
              opacity = 1.0) %>%
  addLegend(pal = colours, values = risk.2015, 
            opacity = 1, title= maplegend_Risks) %>%
  addScaleBar(position="bottomleft")
