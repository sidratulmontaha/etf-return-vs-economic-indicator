library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)
library(dplyr)
library(sqldf)
library(xlsx)
options(scipen=999)

setAs("character","oecd_date", function(from) as.Date(paste0("01-",from),format="%d-%b-%Y"))
indicator_database <- read.csv(file = "OECD.csv", header = TRUE, stringsAsFactors=FALSE, colClasses = c(rep("character",9),"oecd_date",rep("character",6),"numeric",rep("character",2)))
#excel formula: = "country_etfs <- rbind(country_etfs,c(""&TRIM(LEFT(A2,FIND("~",SUBSTITUTE(A2&"","","~",MAX(1,LEN(A2)-LEN(SUBSTITUTE(A2&"","","")))))))&"",""&TRIM(RIGHT(SUBSTITUTE(A2,"",REPT("",100)),100))&"'))"

country_etfs <- matrix(ncol = 2,nrow = 0)
country_etfs <- rbind(country_etfs, c("China (People's Republic of)",'GXC'))
country_etfs <- rbind(country_etfs, c('Nigeria', 'NGE'))
country_etfs <- rbind(country_etfs, c('Austria', 'EWO'))
country_etfs <- rbind(country_etfs, c('Greece', 'GREK'))
country_etfs <- rbind(country_etfs, c('Vietnam', 'VNM'))
country_etfs <- rbind(country_etfs, c('Chile', 'ECH'))
country_etfs <- rbind(country_etfs, c('Italy', 'EWI'))
country_etfs <- rbind(country_etfs, c('Portugal', 'PGAL'))
country_etfs <- rbind(country_etfs, c('Korea', 'EWY'))
country_etfs <- rbind(country_etfs, c('Turkey', 'TUR'))
country_etfs <- rbind(country_etfs, c('Netherlands', 'EWN'))
country_etfs <- rbind(country_etfs, c('France', 'EWQ'))
country_etfs <- rbind(country_etfs, c('India', 'PIN'))
country_etfs <- rbind(country_etfs, c('Thailand', 'THD'))
country_etfs <- rbind(country_etfs, c('South Africa', 'EZA'))
country_etfs <- rbind(country_etfs, c('Spain', 'EWP'))
country_etfs <- rbind(country_etfs, c('Hong Kong', 'EWH'))
country_etfs <- rbind(country_etfs, c('Bengium', 'EWK'))
country_etfs <- rbind(country_etfs, c('Singapore', 'EWS'))
country_etfs <- rbind(country_etfs, c('Germany', 'EWG'))
country_etfs <- rbind(country_etfs, c('Malaysia', 'EWM'))
country_etfs <- rbind(country_etfs, c('Brazil', 'EWZ'))
country_etfs <- rbind(country_etfs, c('Peru', 'EPU'))
country_etfs <- rbind(country_etfs, c('Egypt', 'EGPT'))
country_etfs <- rbind(country_etfs, c('Taiwan', 'EWT'))
country_etfs <- rbind(country_etfs, c('Japan', 'EWJ'))
country_etfs <- rbind(country_etfs, c('Switzerland', 'EWL'))
country_etfs <- rbind(country_etfs, c('Indonesia', 'IDX'))
country_etfs <- rbind(country_etfs, c('United Kingdom', 'EWU'))
country_etfs <- rbind(country_etfs, c('Sweden', 'EWD'))
country_etfs <- rbind(country_etfs, c('Mexico', 'EWW'))
country_etfs <- rbind(country_etfs, c('New Zealand', 'ENZL'))
country_etfs <- rbind(country_etfs, c('Columbia', 'GXG'))
country_etfs <- rbind(country_etfs, c('Australia', 'EWA'))
country_etfs <- rbind(country_etfs, c('Israel', 'EIS'))
country_etfs <- rbind(country_etfs, c('Canada', 'EWC'))
country_etfs <- rbind(country_etfs, c('Saudi Arabia', 'KSA'))
country_etfs <- rbind(country_etfs, c('Philippines', 'EPHE'))
country_etfs <- rbind(country_etfs, c('United Arab Emirates', 'UAE'))
country_etfs <- rbind(country_etfs, c('Ireland', 'EIRL'))
country_etfs <- rbind(country_etfs, c('Poland', 'PLND'))
country_etfs <- rbind(country_etfs, c('United States', 'EUSA'))
country_etfs <- rbind(country_etfs, c('Norway', 'NORW'))
country_etfs <- rbind(country_etfs, c('Qatar', 'QAT'))
country_etfs <- rbind(country_etfs, c('Denmark', 'EDEN'))
country_etfs <- rbind(country_etfs, c('Finland', 'EFNL'))
country_etfs <- rbind(country_etfs, c('Russia', 'ERUS'))
country_etfs <- rbind(country_etfs, c('Czech Republic', 'IEER.MI'))
country_etfs <- rbind(country_etfs, c('Slovenia', 'IEER.MI'))
country_etfs <- rbind(country_etfs, c('Hungary', 'IEER.MI'))
country_etfs <- rbind(country_etfs, c('Euro area (19 countries)', 'EZU'))
country_etfs <- rbind(country_etfs, c('Slovak Republic', 'IEER.MI'))

oecd_exclusion_list <- c("Estonia" , "OECD + Major Six NME", "Major Five Asia", "NAFTA", "Four Big European", "OECD - Europe", "G7", "OECD - Total") 
indicator_exclusion_list <-  c("Leading Indicators OECD > Component series > Share prices > Normalised")

indicator_countries <- unique(indicator_database[,"Country"])
indicator_list <- unique(indicator_database[, "Subject"])
indicator_list <- indicator_list[ends_with("Normalised",FALSE, indicator_list)]

regression_result <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(regression_result) <-  c("country","indicator", "intercept_estimate", "intercept_p_value" ,"r_squared")

for(country in indicator_countries){
  if(country %in% oecd_exclusion_list){
    next() 
  }

  country_etf <- country_etfs[country_etfs[, 1] == country,][2]
  quotes <- quantmod::getSymbols(Symbols = country_etf, src = "yahoo", adjust = TRUE, auto.assign = FALSE) 

  quotes_monthly <- monthlyReturn(quotes)
  quotes_monthly <- data.frame(date=index(quotes_monthly), coredata(quotes_monthly))
  quotes_monthly$date <- format(quotes_monthly$date,"%b-%Y")
  colnames(quotes_monthly) <- c("month", "monthly_return")

  print(paste0("Country: ", country, " ETF: " , country_etf))

  for(indicator in indicator_list){ 

    #country <- "Hungary" 

    #indicator <- "Leading Indicators OECD > Leading indicators > CLI > Normalised" 

    if(indicator %in% indicator_exclusion_list){
      next() 
    }
    country_indicator <- indicator_database[indicator_database['Country'] == country & indicator_database['Subject'] == indicator,]
    if(nrow(country_indicator) ==0){
      next() 
    }
    indicator_df <- subset(country_indicator,select = c('Time','Value'))
    indicator_df <- indicator_df[order(as.Date(indicator_df$Time)),]
    indicator_df$Time <- format(indicator_df$Time,"%b-%Y")
    indicator_df$indicator_change <- indicator_df$Value/lag(indicator_df$Value, 1) - 1
    indicator_df <- na.omit(indicator_df)
    indicator_df$Value <- NULL 
    
    colnames(indicator_df) <- c("month", "indicator_change")
    merged_indicator_quote <- merge(indicator_df, quotes_monthly, by="month")

    #Lagged returns vs indicator
    #merged_indicator_quote <- transform(merged_indicator_quote, monthly_return = c(merged_indicator_quote$monthly_return[-l], NA))
    
    model <- lm(formula = merged_indicator_quote$monthly_return ~ merged_indicator_quote$indicator_change, data = merged_indicator_quote)
    
    model_summary <- summary(model)
    #scatter.smooth(x=merged_indicator_quote$indicator_change,y=merged_indicator_quote$monthly_return, main = "Monthly return vs indicator change")
    
    regression_result[nrow(regression_result) +1,] <- list(country,indicator, model_summary[['coefficients']][2,1],model_summary[['coefficients']][2,4], model_summary[['r.squared']])
    #model_summary
  }
}
