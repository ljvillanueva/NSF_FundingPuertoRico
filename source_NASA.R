library(DBI)
library(RMySQL)
library(ggplot2)

#Open MySQL connection
drv <- dbDriver("MySQL")
con <- dbConnect(drv, user="", password = "", host="localhost", dbname = "NSFfunding")

#Execute query
res <- dbSendQuery(con, "SELECT COUNT(*) AS no_awards, State, Year, SUM(FundsToDate) AS Funding, IF(State = 'PR',1,0) AS PR from funding WHERE State NOT IN('12', 'NA', 'ON') AND Agency LIKE 'NASA' GROUP BY State, Year")

#Assign results to data1
data1 <- fetch(res, n = -1)
#Get number of records
no_records<-dim(data1)[1]

attach(data1)

head(data1)

funding_change <- data.frame()
for (i in 1:no_records){
	datapoint<-data1[i,]
	j <- i-1
	if (datapoint$Year != "2007"){
		prev_datapoint <- data1[j,]
		this_year <- as.numeric(datapoint$Year)
		past_year <- this_year - 1
		past_year <- as.character(past_year)
		
		this_val <- round(((datapoint$Funding - prev_datapoint$Funding) /  prev_datapoint$Funding) * 100, 1)
		cat(paste(datapoint$State, " ", past_year, ": ", this_val,"%\n", sep=""))
		cat(datapoint$State, datapoint$Year, this_val, datapoint$PR)
		cat("\n")
		funding_change <- rbind(funding_change, cbind(datapoint$State, datapoint$Year, this_val, datapoint$Funding, datapoint$PR))
	}
}

names(funding_change) <- c("State", "Year", "Diff", "Funding", "PR")
funding_change <- transform(funding_change, State = as.character(State), 
							Year = as.character(Year), 
							Diff = as.numeric(as.character(Diff)),
							Funding = as.numeric(as.character(Funding)),
							PR = as.factor(PR)
							)

head(funding_change)

funding_change$Diff <- as.numeric(as.character(funding_change$Diff))

png(file="funding_comparison_NASA.png",height=600,width=1000)

  p <- ggplot(funding_change, aes(Year, Diff))
  p + geom_point(aes(colour = PR, alpha=funding_change$PR), size=12) + scale_y_continuous() + geom_abline(intercept = 0, slope = 0) + labs(x = "Año", y="Cambio en fondos comparado al año anterior (%)") + opts(plot.title = theme_text(size=20), axis.title.x = theme_text(size=18), axis.title.y = theme_text(size=18, angle=90), axis.text.x = theme_text(size=16), axis.text.y = theme_text(size=16), legend.position="none")

dev.off()
png(file="funding_byyear_NASA.png",height=600,width=1000)

  p <- ggplot(data1, aes(Year, Funding/1000000))
  p + geom_point(aes(colour = PR, alpha=data1$PR), size=12) + scale_y_continuous()  + opts(plot.title = theme_text(size=20), axis.title.x = theme_text(size=18), axis.title.y = theme_text(size=18, angle=90), axis.text.x = theme_text(size=16), axis.text.y = theme_text(size=16), legend.position="none") + labs(x = "Año", y= "Fondos (millones de dólares)")

dev.off()
