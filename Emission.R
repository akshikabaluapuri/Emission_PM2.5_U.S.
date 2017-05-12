# Examining the decreased PM2.5 Emission: 1999-2008.

if(!file.exists(".//air_pollution")){
  dir.create(".//air_pollution")
}
unzip(zipfile =  ".//air_pollution/exdata%2Fdata%2FNEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#1 Reading data
totalemission <- tapply(NEI$Emissions,NEI$year,sum)
# png file
png('plot1.png')
# plotting
barplot(totalemission,xlab="years",ylab = "TotalEmission(ton)",
        main="Toatl emission per year")
dev.off()



#2 Reading and subsetting data
baltimore <- subset(NEI, fips == "24510")
# summing emission per year
totalemission <- tapply(baltimore$Emissions,baltimore$year,sum)
# png file
png('plot2.png')
#plotting
barplot(totalemission,xlab = "year",ylab = "Total Emission(ton)",
        main= "Total emission per year in Baltimore")
dev.off()




#3 Reading and subsetting data
baltimore <- subset(NEI, fips == "24510")
baltimore_type<- aggregate(baltimore[c("Emissions")],list(type=baltimore$type,year=baltimore$year),sum)
# png file
png("plot3.png")
# plotting
qplot(year,Emissions,data =baltimore_type,color=type,geom="line")+
  ggtitle("Total Emission Baltimore County by Sources Type")+xlab("year")+ylab("PM2.5 Emissions")
dev.off()


#4 Reading and subsetting data
balti_scc_coal <- SCC[grepl("Coal", SCC$Short.Name),]
balti_nei_coal <- NEI[NEI$SCC %in% balti_scc_coal$SCC ,]
# png file
png("plot4.png")
# plotting
ggplot(balti_nei_coal,aes(x=factor(year),y=Emissions,fill=type))+geom_bar(stat="identity",
   width=.4)+xlab("year")+ylab("Coals Related PM2.5 Emission")+
  ggtitle("Total Coal-Related PM2.5 Emission")
dev.off()




#5 Baltimore Datab (ON-ROAD)
balti_vehicle <- subset(NEI, fips == "24510" & type == "ON-ROAD")
balti_v_agg <- aggregate(balti_vehicle[c("Emissions")], list(type=balti_vehicle$type,
                                                             year=balti_vehicle$year,
                                                             zip=balti_vehicle$fips ),sum)
# png file
png("plot5.png")
# plotting
qplot(year,Emissions,data =balti_v_agg ,color=type,geom="line")+theme_gray()+
  ggtitle("Total Emission Baltimore County")+xlab("year")+ylab("EmissionsLevels")
dev.off()



#6 Baltimore Data 
balti_vehicle <- subset(NEI, fips == "24510" & type == "ON-ROAD")
balti_v_agg <- aggregate(balti_vehicle[c("Emissions")], list(type=balti_vehicle$type,
                                                             year=balti_vehicle$year,
                                                             zip=balti_vehicle$fips ),sum)
# Los-Angeles Data
la <- subset(NEI, fips=="06037" & type=="ON-ROAD")
la_agg <- aggregate(la[c("Emissions")],list(type=la$type,year=la$year,zip=la$fips ),sum)
# Row binding( Baltimore and Los Angeles)
comp_la_balti <- rbind(balti_v_agg,la_agg)
#png file
png("plot6.png")
# Comparing by plotting 
qplot(year,Emissions,data =comp_la_balti ,color=zip,geom="line", ylim = c(-100,5500))+
  ggtitle("Motor Vehicle Emissions in Baltimore City(24510) v/s Los Angeles County(06037")+
  xlab("year")+ylab("EmissionsLevels")
dev.off()
