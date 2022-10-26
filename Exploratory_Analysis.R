# Loading the NEI and SCC data frames from the .rds files.
SCC <- readRDS("./Source_Classification_Code.rds")
NEI <- readRDS("./summarySCC_PM25.rds")

#use ggplot library
library(ggplot2)

#getting short informations from emissions
str(NEI)

# getting the unique of years of emission from emissions
years <- unique(NEI$year)

#1. Have total emissions from PM2.5 decreased in the United States 
#from 1999 to 2008? Using the base plotting system, make a plot showing the total
#PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

# calculating the total emissions by year
total_y <- tapply(NEI$Emissions, NEI$year, sum)
total_y
png('plot1.png')
barplot(total_y,type="l",xlab = "Year",ylab="Total Emissions",main = 
       "Total Emissions in the U.S")
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland 
#(fips == “24510”) from 1999 to 2008? Use the base plotting system to make a 
#plot answering this question.

#Calulate total emissions of Baltimore city by year
Baltimore_emissions <- NEI[NEI$fips=="24510",]
total_Baltimore_y <- tapply(Baltimore_emissions$Emissions,Baltimore_emissions$year,sum)
total_Baltimore_y

png('plot2.png')
plot(total_Baltimore_y,type="l",xaxt ="n",xlab = "Year",ylab="Total Emissions",
     main = "Total Emissions in the Baltimore")
axis(side=1,labels=as.character(years),at=1:length(years))
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, 
#nonroad) variable, which of these four sources have seen decreases in emissions 
#from 1999 2008 for Baltimore City? Which have seen increases in emissions from 1999 2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
head(Baltimore_emissions)

# calculating emissions in baltimore city by year and type

sum_bal_yr_type <- aggregate(Emissions~year + type, Baltimore_emissions, sum)
head(sum_bal_yr_type)
png('plot3.png', width=640, height=480)
g <- ggplot(sum_bal_yr_type, aes(year, Emissions, color = type))
g1 <- g + geom_line() + xlab("year") +ylab(expression('Total PM'[2.5]*" Emissions")) +
  ggtitle('Total Emissions in Baltimore City, Maryland (fips == "24510") from 1999 to 2008')
print(g1)
dev.off()

#Across the United States, how have emissions from coal combustion-related 
#sources changed from 1999–2008?
str(SCC)
combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case = TRUE)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case = TRUE)
combustions <- (combustion & coal)

combustionSCC <- SCC[combustions,]$SCC
combustionNEI <- NEI[NEI$SCC %in% combustionSCC,]

png("plot4.png")
ggp <- ggplot(combustionNEI,aes(factor(year),Emissions/10^5)) +
  geom_bar(stat="identity",fill="grey",width=0.75) +
  theme_bw() +  guides(fill=FALSE) +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
  labs(title=expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))

print(ggp)

dev.off()

#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
png("plot5.png",width=480,height=480,units="px",bg="transparent")
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data to Baltimore's fip
baltimoreVehicles <- vehiclesNEI[vehiclesNEI$fips=="24510",]

#6.Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == “06037”). 
#Which city has seen gvehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data by each city's fip and add city name.
vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]
vehiclesBaltimoreNEI$city <- "Baltimore City"

vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesLANEI$city <- "Los Angeles County"

# Combine the two subsets with city name into one data frame
bothNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

png("plot6.png",width=480,height=480,units="px",bg="transparent")

ggp <- ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

print(ggp)

dev.off()


