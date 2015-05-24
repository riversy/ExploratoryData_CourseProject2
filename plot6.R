plot6 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)
        library(ggplot2)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")                           

        vehSCC <- filter(SCC, grepl("Veh", Short.Name))
        mergedNEI <-merge(NEI, vehSCC, by="SCC", all = FALSE) 

        sum <- select(mergedNEI, Emissions, year, fips) %>% filter(fips %in% c("24510", "06037")) %>% group_by(year, fips) %>% summarise(Emissions = sum(Emissions))

        p <- ggplot(sum, aes(year, Emissions, colour=fips))
        
        p + 
        geom_point() + 
        stat_smooth(method="lm", se=FALSE)+
        ggtitle("Comparsion of PM2.5 Vehicale Emissions \n between Baltimore City (24510) and Los Angeles Country (06037)")

        dev.copy(png, file = "plot6.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot6()