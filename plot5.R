plot5 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)
        library(ggplot2)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")                           

        vehSCC <- filter(SCC, grepl("Veh", Short.Name))
        mergedNEI <-merge(NEI, vehSCC, by="SCC", all = FALSE) 

        sum <- select(mergedNEI, Emissions, year, fips) %>% filter(fips == 24510) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))
        
        p <- ggplot(sum, aes(year, Emissions))
        
        p + 
        geom_point() + 
        stat_smooth(method="lm", se=FALSE) + 
        ggtitle("PM2.5 Emissions in Baltimore City \n from motor vehicle sources")

        dev.copy(png, file = "plot5.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot5()