plot4 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)
        library(ggplot2)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")            
        
        coalSCC <- filter(SCC, grepl("Ext Comb|Int Comb", Short.Name) & grepl("Coal", Short.Name))
        mergedNEI <-merge(NEI, coalSCC, by="SCC", all = FALSE) 

        sum <- select(mergedNEI, Emissions, year) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

        View(sum)
        
        p <- ggplot(sum, aes(year, Emissions))
        p + 
        geom_point() + 
        stat_smooth(method="lm", se=FALSE) + 
        ggtitle("PM2.5 Emissions \n from coal combustion-related sources")

        dev.copy(png, file = "plot4.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot4()