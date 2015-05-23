plot3 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)
        library(ggplot2)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")            
        
        sum <- select(NEI, Emissions, year, fips, type) %>% filter(fips == 24510) %>% group_by(year, type) %>% summarise(Emissions = sum(Emissions))

        p <- ggplot(sum, aes(year, Emissions))
        p + 
        geom_point() + 
        facet_wrap( ~ type, ncol = 2) + 
        stat_smooth(method="lm", se=FALSE)

        dev.copy(png, file = "plot3.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot3()