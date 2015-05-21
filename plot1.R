plot1 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")
            
        
        sum <- select(NEI, Emissions, year) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))
        barplot(sum$Emissions, names.arg = sum$year, xlab="Years", ylab = "Emissions")

        dev.copy(png, file = "plot1.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot1()