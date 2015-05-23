plot2 = function() {
  
    list = dir()
    
    if (match("summarySCC_PM25.rds", list) && match("Source_Classification_Code.rds", list)){
        
        library(dplyr)

        NEI <- readRDS("summarySCC_PM25.rds")
        SCC <- readRDS("Source_Classification_Code.rds")            
        
        sum <- select(NEI, Emissions, year, fips) %>% filter(fips == 24510) %>% group_by(year) %>% summarise(Emissions = sum(Emissions))

        with(sum, 
            plot(
                year,
                Emissions, 
                pch = 19,
                xlab="Years",  
                ylab = "Emissions, tons",
                main = "PM2.5 in the Baltimore City, Maryland"
            )
        ) 

        model <- lm(Emissions ~ year, sum)    
        abline(model, col = "green") 

        dev.copy(png, file = "plot2.png")
        dev.off()
        
    } else {
        print("Can't find proper files.")
    }
}

plot2()