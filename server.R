#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dispStations<-readRDS("data/fullStations")
    windMat<-readRDS("data/fullStationsWindMat")
    bins<-0.5:29.5
    # Create the map
    output$map <- renderLeaflet({
        Emap<-dispStations %>% leaflet() %>%
            addTiles(
                urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
            ) %>%
            addCircleMarkers(lng=dispStations$lon,lat=dispStations$lat,color="green",
                             weight=1,radius=dispStations$meanWind,
                             popup=paste(dispStations$Name,format(dispStations$meanWind,digits=2),
                                         " m/s"),
                             layerId=dispStations$Name)
        Emap
    })
    ## get click info and print hist
    observe({
        click<-input$map_marker_click
        if(is.null(click))
            return({output$binsHist<-renderPlot(plot.new())})
        idx<-which(dispStations$Name == click$id )
        mw<-dispStations$meanWind[idx]
        k<-2
        fitWeibull<-k/mw*(bins/mw)^(k-1)*exp(-(bins/mw)^k)
        # fit.gamma <- fitdist(x, distr = "weibull", method = "mle")
        Rsquared<-1-sum((fitWeibull-windMat[idx,])^2)/sum((windMat[idx,]-mean(windMat[idx,]))^2)
        output$binsHist<- renderPlot({
            # ggplot(windMat[idx,])
            plot(bins,windMat[idx,],ylab="Wind distribution",xlab="bins (wind in m/s)",
                 ylim=c(0,ceiling(max(c(windMat[idx,],fitWeibull))*10)/10),pch=18,
                 main=paste0("Rsquared : ",format(Rsquared,digits=3)))
            lines(bins,fitWeibull,col="red",lty=2)
            grid()
            legend("topright",legend = c("measured distribution","Weibull fit"),
                   pch=c(18,NA_integer_),lty=c(0,2),col=c("black","red"))
        })
        # output$Click_text<-renderText({
        #     text2
        # })
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function(){
            paste("windData-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.table(data.frame(dispStations,windMat), file, row.names = FALSE)
        }
    )
})