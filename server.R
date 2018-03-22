
shinyServer(function(input, output) {
    dispStations<-readRDS("data/fullStations")
    windMat<-readRDS("data/fullStationsWindMat")
    dispStations<-windMat[,1:9]
    windMat<-windMat %>% select(-c(1:9)) %>% as.matrix()
    nBins<-33
    Bins<-seq(0.5,length.out=nBins,by=1)
    distReCreate<-function(windTable,bins){
        X<-NULL
        for (i in 1:length(bins)){
            X<-c(X,runif(windTable[i],bins[i]-0.5,bins[i]+0.5))
        }
        X
    }

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
        ##retreive station metrics
        idx<-which(dispStations$Name == click$id )[1]
        expDist<-windMat[idx,][which(windMat[idx,]>0)]
        bins<-Bins[which(windMat[idx,]>0)]
        len<-dispStations$len[idx]
        mw<-dispStations$meanWind[idx]
        sdw<-dispStations$sdWind[idx]
        podw<-dispStations$windDens[idx]
        wsm<-dispStations$windSupMean[idx]

        ##Method 1 - 1st moment
        k1<-2
        C1<-mw/gamma(1+1/k1)

        ##Method 2 - 1st et 2nd moments
        k2<-(sdw/mw)^(-1.086)
        C2<-mw/gamma(1+1/k2)

        ##Method 3 - 1st et 3rd moments
        Epf<-podw/mw^3
        optiRes<-optim(par = 2,
                       fn = function(k){abs(Epf-(gamma(1+3/k)/gamma(1+1/k)^3))},
                       method="BFGS",control=list(abstol=0.001))
        k3<-optiRes$par
        C3<-mw/gamma(1+1/k3)

        ##Method 4 - WASP method
        optiRes<-optim(par = 2,
                       fn = function(k){abs(mw^k/podw^(k/3)*gamma(1+3/k)^(k/3)+log(wsm))},
                       method="BFGS",control=list(abstol=0.0001))
        # optiRes<-optim(par = 2,
        #                fn = function(k){abs(gamma(1/k+1)^k+log(wsm))},
        #                method="BFGS",control=list(abstol=0.0001)) ##other expression
        k4<-optiRes$par
        C4<-(podw/gamma(1+3/k4))^(1/3)

        ##Cumulative Density Function for Weibull distributions
        CDFW<-function(V,k,C){1-exp(-(V/C)^k)}

        ##Goodness of fit computation function
        R2fun<-function(k,C,method=input$goodness.method[1]){
            if(is.null(method))
                NA
            else if(method=="R squared")
                round(1-sum((expDist/len-dweibull(bins,k,C))^2)/sum((expDist/len-mean(expDist/len))^2),digits=5)
            else if(method=="Cramer-Von Mises"){
                X<-sort(distReCreate(expDist,bins))
                round(1/(12*len)+sum((CDFW(X,k,C)-(1:len-0.5)/len)^2),digits = 4)
            } else if (method=="Anderson-Darling"){
                X<-sort(distReCreate(expDist,bins))
                round(-len-1/len*
                          sum((2*(1:len)-1)*
                                  (log(CDFW(X,k,C))+log(1-CDFW(X[len+1-(1:len)],k,C)))),
                      digits = 4)
            } else if(method=="RTAD"){
                X<-sort(distReCreate(expDist,bins))
                round(len/2+2*sum(CDFW(X,k,C))-1/len*
                          sum((2*(1:len)-1)*log(1-CDFW(X[len+1-(1:len)],k,C))),
                      digits = 4)

            } else if(method=="RTAD2"){
                X<-sort(distReCreate(expDist,bins))
                round(2*sum(log(1-CDFW(X,k,C)))+
                                    1/len*sum((2*(1:len)-1)/(1-CDFW(X[len+1-(1:len)],k,C))),
                      digit=4)
            } else
                NULL
        }

        ## computing goodness of fit
        R2M1<-R2fun(k1,C1);R2M2<-R2fun(k2,C2)
        R2M3<-R2fun(k3,C3);R2WAsP<-R2fun(k4,C4)

        ## ploting
        output$binsHist<- renderPlot({
            ##init legend infos
            N<-length(input$models)+1
            legDF<-data.frame(legendStr=character(N),pch=as.integer(N),
                              lty=as.integer(N),col=as.character(N),stringsAsFactors = F)
            legDF[1,]<-c("Exp",21,0,"black");ind<-2

            #ploting the experimental distribution
            if(input$logy)
                plot(bins,expDist/len,ylab="Wind distribution",xlab="bins (wind in m/s)",
                     pch=21,main="",xlim=c(0,max(bins)), log="y",type="b")
            else
                plot(bins,expDist/len,ylab="Wind distribution",xlab="bins (wind in m/s)",
                     pch=21,main="",xlim=c(0,max(bins)),type="b",
                     ylim=c(0,max(c(expDist/len,dweibull(bins,k1,C1)))+0.01))
            #ploting the fits
            if(length(grep("Rayleigh-Weibull",input$models))==1){
                lines(bins,dweibull(bins,k1,C1),col="red",lty=2)
                legDF[ind,]<-c(paste0("Rayleigh-Weibull. Goodness=",R2M1),NA_integer_,2,"red");ind<-ind+1
            }
            if(length(grep("Weibull M1 & M2",input$models))==1){
                lines(bins,dweibull(bins,k2,C2),col="blue",lty=2)
                legDF[ind,]<-c(paste0("Weibull M2. Goodness=",R2M2),NA_integer_,2,"blue");ind<-ind+1
            }
            if(length(grep("Weibull M1 & M3",input$models))==1){
                lines(bins,dweibull(bins,k3,C3),col="green",lty=2)
                legDF[ind,]<-c(paste0("Weibull M3. Goodness=",R2M3),NA_integer_,2,"green");ind<-ind+1}
            if(length(grep("WAsP",input$models))==1){
                lines(bins,dweibull(bins,k4,C4),col="orange",lty=2)
                legDF[ind,]<-c(paste0("WAsP. Goodness=",R2WAsP),NA_integer_,2,"orange");ind<-ind+1
            }
            # lines(c(mw,mw),c(0,1),col="black",lty=2)
            grid()
            legend(if(input$logy){"bottomleft"}else{"topright"},legend = legDF$legendStr,
                   pch=as.integer(legDF$pch),lty=as.integer(legDF$lty),col=legDF$col)
        })
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
