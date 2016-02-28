library(shiny)
library(caret)

train<-read.csv("train.csv")

train$Wilderness_Area1=as.factor(train$Wilderness_Area1)
train$Wilderness_Area2=as.factor(train$Wilderness_Area2)
train$Wilderness_Area3=as.factor(train$Wilderness_Area3)
train$Wilderness_Area4=as.factor(train$Wilderness_Area4)
train$Soil_Type1=as.factor(train$Soil_Type1)
train$Soil_Type2=as.factor(train$Soil_Type2)
train$Soil_Type3=as.factor(train$Soil_Type3)
train$Soil_Type4=as.factor(train$Soil_Type4)
train$Soil_Type5=as.factor(train$Soil_Type5)
train$Soil_Type6=as.factor(train$Soil_Type6)
train$Soil_Type7=as.factor(train$Soil_Type7)
train$Soil_Type8=as.factor(train$Soil_Type8)
train$Soil_Type9=as.factor(train$Soil_Type9)
train$Soil_Type10=as.factor(train$Soil_Type10)
train$Soil_Type11=as.factor(train$Soil_Type11)
train$Soil_Type12=as.factor(train$Soil_Type12)
train$Soil_Type13=as.factor(train$Soil_Type13)
train$Soil_Type14=as.factor(train$Soil_Type14)
train$Soil_Type15=as.factor(train$Soil_Type15)
train$Soil_Type16=as.factor(train$Soil_Type16)
train$Soil_Type17=as.factor(train$Soil_Type17)
train$Soil_Type18=as.factor(train$Soil_Type18)
train$Soil_Type19=as.factor(train$Soil_Type19)
train$Soil_Type20=as.factor(train$Soil_Type20)
train$Soil_Type21=as.factor(train$Soil_Type21)
train$Soil_Type22=as.factor(train$Soil_Type22)
train$Soil_Type23=as.factor(train$Soil_Type23)
train$Soil_Type24=as.factor(train$Soil_Type24)
train$Soil_Type25=as.factor(train$Soil_Type25)
train$Soil_Type26=as.factor(train$Soil_Type26)
train$Soil_Type27=as.factor(train$Soil_Type27)
train$Soil_Type28=as.factor(train$Soil_Type28)
train$Soil_Type29=as.factor(train$Soil_Type29)
train$Soil_Type30=as.factor(train$Soil_Type30)
train$Soil_Type31=as.factor(train$Soil_Type31)
train$Soil_Type32=as.factor(train$Soil_Type32)
train$Soil_Type33=as.factor(train$Soil_Type33)
train$Soil_Type34=as.factor(train$Soil_Type34)
train$Soil_Type35=as.factor(train$Soil_Type35)
train$Soil_Type36=as.factor(train$Soil_Type36)
train$Soil_Type37=as.factor(train$Soil_Type37)
train$Soil_Type38=as.factor(train$Soil_Type38)
train$Soil_Type39=as.factor(train$Soil_Type39)
train$Soil_Type40=as.factor(train$Soil_Type40)
train$Cover_Type=as.factor(train$Cover_Type)
## Obtain new zero variance infor 
nearZeroVar(train,saveMetrics = TRUE)

## Remove zero variance and id field
train$Soil_Type7=NULL
train$Soil_Type15=NULL

train$Id=NULL

# Change names of outcome variables 
# else Caret won't be able to create confusion matrix
train$Cover_Type = as.character(train$Cover_Type)
train$Cover_Type[train$Cover_Type == "1"] <- "Seg1"
train$Cover_Type[train$Cover_Type == "2"] <- "Seg2"
train$Cover_Type[train$Cover_Type == "3"] <- "Seg3"
train$Cover_Type[train$Cover_Type == "4"] <- "Seg4"
train$Cover_Type[train$Cover_Type == "5"] <- "Seg5"
train$Cover_Type[train$Cover_Type == "6"] <- "Seg6"
train$Cover_Type[train$Cover_Type == "7"] <- "Seg7"
train$Cover_Type = as.factor(train$Cover_Type)

# Save all the features col names used to predict
x_vars = setdiff(names(train),c("Cover_Type"))


set.seed(1234)	
tuneGrid <-  expand.grid(n.trees = c(100),interaction.depth = c(8) ,shrinkage = 0.2, n.minobsinnode=10)
fitControl <- trainControl(method = "none", classProbs = TRUE)
GBMmodel <- train(Cover_Type ~ .,data = train,method = "gbm",trControl = fitControl,tuneGrid=tuneGrid,verbose = TRUE,metric = "ROC")


shinyServer(
  function(input, output){
    
    elevation  = reactive({input$elevation})
    aspect = reactive({input$aspect})
    slope = reactive({input$slope})
    horiz_dist_hydr=reactive({input$horiz_dist_hydr})
    vert_dist_hydr=reactive({input$vert_dist_hydr})
    horiz_dist_road=reactive({input$horiz_dist_road})
    hill_shade_9=reactive({input$hill_shade_9})
    hill_shade_noon=reactive({input$hill_shade_noon})
    hill_shade_3=reactive({input$hill_shade_3})
    horiz_dist_fire=reactive({input$horiz_dist_fire})
    wilderness = reactive({substr(input$wilderness,1,2)})
    soiltype= reactive({substr(input$soil_type,1,2)})
    
    
    output$elevationText<-renderText({paste("Elevation (meters):",elevation())})
    output$aspectText<-renderText({paste("Aspect (degrees):",aspect())})
    output$slopeText<-renderText({paste("Slope (degrees):",slope())})
    output$horiz_dist_hydrText<-renderText({paste("Horizontal Distance To Hydrology(meters):",horiz_dist_hydr())})
    output$vert_dist_hydrText<-renderText({paste("Vertical Distance To Hydrology (meters):",vert_dist_hydr())})
    output$horiz_dist_roadText<-renderText({paste("Horizontal Distance To Roadways(meters):",horiz_dist_road())})
    output$hill_shade_9Text<-renderText({paste("Hillshade at 9am of Summer Solstice(0 to 255):",hill_shade_9())})
    output$hill_shade_noonText<-renderText({paste("Hillshade  at Noon of Summer Solstice(0 to 255):",hill_shade_noon())})
    output$hill_shade_3Text<-renderText({paste("Hillshade at 3pm of Summer Solstice(0 to 255):",hill_shade_3())})
    output$horiz_dist_fireText<-renderText({paste("Horizontal Distance To Wildfire Ignition Points(meters):",horiz_dist_fire())})
    output$wildernessText<-renderText({paste("Wilderness Area:",wilderness())})
    output$soil_typeText<-renderText({paste("Soil Type:",soiltype())})

    
    userInput<- reactive({
      data.frame(
        Elevation=c(as.numeric(elevation())),
        Aspect=c(as.numeric(aspect())),
        Slope=c(as.numeric(slope())),
        Horizontal_Distance_To_Hydrology=c(as.numeric(horiz_dist_hydr())),
        Vertical_Distance_To_Hydrology=c(as.numeric(vert_dist_hydr())),
        Horizontal_Distance_To_Roadways=c(as.numeric(horiz_dist_road())),
        Hillshade_9am=c(as.numeric(hill_shade_9())),
        Hillshade_Noon=c(as.numeric(hill_shade_noon())),
        Hillshade_3pm=c(as.numeric(hill_shade_3())),
        Horizontal_Distance_To_Fire_Points=c(as.numeric(horiz_dist_fire())),
        Wilderness_Area1=if(as.numeric(wilderness())==1) as.factor(c(1)) else as.factor(c(0)),
        Wilderness_Area2=if(as.numeric(wilderness())==2) as.factor(c(1)) else as.factor(c(0)),
        Wilderness_Area3=if(as.numeric(wilderness())==3) as.factor(c(1)) else as.factor(c(0)),
        Wilderness_Area4=if(as.numeric(wilderness())==4) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type1=if(as.numeric(soiltype())==1) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type2=if(as.numeric(soiltype())==2) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type3=if(as.numeric(soiltype())==3) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type4=if(as.numeric(soiltype())==4) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type5=if(as.numeric(soiltype())==5) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type6=if(as.numeric(soiltype())==6) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type8=if(as.numeric(soiltype())==8) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type9=if(as.numeric(soiltype())==9) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type10=if(as.numeric(soiltype())==10) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type11=if(as.numeric(soiltype())==11) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type12=if(as.numeric(soiltype())==12) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type13=if(as.numeric(soiltype())==13) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type14=if(as.numeric(soiltype())==14) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type16=if(as.numeric(soiltype())==16) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type17=if(as.numeric(soiltype())==17) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type18=if(as.numeric(soiltype())==18) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type19=if(as.numeric(soiltype())==19) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type20=if(as.numeric(soiltype())==20) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type21=if(as.numeric(soiltype())==21) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type22=if(as.numeric(soiltype())==22) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type23=if(as.numeric(soiltype())==23) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type24=if(as.numeric(soiltype())==24) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type25=if(as.numeric(soiltype())==25) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type26=if(as.numeric(soiltype())==26) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type27=if(as.numeric(soiltype())==27) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type28=if(as.numeric(soiltype())==28) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type29=if(as.numeric(soiltype())==29) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type30=if(as.numeric(soiltype())==30) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type31=if(as.numeric(soiltype())==31) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type32=if(as.numeric(soiltype())==32) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type33=if(as.numeric(soiltype())==33) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type34=if(as.numeric(soiltype())==34) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type35=if(as.numeric(soiltype())==35) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type36=if(as.numeric(soiltype())==36) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type37=if(as.numeric(soiltype())==37) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type38=if(as.numeric(soiltype())==38) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type39=if(as.numeric(soiltype())==39) as.factor(c(1)) else as.factor(c(0)),
        Soil_Type40=if(as.numeric(soiltype())==40) as.factor(c(1)) else as.factor(c(0)))
    })
    
    prediction <- reactive({
      prediction <- predict(GBMmodel,newdata = userInput())
    })
    predictionText <- reactive({
      p<-as.numeric(prediction())
      p
      predictionText <- 
        if(p==1) "Spruce/Fir" else
          if(p==2) "Lodgepole Pine" else
            if(p==3) "Ponderosa Pine" else
              if(p==4) "Cottonwood/Willow" else
                if(p==5) "Aspen" else
                  if(p==6) "Douglas-fir" else
                    if(p==7) "Krummholz" else "Unknown Tree Type"
    })
    
    output$prediction<- renderText({predictionText()})
        
  }
)