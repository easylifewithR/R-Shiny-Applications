library(shiny)
library(EBImage)
library(dplyr)
library(ggmap)
library(googleAuthR)
library(RoogleVision)
# setwd("/Users/akashgupta/Documents/Learning/RShiny/ImageXRay")

#options("googleAuthR.webapp.client_id" = client_id)
#options("googleAuthR.webapp.client_secret" = secret)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))
#gar_auth()
service_token <- gar_auth_service(json_file="Vision2ServiceAccount.json")

color = c("red", "green", "blue", "orange", "yellow", "violet", "magenta", "indianred", "orchid", "pink", "gray")  #different color to circle the face

toTitle = function(x) {
  s = strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

toTitleUnderScore = function(x) {
  s = strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
}

shinyServer(function(input, output, session) { 

  # Extracting a file path
  imagepath = reactive({
     if(is.null(input$image)) return(paste0("Image", sample(4:6, 1), ".jpg"))
    #if(is.null(input$image)) return(paste0("Image", 5, ".jpg")) 
    input$image$datapath
  })
  
  # Finding the emotion in image if there is a face
  emotion = reactive({
      temp = getGoogleVisionResponse(imagepath(), feature = "FACE_DETECTION")
  })

  # Produce file either original or with face detection
  output$yourImage = renderImage({
    temp = emotion()
    if(sum(names(temp) %in% c("error")) >= 1){
      list(src = imagepath(), height = "100%", width = "100%", align = "middle")
    }
    else{ #create an image with face identified
      temp = temp %>% select(fdBoundingPoly)
      if (sum(grep("\\b.PNG\\b", imagepath())) == 1 || sum(grep("\\b.png\\b", imagepath())) == 1){ #Checking if the extracted file has.png string in it
      image1 = readImage(files = imagepath(), type = "png")
      png("faceDetection.png")}  # still need to work how to get around both jpeg and png files
      else{
      image1 = readImage(files = imagepath(), type = "jpeg") 
      jpeg("faceDetection.jpg")}
      plot(image1)
      for (i in 1:nrow(temp)){
        # i = 1
        xdim = temp$fdBoundingPoly$vertices[[i]][1][[1]]
        ydim = temp$fdBoundingPoly$vertices[[i]][2][[1]]
        polygon(x= xdim, y=ydim, lwd=4, border = color[i])
      }
      dev.off()
      if (sum(grep("\\b.PNG\\b", imagepath())) == 1)
        return(list(src ="faceDetection.png", height = "100%", width = "100%", align = "middle"))
      else
        return(list(src ="faceDetection.jpg", height = "100%", width = "100%", align = "middle"))
    }
  }, deleteFile = F)
  
  
  output$object = renderTable({
      temp = getGoogleVisionResponse(imagepath(), feature = "LABEL_DETECTION")
      temp = temp %>% select(description)
      temp = head(temp, 5)
      temp = rename(temp, Clues = description)
      specCap = sapply(temp$Clues, toTitle) #Capitilizing the first letter of each specification
      temp$Clues = specCap
      rm(specCap)
      return(temp)
  })
  
  landmark = reactive({
      temp = getGoogleVisionResponse(imagepath(), feature = "LANDMARK_DETECTION")
  })
  
  output$landmarkBest = renderText({
    temp = landmark()
    # if (imagepath() == "NULL"){
    #  temp = getGoogleVisionResponse("OkStateLibrary.jpg", feature = "LANDMARK_DETECTION")  
    #   temp = temp %>% select(description, score) }
    # else{
    # temp = getGoogleVisionResponse(imagepath(), feature = "LANDMARK_DETECTION")
    if(sum(names(temp) %in% c("error")) >= 1){
      return(paste("No landmark found"))}
    else{
        temp = temp[which.max(temp$score), ]
        if(nrow(temp) >1){
          rand = sample(1:nrow(temp), 1)
          temp = temp[rand, ]
          return(paste0("Image X-Ray identifies this image is of ", temp$description, "with", temp$score, "%confidence" ))}
        else{
          lat = temp$locations[[1]]$latLng$latitude
          lon = temp$locations[[1]]$latLng$longitude
          temp1 = revgeocode(c(lon, lat), output = "more")
          return(paste0(temp$description, " located at ", temp1$locality, ", ", temp1$country,"."))}
      }
    })
  
  output$landmarkOther = renderTable({
    temp = landmark()
    if(sum(names(temp) %in% c("error")) >= 1 || nrow(temp) <= 1){
      return(NULL)}
    else{
      temp = temp[!(1:nrow(temp) %in% which.max(temp$score)), ]
      temp = temp %>% select(description, score)
      temp = rename(temp, "Other Suggested Landmark" = description, "Confidence" = score)
      return(temp)
    }
  })
  
  output$emotion = renderTable({
    temp = emotion()
    if(sum(names(temp) %in% c("error")) >= 1){
      # temp = "Sorry, we do not recognize any face"
      # temp = as.data.frame(temp)
      # names(temp)[1] = "Warning"
      return(NULL)
    }
    else{
      # Following code when we want to output the emotion
      temp = temp %>% select(joyLikelihood, sorrowLikelihood, angerLikelihood)
      names(temp) = c("Happy", "Sad", "Anger")
      personNumber = paste0(rep("Person", nrow(temp)), 1:nrow(temp), " (", color[1:nrow(temp)],")")
      temp = cbind(Person = personNumber, temp)
      temp$Happy = toTitleUnderScore(temp$Happy)
      temp$Sad = toTitleUnderScore(temp$Sad)
      temp$Anger = toTitleUnderScore(temp$Anger)
      #temp$Surprise = toTitleUnderScore(temp$Surprise)
      return(temp)
    }
  })
  
})
