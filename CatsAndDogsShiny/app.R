library(shiny)

server <- shinyServer(function(input, output, session) {
  
  #colMax <- function(data) sapply(data, max, na.rm = TRUE)
  #colMin <- function(data) sapply(data, min, na.rm = TRUE)
  
  #Preparar red neuronal
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
  setwd("..")
  datos_minmax=read.csv("MinMaxValuesBueno.csv")
  datos_minmax[1] <- NULL 
  
  library(neuralnet)
  
  # MODELO
  # -----------------------------------------------------
  modelo <- readRDS("./final_model.rds")
  
  output$files <- renderTable(input$files)
  
  files <- reactive({
    files <- input$files
    files$datapath <- gsub("\\\\", "/", files$datapath)
    files
  })
  
  #Añado la función para obtener el csv
  source("funcionCSV.R")
  
  output$images <- renderUI({
    if(is.null(input$files)) return(NULL)
    image_output_list <- 
      lapply(1:nrow(files()),
             function(i)
             {
               imagename = paste0("image", i)
               imageOutput(imagename, width = '300px')
             })
    
    do.call(tagList, image_output_list)
  })
  
  getData <- reactive({
    if(is.null(input$files)) return(NULL)
    return (TRUE)
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(getData()))
  })
  
  observe({
    if(is.null(input$files)) return(NULL)
    for (i in 1:nrow(files()))
    {
      print(i)
      local({
        my_i <- i
        imagename = paste0("image", my_i)
        print(imagename)
        output[[imagename]] <- 
          renderImage({
            width  <- 300
            height <- 300
            list(src = files()$datapath[my_i],
                 width = width,
                 height = height,
                 alt = "Error al renderizar la imagen")
          }, deleteFile = FALSE)
      })
    }
    #Convertir a CSV la imagen
    print(input$files$datapath)
    datagram <- NULL
    datagram <- crearCSVImagen(input$files, datos_minmax)
    print(datagram)
    file.remove("actual .csv")
    write.csv(datagram,file=paste("actual", ".csv"))
    #filename = paste(input$files$name, ".csv")
    #print("prueba")
  })
  
  observeEvent(input$predecir, {
    #Aquí habría que poner la prediccion con la red neuronal
    datos_test <- read.csv(paste("actual", ".csv"))
    datos_test[1]<-NULL
    datos_test$animaltype=0
    #print(datos_test)
    prediccion  <- compute(modelo, within(datos_test,rm(animaltype)))
    #print(prediccion)
    valor <- prediccion$net.result
    print(valor[1,1])
    updateSliderInput(session, "slider1", value = valor[1,1],
                      min =0, max = 1, step = 0.01)
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
})

ui <- shinyUI(fluidPage(
  titlePanel("Cats and Dogs"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = 'files', 
                label = 'Selecciona un perro o un gato',
                multiple = TRUE,
                accept=c('image/png', 'image/jpeg')),
      conditionalPanel(
        condition = "output.fileUploaded",
        actionButton("predecir","Predecir Imagen")
      )
    ),
    mainPanel(
      uiOutput('images'),
      fluidRow(
        column(3, img(src = 'cat.jpg', height = '250px', width = '250px')  
        ),
        column(4, offset = 1,sliderInput("slider1", label = h3("En que porcentaje la imagen se parece a un gato (0) o un perro (1):"), min = 0, 
                           max = 1, value = 0.5) 
        ),
        column(4,img(src = 'dog.jpg', height = '250px', width = '250px')
        )
      )
      #tableOutput('files'),
    )
  )
))

shinyApp(ui=ui,server=server)
