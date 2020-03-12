#
# This is a Shiny web application. You can run the application by clicking
library(shiny)
library(shinydashboard)
library(DBUmaps)
library(leaflet)
library(rapportools)
library(mapview)


ui <- dashboardPage(

    dashboardHeader(title = "DBUMaps Api Web",dropdownMenu(type = "messages",
                                                           messageItem(
                                                               from = "Departamento Informatica",
                                                               message = "Universidad del Tachira"
                                                           ),
                                                           messageItem(
                                                               from = "Diego Bolivar",
                                                               message = "Nuevo Ingeniero",
                                                               icon = icon("question"),
                                                               time = "11:11"
                                                           ),
                                                           messageItem(
                                                               from = "Soporte Shiny",
                                                               message = "Aplicacion Culminada",
                                                               icon = icon("life-ring"),
                                                               time = "2020-03-1"
                                                           )
    ),dropdownMenu(type = "notifications",
                   notificationItem(
                       text = "Comunidad Unet",
                       icon("users")
                   ),
                   notificationItem(
                       text = "Viaja Colombia",
                       icon("truck"),
                       status = "success"
                   ),
                   notificationItem(
                       text = "Servidor en un 90%",
                       icon = icon("exclamation-triangle"),
                       status = "warning"
                   )
    ),dropdownMenu(type = "tasks", badgeStatus = "success",
                   taskItem(value = 90, color = "green",
                            "Documentation"
                   ),
                   taskItem(value = 17, color = "aqua",
                            "Project X"
                   ),
                   taskItem(value = 75, color = "yellow",
                            "Server deployment"
                   ),
                   taskItem(value = 80, color = "red",
                            "Overall project"
                   )
    )),
    dashboardSidebar(
        tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
        sidebarMenu(id = "MenuPrincipal",
                    #menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("Cargar Archivo", tabName = "widgets", icon = icon("table")),
                    menuItem("Geomaps", tabName = "Geomaps", icon = icon("bar-chart-o")),
                    menuItem("Estadisticas", tabName = "estadisticas",icon = icon("file-code-o")
                    )
                    #menuItem("MapaG",tabName = "mapag", icon = icon("bar-chart-o"))
        )
    ),
    dashboardBody(
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }
                              '))),
        tags$head(tags$style(HTML('
                              .skin-blue .main-header .logo {
                              background-color: #3c8dbc;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #3c8dbc;
                              }
                              '))),
        tabItems(
        tabItem(tabName = "widgets",

            sidebarPanel(

            # Input: Select a file ----
            fileInput("file1", "Cargar Archivo",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),

            # Horizontal line ----
            tags$hr(),

            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),

            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),

            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),

            # Horizontal line ----
            tags$hr(),

            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")

        ),

        # Main panel for displaying outputs ----
        mainPanel(

            # Output: Data file ----
            tableOutput("contents"),height = 10

        ),height = 10),tabItem(tabName = "Geomaps", titlePanel(title = HTML('<h1 class="text-center">Función Principal Geomaps</h1>')),
            div(style="height:20%",
                   div(class="row",

                       div(class="col-xs-3 col-md-3" ,textInput(inputId="APiKey", label="Api Key", value = "",width = 300)),
                       div(class="col-xs-1 col-md-1" ,textInput(inputId="Rango1", label="Intervalo 1", value = "")),
                       div(class="col-xs-1 col-md-1" ,textInput(inputId="Rango2", label="Intervalo 2", value = "")),
                       div(class="col-xs-1 col-md-1" ,textInput(inputId="Rango3", label="Intervalo 3", value = "")),
                       div(class="col-xs-1 col-md-1" ,textInput(inputId="Rango4", label="Intervalo 4", value = "")),
                       div(class="col-xs-3 col-md-3" ,selectInput("SeleccionElemento","Elemento",c("NONE",""),selected="NONE")),
                       div(class="col-xs-2 col-md-2", style="top:25px", actionButton(inputId = "Generar",label = "Generar mapa",style="color:white",class="btn btn-primary"))
                   )
            ),
            div(style="height:80%",
                   leafletOutput("mymap2",height = 570)
            )
        ),

        tabItem(tabName = "estadisticas", titlePanel("Informacion Relevante"),

                fluidRow(
                    # A static infoBox
                    infoBoxOutput("NewOrders"),
                    # Dynamic infoBoxes
                    infoBoxOutput("progressBox"),
                    infoBoxOutput("approvalBox")
                ),

                # infoBoxes with fill=TRUE
                fluidRow(
                    #infoBoxOutput("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE),
                    infoBoxOutput("progressBox2"),
                    infoBoxOutput("approvalBox2")
                ),

                box(
                    title = "Histograma", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("plot10", height = 250)
                )

                #fluidRow(
                # Clicking this will increment the progress amount
                # box(width = 4, actionButton("count", "Increment progress"))
                #)
        )
        )


    )
)

server <- function(input, output,session) {
    set.seed(122)
    histdata <- rnorm(500)

    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })




    output$contents <- renderTable({

        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.

        req(input$file1)

        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)

        datos3<- read.csv(input$file1$datapath,header = T,sep = ";")
        nombrecol2<-names(datos3)


        updateSelectInput(session,"SeleccionElemento",choices=nombrecol2[-1])

        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }



    })


    # Same as above, but with fill=TRUE

    #output$approvalBox2 <- renderInfoBox({
    # infoBox(
    #  "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
    # color = "yellow", fill = TRUE
    #)
    #})

    output$mymap<- renderLeaflet({
        Geomaps("AIzaSyC0f984poODD2An9DiFFYfFmbRWrXlmq3Q","C:/Users/Usuario/Documents/PruebaDiego.txt",2,10,20,30,40)
    })

    observeEvent(input$Generar, {



        if(is.null(input$file1))  {
            showNotification("Debe Cargar Archivo",type = "error")
        }
        else
        {
            if(is.empty(input$Rango1)||is.empty(input$Rango2)||is.empty(input$Rango3)||is.empty(input$Rango4)||is.empty(input$SeleccionElemento))
            {
                showNotification("Debe Rellenar todos los campos",type = "error")
            }
            else
            {
                req(input$file1)
                datos2<- read.csv(input$file1$datapath,header = T,sep = ";")
                nombrecol<-names(datos2)
                elemento=0

                for (i in 1:length(nombrecol)) {
                    if(nombrecol[i]==input$SeleccionElemento)
                    {
                        elemento=i
                    }
                }


                Caracteristica=datos2[elemento]
                Carac=Caracteristica[,1]
                parametro<-Carac
                contador=0
                contador2=0
                contador3=0

                Menores=0
                Menores2=0
                Menores3=0

                print(paste("Elemento elegido",elemento))






                showNotification("Generando Mapa",type = "message", duration = 10)
                output$mymap2<- renderLeaflet({
                    Geomaps(input$APiKey,input$file1$datapath,elemento,as.numeric(input$Rango1),as.numeric(input$Rango2),as.numeric(input$Rango3),as.numeric(input$Rango4))

                })


                #observeEvent(input$Descarga, {
                # mapshot(mymap2, url  =  paste0 ( getwd (),  "/map.html" ))
                #})




                for (i in 1:length(parametro))
                {
                  print(paste("i= ",parametro[i]," ",input$Rango1," ",input$Rango2, " ",input$Rango3," ",input$Rango4," ",contador," ",contador2," ",contador3))
                    if(parametro[i]<as.numeric(input$Rango2))
                    {
                        contador=contador+1

                    }
                    else if(parametro[i]>=as.numeric(input$Rango2) & parametro[i]<=as.numeric(input$Rango4))
                    {
                        contador2=contador2+1
                    }
                    else if(parametro[i]>as.numeric(input$Rango4))
                    {contador3=contador3+1}
                  else
                  {print("Valor Desconocido")}
                }
                Menores=(contador/length(parametro))*100
                Menores2=(contador2/length(parametro))*100
                Menores3=(contador3/length(parametro))*100

                print(paste("Contador =",contador))
                print(paste("Contador2 =",contador2))
                print(paste("Contador3 =",contador3))

                output$NewOrders <- renderInfoBox({
                    infoBox(
                        paste(nombrecol[elemento]," Niveles Bajos"), paste(Menores,"%"),
                        color = "green", fill = TRUE
                    )
                })

                output$progressBox <- renderInfoBox({
                    infoBox(
                        paste(nombrecol[elemento]," Niveles Medios"), paste(Menores2,"%"),
                        color = "orange", fill = TRUE
                    )
                })
                output$approvalBox <- renderInfoBox({
                    infoBox(
                        paste(nombrecol[elemento]," Niveles Altos"), paste(Menores3,"%"),
                        color = "red", fill = TRUE
                    )
                })
                output$plot10 <- renderPlot({
                    hist(parametro,main = paste("Distribucion de ",nombrecol[elemento],"en la data"),
                      xlab = paste("Escalas de Concentracion de ",nombrecol[elemento]),

                       breaks = 5,

                      xlim = c(0,as.numeric(input$Rango4)),
                      col = "blue"
                      #range(as.numeric(input$Rango1),as.numeric(input$Rango2),as.numeric(input$Rango3),as.numeric(input$Rango4))
                      )

                 #barplot(parametro,main = paste("Distribucion de ",nombrecol[elemento]))

                })



            }


        }

    })
}

shinyApp(ui, server)

