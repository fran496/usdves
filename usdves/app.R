library(shiny)

usdves <- read.csv2("../data/usdves.csv", stringsAsFactors=F)
usdves$years <- lubridate::year(usdves$date)
usdves$months <- as.character(lubridate::month(usdves$date, label=T, abbr=F))

ui <- fluidPage(
    titlePanel("Datos del par USD/VES diario"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("year",
                        label="Año",
                        choices=c("Todo", usdves$years)),
            selectizeInput("month",
                        label="Mes",
                        choices="Todo"),
             downloadButton("download", "Descargar tabla")
            ),
        mainPanel(
            reactable::reactableOutput("RT")
        )
    )
)

# Incluir función que escriba la tabla filtrada y ejecutar la función
# en el parámetro de la función write.table
server <- function(input, output, session) {


    filteredData <- reactive({
        data <- usdves

        if (input$year != "Todo") {
            data <- data[data$years == input$year,]
        }

        if (input$month != "Todo") {
            data <- data[data$months == input$month,]
        }

        return(data)
    })

    dataReady <- reactive({
        filteredData()[, c("date", "usdves")]
    })

    observeEvent(input$year, {
        data <- filteredData()

        updateSelectizeInput(session,
                             "month",
                             choices=c("Todo",
                                       data$months[data$years==input$year])
        )
    })


    output$RT <- renderReactable({
        reactable::reactable(dataReady(),
                  minRow=10,
                  defaultPageSize=10,
                  highlight=T,
                  striped=T,
                  fullWidth=T,
                  defaultSorted=list("date"="asc"),
                  columns=list(
                      "date"=colDef(name="Fecha",
                                    format=colFormat(datetime=T)),
                      "usdves"=colDef(name="USD/VES",
                                      format=colFormat(prefix="Bs.S ",
                                                       separators=T))
                  ))
        })

    output$download <- downloadHandler(
        filename=function() {
            paste0("usdves-", input$month, "-", input$year, ".csv")
        },
        content=function(file) {
            write.table(dataReady(),
                        file,
                        row.names=F,
                        quote=F,
                        sep=";")
        }
    )
}

shinyApp(ui, server)
