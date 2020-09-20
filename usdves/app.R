library(shiny)
library(reactable)
library(crosstalk)
library(lubridate)

usdves <- read.csv2("../data/usdves.csv", stringsAsFactors=F)

showAll <- function(availableInputValues) {
    if(length(availableInputValues) > 1) {
        "Todo"
    } else {
        NULL
    }
}

ui <- fluidPage(
    titlePanel("Data del par USDVES diario"),
    sidebarLayout(
        sidebarPanel(
            selectInput("year", label="Año", choices=c("Todo"="",
                                                       year(usdves$date))),
            selectInput("month",label="Mes", choices=c("Todo"="")),
            downloadButton("download", "Descargar tabla")
            ),
        mainPanel(
            reactableOutput("RT")
        )
    )
)

# Incluir función que escriba la tabla filtrada y ejecutar la función
# en el parámetro de la función write.table
server <- function(input, output, session) {


    filteredData <- reactive({
        data <- usdves
        year <- year(data$date)
        month <- month(data$date, label=T, abbr=F)

        if (input$year != "") {
            data <- data[year==input$year,]
        }

        if (input$month != "") {
            data <- data[month==input$month,]
        }

        return(data)
    })


    observeEvent(input$year, {
        data <- filteredData()
        months <- as.character(month(data$date, label=T, abbr=F))
        updateSelectInput(session,
                          "month",
                          choices= months)
    })

    output$RT <- renderReactable({
        reactable(filteredData(), minRow=10, defaultPageSize=10,
                  columns=list(
                      "date"=colDef(name="Fecha",
                                     format=colFormat(datetime=T)),
                      "usdves"=colDef(name="USDVES",
                                      format=colFormat(prefix="Bs.S ",
                                                       separators=T))
                  ),
                  highlight=T, striped=T,
                  fullWidth=T, defaultSorted=list("date"="asc"))
        })

    output$download <- downloadHandler(
        filename=function() {
            paste0("usdves-", input$month, "-", input$year, ".csv")
        },
        content=function(file) {
            write.table(filteredData(), file, row.names=F, quote=F, sep=";")
        }
    )
}

shinyApp(ui, server)
