library(shiny)
library(reactable)
library(crosstalk)
library(lubridate)

usdves <- read.csv2("../data/usdves.csv", stringsAsFactors=F)
table <- SharedData$new(usdves)

ui <- fluidPage(
    titlePanel("Data del par USDVES diario"),
    sidebarLayout(
        sidebarPanel(
            filter_select("year", "Año", table, ~year(date)),
            filter_select("month", "Mes", table, ~month(date, label=T, abbr=F)),
            downloadButton("download", "Descargar tabla")
            ),
        mainPanel(
            reactableOutput("RT")
        )
    )
)

server <- function(input, output) {
    output$RT <- renderReactable({
        reactable(table, minRow=10, defaultPageSize=10,
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
            paste("usdves.csv", sep="-")
        },
        content=function(file) {
            write.table(usdves[input[["RT_rows_all"]],],
                        file, row.names=F, quote=F, sep=";")
        }
    )
}

shinyApp(ui, server)
