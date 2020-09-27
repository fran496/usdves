library(shiny)
library(reactable)
library(ggplot2)

usdves <- read.csv2("../data/usdves.csv", stringsAsFactors=F)
usdves$usdves <- as.numeric(usdves$usdves)
usdves$years <- lubridate::year(usdves$date)
usdves$months <- as.character(lubridate::month(usdves$date, label=T, abbr=F))
last_price <- usdves$usdves[nrow(usdves)]

ui <- fluidPage(
    theme=shinythemes::shinytheme("sandstone"),
    titlePanel("Datos del par USD/VES diario"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput("year",
                        label="Año",
                        choices=c("Todo", usdves$years)),
            selectizeInput("month",
                        label="Mes",
                        choices="Todo"),
            radioButtons("file_extension",
                         label="Extensión del archivo:",
                         choices=c(".csv", ".xlsx")),
            downloadButton("download", "Descargar tabla"),
            hr(),
            numericInput("dollars",
                         label="Calculadora de precio con última cotización",
                         value=0,
                         min=0),
            verbatimTextOutput("price")
        ),
        mainPanel(
            reactableOutput("RT"),
            plotOutput("plot1")
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

    output$price <- renderText({
        price <- input$dollars*last_price
        formatted_price <- format(price,
                                  big.mark=".",
                                  decimal.mark=",",
                                  digits=2,
                                  nsmall=2,
                                  scientific=F)
        paste0("Bs.S ", formatted_price)
    })


    output$plot1 <- renderPlot({
        ggplot(data.frame(x=0:100), aes(x)) +
            stat_function(fun=function(x) (last_price*x)/(10**6),
                          xlim=c(0, 100),
                          colour="darkgreen") +
            labs(title="Cotización del dolar norteamericano en bolívares soberanos",
                 x="Cantidad de dólares",
                 y="USD/VES (Millones de bolívares soberanos)",
                 caption="Fuente: Banco Central de Venezuela (BCV)") +
            theme_light()
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
        reactable(dataReady(),
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
          paste0("usdves-", input$month, "-",input$year, input$file_extension)
        },
        content=function(file) {
            if (input$file_extension == ".xlsx") {
              writexl::write_xlsx(dataReady(), file, col_names=T)
            } else {
              write.table(dataReady(),
                          file,
                          row.names=F,
                          quote=F,
                          sep=";")
            }
        }
    )
}

shinyApp(ui, server)
