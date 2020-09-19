library(shiny)

ui <- fluidPage(
    titlePanel("Descargar datos"),
    sidebarLayout(
        sidebarPanel(
            selectInput("Año", "Escoje un año:",
                        choices = c(2020)),

            selectInput("Mes", "Escoje un mes:",
                        choices = c("Septiembre")),
            downloadButton("downloadData", "Download")

        ),
        mainPanel(
            tableOutput("table")
        )
    )
)

server <- function(input, output) {

    datasetInput <- reactive({
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
    })

    output$table <- renderTable({
        datasetInput()
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )

}

# Create Shiny app ----
shinyApp(ui, server)
