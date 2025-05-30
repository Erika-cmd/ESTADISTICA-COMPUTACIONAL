library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)
library(bslib)
library(janitor)
library(DescTools)
library(nortest)
library(car)
library(MASS)
library(ggpubr)
library(ggfortify)
library(reshape2)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("🔬 SmartStats: Explorador Inteligente de Datos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sube archivo CSV o Excel", accept = c(".csv", ".xlsx")),
      uiOutput("var_select"),
      uiOutput("grupo_select"),
      selectInput("tipo", "Tipo de Variable:", c("Cualitativa", "Cuantitativa")),
      selectInput("prueba", "Prueba Estadística:",
                  choices = c("Shapiro-Wilk", "Jarque-Bera", "Lilliefors",
                              "Anderson-Darling", "Kolmogorov-Smirnov",
                              "t-Student", "ANOVA", "Chi-cuadrado")),
      actionButton("run", "Ejecutar Análisis", icon = icon("play")),
      actionButton("update", "Actualizar Resultados", icon = icon("refresh")),
      br(), br(),
      htmlOutput("resumen_prueba")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vista previa", tableOutput("preview")),
        tabPanel("Descriptivos", verbatimTextOutput("resumen")),
        tabPanel("Gráfico", plotOutput("grafico")),
        tabPanel("Resultados", verbatimTextOutput("resultados")),
        tabPanel("Interpretación", htmlOutput("interpretacion"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    if (ext == "csv") read_csv(input$file$datapath)
    else if (ext == "xlsx") read_excel(input$file$datapath)
    else NULL
  })
  
  output$preview <- renderTable({ head(datos(), 10) })
  
  output$var_select <- renderUI({
    req(datos())
    selectInput("variable", "Variable principal:", names(datos()))
  })
  
  output$grupo_select <- renderUI({
    req(datos())
    selectInput("grupo", "Variable de grupo (si aplica):", c("Ninguna", names(datos())))
  })
  
  output$resumen <- renderPrint({
    req(input$variable)
    var <- datos()[[input$variable]]
    if (input$tipo == "Cuantitativa") summary(var)
    else tabyl(var)
  })
  
  output$grafico <- renderPlot({
    req(input$variable)
    var <- datos()[[input$variable]]
    
    if (input$prueba %in% c("Shapiro-Wilk", "Jarque-Bera", "Lilliefors", "Anderson-Darling", "Kolmogorov-Smirnov")) {
      # Pruebas de normalidad - Histograma o gráfico Q-Q
      ggplot(datos(), aes_string(x = input$variable)) +
        geom_histogram(bins = 30, fill = "#2c3e50") +
        theme_minimal() +
        labs(title = "Distribución de la Variable (Prueba de Normalidad)")
    } else if (input$prueba == "t-Student") {
      # t-Student - Boxplot para comparar dos grupos
      validate(need(input$grupo != "Ninguna", "Debes seleccionar una variable de grupo"))
      ggplot(datos(), aes_string(x = input$grupo, y = input$variable)) +
        geom_boxplot(fill = "#3498db") +
        theme_minimal() +
        labs(title = "Comparación de Grupos (t-Student)")
    } else if (input$prueba == "ANOVA") {
      # ANOVA - Boxplot para comparar tres o más grupos
      validate(need(input$grupo != "Ninguna", "Debes seleccionar una variable de grupo"))
      ggplot(datos(), aes_string(x = input$grupo, y = input$variable)) +
        geom_boxplot(fill = "#3498db") +
        theme_minimal() +
        labs(title = "Comparación de Grupos (ANOVA)")
    } else if (input$prueba == "Chi-cuadrado") {
      # Chi-cuadrado - Gráfico de barras para distribución de categorías
      ggplot(datos(), aes_string(x = input$variable)) +
        geom_bar(fill = "#3498db") +
        theme_minimal() +
        labs(title = "Distribución de Categorías (Chi-cuadrado)")
    }
  })
  
  resultado <- eventReactive(input$run, {
    var <- datos()[[input$variable]]
    grupo <- if (input$grupo != "Ninguna") datos()[[input$grupo]] else NULL
    
    switch(input$prueba,
           "Shapiro-Wilk" = shapiro.test(var),
           "Jarque-Bera" = jarque.bera.test(var),
           "Lilliefors" = lillie.test(var),
           "Anderson-Darling" = ad.test(var),
           "Kolmogorov-Smirnov" = ks.test(var, "pnorm", mean(var), sd(var)),
           "t-Student" = {
             validate(need(!is.null(grupo), "Selecciona una variable de grupo para t-test"))
             t.test(var ~ grupo)
           },
           "ANOVA" = {
             validate(need(!is.null(grupo), "Selecciona una variable de grupo para ANOVA"))
             aov(var ~ grupo, data = datos())
           },
           "Chi-cuadrado" = {
             tabla <- table(datos()[[input$variable]], grupo)
             chisq.test(tabla)
           },
           NULL)
  })
  
  output$resultados <- renderPrint({
    validate(need(input$run > 0, "Haz clic en Ejecutar"))
    print(resultado())
  })
  
  output$interpretacion <- renderUI({
    req(resultado())
    p <- resultado()$p.value
    interpretacion <- switch(input$prueba,
                             "Shapiro-Wilk" = if (p > 0.05) "No se rechaza H0. La muestra sigue una distribución normal." else "Se rechaza H0. La muestra no sigue una distribución normal.",
                             "Jarque-Bera" = if (p > 0.05) "No se rechaza H0. La distribución es normal." else "Se rechaza H0. La distribución no es normal.",
                             "Lilliefors" = if (p > 0.05) "No se rechaza H0. La distribución es normal." else "Se rechaza H0. La distribución no es normal.",
                             "Anderson-Darling" = if (p > 0.05) "No se rechaza H0. La muestra sigue una distribución normal." else "Se rechaza H0. La muestra no sigue una distribución normal.",
                             "Kolmogorov-Smirnov" = if (p > 0.05) "No se rechaza H0. La muestra sigue una distribución normal." else "Se rechaza H0. La muestra no sigue una distribución normal.",
                             "t-Student" = if (p > 0.05) "No hay diferencias significativas entre los grupos." else "Hay diferencias significativas entre los grupos.",
                             "ANOVA" = if (p > 0.05) "No hay diferencias significativas entre los grupos." else "Hay diferencias significativas entre los grupos.",
                             "Chi-cuadrado" = if (p > 0.05) "No hay asociación entre las variables." else "Hay asociación entre las variables.",
                             "No se ha seleccionado una prueba")
    HTML(interpretacion)
  })
  
  output$resumen_prueba <- renderUI({
    req(input$prueba)
    resumen <- switch(input$prueba,
                      "Shapiro-Wilk" = "Prueba de normalidad para distribuciones pequeñas.",
                      "Jarque-Bera" = "Test de normalidad basado en asimetría y curtosis.",
                      "Lilliefors" = "Prueba de normalidad cuando los parámetros son desconocidos.",
                      "Anderson-Darling" = "Test de normalidad que da más peso a las colas.",
                      "Kolmogorov-Smirnov" = "Compara la distribución empírica de la muestra con una distribución teórica.",
                      "t-Student" = "Prueba para comparar dos medias de grupos independientes.",
                      "ANOVA" = "Prueba para comparar más de dos medias.",
                      "Chi-cuadrado" = "Test de independencia entre variables cualitativas.",
                      "No se ha seleccionado una prueba"
    )
    HTML(resumen)
  })
}

shinyApp(ui = ui, server = server)
