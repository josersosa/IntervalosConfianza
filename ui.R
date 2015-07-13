library(shiny)

# Definición de la interface gráfica (GUI)
shinyUI(fluidPage(withMathJax(),

  # Application title
  titlePanel("Ejemplo de Intervalos de Confianza"),
  
  # Barra lateral con los controles
  sidebarLayout(position = "left",
    sidebarPanel( withMathJax(
      helpText("El estimador puntual utilizado (la media muestral) se calculará sobre una muestra de tamaño n."),
      sliderInput("n",
                  "Tamaño de la muestra (n):",
                  min = 10,
                  max = 200,
                  value = 30),
      helpText("El nivel de confianza (\\(1-\\alpha\\)) suele ser alto, comunmente 95%. La significancia es el complemento de este valor (\\(\\alpha\\).)"),
      sliderInput("alpha",
                  "Significancia (alpha):",
                  min = 0.01,
                  max = 0.5,
                  value = 0.1,
                  step= 0.005),
      sliderInput("num_CIs",
                  "Número de intervalos a mostrar:",
                  min = 1,
                  max = 150,
                  value = 50,
                  step= 1),
      #numericInput("num_CIs", "Número de intervalos a mostrar:", 50),
      #submitButton("Re-generar intevalos"),
      br(),
      checkboxInput('Mostrar_fdp_Pob', 'Mostrar fdp de la población', TRUE),
      checkboxInput('Mostrar_hist_Pob', 'Mostrar histograma de la población', TRUE),
      checkboxInput('Mostrar_fdp_Muest', 'Mostrar densidad de la muestra', TRUE),
      checkboxInput('Mostrar_hist_Muest', 'Mostrar histograma de la muestra', TRUE),
      checkboxInput('MostrarParam', 'Otros parámetros', FALSE),
      br(),
      conditionalPanel(condition = "input.MostrarParam == true",
                       helpText("Los parámetros poblacionales (\\(\\mu\\) y \\(\\sigma\\)) son desconocidos, pero definiremos valores hipotéticos para generar la población:"),
                       sliderInput("mu",
                                   "Media poblacional (mu):",
                                   min = 0,
                                   max = 50,
                                   value = 40,
                                   step= 1),
                       sliderInput("sigma",
                                   "Desviación estandar poblacional (sigma):",
                                   min = 0,
                                   max = 20,
                                   value = 10,
                                   step= 1),
                       sliderInput(inputId = "RangoX",
                                   label = "límites de graficación:",
                                   min = 0, max = 100, value = c(30,50), step = 1)
      )
      
            
    )),

    # Paneles con las salidas
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Visión probabilística", plotOutput("visProb")), 
                  tabPanel("Media muestral", plotOutput("Medias")),
                  tabPanel("Visión frecuentista", plotOutput("visFrec")),
                  tabPanel("Ecuaciones", uiOutput("Ecuaciones")))
    )
  )
))
