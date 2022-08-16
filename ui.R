library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)

navbarPage(
  "EFEITOS DAS VARIANTES DO COVID-19 NA POPULAÇÃO MATERNA",
  tabPanel(
    "Análise Descritiva",
    icon = icon("bar-chart-o"),
    fluidPage(
      theme = shinythemes::shinytheme("yeti"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          #"Descritiva",
          selectInput(
            "var_resposta",
            h5("Selecione a variável resposta:"),
            c("Casos",
              "Óbitos",
              "Suporte ventilatório",
              "UTI",
              "Febre",
              "Tosse",
              "Fadiga")
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              "Frequência",
              plotOutput("barplot", width = "100%", height = "550px")
            ),
            tabPanel(
              "Série",
              plotOutput("serie", width = "100%", height = "550px")
            ),
            tabPanel(
              "Vacinados x Não vacinados",
              highcharter::highchartOutput("prop", height = "550px"),
              fluidPage(
                includeMarkdown("descricao_graf.md")
              )
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Modelo de Regressão Logística",
    icon = icon("th", lib = "glyphicon"),
    fluidPage(sidebarLayout(
      sidebarPanel(
        width = 3,
        #"Modelo de Regressão Logística",
        selectInput(
          "var_resposta2",
          h5("Selecione a variável resposta:"),
          c("Óbitos"="evolucao",
            "Suporte ventilatório"="suport_ven",
            "UTI"="uti",
            "Febre"="febre",
            "Tosse"="tosse",
            "Fadiga"="fadiga")
        )
      ),
      mainPanel(
        width = 5,
        tabsetPanel(
          tabPanel(
            "Ajuste",
            verbatimTextOutput("print1")
            
          ),
          tabPanel(
            "Diagnóstico",
            h4(strong("Teste de Hosmer-Lemeshow")),
            verbatimTextOutput("print2"),
            hr(),
            h4(strong("Gráfico de Envelope")),
            plotOutput("hnpplot")
          )
        )
      )
    )
    )
  ),
  tabPanel(
    "Sobre",
    icon = icon("info-sign", 
                lib = "glyphicon"),
    fluidPage(
       includeMarkdown("sobre.md")
    )
  )
  
)