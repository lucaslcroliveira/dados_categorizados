library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)

navbarPage(
  "Dados categorizados",
  tabPanel(
    "Descritiva",
    icon = icon("bar-chart-o"),
    fluidPage(
      theme = shinythemes::shinytheme("flatly"),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          "Descritiva",
          selectInput(
            "var_resposta",
            "Selecione a variável resposta:",
            c("Casos",
              "Óbitos")
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
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Regressão logística",
    icon = icon("th", lib = "glyphicon"),
    fluidPage(sidebarLayout(
      sidebarPanel(),
      mainPanel(verbatimTextOutput("anova"),
                plotOutput("plot2"))
    ))
  )
)