library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)
library(patchwork)

function(input, output) {
  output$barplot <- renderPlot({
    if(input$var_resposta == "Casos"){
      dados %>% 
        ggplot(aes(variante)) + 
        geom_bar(fill = "aquamarine4", color = "gray50", width = 0.5) + 
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(color = "black", size = 10),
              axis.text.y = element_text(color = "black", size = 10)) +
        labs(x = "Variante",
             y = "Casos",
             title = "Casos por variante")
    } else{
      dados %>% 
        filter(evolucao == "obito") %>% 
        ggplot(aes(variante)) + 
        geom_bar(fill = "aquamarine4", color = "gray50", width = 0.5) + 
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(color = "black", size = 10),
              axis.text.y = element_text(color = "black", size = 10)) +
        labs(x = "Variante",
             y = "Casos",
             title = "Óbitos por variante")
    }
  })
  
  output$serie <- renderPlot({
    if(input$var_resposta == "Casos"){
      dados %>%
        group_by(DT_SIN_PRI) %>%
        summarise(casos = n()) %>%
        ggplot(aes(DT_SIN_PRI, casos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "red4",
                   linetype = 2) +
        theme(axis.text.x = element_text(color = "black", size = 10),
              axis.text.y = element_text(color = "black", size = 10)) +
        annotate(
          x = in_gama,
          y = -Inf,
          label = "Gama",
          vjust = -0.5,
          geom = "label"
        ) +
        geom_vline(xintercept = in_delta,
                   colour = "purple4",
                   linetype = 3) +
        annotate(
          x = in_delta,
          y = +Inf,
          label = "Delta",
          vjust = 2,
          geom = "label"
        ) +
        geom_vline(xintercept = in_omicron,
                   colour = "yellow4",
                   linetype = 4) +
        annotate(
          x = in_omicron,
          y = +Inf,
          label = "Omicron",
          vjust = 2,
          geom = "label"
        ) +
        geom_vline(xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
                   colour = "green4",
                   linetype = 5) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(
          x = "Data do primeiro sintoma",
          y = "N° de casos diários"
        )
    } else{
      dados %>%
        filter(evolucao == "obito") %>% 
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(axis.text.x = element_text(color = "black", size = 10),
              axis.text.y = element_text(color = "black", size = 10)) +
        annotate(
          x = in_gama,
          y = -Inf,
          label = "Gama",
          vjust = -0.5,
          geom = "label"
        ) +
        geom_vline(xintercept = in_delta,
                   colour = "purple4",
                   linetype = 3) +
        annotate(
          x = in_delta,
          y = +Inf,
          label = "Delta",
          vjust = 2,
          geom = "label"
        ) +
        geom_vline(xintercept = in_omicron,
                   colour = "yellow4",
                   linetype = 4) +
        annotate(
          x = in_omicron,
          y = +Inf,
          label = "Omicron",
          vjust = 2,
          geom = "label"
        )  +
        geom_vline(xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
                   colour = "green4",
                   linetype = 5) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(
          x = "Data do primeiro sintoma",
          y = "N° de óbitos diários"
        )
    }
  })
}