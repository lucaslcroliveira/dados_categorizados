library(shiny)
library(shinydashboard)
library(tidyverse)
library(xtable)
library(knitr)
library(patchwork)
library(highcharter)
library(glmtoolbox)
library(hnp)

function(input, output) {
  output$barplot <- renderPlot({
    if (input$var_resposta == "Óbitos") {
      dados %>%
        filter(evolucao == "obito") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Óbitos")
    }
    else if (input$var_resposta == "Casos") {
      dados %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Casos")
    }
    else if (input$var_resposta == "UTI") {
      dados %>%
        filter(uti == "sim") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "UTI")
    }
    else if (input$var_resposta == "Suporte ventilatório") {
      dados %>%
        filter(suport_ven == "invasivo") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Suporte ventilatório")
    }
    else if (input$var_resposta == "Febre") {
      dados %>%
        filter(febre == "sim") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Febre")
    }
    else if (input$var_resposta == "Tosse") {
      dados %>%
        filter(tosse == "sim") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Tosse")
    }
    else {
      dados %>%
        filter(fadiga == "sim") %>%
        ggplot(aes(variante)) +
        geom_bar(fill = "aquamarine4",
                 color = "gray50",
                 width = 0.5) +
        theme(
          legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
        labs(x = "Variante",
             y = "Fadiga")
    }
  })
  
  output$serie <- renderPlot({
    if(input$var_resposta == "Óbitos"){
      dados %>%
        filter(evolucao == "obito") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de óbitos diários")
    }
    else if(input$var_resposta == "Casos"){
      dados %>%
        group_by(DT_SIN_PRI) %>%
        summarise(casos = n()) %>%
        ggplot(aes(DT_SIN_PRI, casos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "red4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de casos diários")
    }
    else if(input$var_resposta == "UTI"){
      dados %>%
        filter(uti == "sim") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de 
             internações em UTI")
    }
    else if(input$var_resposta == "Suporte ventilatório"){
      dados %>%
        filter(suport_ven == "invasivo") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de suporte ventilatório invasivo")
    }
    else if(input$var_resposta == "Febre"){
      dados %>%
        filter(febre == "sim") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de gest/puerp com febre")
    }
    else if(input$var_resposta == "Tosse"){
      dados %>%
        filter(tosse == "sim") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de gest/puerp com tosse")
    }
    else {
      dados %>%
        filter(fadiga == "sim") %>%
        group_by(DT_SIN_PRI) %>%
        summarise(obitos = n()) %>%
        ggplot(aes(DT_SIN_PRI, obitos)) +
        geom_line() +
        geom_vline(xintercept = in_gama,
                   colour = "green4",
                   linetype = 2) +
        theme(
          axis.text.x = element_text(color = "black", size = 10),
          axis.text.y = element_text(color = "black", size = 10),
          axis.title.x = element_text(color = "black", size = 15),
          axis.title.y = element_text(color = "black", size = 15)
        ) +
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
        geom_vline(
          xintercept = as.Date("01-05-2021", format = "%d-%m-%Y"),
          colour = "green4",
          linetype = 5
        ) +
        annotate(
          x = as.Date("01-05-2021", format = "%d-%m-%Y"),
          y = -Inf,
          label = "Vacinação",
          vjust = -0.5,
          geom = "label"
        ) +
        labs(x = "Data do primeiro sintoma",
             y = "N° de gest/puerp com fadiga")
    }
  })
  
  tab1 <- reactive({
    if (input$var_resposta == "Óbitos") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <-
        data.frame(prop.table(table(d1$variante, d1$evolucao), 1))
      dados1$vacina <- "sim"
      dados2 <-
        data.frame(prop.table(table(d2$variante, d2$evolucao), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "obito")
      dados2 <- dados2 %>% filter(Var2 == "obito")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else if (input$var_resposta == "Casos") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <- data.frame(prop.table(table(d1$variante)))
      dados1$vacina <- "sim"
      dados2 <- data.frame(prop.table(table(d2$variante)))
      dados2$vacina <- "não"
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else if (input$var_resposta == "Suporte ventilatório") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <- data.frame(prop.table(table(d1$variante, d1$suport_ven), 1))
      dados1$vacina <- "sim"
      dados2 <- data.frame(prop.table(table(d2$variante, d2$suport_ven), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "invasivo")
      dados2 <- dados2 %>% filter(Var2 == "invasivo")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else if (input$var_resposta == "UTI") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <- data.frame(prop.table(table(d1$variante, d1$uti), 1))
      dados1$vacina <- "sim"
      dados2 <- data.frame(prop.table(table(d2$variante, d2$uti), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "sim")
      dados2 <- dados2 %>% filter(Var2 == "sim")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else if (input$var_resposta == "Febre") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <-
        data.frame(prop.table(table(d1$variante, d1$febre), 1))
      dados1$vacina <- "sim"
      dados2 <-
        data.frame(prop.table(table(d2$variante, d2$febre), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "sim")
      dados2 <- dados2 %>% filter(Var2 == "sim")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else if (input$var_resposta == "Tosse") {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <-
        data.frame(prop.table(table(d1$variante, d1$tosse), 1))
      dados1$vacina <- "sim"
      dados2 <-
        data.frame(prop.table(table(d2$variante, d2$tosse), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "sim")
      dados2 <- dados2 %>% filter(Var2 == "sim")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
    else {
      d1 <- dados %>%  filter(vacina_cov == "sim")
      d2 <- dados %>%  filter(vacina_cov == "não")
      dados1 <-
        data.frame(prop.table(table(d1$variante, d1$fadiga), 1))
      dados1$vacina <- "sim"
      dados2 <-
        data.frame(prop.table(table(d2$variante, d2$fadiga), 1))
      dados2$vacina <- "não"
      dados1 <- dados1 %>% filter(Var2 == "sim")
      dados2 <- dados2 %>% filter(Var2 == "sim")
      dados <- rbind(dados1, dados2)
      dados$Freq <- round(dados$Freq * 100, 2)
      return(dados)
    }
  })
  
  output$prop <- highcharter::renderHighchart({
    hchart(tab1(),
           type = "line",
           hcaes(
             x = Var1,
             y = Freq,
             group = vacina,
             colour = vacina
           )) %>%
      hc_xAxis(title = list(text = "Variantes")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  output$print1 <- renderPrint({
    summary(glm(data = dados, as.factor(get(input$var_resposta2)) ~ vacina_cov+variante, family = binomial))
  })
  
  output$print2 <- renderPrint({
    hltest(glm(data = dados, as.factor(get(input$var_resposta2)) ~ vacina_cov+variante, family = binomial))
  })
  
  output$hnpplot <- renderPlot({
    hnp(glm(data = dados, as.factor(get(input$var_resposta2)) ~ vacina_cov+variante, family = binomial))
  })
}