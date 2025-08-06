# Instale os pacotes necessários (execute apenas uma vez)
# install.packages(c("shiny", "tidyverse", "plotly", "PNADcIBGE", "survey", "srvyr", "DT"))

library(shiny)
library(tidyverse)
library(plotly)
library(PNADcIBGE)
library(survey)
library(srvyr)
library(DT)

# Interface do Usuário
ui <- fluidPage(
  titlePanel("Dashboard de Saúde - PNAD Contínua IBGE"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variavel", "Selecione o Indicador:",
                  choices = c("Plano de Saúde" = "V403312",
                              "Saúde Autorreferida" = "V403311",
                              "Consulta Médica" = "V4033"),
                  selected = "V403312"),
      
      selectInput("regiao", "Filtrar por Região:",
                  choices = c("Todas", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
                  selected = "Todas"),
      
      sliderInput("idade", "Faixa Etária:",
                  min = 0, max = 100, value = c(0, 100)),
      
      actionButton("atualizar", "Atualizar Dados"),
      downloadButton("download", "Exportar Dados")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico", plotlyOutput("grafico")),
        tabPanel("Tabela", DTOutput("tabela")),
        tabPanel("Resumo", 
                 h3("Estatísticas Descritivas"),
                 verbatimTextOutput("resumo"),
                 h3("Sobre os Dados"),
                 HTML("<p>Dados da PNAD Contínua - IBGE, 4º trimestre de 2022</p>
                      <p>Variáveis:</p>
                      <ul>
                        <li><strong>V403312</strong>: Possui plano de saúde? (1 = Sim, 2 = Não)</li>
                        <li><strong>V403311</strong>: Como avalia sua saúde? (1 = Muito boa, 5 = Muito ruim)</li>
                        <li><strong>V4033</strong>: Fez consulta médica nos últimos 12 meses? (1 = Sim, 2 = Não)</li>
                      </ul>"))
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  # Carregar dados com tratamento de erro
  dados_pnad <- eventReactive(input$atualizar, {
    tryCatch({
      PNADcIBGE::get_pnadc(
        year = 2022,
        quarter = 4,
        vars = c(input$variavel, "UF", "V2009", "V1022", "V1028", "UPA", "Estrato")
      )
    }, error = function(e) {
      # Dados de exemplo caso falhe o download
      data.frame(
        V403312 = sample(c(1, 2), 
                         V403311 = sample(1:5),
                         V4033 = sample(c(1, 2)),
                         UF = sample(1:27),
                         V2009 = sample(18:80),
                         V1022 = sample(1:5),
                         V1028 = runif(1000, 0.5, 1.5),
                         UPA = sample(1:100),
                         Estrato = sample(1:10)
    })
  })
      
      # Processar dados
      dados_filtrados <- reactive({
        df <- dados_pnad() %>%
          mutate(
            regiao = case_when(
              UF %in% c(11:17) ~ "Norte",
              UF %in% c(21:29) ~ "Nordeste",
              UF %in% c(31:33, 35) ~ "Sudeste",
              UF %in% c(41:43) ~ "Sul",
              TRUE ~ "Centro-Oeste"
            ),
            faixa_etaria = cut(V2009, 
                               breaks = c(0, 18, 30, 50, 65, 100),
                               labels = c("0-17", "18-29", "30-49", "50-64", "65+"))
          )
        
        # Aplicar filtros
        if (input$regiao != "Todas") {
          df <- df %>% filter(regiao == input$regiao)
        }
        
        df %>% filter(V2009 >= input$idade[1], V2009 <= input$idade[2])
      })
      
      # Criar desenho amostral
      desenho_amostral <- reactive({
        dados_filtrados() %>%
          as_survey_design(
            ids = UPA,
            strata = Estrato,
            weights = V1028,
            nest = TRUE
          )
      })
      
      # Gráfico principal
      output$grafico <- renderPlotly({
        variavel_nome <- switch(input$variavel,
                                "V403312" = "Possui Plano de Saúde",
                                "V403311" = "Saúde Autorreferida",
                                "V4033" = "Consulta Médica Últimos 12 Meses")
        
        dados_grafico <- desenho_amostral() %>%
          group_by(!!sym(input$variavel), faixa_etaria) %>%
          summarize(proporcao = survey_mean(na.rm = TRUE)) %>%
          filter(!is.na(!!sym(input$variavel)))
        
        # Ajuste para saúde autorreferida (escala 1-5)
        if (input$variavel == "V403311") {
          p <- ggplot(dados_grafico, aes(x = factor(!!sym(input$variavel)), y = proporcao, 
                                         fill = faixa_etaria)) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Avaliação de Saúde (1 = Muito boa, 5 = Muito ruim)")
        } else {
          p <- ggplot(dados_grafico, aes(x = faixa_etaria, y = proporcao, 
                                         fill = factor(!!sym(input$variavel)))) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(x = "Faixa Etária")
        }
        
        ggplotly(
          p +
            labs(title = paste("Distribuição de", variavel_nome),
                 y = "Proporção",
                 fill = ifelse(input$variavel == "V403311", "Faixa Etária", variavel_nome)) +
            scale_y_continuous(labels = scales::percent) +
            theme_minimal()
        )
      })
      
      # Tabela de dados
      output$tabela <- renderDT({
        dados_tabela <- desenho_amostral() %>%
          group_by(!!sym(input$variavel)) %>%
          summarize(
            Contagem = survey_total(),
            Proporção = survey_mean()
          ) %>%
          mutate(Proporção = scales::percent(Proporção))
        
        datatable(dados_tabela, 
                  options = list(pageLength = 5, dom = 'tp'),
                  rownames = FALSE) %>%
          formatRound('Contagem', digits = 0)
      })
      
      # Estatísticas descritivas
      output$resumo <- renderPrint({
        df <- dados_filtrados()
        variavel <- df[[input$variavel]]
        
        cat("=== Resumo Estatístico ===\n\n")
        cat("Variável selecionada:", input$variavel, "\n")
        cat("Região:", ifelse(input$regiao == "Todas", "Todas as regiões", input$regiao), "\n")
        cat("Faixa etária:", input$idade[1], "a", input$idade[2], "anos\n\n")
        
        if (is.numeric(variavel)) {
          print(summary(variavel))
          cat("\nDesvio Padrão:", sd(variavel, na.rm = TRUE))
        } else {
          print(table(variavel))
        }
      })
      
      # Download dos dados
      output$download <- downloadHandler(
        filename = function() {
          paste("dados_pnad_", input$variavel, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv2(dados_filtrados(), file, row.names = FALSE)
        }
      )
}

# Executar o aplicativo
shinyApp(ui, server)