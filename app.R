library(RCurl)

### Filmes nacionais lançados 1995-2018


#data_lancados <- getURL("https://github.com/viniciusbgodinho/cinema_brasileiro/blob/master/filmes%20lancados.csv")





library(RCurl)
data_lancados <- getURL("https://raw.githubusercontent.com/viniciusbgodinho/cinema_brasileiro/master/filmes%20lancados.csv", encoding = "UTF-8")
df <- read.csv(text=data_lancados,head = F, sep = ";" , dec=',')




df <- df[-(1:2),]

colnames(df) <- df[1,]

df <- df[-1,]
colnames(df)[1] <- "Ano"
colnames(df)[3] <- "Titulo"
colnames(df)[4] <- "Direcao"
colnames(df)[5] <- "Genero"
colnames(df)[7] <- "Estado"
colnames(df)[12] <- "Renda"
colnames(df)[13] <- "Publico"

library(dplyr)
library('stringr')
library(shiny)
library(shinyWidgets)
library(shinythemes)



df_table <- df %>%
    select("Ano","Titulo", "Direcao","Estado", "Genero", "Renda", "Publico")


df_table$Titulo <- str_sub(df_table$Titulo, start = 2) 
df_table$Titulo <- str_replace(df_table$Titulo, "\"", "") 

df_table$Renda <- str_sub(df_table$Renda, start = 2) 
df_table$Renda <- str_replace(df_table$Renda, "\"", "") 

df_table$Publico <- str_replace(df_table$Publico, ",", "") 
df_table$Publico <- str_replace(df_table$Publico, ",", "") 


df_table <- df_table %>%
    filter(Publico != "ND", Renda != "ND") 


df_table$Publico <- as.numeric(df_table$Publico)

df_table$Renda <- as.numeric(df_table$Renda)


###filmes mais de 500 mil

data_500k <- getURL("https://raw.githubusercontent.com/viniciusbgodinho/cinema_brasileiro/master/filmes_500_k.csv", encoding = "UTF-8")
df_2 <- read.csv(text=data_500k,head = F, sep = ";" )



df_2 <- df_2[-(1:2),]

colnames(df_2) <- df_2[1,]

df_2 <- df_2[-1,]

df_2  <- df_2[,-1]

colnames(df_2)[1] <- "Titulo"
colnames(df_2)[2] <- "Direcao"
colnames(df_2)[3] <- "Produtora"
colnames(df_2)[4] <- "Estado"
colnames(df_2)[6] <- "Ano"
colnames(df_2)[7] <- "Publico"


df_2$Titulo <- str_replace(df_2$Titulo,"\"", "" )

df_2$Titulo <- str_replace(df_2$Titulo,"\"", "" )


df_table_2 <- df_2 %>%
    select(Ano, Titulo, Direcao, Produtora, Estado, Publico)

df_table_2$Publico <- str_replace(df_table_2$Publico,",","")
df_table_2$Publico <- str_replace(df_table_2$Publico,",","")


df_table_2$Publico <- as.numeric(df_table_2$Publico) 


ui <- bootstrapPage(
    navbarPage(theme = shinytheme("united"), collapsible = TRUE,
               "Cinema Brasileiro", id="nav",
               
               
               tabPanel("Filmes Lançados Comercialmente (1995-2018)",
                        
                        sidebarLayout(
                            sidebarPanel(
                                
                                
                                
                                column(4,
                                       selectInput("ano",
                                                   "Ano:",
                                                   c("Todos",
                                                     unique(as.character(df_table$Ano ))))
                                ),
                                column(4,
                                       selectInput("direcao",
                                                   "Direcao:",
                                                   c("Todos",
                                                     unique(as.character(df_table$Direcao))))
                                ),
                                
                                column(4,
                                       selectInput("genero",
                                                   "Genero:",
                                                   c("Todos",
                                                     unique(as.character(df_table$Genero))))
                                )
                            ),
                            # Create a new row for the table.
                            DT::dataTableOutput("tabela")
                            
    
) ),

tabPanel("Filmes com mais de 500 mil Espectadores (1970-2018)",
         
         sidebarLayout(
             sidebarPanel(
                 
                 
                 
                 column(4,
                        selectInput("ano_2",
                                    "Ano:",
                                    c("Todos",
                                      unique(as.character(df_table_2$Ano ))))
                 ),
                 column(4,
                        selectInput("direcao_2",
                                    "Direcao:",
                                    c("Todos",
                                      unique(as.character(df_table_2$Direcao))))
                 ),
                 
                 column(4,
                        selectInput("produtora_2",
                                    "Produtora:",
                                    c("Todos",
                                      unique(as.character(df_table_2$Produtora))))
                 )
             ),
             # Create a new row for the table.
             DT::dataTableOutput("tabela_2")
             
             
         ) )









))          








server = shinyServer(function(input, output) {
    
    # Filter data based on selections
    output$tabela <- DT::renderDataTable(DT::datatable({
        
        if (input$ano != "Todos") {
            df_table <- df_table[df_table$Ano == input$ano,]
        }
        if (input$direcao != "Todos") {
            df_table <- df_table[df_table$Direcao == input$direcao,]
        }
        if (input$genero != "Todos") {
            df_table <- df_table[df_table$Genero == input$genero,]
        }
        df_table
    }))
    
    
    
    output$tabela_2 <- DT::renderDataTable(DT::datatable({
        
        if (input$ano_2 != "Todos") {
            df_table_2 <- df_table_2[df_table_2$Ano == input$ano_2,]
        }
        if (input$direcao_2 != "Todos") {
            df_table_2 <- df_table_2[df_table_2$Direcao == input$direcao_2,]
        }
        if (input$produtora_2 != "Todos") {
            df_table_2 <- df_table_2[df_table_2$Produtora == input$produtora_2,]
        }
        df_table_2
    }))
    
    
}
)


shinyApp(ui, server)