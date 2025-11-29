library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Model CAPM"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          width = 3,
          
          helpText("Analiza porównawcza modelu liniowego CAPM (Model A) vs. modelu kwadratowego (Model B)."),
          
          textInput("ticker_spolki", "Ticker Spółki", value = "AAPL"),
          textInput("ticker_rynku", "Ticker Rynku", value = "^GSPC"),
          
          dateRangeInput("daty", "Zakres dat",
                         start = "2020-01-01",
                         end = Sys.Date()),
          
          radioButtons("poziom_istotnosci", "Wybierz poziom istotności (α):",
                       choices = c("5% (0.05)" = 0.05,
                                   "1% (0.01)" = 0.01,
                                   "10% (0.10)" = 0.10),
                       selected = 0.05),
          
          
          actionButton("analizuj", "Uruchom Analizę", class = "btn-primary")
        ),

        mainPanel(
            width=9,
            
            tabsetPanel(type="tabs",
                        
                        tabPanel("1. Porównanie modeli (AIC)",
                                 br(), # Dodaje mały odstęp
                                 helpText("Kryterium AIC służy do wyboru modelu. Lepszy jest model z NIŻSZĄ wartością AIC."),
                                 verbatimTextOutput("aic_output"),
                                 h4("Wniosek:"),
                                 textOutput("wniosek_aic")
                        ),
                        tabPanel("2. Wykres Dopasowania",
                                 br(),
                                 helpText("Wykres pokazuje chmurę punktów (dane) oraz linię/krzywą dopasowania najlepszego wybranego modelu."),
                                 plotOutput("wykres_dopasowania", height = "500px") # Wyższy wykres
                        ),
                        tabPanel("3. Parametry Modelu",
                                 br(),
                                 helpText("Poniżej znajduje się automatyczna interpretacja oszacowanych parametrów dla najlepszego modelu."),
                                 
                                 uiOutput("interpretacja_parametrow"), 
                                 
                                 hr(), 
                                 h4("Surowe wyniki modelu (dla weryfikacji):"),
                                 verbatimTextOutput("podsumowanie_modelu")
                        ),
                        tabPanel("4. Diagnostyka Reszt",
                                 br(),
                                 helpText("Weryfikacja założenia o normalności reszt. Reszty nie są normalne, występują wartości odstające."),
                                 h4("Formalny Test Shapiro-Wilka"),
                                 verbatimTextOutput("test_normalnosci"),
                                 br(),
                                 h4("Wykresy Diagnostyczne"),
                                 plotOutput("wykresy_diagnostyczne", height = "400px")
                        )
            )
            
        )
    )
)
