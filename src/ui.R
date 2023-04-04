#Header
header <- dashboardHeader(
  title = "Analiza zanieczyszczenia powietrza",
  titleWidth = 450
)


#Sidebar content
sidebar <- dashboardSidebar(
    sidebarMenu(
      style = "position: fixed;
               width: 220px;
               overflow: visible;",
      menuItem(
        "DATA",
        tabName = "data",
        icon = icon("calendar"),
        br(),
        #Ustawianie przycisku do zakresu daty jaka ma być wyświetlana na wykresie
        dateRangeInput(
          inputId = "data_wyswietl",
          label = h4("Wybierz zakres daty: "),
          start = min(Delhi_3lata$date, na.rm = TRUE), 
          end = max(Delhi_3lata$date, na.rm = TRUE),
          min = min(Delhi_3lata$date, na.rm = TRUE),
          max = max(Delhi_3lata$date, na.rm = TRUE)
        ),
        selectInput("rok_wyswietl", label = h4("Wybierz rok: "), 
                    choices = list("2019" = 2019, "2020" = 2020, "2021" = 2021), 
                    selected = 2021),
        selectInput("miesiac_wyswietl", label = h4("Wybierz miesiąc: "), 
                    choices = list("styczeń" = "styczeń", "luty" = "luty", "marzec" = "marzec",
                                   "kwiecień" = "kwiecień", "maj" = "maj", "czerwiec" = "czerwiec",
                                   "lipiec" = "lipiec", "sierpień" = "sierpień", "wrzesień" = "wrzesień",
                                   "październik" = "październik", "listopad" = "listopad", 
                                   "grudzień" = "grudzień"),
                    selected = 1)
      )   
    )
)

#Body
b1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3"))



b2 <- fluidRow(
    #Box z mapą interaktywną x4
    tabBox(
      title = "Lokalizacja", 
      height = "600px", 
      tabPanel("Świat", leafletOutput("Świat",height = "550px",width = "100%")),
      tabPanel("Delhi", leafletOutput("Delhi",height = "550px",width = "100%")),
      tabPanel("Pekin", leafletOutput("Pekin",height = "550px",width = "100%")),
      tabPanel("Helsinki", leafletOutput("Helsinki",height = "550px",width = "100%"))
    ),
    
    #Box z wykresami z linią regresji 
    tabBox(
      side = "left", 
      height = "600px", 
      selected = "Centrum",
      tabPanel("Centrum", plotlyOutput("plotCentrum",height = "550px",width = "100%")),
      tabPanel("Obrzeża", plotlyOutput("plotObrzeza",height = "550px",width = "100%")),
    )
)

b3 <- fluidRow(
  #Box z wykresami, rząd 2
  tabBox(
    title = "Wykres sezonowości", 
    height = "500px",
    width = "100%",
    selected = "Delhi",
    tabPanel("Delhi", plotlyOutput("plotDelhi2",height = "450px",width = "100%")),
    tabPanel("Pekin", plotlyOutput("plotPekin2",height = "450px",width = "100%")),
    tabPanel("Helsinki", plotlyOutput("plotHelsinki2",height = "450px",width = "100%"))
  )
)

b4 <- fluidRow(
  #Box z wykresami słupkowymi, rząd 3
  tabBox(
    title = "Poziom zanieczyszczenia", 
    height = "500px",
    width = "100%",
    tabPanel("",plotlyOutput("plotIndex",height = "450px",width = "100%"))
  )
)

body <- dashboardBody(b1, b2, b3, b4) 

ui <- dashboardPage(skin = "black",header,sidebar,body)
