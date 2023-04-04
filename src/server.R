server <- function(input, output, session) {
  
  output$value <- renderPrint({ input$select })
  
  output$value <- renderPrint({ input$select2 })
  
  
  
  #wyświetlanie box-a ze średnią wartością zanieczyszczenia dla Delhidla danego roku i miesiąca
  output$value1 <- renderValueBox({
    valueBox(
      t <- formatC(as.numeric(DELHI_SR[input$miesiac_wyswietl,input$rok_wyswietl]),
                   digits = 2, format = "f", big.mark=',')
      ,paste('Średnie zanieczyszczenie Delhi w roku: ', input$rok_wyswietl, 
             'dla miesiąca: ', input$miesiac_wyswietl) 
      ,icon = icon("stats",lib='glyphicon',"fa-xs") #fa-xs wielkość ikony 
      ,color = "red"
      )
  })
  
  
  
  #wyświetlanie box-a ze średnią wartością zanieczyszczenia dla Pekinu
  output$value2 <- renderValueBox({
    valueBox(
      formatC(as.numeric(PEKIN_SR[input$miesiac_wyswietl,input$rok_wyswietl]),
              digits = 2, format = "f", big.mark=',')
      ,paste('Średnie zanieczyszczenie Pekinu w roku: ', input$rok_wyswietl, 
             'dla miesiąca: ', input$miesiac_wyswietl) 
      ,icon = icon("stats",lib='glyphicon',"fa-xs")
      ,color = "yellow")
  })
  
  
  
  #wyświetlanie box-a ze średnią wartością zanieczyszczenia dla Helsinek
  output$value3 <- renderValueBox({
    valueBox(
      formatC(as.numeric(HELSINKI_SR[input$miesiac_wyswietl,input$rok_wyswietl]),
              digits = 2, format = "f", big.mark=',')
      ,paste('Średnie zanieczyszczenie Helsinek w roku: ', input$rok_wyswietl, 
             'dla miesiąca: ', input$miesiac_wyswietl)
      ,icon = icon("stats",lib='glyphicon',"fa-xs")
      ,color = "green")
  })
  
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputDelhi2 <- reactive({
    Delhi2_3lata <- Delhi2_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputDelhi3 <- reactive({
    Delhi3_3lata <- Delhi3_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputPekin <- reactive({
    Pekin_3lata <- Pekin_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputPekin2 <- reactive({
    Pekin3_3lata <- Pekin3_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })
  
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputHelsinki <- reactive({
    Helsinki_3lata <- Helsinki_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })
  
  #Ustawianie reakcji wykresu na zmianę zakresu daty 
  dataInputHelsinki2 <- reactive({
    Helsinki3_3lata <- Helsinki3_3lata %>% 
      filter(date >= input$data_wyswietl[1] & date <= input$data_wyswietl[2])
  })

  #Wyświetlanie mapy świata z lokalizacją 3 wybranych miast na świecie
  output$Świat <- renderLeaflet({
    
    #dodanie kolorów w zaleznosci od zanieczyszczenia
    
    map <- leaflet(lokalizacje_swiat) %>% 
      addTiles(group = "OSM") %>% 
      addCircleMarkers(
        options = markerOptions(col = ~coll),
        color = ~coll,
        clusterOptions = markerClusterOptions(), 
        popup = popup_)
    map %>%
      #setView(40,40,2) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMiniMap()
  })
   
  #Wyświetlanie mapy z lokalizacją czujników w Delhi
  output$Delhi <- renderLeaflet({
    Delhi_pkt <- leaflet() %>% 
      addTiles() %>%
      addMarkers(lat=28.696022012351253, lng=77.18107596375631, popup="Satyawati College, Delhi") %>%
      addMarkers(lat=28.67090991328277, lng=77.12846544257657, popup="Punjabi Bagh, Delhi") %>%
      addMarkers(lat=28.701421327989603, lng=77.16498887962415, popup="Delhi Institute Of Tool Engineering") %>%
      fitBounds(lng1 = 77, lng2 = 77.5	, lat1 = 28.3, lat2 = 29) %>%
      setView(lng = 77.16, lat = 28.67, zoom = 11)
    Delhi_pkt
  })
  
  #Wyświetlanie mapy z lokalizacją czujników w Pekinie
  output$Pekin <- renderLeaflet({
    Pekin_pkt <- leaflet() %>% 
      addTiles() %>%
      addMarkers(lat=39.70910823530599, lng=116.79610210988997, popup="Yongledianzhen, Tongzhou, Pekin") %>%
      addMarkers(lat=40.140470935483826, lng=117.10023830315099, popup="Pinggu New Town, Pekin") %>%
      addMarkers(lat=40.459452228921315, lng=115.98451704935258, popup="Yanqing District, Pekin") %>%
      setView(lng = 116.5, lat = 40.0, zoom = 8)
    Pekin_pkt
  })
  
  #Wyświetlanie mapy z lokalizacją czujników w Helsinkach
  output$Helsinki <- renderLeaflet({
    Helsinki_pkt <- leaflet() %>% 
      addTiles() %>%
      addMarkers(lat=60.16996629091204, lng=24.93836911403921, popup="Helsinki Centrum") %>%
      addMarkers(lat=60.18002146137783, lng=24.949833908093428, popup="Kallio 2, Helsinki") %>%
      addMarkers(lat=60.2018011333608 , lng=24.945685436247874 , popup="Mäkelänkatu, Helsinki") %>%
      setView(lng = 24.9383, lat = 60.18, zoom = 12)
    Helsinki_pkt
  })
  
  
  #Wyświetlanie subplotu / wykresu z  czujnikami w Delhi   
  output$plotCentrum <- renderPlotly({
    
   
    #Satyawati College, Delhi
    g3lata_Delhi_pkt3 <- ggplot(data = dataInputDelhi3(), 
                                aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE) 
    
    #Yongledianzhen, Pekin
    g3lata_Pekin_pkt1 <- ggplot(data = dataInputPekin(), 
                                aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE) +
      labs(y = "pm25")
    
    
    #Helsinki centrum
    g3lata_Helsinki_pkt1 <- ggplot(data = dataInputHelsinki(), 
                                   aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE) +
      labs(x = "data") 
    
    
    g3lata_Centrum_pkty <- subplot(g3lata_Delhi_pkt3, g3lata_Pekin_pkt1, g3lata_Helsinki_pkt1, 
                                 nrows = 3, titleY = TRUE, margin = c(0.07, 0.0, 0.05, 0.01)) %>% 
      layout(annotations = list(
      list(x = 0.0 , y = 1.01, text = "Delhi", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.0 , y = 0.6, text = "Pekin", showarrow = F, xref='paper', yref='paper'),
      list(x = 0.0 , y = 0.24, text = "Helsinki", showarrow = F, xref='paper', yref='paper'))
    )
    
    ggplotly(g3lata_Centrum_pkty)
  })
  
  
  #Wyświetlanie wykresu z pierwszym czujnikiem w Pekinie 
  output$plotObrzeza <- renderPlotly({
    
    #Punjabi Bagh, Delhi
    g3lata_Delhi_pkt2 <- ggplot(data = dataInputDelhi2(), 
                                aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE)
    
    
    #Obrzeza Pekin
    #Yanqing District
    g3lata_Pekin_pkt3 <- ggplot(data = dataInputPekin2(), 
                                aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE) +
      labs(y = "pm25")  
    
    
    #Obrzeza Helsinki, Makelankatu 
    g3lata_Helsinki_pkt3 <- ggplot(data = dataInputHelsinki2(), 
                                   aes(x = date, y = pm25)) +
      geom_line(size =0.4) +
      geom_smooth(method = "gam", size = 0.4, se = FALSE) +
      labs(x = "data") 
    
    g3lata_Obrzeza_pkty <- subplot(g3lata_Delhi_pkt2, g3lata_Pekin_pkt3, g3lata_Helsinki_pkt3, 
                                 nrows = 3, titleY = TRUE, margin = c(0.07, 0.0, 0.05, 0.01)) %>% 
      layout(annotations = list(
        list(x = 0.0 , y = 1.01, text = "Delhi", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.0 , y = 0.6, text = "Pekin", showarrow = F, xref='paper', yref='paper'),
        list(x = 0.0 , y = 0.24, text = "Helsinki", showarrow = F, xref='paper', yref='paper'))
      )
    
    ggplotly(g3lata_Obrzeza_pkty)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  # WYKRESY SEZONOWOŚCI
  
  #DELHI
  
  output$plotDelhi2 <- renderPlotly({
    
    
    Delhi_season_gg <- subplot(style(season_Delhi1,showlegend = F), style(season_Delhi2,showlegend = F), season_Delhi3,
                                   titleY = TRUE) 
      
    gd <- ggplotly(Delhi_season_gg)
    
  })
  
  #PEKIN
  
  output$plotPekin2 <- renderPlotly({
    
    
    Pekin_season_gg <- subplot(style(season_Pekin1,showlegend = F), style(season_Pekin2,showlegend = F), season_Pekin3,
                               titleY = TRUE)  
    
    gp <- ggplotly(Pekin_season_gg ) 
  })
  
  #HELSINKI
  
  output$plotHelsinki2 <- renderPlotly({
    
    
    Helsinki_season_gg <- subplot(style(season_Helsinki1,showlegend = F), style(season_Helsinki2,showlegend = F), season_Helsinki3,
                               titleY = TRUE)  
    
    gh <- ggplotly(Helsinki_season_gg) 
  })
  
  
  
  
  
  # WYKRESY SŁUPKOWE
  
  #wyswietlanie wykresu slupkowego w zaleznosci od roku:
  gg_stan2019 <- reactive(gg_stan2019_plotly)
  gg_stan2020 <- reactive(gg_stan2020_plotly)
  gg_stan2021 <- reactive(gg_stan2021_plotly)
  
  graphInput <- reactive({
    switch(input$rok_wyswietl,
           "2019" = gg_stan2019(),
           "2020" = gg_stan2020(),
           "2021" = gg_stan2021())
  })
  
  output$plotIndex <- renderPlotly({
    graphInput()
 })
  
  
  
  
  
  
}
