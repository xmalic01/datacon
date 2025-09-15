library(shiny)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)

CPZP = read.csv("cpzp_casova_rada.csv")
OZP = read.csv("ozp_casova_rada.csv")

CPZP$Datum_udalosti = as.Date(CPZP$Datum_udalosti)
CPZP$ockovany = as.factor(CPZP$ockovany)
levels(CPZP$ockovany) = c(0,1,2)
CPZP$vakcinovan = as.factor(CPZP$vakcinovan)
CPZP$vek_kategorie = as.factor(CPZP$vek_kategorie)
CPZP$ukazatel = as.factor(CPZP$ukazatel)
levels(CPZP$ukazatel) = c(0,2,1)
CPZP$day_type = as.factor(CPZP$day_type)
CPZP$Pohlavi = factor(CPZP$Pohlavi,labels = c("M","Ž"))

OZP$Datum_udalosti = as.Date(OZP$Datum_udalosti)
OZP$ockovany = as.factor(OZP$ockovany)
levels(OZP$ockovany) = c(0,1,2)
OZP$vakcinovan = as.factor(OZP$vakcinovan)
OZP$vek_kategorie = as.factor(OZP$vek_kategorie)
OZP$ukazatel = as.factor(OZP$ukazatel)
levels(OZP$ukazatel) = c(0,2,1)
OZP$day_type = as.factor(OZP$day_type)
OZP$Pohlavi = factor(OZP$Pohlavi,labels = c("M","Ž"))


#UI#############################################################################

ui <- fluidPage(
    theme = bs_theme(bootswatch = "flatly"),
    # theme = bs_theme(bootswatch = "darkly"),
    # Application title
    titlePanel("Časová řada - kortikoidy"),

    sidebarLayout(
        sidebarPanel(
          # VYBRANI VEKOVYCH KATEGORII
            checkboxGroupInput(inputId = "vekoveKategorie",
                        label = "Věkové kategorie:",
                        choices = levels(CPZP$vek_kategorie),
                        selected = levels(CPZP$vek_kategorie)),
            
          # POHLAVI
            checkboxGroupInput(inputId = "pohlavi",
                               label = "Pohlaví:",
                               choices = c("M","Ž"),
                               selected = c("M","Ž")),
          
          # VYBRANI ROZSAHU DATUMU
            sliderInput(inputId = "dateRange",
                        label = "Rozsah osy X:",
                        min = min(CPZP$Datum_udalosti),
                        max = max(CPZP$Datum_udalosti),
                        value = as.Date(c(min(CPZP$Datum_udalosti),
                                          max(CPZP$Datum_udalosti)))),
            
          # VYBRANI ROZSAHU Y: CPZP 
          sliderInput(inputId = "yRange_CPZP",
                        "Rozsah osy Y pro CPZP:",
                        min = 0,
                        max = 250000,
                        value = c(0,200000),
                        step = 1000),
          
          # VYBRANI ROZSAHU Y: OZP 
          sliderInput(inputId = "yRange_OZP",
                      "Rozsah osy Y pro OZP:",
                      min = 0,
                      max = 150000,
                      value = c(0,125000),
                      step = 1000),
            
          # FILTROVANI VIKENDU
            checkboxInput(inputId = "vikendFiltr",
                      label = "Zobrazit víkendy a svátky?",
                      value = FALSE),
            
          # SPOJIT OCKOVANE do jedne skupiny (jeste neockovani a uz ockovani)
            checkboxInput(inputId = "spojitOckovane",
                          label = "Spojit zatím neočkované a už očkované?",
                          value = FALSE),
          
          # PREPNOUT MEZI ABSOLUTNIM MNOZSTVIM A MNOZSTVIM NA OSOBU
          checkboxInput(inputId = "abs_rel_switch",
                        label = "Zobrazit množství na osobu",
                        value = FALSE),
            
          # RESET BUTTON
            actionButton(inputId = "resetButton",
                         label = "Původní nastavení")
            
            
        ),

        # MAIN PANEL
        mainPanel(
          plotlyOutput("cr_CPZP",height = "400px",width = "800px"),
          plotlyOutput("cr_OZP",height = "400px",width = "800px")
        )
        
    )
)
# SERVER########################################################################

server <- function(input, output, session) {
#CPZP REAKTIVNI DATA############################################################
  # CPZP reaktivni data
  CPZP_reactive = reactive({
    CPZP %>%
      # filtr rozsahu osy X
      #filter(Datum_udalosti >= input$dateRange[1],
      #        Datum_udalosti <= input$dateRange[2]) %>%
      # vykreslovani vikendu a svatku
      { if (!input$vikendFiltr) filter(.,day_type=="PD") else .} %>%
      # filtr vekovych kategorii
      filter(vek_kategorie %in% input$vekoveKategorie) %>%
      #spojit jeste neockovane a uz ockovane?
      filter(Pohlavi %in% input$pohlavi) %>% 
      { if (!input$spojitOckovane) group_by(.,Datum_udalosti,ockovany,vakcinovan,ukazatel,day_type) %>%
      reframe(n=sum(ekviv_prepocet_abs),
              pocet_ve_skupine = sum(pocet_lidi),
              n_rel = round(n/pocet_ve_skupine,2),
              n_round = round(n))
        else group_by(.,Datum_udalosti,ockovany,day_type) %>%
          reframe(n=sum(ekviv_prepocet_abs),
                  pocet_ve_skupine = sum(pocet_lidi),
                  n_rel = round(n/pocet_ve_skupine,2),
                  n_round = round(n),
                  ukazatel = as.factor((as.numeric(ockovany)-1)*2))
          }
  })
#OZP REAKTIVNI DATA##############################################################
  # OZP reaktivni data
  OZP_reactive = reactive({
    OZP %>%
      # filtr rozsahu osy X
      #filter(Datum_udalosti >= input$dateRange[1],
      #        Datum_udalosti <= input$dateRange[2]) %>%
      # vykreslovani vikendu a svatku
      { if (!input$vikendFiltr) filter(.,day_type=="PD") else .} %>%
      # filtr vekovych kategorii
      filter(vek_kategorie %in% input$vekoveKategorie) %>%
      #spojit jeste neockovane a uz ockovane?
      filter(Pohlavi %in% input$pohlavi) %>% 
      { if (!input$spojitOckovane) group_by(.,Datum_udalosti,ockovany,vakcinovan,ukazatel,day_type) %>%
          reframe(n=sum(ekviv_prepocet_abs),
                  pocet_ve_skupine = sum(pocet_lidi),
                  n_rel = round(n/pocet_ve_skupine,2),
                  n_round = round(n))
        else group_by(.,Datum_udalosti,ockovany,day_type) %>%
          reframe(n=sum(ekviv_prepocet_abs),
                  pocet_ve_skupine = sum(pocet_lidi),
                  n_rel = round(n/pocet_ve_skupine,2),
                  n_round = round(n),
                  ukazatel = as.factor((as.numeric(ockovany)-1)*2))
      }
  })
#TLACITKO NA RESETOVANI GRAFU####################################################
  #tlacitko na resetovani rozsahu os
  observeEvent(input$resetButton,{
    updateSliderInput(session,"dateRange",value=as.Date(c(min(CPZP$Datum_udalosti),
                                                          max(CPZP$Datum_udalosti))))
    updateSliderInput(session,"yRange_CPZP",max=250000,value=c(0,200000),step=1000)
    
    updateSliderInput(session,"yRange_OZP",max=150000,value=c(0,125000),step=1000)
    
    updateCheckboxInput(session,"vikendFiltr",value=FALSE)
    
    updateCheckboxInput(session,"spojitOckovane",value = FALSE)
    
    updateSelectInput(session,"vekoveKategorie",selected = levels(CPZP$vek_kategorie))
    
    updateCheckboxInput(session,"abs_rel_switch",value = FALSE)
    
    updateSelectInput(session,"pohlavi",selected = c("M","Ž"))
  })
  
  #detekovani zmeny z absolutnich na relativni hodnoty ########
  observeEvent(input$abs_rel_switch,{
    if(input$abs_rel_switch){
      updateSliderInput(session,"yRange_CPZP",max=3000,value = c(0,1200),step=10)
      
      updateSliderInput(session,"yRange_OZP",max=3000,value = c(0,1200),step=10)
    }
    else{
      updateSliderInput(session,"yRange_CPZP",max=250000,value=c(0,200000),step=1000)
      
      updateSliderInput(session,"yRange_OZP",max=150000,value=c(0,125000),step=1000)
    }
  })
    
#GRAFY##########################################################################
  
#CPZP#########################################################################
    barvy = hue_pal()(3)
    output$cr_CPZP <- renderPlotly({
      #urceni jestli se maji vykreslit absolutni nebo relativni hodnoty
      n_type = if (input$abs_rel_switch) "n_rel" else "n_round"
      
      plot_ly(alpha=0.7) %>% 
        add_trace(data = CPZP_reactive() %>% filter(ukazatel==0),#ukazatel=="NN"),
              x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel,
              type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
              hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                     "Celkem: %{y:,1f} <br>",
                                     "Pocet osob: %{customdata}"),
              colors=c(barvy[1],barvy[3],barvy[2]),
              marker = list(
                size = 4
              ),name="Nikdy Neočkovaní") %>% 
        add_trace(data = CPZP_reactive() %>% filter(ukazatel==2),
                  x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel, 
                  type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
                  hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                         "Celkem: %{y:,1f} <br>",
                                         "Pocet osob: %{customdata}"),
                  colors=c(barvy[1],barvy[3],barvy[2]),
                  marker = list(
                    size = 4
                  ),name="Očkovaní") %>%
        add_trace(data = CPZP_reactive() %>% filter(ukazatel==1),
                  x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel, 
                  type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
                  hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                         "Celkem: %{y:,1f} <br>",
                                         "Pocet osob: %{customdata}"),
                  colors=c(barvy[1],barvy[3],barvy[2]),
                  marker = list(
                    size = 4
                  ),name="Zatím Neočkovaní") %>%
        layout(
          xaxis = list(
            title="Datum",
            type="date",
            range = as.list(input$dateRange)
          ),
          yaxis = list(
            title="Množství",
            range=as.list(input$yRange_CPZP)
          ),
          legend = list(
            orientation = "h", #horizontalne
            x = 0.5,           #uprostred
            y = -0.2,          #pod grafem
            xanchor = "center",
            yanchor = "top"    #y je vrch legendy
          ),
          title = "ČPZP"
        ) %>% 
        config(locale = "cs")
    })
#OZP############################################################################
    output$cr_OZP <- renderPlotly({
      #urceni jestli se maji vykreslit absolutni nebo relativni hodnoty
      n_type = if (input$abs_rel_switch) "n_rel" else "n_round"
      
      plot_ly(alpha=0.7) %>% 
        add_trace(data = OZP_reactive() %>% filter(ukazatel==0),#ukazatel=="NN"),
                  x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel,
                  type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
                  hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                         "Celkem: %{y:,1f} <br>",
                                         "Pocet osob: %{customdata}"),
                  colors=c(barvy[1],barvy[3],barvy[2]),
                  marker = list(
                    size = 4
                  ),name="Nikdy Neočkovaní") %>% 
        add_trace(data = OZP_reactive() %>% filter(ukazatel==2),
                  x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel, 
                  type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
                  hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                         "Celkem: %{y:,1f} <br>",
                                         "Pocet osob: %{customdata}"),
                  colors=c(barvy[1],barvy[3],barvy[2]),
                  marker = list(
                    size = 4
                  ),name="Očkovaní") %>%
        add_trace(data = OZP_reactive() %>% filter(ukazatel==1),
                  x=~Datum_udalosti,y=as.formula(paste0("~",n_type)),color = ~ukazatel, 
                  type = "scatter", mode = "markers",customdata = ~pocet_ve_skupine,
                  hovertemplate = paste0("Datum: %{x|%d.%m.%Y} <br>",
                                         "Celkem: %{y:,1f} <br>",
                                         "Pocet osob: %{customdata}"),
                  colors=c(barvy[1],barvy[3],barvy[2]),
                  marker = list(
                    size = 4
                  ),name="Zatím neočkovaní") %>%
        layout(
          xaxis = list(
            title="Datum",
            type="date",
            range = as.list(input$dateRange)
          ),
          yaxis = list(
            title="Množství",
            range=as.list(input$yRange_OZP)
          ),
          legend = list(
            orientation = "h", #horizontalne
            x = 0.5,           #uprostred
            y = -0.2,          #pod grafem
            xanchor = "center",
            yanchor = "top"    #y je vrch legendy
          ),
          title = "OZP"
        ) %>% 
        config(locale = "cs")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
