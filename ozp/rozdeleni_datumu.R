library(tidyverse)
library(scales)
library(patchwork)

data = read.csv("ozp_cleaned.csv")

data$Datum_udalosti = as.Date(data$Datum_udalosti)
data$Typ_udalosti = as.factor(data$Typ_udalosti)
data$vakcinovan = as.factor(data$vakcinovan)
data$ockovany = as.factor(data$ockovany)
data$datum_prvniho_ockovani = as.Date(data$datum_prvniho_ockovani)
data$datum_druheho_ockovani = as.Date(data$datum_druheho_ockovani)
data$datum_tretiho_ockovani = as.Date(data$datum_tretiho_ockovani)
data$vek_kategorie = as.factor(data$vek_kategorie)
data$prvni_predpis = as.factor(data$prvni_predpis)
data$pocet_predpisu_kortikoidu = as.factor(data$pocet_predpisu_kortikoidu)
data$aprox_datum_narozeni = as.Date(data$aprox_datum_narozeni)

#defaultni barvy, ktere ggplot2 pouziva pro zobrazeni 5 ruznych skupin
barvy_vek = hue_pal()(5)
barvy_3 = hue_pal()(3)

# DISTRIBUCE DATUMU OCKOVANI ###################################################
data$vek_pri_prvnim_ockovani = 
  cut(floor(time_length(data$datum_prvniho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))
data$vek_pri_druhem_ockovani = 
  cut(floor(time_length(data$datum_druheho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))
data$vek_pri_tretim_ockovani = 
  cut(floor(time_length(data$datum_tretiho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))

#distribuce datumu vsech ockovani podle vekove skupiny (prvni, druhe, treti ockovani)
cr_vsechny_vakciny_vek = data %>% 
  filter(Typ_udalosti=="vakcinace") %>% 
  group_by(Datum_udalosti,vek_kategorie) %>% 
  summarise(n=n(),.groups = "drop")

#distribuce datumu prvnich ockovani podle vekove skupiny
{
  cr_prvni_vakciny_vek= data %>% 
    filter(Typ_udalosti=="vakcinace",Datum_udalosti==datum_prvniho_ockovani) %>% 
    group_by(Id_pojistence) %>% 
    select(Id_pojistence,datum_prvniho_ockovani,vek_pri_prvnim_ockovani)%>% 
  group_by(datum_prvniho_ockovani,vek_pri_prvnim_ockovani) %>% 
  summarise(n=n(),.groups = "drop")
  }
#distribuce datumu druhych ockovani podle vekove skupiny
{
  cr_druhe_vakciny_vek = data %>% 
    filter(Typ_udalosti=="vakcinace",Datum_udalosti==datum_druheho_ockovani) %>% 
    group_by(Id_pojistence) %>% 
    select(Id_pojistence,datum_druheho_ockovani,vek_pri_druhem_ockovani)%>% 
  group_by(datum_druheho_ockovani,vek_pri_druhem_ockovani) %>% 
  summarise(n=n(),.groups = "drop")
}
#distribuce datumu tretich ockovani podle vekovych kategorii
{
  cr_treti_vakciny_vek = data %>% 
    filter(Typ_udalosti=="vakcinace",Datum_udalosti==datum_tretiho_ockovani) %>% 
    group_by(Id_pojistence) %>% 
    select(Id_pojistence,datum_tretiho_ockovani,vek_pri_tretim_ockovani)%>% 
  group_by(datum_tretiho_ockovani,vek_pri_tretim_ockovani) %>% 
  summarise(n=n(),.groups = "drop")
}

# GRAFY DISTRIBUCI DATUMU OCKOVANI ##########
#distribuce vsech ockovani vubec
{
  ggplot(cr_vsechny_vakciny_vek,aes(x=Datum_udalosti,y=n))+
  geom_col()+
  labs(title="Distribuce očkování",
       x = "Datum",
       y = "Počet očkování")+
  ylim(0,12500)+
  scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60))+
  geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                    "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
             linetype="dashed",color="red")+
  theme_minimal()
}
#vsechny ockovani podle veku
{
  ggplot(cr_vsechny_vakciny_vek,aes(x=Datum_udalosti,y=n,fill=vek_kategorie))+
  geom_col()+
  labs(title="Distribuce očkování",
       x = "Datum",
       y = "Počet očkování",
       fill = "Věk při očkování")+
  ylim(0,12500)+
  scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60))+
  geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                    "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
             linetype="dashed",color="red")+
  theme_minimal()
}
#vsechny 3 vlny ve 3 grafech najednou
{
  v1 = ggplot(cr_prvni_vakciny_vek,aes(x=datum_prvniho_ockovani,y=n,fill=vek_pri_prvnim_ockovani))+
    geom_col()+
    labs(title="Distribuce prvních očkování, OZP",
         x = "",
         y = "",
         fill = "Věk při očkování"
    )+
    ylim(0,12500)+
    scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60),
                 limits = as.Date(c("2021-1-1","2022-7-1")))+
    geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                      "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
               linetype="dashed",color="red")+
    theme_minimal()
  # theme(legend.position = "none")
  
  
  v2 = ggplot(cr_druhe_vakciny_vek,aes(x=datum_druheho_ockovani,y=n,fill = vek_pri_druhem_ockovani))+
    geom_col()+
    labs(title="Distribuce druhých očkování",
         x = "",
         y = "",
         fill = "Věk při očkování")+
    ylim(0,12500)+
    scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60),
                 limits = as.Date(c("2021-1-1","2022-7-1")))+
    geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                      "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
               linetype="dashed",color="red")+
    theme_minimal()
  # theme(legend.position = "right")
  
  
  v3 = ggplot(cr_treti_vakciny_vek,aes(x=datum_tretiho_ockovani,y=n,fill = vek_pri_tretim_ockovani))+
    geom_col()+
    labs(title="Distribuce třetích očkování",
         x = "Datum",
         y = "",
         fill="Věk při očkování"
    )+
    ylim(0,12500)+
    scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60),
                 limits = as.Date(c("2021-1-1","2022-7-1")))+
    geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                      "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
               linetype="dashed",color="red")+
    theme_minimal()
  # theme(legend.position = "none")
  
  combined = v1 + v2 + v3 & theme(legend.position = "right")
  combined + plot_layout(guides = "collect",nrow = 3)
}

#DATUMY UVOLNOVANI OCKOVANI:####################################################
# 
#> 5+:  13. 12. 2021 !(5+, ne 0+)!
#> 12+: 1.  7.  2021
#> 16+: 4.  6.  2021
#> 30+: 26. 5.  2021
#> 35+: 24. 5.  2021
#> 40+: 17. 5.  2021
#> 45+: 10. 5.  2021
#> 50+: 5.  5.  2021
#> 55+: 28. 4.  2021
#> 60+: 23. 4.  2021
#> 65+: 14. 4.  2021
#> 70+: 1.  3.  2021
#> 80+: 15. 1.  2021
#> zdravotnici a obyvatele domu pro seniory: 27. 12. 2020
datumy_zpristupneni_ockovani = as.Date(c("2021-12-13",                                    #0-11
                                         "2021-7-1","2021-6-4",                           #12-29
                                         "2021-5-26","2021-5-24","2021-5-17","2021-5-10", #30-49
                                         "2021-5-5","2021-5-28","2021-4-23","2021-4-14",  #50-69
                                         "2021-3-1","2021-1-15"))                         #70+
#datumy ockovany cutoff a peaky 1
{
  # summary(prvni_vakcinace_vek[prvni_vakcinace_vek$vek_kategorie=="0-11","datum_prvniho_ockovani"])
  datumy_ockovani_cutoff_1 = as.Date(c("2021-12-13","2022-1-28",  #0-11
                                       "2021-6-4","2021-8-13",     #12-29 
                                       "2021-5-10","2021-6-18",   #30-49 
                                       "2021-4-14","2021-6-4",    #50-69
                                       "2021-3-1","2021-4-23"     #70+
  ))  
  
  
  peaky_v_ockovanosti_1 = as.Date(c("2022-1-3",#0-11
                                    "2021-7-5",#12-29
                                    "2021-5-29",#30-49
                                    "2021-5-5",#50-69
                                    "2021-3-28" #70+
  ))
}
#datumy ockovani cutoff a peaky 2
{
  datumy_ockovani_cutoff_2 = as.Date(c("2022-1-3","2022-3-1",   #0-11
                                       "2021-7-13","2021-9-3",  #12-29
                                       "2021-6-20","2021-7-30", #30-49
                                       "2021-5-21","2021-7-21", #50-69
                                       "2021-3-28","2021-7-1"   #70+
  ))
  
  peaky_v_ockovanosti_2 = as.Date(c("2022-1-28",  #0-11
                                    "2021-8-9",   #12-29
                                    "2021-7-14",  #30-49
                                    "2021-6-16",  #50-69
                                    "2021-5-10"    #70+
  ))
}
#datumy ockovani cutoff a peaky 3
{
  datumy_ockovani_cutoff_3 = as.Date(c("2022-1-3","2022-3-1",   #0-11
                                       "2022-1-1","2022-2-11",  #12-29
                                       "2021-12-13","2022-1-28",  #30-49
                                       "2021-11-29","2022-1-21", #50-69
                                       "2021-10-18","2021-12-17"   #70+
  ))
  
  peaky_v_ockovanosti_3 = as.Date(c("2022-1-28",  #0-11
                                    "2022-1-21",  #12-29
                                    "2022-1-4",   #30-49
                                    "2021-12-20", #50-69
                                    "2021-11-23"   #70+
  ))
}

# GRAFY OCKOVANI ###############################################################
#prvni vlna
{
  #vsechno spolecne
  {
    ggplot(cr_prvni_vakciny_vek,aes(x=datum_prvniho_ockovani,y=n,fill=vek_pri_prvnim_ockovani))+
      geom_col()+
      labs()+
      geom_vline(xintercept = datumy_zpristupneni_ockovani,
                 linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_ockovani_cutoff_1,linetype="dashed",color="gray")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,9000)+
      theme_minimal()
  }
  #0-11
  o1_0_11={
    ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="0-11",],
           aes(x=datum_prvniho_ockovani,y=n))+
      geom_col(fill = barvy_vek[1])+
      # geom_vline(xintercept = peaky_v_ockovanosti_1[1],linetype="dashed",color="red")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[1],linetype="dashed",color="gray")+
      geom_vline(xintercept = datumy_ockovani_cutoff_1[1:2],linetype="dashed")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 0-11",
           x="", y="")+      
      theme_minimal()
  }
  #12-29
  o1_12_29={
    ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="12-29",],
           aes(x=datum_prvniho_ockovani,y=n))+
      geom_col(fill = barvy_vek[2])+
      geom_col(fill = barvy_3[1])+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[2:3],linetype="dashed",color="gray")+
      geom_vline(xintercept=datumy_ockovani_cutoff_1[3:4],linetype="dashed")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_1[2],color="red",linetype="dashed")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      # xlim(as.Date(c("2021-3-1","2022-2-1")))+
      ylim(0,4000)+
      labs(title = "Očkovaní, 12-29",
           x="", y="")+      
      theme_minimal()
  }
  #30-49
  o1_30_49={
    ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="30-49",],
           aes(x=datum_prvniho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[3])+
      geom_col(fill = barvy_3[2])+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[4:7],linetype="dashed",color="gray")+
      geom_vline(xintercept = datumy_ockovani_cutoff_1[5:6],linetype="dashed")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[7],linetype="dashed")+
      # geom_vline(xintercept = peaky_v_ockovanosti_1[3],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Očkovaní, 30-49",
           x="", y="")+      
      theme_minimal()
  }
  #50-69
  o1_50_69={
    ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="50-69",],
           aes(x=datum_prvniho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[4])+
      geom_col(fill = barvy_3[3])+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[8:11],linetype="dashed",color="gray")+
      geom_vline(xintercept = datumy_ockovani_cutoff_1[7:8],linetype="dashed")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[11],linetype="dashed")+
      # geom_vline(xintercept = peaky_v_ockovanosti_1[4],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Očkovaní, 50-69",
           x="", y="")+      
      theme_minimal()
  }
  #70+
  o1_70={
    ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="70+",],
           aes(x=datum_prvniho_ockovani,y=n))+
      geom_col(fill = barvy_vek[5])+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[12:13],linetype="dashed",color="gray")+
      geom_vline(xintercept = datumy_ockovani_cutoff_1[9:10],linetype="dashed")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[13],linetype="dashed")+
      # geom_vline(xintercept = peaky_v_ockovanosti_1[5],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 70+",
           x="", y="")+      
      theme_minimal()
  }
}
combined = o1_12_29 + o1_30_49 + o1_50_69 #+ o1_70
combined + plot_layout()

#druha vlna
{
  #vsechno spolecne
  {
    ggplot(cr_druhe_vakciny_vek,aes(x=datum_druheho_ockovani,y=n,fill=vek_pri_druhem_ockovani))+
      geom_col()+
      geom_vline(xintercept = datumy_zpristupneni_ockovani,
                 linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_ockovani_cutoff_1,linetype="dashed",color="gray")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,9000)+
      theme_minimal()
  }
  #0-11
  o2_0_11={
    ggplot(cr_druhe_vakciny_vek[cr_druhe_vakciny_vek$vek_pri_druhem_ockovani=="0-11",],
           aes(x=datum_druheho_ockovani,y=n))+
      geom_col(fill = barvy_vek[1])+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[1],linetype="dashed",color="red")+
      geom_vline(xintercept = datumy_ockovani_cutoff_2[1:2],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[1],linetype="dashed",color="gray")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 0-11",
           x="", y="")+      
      theme_minimal()
  }
  #12-29
  o2_12_29={
    ggplot(cr_druhe_vakciny_vek[cr_druhe_vakciny_vek$vek_pri_druhem_ockovani=="12-29",],
           aes(x=datum_druheho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[2])+
      geom_col(fill = barvy_3[1])+
      geom_vline(xintercept=datumy_ockovani_cutoff_2[3:4],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[2:3],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[2],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "12-29",
           x="", y="")+      
      theme_minimal()
  }
  #30-49
  o2_30_49={
    ggplot(cr_druhe_vakciny_vek[cr_druhe_vakciny_vek$vek_pri_druhem_ockovani=="30-49",],
           aes(x=datum_druheho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[3])+
      geom_col(fill = barvy_3[2])+
      geom_vline(xintercept = datumy_ockovani_cutoff_2[5:6],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[4:7],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[4:6],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[3],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "30-49",
           x="", y="")+      
      theme_minimal()
  }
  #50-69
  o2_50_69 ={
    ggplot(cr_druhe_vakciny_vek[cr_druhe_vakciny_vek$vek_pri_druhem_ockovani=="50-69",],
           aes(x=datum_druheho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[4])+
      geom_col(fill = barvy_3[3])+
      geom_vline(xintercept = datumy_ockovani_cutoff_2[7:8],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[8:11],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[8:10],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[4],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "50-69",
           x="", y="")+      
      theme_minimal()
  }
  #70+
  o2_70 ={
    ggplot(cr_druhe_vakciny_vek[cr_druhe_vakciny_vek$vek_pri_druhem_ockovani=="70+",],
           aes(x=datum_druheho_ockovani,y=n))+
      geom_col(fill = barvy_vek[5])+
      geom_vline(xintercept = datumy_ockovani_cutoff_2[9:10],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[12:13],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[12],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[5],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 70+",
           x="", y="")+      
      theme_minimal()
  }
}
combined = o2_12_29 + o2_30_49 + o2_50_69 #+ o2_70
combined + plot_layout()

#treti vlna
{
  #vsechno spolecne
  {
    ggplot(cr_treti_vakciny_vek,aes(x=datum_tretiho_ockovani,y=n,fill=vek_pri_tretim_ockovani))+
      geom_col()+
      geom_vline(xintercept = datumy_zpristupneni_ockovani,
                 linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_ockovani_cutoff_1,linetype="dashed",color="gray")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,9000)+
      theme_minimal()
  }
  #0-11 NENI OCKOVAN PRAKTICKY NIKDO
  o3_0_11 = {
    ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="0-11",],
           aes(x=datum_tretiho_ockovani,y=n))+
      geom_col(fill = barvy_vek[1])+
      # geom_vline(xintercept = peaky_v_ockovanosti_2[1],linetype="dashed",color="red")+
      geom_vline(xintercept = datumy_ockovani_cutoff_2[1:2],linetype="dashed",color="gray")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[1],linetype="dashed")+
      # xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 0-11",
           x="", y="")+      
      theme_minimal()
  }
  #12-29
  o3_12_29={
    ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="12-29",],
           aes(x=datum_tretiho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[2])+
      geom_col(fill = barvy_3[1])+
      geom_vline(xintercept=datumy_ockovani_cutoff_3[3:4],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[2:3],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_3[2],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "12-29",
           x="", y="")+      
      theme_minimal()
  }
  #30-49
  o3_30_49={
    ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="30-49",],
           aes(x=datum_tretiho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[3])+
      geom_col(fill = barvy_3[2])+
      geom_vline(xintercept = datumy_ockovani_cutoff_3[5:6],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[4:7],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[4:6],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_3[3],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "30-49",
           x="", y="")+      
      theme_minimal()
  }
  #50-69
  o3_50_69={
    ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="50-69",],
           aes(x=datum_tretiho_ockovani,y=n))+
      # geom_col(fill = barvy_vek[4])+
      geom_col(fill = barvy_3[3])+
      geom_vline(xintercept = datumy_ockovani_cutoff_3[7:8],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[8:11],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[8:10],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_3[4],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "50-69",
           x="", y="")+      
      theme_minimal()
  }
  #70+
  o3_70 ={
    ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="70+",],
           aes(x=datum_tretiho_ockovani,y=n))+
      geom_col(fill = barvy_vek[5])+
      geom_vline(xintercept = datumy_ockovani_cutoff_3[9:10],linetype="dashed")+
      geom_vline(xintercept = datumy_zpristupneni_ockovani[12:13],linetype="dashed",color="gray")+
      # geom_vline(xintercept = datumy_zpristupneni_ockovani[12],linetype="dashed",color="gray50")+
      # geom_vline(xintercept = peaky_v_ockovanosti_3[5],linetype="dashed",color="red")+
      xlim(as.Date(c("2020-12-27","2022-5-1")))+
      ylim(0,4000)+
      labs(title = "Věková kategorie: 70+",
           x="", y="")+      
      theme_minimal()
  }
}
combined = o3_12_29 + o3_30_49 + o3_50_69 #+ o3_70
combined + plot_layout()

#dohromady v jednom plotu
combined = o1_12_29 + o1_30_49 + o1_50_69 + o2_12_29 + o2_30_49 + o2_50_69 + o3_12_29 + o3_30_49 + o3_50_69
combined + plot_layout()

# vytvoreni referencnich dat pro neockovane ####################################
# vybrani neockovanych z jednotlivych vekovych trid
#prvni vlna
{
  neockovani_1_12_29 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_1[3]-aprox_datum_narozeni,"year")>=12,
           time_length(datumy_ockovani_cutoff_1[4]-aprox_datum_narozeni,"year")<=29) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_1_30_49 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_1[5]-aprox_datum_narozeni,"year")>=30,
           time_length(datumy_ockovani_cutoff_1[6]-aprox_datum_narozeni,"year")<=49) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_1_50_69 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_1[7]-aprox_datum_narozeni,"year")>=50,
           time_length(datumy_ockovani_cutoff_1[8]-aprox_datum_narozeni,"year")<=69) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_1_70 = data %>% 
    filter(ockovany==0, time_length(datumy_ockovani_cutoff_1[9]-aprox_datum_narozeni,"year")>=70) %>% 
    pull(Id_pojistence) %>% 
    unique()
}
#druha vlna
{
  neockovani_2_12_29 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_2[3]-aprox_datum_narozeni,"year")>=12,
           time_length(datumy_ockovani_cutoff_2[4]-aprox_datum_narozeni,"year")<=29) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_2_30_49 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_2[5]-aprox_datum_narozeni,"year")>=30,
           time_length(datumy_ockovani_cutoff_2[6]-aprox_datum_narozeni,"year")<=49) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_2_50_69 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_2[7]-aprox_datum_narozeni,"year")>=50,
           time_length(datumy_ockovani_cutoff_2[8]-aprox_datum_narozeni,"year")<=69) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_2_70 = data %>% 
    filter(ockovany==0, time_length(datumy_ockovani_cutoff_2[9]-aprox_datum_narozeni,"year")>=70) %>% 
    pull(Id_pojistence) %>% 
    unique()
}
#treti vlna
{
  neockovani_3_12_29 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_3[3]-aprox_datum_narozeni,"year")>=12,
           time_length(datumy_ockovani_cutoff_3[4]-aprox_datum_narozeni,"year")<=29) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_3_30_49 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_3[5]-aprox_datum_narozeni,"year")>=30,
           time_length(datumy_ockovani_cutoff_3[6]-aprox_datum_narozeni,"year")<=49) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_3_50_69 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_3[7]-aprox_datum_narozeni,"year")>=50,
           time_length(datumy_ockovani_cutoff_3[8]-aprox_datum_narozeni,"year")<=69) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  neockovani_3_70 = data %>% 
    filter(ockovany==0,time_length(datumy_ockovani_cutoff_3[9]-aprox_datum_narozeni,"year")>=70) %>% 
    pull(Id_pojistence) %>% 
    unique()
}
#druha vlna, ockovani jenom v prvni
{
  ockovani_jednou_2_12_29 = data %>% 
    filter(pocet_vakcinaci==1,vek_pri_prvnim_ockovani=="12-29",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[3],datumy_ockovani_cutoff_1[4])) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_2_30_49 = data %>% 
    filter(pocet_vakcinaci==1,vek_pri_prvnim_ockovani=="30_49",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[5],datumy_ockovani_cutoff_1[6])) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_2_50_69 = data %>% 
    filter(pocet_vakcinaci==1,vek_pri_prvnim_ockovani=="50-69",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[7],datumy_ockovani_cutoff_1[8])) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_2_70 = data %>% 
    filter(pocet_vakcinaci==1, vek_pri_prvnim_ockovani=="70+",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[9],datumy_ockovani_cutoff_1[10])) %>% 
    pull(Id_pojistence) %>% 
    unique()
}
#treti vlna, ockovani jenom jednou nebo dvakrat
{
  ockovani_jednou_3_12_29 = data %>% 
    filter(pocet_vakcinaci<=2,vek_pri_prvnim_ockovani=="12-29",
           if_else(!is.na(datum_druheho_ockovani),between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[3],
                                                          datumy_ockovani_cutoff_2[4]),
                   between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[3],datumy_ockovani_cutoff_1[4]))
           ) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_3_30_49 = data %>% 
    filter(pocet_vakcinaci<=2,vek_pri_prvnim_ockovani=="30-49",
           if_else(!is.na(datum_druheho_ockovani),between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[5],
                                                          datumy_ockovani_cutoff_2[6]),
                   between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[5],datumy_ockovani_cutoff_1[6]))
    ) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_3_50_69 = data %>% 
    filter(pocet_vakcinaci<=2,vek_pri_prvnim_ockovani=="50-69",
           if_else(!is.na(datum_druheho_ockovani),between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[7],
                                                          datumy_ockovani_cutoff_2[8]),
                   between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[7],datumy_ockovani_cutoff_1[8]))
    ) %>% 
    pull(Id_pojistence) %>% 
    unique()
  
  ockovani_jednou_3_70 = data %>% 
    filter(pocet_vakcinaci<2,vek_pri_prvnim_ockovani=="70+",
           if_else(!is.na(datum_druheho_ockovani),between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[9],
                                                         datumy_ockovani_cutoff_2[10]),
                          between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[9],datumy_ockovani_cutoff_1[10]))
           ) %>% 
    pull(Id_pojistence) %>% 
    unique()
}

#filtrovani "validnich" ockovani - distribuce rozdeleni datumu v danych casovych intervalech
#prvni vlna
{
  validni_prvni_vakciny_12_29 = cr_prvni_vakciny_vek %>% 
    filter(vek_pri_prvnim_ockovani=="12-29",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[3],datumy_ockovani_cutoff_1[4]))
  n_sum = sum(validni_prvni_vakciny_12_29$n)
  validni_prvni_vakciny_12_29$date_prob = validni_prvni_vakciny_12_29$n/n_sum
  
  validni_prvni_vakciny_30_49 = cr_prvni_vakciny_vek %>% 
    filter(vek_pri_prvnim_ockovani=="30-49",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[5],datumy_ockovani_cutoff_1[6]))
  n_sum = sum(validni_prvni_vakciny_30_49$n)
  validni_prvni_vakciny_30_49$date_prob = validni_prvni_vakciny_30_49$n/n_sum
  
  validni_prvni_vakciny_50_69 = cr_prvni_vakciny_vek %>% 
    filter(vek_pri_prvnim_ockovani=="50-69",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[7],datumy_ockovani_cutoff_1[8]))
  n_sum = sum(validni_prvni_vakciny_50_69$n)
  validni_prvni_vakciny_50_69$date_prob = validni_prvni_vakciny_50_69$n/n_sum
  
  validni_prvni_vakciny_70 = cr_prvni_vakciny_vek %>% 
    filter(vek_pri_prvnim_ockovani=="70+",
           between(datum_prvniho_ockovani,datumy_ockovani_cutoff_1[9],datumy_ockovani_cutoff_1[10]))
  n_sum = sum(validni_prvni_vakciny_70$n)
  validni_prvni_vakciny_70$date_prob = validni_prvni_vakciny_70$n/n_sum
}
#druha vlna
{
  validni_druhe_vakciny_12_29 = cr_druhe_vakciny_vek %>% 
    filter(vek_pri_druhem_ockovani=="12-29",
           between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[3],datumy_ockovani_cutoff_2[4]))
  n_sum = sum(validni_druhe_vakciny_12_29$n)
  validni_druhe_vakciny_12_29$date_prob = validni_druhe_vakciny_12_29$n/n_sum
  
  validni_druhe_vakciny_30_49 = cr_druhe_vakciny_vek %>% 
    filter(vek_pri_druhem_ockovani=="30-49",
           between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[5],datumy_ockovani_cutoff_2[6]))
  n_sum = sum(validni_druhe_vakciny_30_49$n)
  validni_druhe_vakciny_30_49$date_prob = validni_druhe_vakciny_30_49$n/n_sum
  
  validni_druhe_vakciny_50_69 = cr_druhe_vakciny_vek %>% 
    filter(vek_pri_druhem_ockovani=="50-69",
           between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[7],datumy_ockovani_cutoff_2[8]))
  n_sum = sum(validni_druhe_vakciny_50_69$n)
  validni_druhe_vakciny_50_69$date_prob = validni_druhe_vakciny_50_69$n/n_sum
  
  validni_druhe_vakciny_70 = cr_druhe_vakciny_vek %>% 
    filter(vek_pri_druhem_ockovani=="70+",
           between(datum_druheho_ockovani,datumy_ockovani_cutoff_2[9],datumy_ockovani_cutoff_2[10]))
  n_sum = sum(validni_druhe_vakciny_70$n)
  validni_druhe_vakciny_70$date_prob = validni_druhe_vakciny_70$n/n_sum
  }
#treti vlna
{
  validni_treti_vakciny_12_29 = cr_treti_vakciny_vek %>% 
    filter(vek_pri_tretim_ockovani=="12-29",
           between(datum_tretiho_ockovani,datumy_ockovani_cutoff_3[3],datumy_ockovani_cutoff_3[4]))
  n_sum = sum(validni_treti_vakciny_12_29$n)
  validni_treti_vakciny_12_29$date_prob = validni_treti_vakciny_12_29$n/n_sum
  
  validni_treti_vakciny_30_49 = cr_treti_vakciny_vek %>% 
    filter(vek_pri_tretim_ockovani=="30-49",
           between(datum_tretiho_ockovani,datumy_ockovani_cutoff_3[5],datumy_ockovani_cutoff_3[6]))
  n_sum = sum(validni_treti_vakciny_30_49$n)
  validni_treti_vakciny_30_49$date_prob = validni_treti_vakciny_30_49$n/n_sum
  
  validni_treti_vakciny_50_69 = cr_treti_vakciny_vek %>% 
    filter(vek_pri_tretim_ockovani=="50-69",
           between(datum_tretiho_ockovani,datumy_ockovani_cutoff_3[7],datumy_ockovani_cutoff_3[8]))
  n_sum = sum(validni_treti_vakciny_50_69$n)
  validni_treti_vakciny_50_69$date_prob = validni_treti_vakciny_50_69$n/n_sum
  
  validni_treti_vakciny_70 = cr_treti_vakciny_vek %>% 
    filter(vek_pri_tretim_ockovani=="70+",
           between(datum_tretiho_ockovani,datumy_ockovani_cutoff_3[9],datumy_ockovani_cutoff_3[10]))
  n_sum = sum(validni_treti_vakciny_70$n)
  validni_treti_vakciny_70$date_prob = validni_treti_vakciny_70$n/n_sum
}

#nahodne prideleni referencnich datumu neockovanym
#prvni vlna
{
  neockovani_1_12_29_datumy = data.frame("Id_pojistence" = neockovani_1_12_29,
                                         "datum_prvniho_ockovani" = sample(
                                           validni_prvni_vakciny_12_29$datum_prvniho_ockovani,
                                           size = length(neockovani_1_12_29),
                                           replace = TRUE,
                                           prob = validni_prvni_vakciny_12_29$date_prob)
  )
  
  neockovani_1_30_49_datumy = data.frame("Id_pojistence" = neockovani_1_30_49,
                                         "datum_prvniho_ockovani" = sample(
                                           validni_prvni_vakciny_30_49$datum_prvniho_ockovani,
                                           size = length(neockovani_1_30_49),
                                           replace = TRUE,
                                           prob = validni_prvni_vakciny_30_49$date_prob)
  )
  
  neockovani_1_50_69_datumy = data.frame("Id_pojistence" = neockovani_1_50_69,
                                         "datum_prvniho_ockovani" = sample(
                                           validni_prvni_vakciny_50_69$datum_prvniho_ockovani,
                                           size = length(neockovani_1_50_69),
                                           replace = TRUE,
                                           prob = validni_prvni_vakciny_50_69$date_prob)
  )
  
  neockovani_1_70_datumy = data.frame("Id_pojistence" = neockovani_1_70,
                                      "datum_prvniho_ockovani"=sample(
                                        validni_prvni_vakciny_70$datum_prvniho_ockovani,
                                        size = length(neockovani_1_70),
                                        replace = TRUE,
                                        prob = validni_prvni_vakciny_70$date_prob)
  )
}
#druha vlna
{
  neockovani_2_12_29_datumy = data.frame("Id_pojistence" = neockovani_2_12_29,
                                         "datum_druheho_ockovani" = sample(
                                           validni_druhe_vakciny_12_29$datum_druheho_ockovani,
                                           size = length(neockovani_2_12_29),
                                           replace = TRUE,
                                           prob = validni_druhe_vakciny_12_29$date_prob
                                         )
  )
  
  neockovani_2_30_49_datumy = data.frame("Id_pojistence" = neockovani_2_30_49,
                                         "datum_druheho_ockovani" = sample(
                                           validni_druhe_vakciny_30_49$datum_druheho_ockovani,
                                           size = length(neockovani_2_30_49),
                                           replace = TRUE,
                                           prob = validni_druhe_vakciny_30_49$date_prob
                                         )
  )
  
  neockovani_2_50_69_datumy = data.frame("Id_pojistence" = neockovani_2_50_69,
                                         "datum_druheho_ockovani" = sample(
                                           validni_druhe_vakciny_50_69$datum_druheho_ockovani,
                                           size = length(neockovani_2_50_69),
                                           replace = TRUE,
                                           prob = validni_druhe_vakciny_50_69$date_prob
                                         )
  )
  
  neockovani_2_70_datumy = data.frame("Id_pojistence" = neockovani_2_70,
                                      "datum_druheho_ockovani" = sample(
                                        validni_druhe_vakciny_70$datum_druheho_ockovani,
                                        size = length(neockovani_2_70),
                                        replace = TRUE,
                                        prob = validni_druhe_vakciny_70$date_prob
                                      )
  )
}
#treti vlna
{
  neockovani_3_12_29_datumy = data.frame("Id_pojistence" = neockovani_3_12_29,
                                         "datum_tretiho_ockovani" = sample(
                                           validni_treti_vakciny_12_29$datum_tretiho_ockovani,
                                           size = length(neockovani_3_12_29),
                                           replace = TRUE,
                                           prob = validni_treti_vakciny_12_29$date_prob
                                         )
  )
  
  neockovani_3_30_49_datumy = data.frame("Id_pojistence" = neockovani_3_30_49,
                                         "datum_tretiho_ockovani" = sample(
                                           validni_treti_vakciny_30_49$datum_tretiho_ockovani,
                                           size = length(neockovani_3_30_49),
                                           replace = TRUE,
                                           prob = validni_treti_vakciny_30_49$date_prob
                                         )
  )
  
  neockovani_3_50_69_datumy = data.frame("Id_pojistence" = neockovani_3_50_69,
                                         "datum_tretiho_ockovani" = sample(
                                           validni_treti_vakciny_50_69$datum_tretiho_ockovani,
                                           size = length(neockovani_3_50_69),
                                           replace = TRUE,
                                           prob = validni_treti_vakciny_50_69$date_prob
                                         )
  )
  
  neockovani_3_70_datumy = data.frame("Id_pojistence" = neockovani_3_70,
                                      "datum_tretiho_ockovani" = sample(
                                        validni_treti_vakciny_70$datum_tretiho_ockovani,
                                        size = length(neockovani_3_70),
                                        replace = TRUE,
                                        prob = validni_treti_vakciny_70$date_prob
                                      ))
}
#druha vlna ockovani jednou
{
  ockovani_jednou_2_12_29_datumy = data.frame("Id_pojistence" = ockovani_jednou_2_12_29,
                                              "datum_druheho_ockovani" = sample(
                                                validni_druhe_vakciny_12_29$datum_druheho_ockovani,
                                                size = length(ockovani_jednou_2_12_29),
                                                replace = TRUE,
                                                prob = validni_druhe_vakciny_12_29$date_prob
                                              ))
  
  ockovani_jednou_2_30_49_datumy = data.frame("Id_pojistence" = ockovani_jednou_2_30_49,
                                              "datum_druheho_ockovani" = sample(
                                                validni_druhe_vakciny_30_49$datum_druheho_ockovani,
                                                size = length(ockovani_jednou_2_30_49),
                                                replace = TRUE,
                                                prob = validni_druhe_vakciny_30_49$date_prob
                                              ))
  
  ockovani_jednou_2_50_69_datumy = data.frame("Id_pojistence" = ockovani_jednou_2_50_69,
                                              "datum_druheho_ockovani" = sample(
                                                validni_druhe_vakciny_50_69$datum_druheho_ockovani,
                                                size = length(ockovani_jednou_2_50_69),
                                                replace = TRUE,
                                                prob = validni_druhe_vakciny_50_69$date_prob
                                              ))
  
  ockovani_jednou_2_70_datumy = data.frame("Id_pojistence" = ockovani_jednou_2_70,
                                           "datum_druheho_ockovani" = sample(
                                             validni_druhe_vakciny_70$datum_druheho_ockovani,
                                             size = length(ockovani_jednou_2_70),
                                             replace = TRUE,
                                             prob = validni_druhe_vakciny_70$date_prob
                                           ))
}
#treti vlna ockovani jednou nebo dvakrat
{
  ockovani_jednou_3_12_29_datumy = data.frame("Id_pojistence" = ockovani_jednou_3_12_29,
                                              "datum_tretiho_ockovani" = sample(
                                                validni_treti_vakciny_12_29$datum_tretiho_ockovani,
                                                size = length(ockovani_jednou_3_12_29),
                                                replace = TRUE,
                                                prob = validni_treti_vakciny_12_29$date_prob
                                              ))
  
  ockovani_jednou_3_30_49_datumy = data.frame("Id_pojistence" = ockovani_jednou_3_30_49,
                                              "datum_tretiho_ockovani" = sample(
                                                validni_treti_vakciny_30_49$datum_tretiho_ockovani,
                                                size = length(ockovani_jednou_3_30_49),
                                                replace = TRUE,
                                                prob = validni_treti_vakciny_30_49$date_prob
                                              ))
  
  ockovani_jednou_3_50_69_datumy = data.frame("Id_pojistence" = ockovani_jednou_3_50_69,
                                              "datum_tretiho_ockovani" = sample(
                                                validni_treti_vakciny_50_69$datum_tretiho_ockovani,
                                                size = length(ockovani_jednou_3_50_69),
                                                replace = TRUE,
                                                prob = validni_treti_vakciny_50_69$date_prob
                                              ))
  
  ockovani_jednou_3_70_datumy = data.frame("Id_pojistence" = ockovani_jednou_3_70,
                                           "datum_tretiho_ockovani" = sample(
                                             validni_treti_vakciny_70$datum_tretiho_ockovani,
                                             size = length(ockovani_jednou_3_70),
                                             replace = TRUE,
                                             prob = validni_treti_vakciny_70$date_prob
                                           ))
}

# spojeni dvojic ID, DATUM do jednoho vektoru
#prvni vlna
neockovani_1 = bind_rows(list(neockovani_1_12_29_datumy,neockovani_1_30_49_datumy,
                              neockovani_1_50_69_datumy,neockovani_1_70_datumy))
neockovani_2 = bind_rows(list(neockovani_2_12_29_datumy,neockovani_2_30_49_datumy,
                              neockovani_2_50_69_datumy,neockovani_2_70_datumy))
neockovani_3 = bind_rows(list(neockovani_3_12_29_datumy,neockovani_3_30_49_datumy,
                              neockovani_3_50_69_datumy,neockovani_3_70_datumy))
ockovani_jednou_2 = bind_rows(list(ockovani_jednou_2_12_29_datumy,ockovani_jednou_2_30_49_datumy,
                                   ockovani_jednou_2_50_69_datumy,ockovani_jednou_2_70_datumy))
ockovani_jednou_3 = bind_rows(list(ockovani_jednou_3_12_29_datumy,ockovani_jednou_3_30_49_datumy,
                                   ockovani_jednou_3_50_69_datumy,ockovani_jednou_3_70_datumy))

#pripojeni datumu do puvodniho datasetu - neockovani
{
  #prvni vlna
  data = data %>% left_join(neockovani_1,by="Id_pojistence",suffix = c("","_new")) %>% 
    mutate(datum_prvniho_ockovani = if_else(!is.na(datum_prvniho_ockovani_new),datum_prvniho_ockovani_new,
                                            datum_prvniho_ockovani)) %>% 
    select(-datum_prvniho_ockovani_new)
  #druha vlna
  data = data %>% left_join(neockovani_2,by="Id_pojistence",suffix = c("","_new")) %>% 
    mutate(datum_druheho_ockovani = if_else(!is.na(datum_druheho_ockovani_new),datum_druheho_ockovani_new,
                                            datum_druheho_ockovani)) %>% 
    select(-datum_druheho_ockovani_new)
  #treti vlna
  data = data %>% left_join(neockovani_3,by="Id_pojistence",suffix = c("","_new")) %>% 
    mutate(datum_tretiho_ockovani = if_else(!is.na(datum_tretiho_ockovani_new),datum_tretiho_ockovani_new,
                                            datum_tretiho_ockovani)) %>% 
    select(-datum_tretiho_ockovani_new)
  
  #druha vlna ockovani jednou
  data = data %>% left_join(ockovani_jednou_2,by="Id_pojistence",suffix = c("","_new")) %>% 
    mutate(datum_druheho_ockovani = if_else(!is.na(datum_druheho_ockovani_new),datum_druheho_ockovani_new,
                                            datum_druheho_ockovani)) %>% 
    select(-datum_druheho_ockovani_new)  
  
  #treti vlna ockovani jednou
  data = data %>% left_join(ockovani_jednou_3,by="Id_pojistence",suffix = c("","_new")) %>% 
    mutate(datum_tretiho_ockovani = if_else(!is.na(datum_tretiho_ockovani_new),datum_tretiho_ockovani_new,
                                            datum_tretiho_ockovani)) %>% 
    select(-datum_tretiho_ockovani_new)
}

#pripocitani vek pri prvnim ockovani
data$vek_pri_prvnim_ockovani = 
  cut(floor(time_length(data$datum_prvniho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))
data$vek_pri_druhem_ockovani = 
  cut(floor(time_length(data$datum_druheho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))
data$vek_pri_tretim_ockovani = 
  cut(floor(time_length(data$datum_tretiho_ockovani-data$aprox_datum_narozeni,"years")),
      breaks=c(-Inf,11,29,49,69,Inf),
      labels=c("0-11","12-29","30-49","50-69","70+"))



write.csv(data,"data_ready.csv",row.names = FALSE)

#GRAFY ROZDELENI DATUMU ########################################################
# aby bylo videt, ze jsou stejny

#prvni nevakciny
{
  #12-29
  cr_prvni_nevakciny_12_29= data %>% 
    filter(ockovany==0,!is.na(datum_prvniho_ockovani),vek_pri_prvnim_ockovani=="12-29") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_prvniho_neockovani = first(datum_prvniho_ockovani)) %>% 
    group_by(datum_prvniho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #30-49
  cr_prvni_nevakciny_30_49= data %>% 
    filter(ockovany==0,!is.na(datum_prvniho_ockovani),vek_pri_prvnim_ockovani=="30-49") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_prvniho_neockovani = first(datum_prvniho_ockovani)) %>% 
    group_by(datum_prvniho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #50-69
  cr_prvni_nevakciny_50_69= data %>% 
    filter(ockovany==0,!is.na(datum_prvniho_ockovani),vek_pri_prvnim_ockovani=="50-69") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_prvniho_neockovani = first(datum_prvniho_ockovani)) %>% 
    group_by(datum_prvniho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
}

#druhe nevakciny
{
  #12-29
  cr_druhe_nevakciny_12_29= data %>% 
    filter(ockovany==0,!is.na(datum_druheho_ockovani),vek_pri_druhem_ockovani=="12-29") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_druheho_neockovani = first(datum_druheho_ockovani)) %>% 
    group_by(datum_druheho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #30-49
  cr_druhe_nevakciny_30_49= data %>% 
    filter(ockovany==0,!is.na(datum_druheho_ockovani),vek_pri_druhem_ockovani=="30-49") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_druheho_neockovani = first(datum_druheho_ockovani)) %>% 
    group_by(datum_druheho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #50-69
  cr_druhe_nevakciny_50_69= data %>% 
    filter(ockovany==0,!is.na(datum_druheho_ockovani),vek_pri_druhem_ockovani=="50-69") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_druheho_neockovani = first(datum_druheho_ockovani)) %>% 
    group_by(datum_druheho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
}

#treti nevakciny
{
  #12-29
  cr_treti_nevakciny_12_29= data %>% 
    filter(ockovany==0,!is.na(datum_tretiho_ockovani),vek_pri_tretim_ockovani=="12-29") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_tretiho_neockovani = first(datum_tretiho_ockovani)) %>% 
    group_by(datum_tretiho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #30-49
  cr_treti_nevakciny_30_49= data %>% 
    filter(ockovany==0,!is.na(datum_tretiho_ockovani),vek_pri_tretim_ockovani=="30-49") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_tretiho_neockovani = first(datum_tretiho_ockovani)) %>% 
    group_by(datum_tretiho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
  
  #50-69
  cr_treti_nevakciny_50_69= data %>% 
    filter(ockovany==0,!is.na(datum_tretiho_ockovani),vek_pri_tretim_ockovani=="50-69") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_tretiho_neockovani = first(datum_tretiho_ockovani)) %>% 
    group_by(datum_tretiho_neockovani) %>% 
    summarise(n=n(),.groups = "drop")
}

#ockovani
{
  ggplot(cr_prvni_vakciny_vek[cr_prvni_vakciny_vek$vek_pri_prvnim_ockovani=="12-29",],
         aes(x=datum_prvniho_ockovani,y=n))+
    geom_col(fill = barvy_vek[2])+
    geom_vline(xintercept = datumy_zpristupneni_ockovani[2:3],linetype="dashed",color="gray")+
    geom_vline(xintercept=datumy_ockovani_cutoff_1[3:4],linetype="dashed",color="black")+
    # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
    # geom_vline(xintercept = peaky_v_ockovanosti_1[2],color="red",linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "OZP, očkovaní, 12-29",
         x="", y="")+      
    theme_minimal()
}
#neockovani
n1_12_29 = {
  ggplot(cr_prvni_nevakciny_12_29,
         aes(x=datum_prvniho_neockovani,y=n))+
    # geom_col(fill = barvy_vek[2])+
    geom_col(fill = barvy_vek[2])+
    geom_vline(xintercept=datumy_ockovani_cutoff_1[3:4],linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "OZP, neočkovaní, 12-29",
         x="", y="")+      
    theme_minimal()
}
n1_30_49 = {
  ggplot(cr_prvni_nevakciny_30_49,
         aes(x=datum_prvniho_neockovani,y=n))+
    # geom_col(fill = barvy_vek[3])+
    geom_col(fill = barvy_3[2])+
    geom_vline(xintercept=datumy_ockovani_cutoff_1[5:6],linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "Neočkovaní, věková kategorie: 12-29",
         x="", y="")+      
    theme_minimal()
}
n1_50_69 = {
  ggplot(cr_prvni_nevakciny_50_69,
         aes(x=datum_prvniho_neockovani,y=n))+
    # geom_col(fill = barvy_vek[4])+
    geom_col(fill = barvy_3[3])+
    geom_vline(xintercept=datumy_ockovani_cutoff_1[7:8],linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "Neočkovaní, věková kategorie: 12-29",
         x="", y="")+      
    theme_minimal()
}

combined = o1_12_29 + o1_30_49 + o1_50_69 + n1_12_29 + n1_30_49 + n1_50_69
combined + plot_layout()

{
  ggplot(cr_treti_vakciny_vek[cr_treti_vakciny_vek$vek_pri_tretim_ockovani=="50-69",],
         aes(x=datum_tretiho_ockovani,y=n))+
    geom_col(fill = barvy_vek[2])+
    # geom_vline(xintercept=datumy_ockovani_cutoff_1[3:4],linetype="dashed",color="gray")+
    # geom_vline(xintercept = datumy_zpristupneni_ockovani[3],linetype="dashed")+
    # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
    # geom_vline(xintercept = peaky_v_ockovanosti_1[2],color="red",linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "Očkovaní",
         x="", y="")+      
    theme_minimal()
}
{
  ggplot(cr_treti_nevakciny_50_69,
         aes(x=datum_tretiho_neockovani,y=n))+
    geom_col(fill = barvy_vek[2])+
    # geom_vline(xintercept=datumy_ockovani_cutoff_1[3:4],linetype="dashed",color="gray")+
    # geom_vline(xintercept = datumy_zpristupneni_ockovani[3],linetype="dashed")+
    # geom_vline(xintercept = datumy_zpristupneni_ockovani[2],linetype="dashed",color="gray50")+
    # geom_vline(xintercept = peaky_v_ockovanosti_1[2],color="red",linetype="dashed")+
    xlim(as.Date(c("2020-12-27","2022-5-1")))+
    ylim(0,4000)+
    labs(title = "Neočkovaní",
         x="", y="")+      
    theme_minimal()
}

