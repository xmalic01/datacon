library(tidyverse)
library(fixest)

data_raw = read.csv("OZP_preskladane.csv")

data_raw$Datum_udalosti = as.Date(data_raw$Datum_udalosti)
data_raw$Id_pojistence=to_integer(data_raw$Id_pojistence)

#celkovy pocet jedincu
id_unique = data_raw$Id_pojistence %>% unique() %>% data.frame()
(pocet_jedincu = id_unique %>% length())
#celkovy pocet ockovanych jedincu
(pocet_ockovanych = data_raw %>% filter(ockovany==1) %>% 
    summarise(n = length(unique(Id_pojistence))) %>% pull(n))
#vypocet a oznaceni prvnich kortikoidovych predpisu vubec####
{
  prvni_predpisy = data_raw %>% 
    filter(!is.na(Equiv_sloucenina)) %>% 
    arrange(Id_pojistence,Datum_udalosti) %>% 
    group_by(Id_pojistence) %>% 
    mutate(datum_prvniho_predpisu=min(Datum_udalosti)) %>% 
    slice(1) %>% 
    select(Id_pojistence,datum_prvniho_predpisu)
}

data_raw = data_raw %>% 
  left_join(prvni_predpisy,by=c("Id_pojistence"))

# data_raw$prvni_predpis=0
# data_raw[(!is.na(data_raw$datum_prvniho_predpisu))&(!is.na(data_raw$Equiv_sloucenina))&
                         # (data_raw$datum_udalosti==data_raw$datum_prvniho_predpisu),] %>% sum()

data_raw = data_raw %>% 
  mutate(prvni_predpis = if_else(!is.na(Equiv_sloucenina)&Datum_udalosti==datum_prvniho_predpisu,1,0))
#####

data_raw$pocet_predpisu_kortikoidu = data_raw %>% 
  group_by(Id_pojistence) %>% 
  mutate(n=sum(as.numeric(prvni_predpis))) %>% 
  pull(n)
#pocet neockovanych bez kortikoidu
(pocet_neockovanych_bez_kortikoidu = data_raw %>% filter(ockovany==0,pocet_predpisu_kortikoidu==0) %>% 
  summarise(n=length(unique(Id_pojistence))) %>% pull(n))
#kolik receptu na kortikoidy ve skupine 31-50
# nemame mesic narozeni - musim ho doplnit - nahodne vybranim z rovnomerneho rozdeleni
set.seed(42)
id_unique$aprox_mesic_narozeni = sample(
  x = 1:12,
  replace = TRUE,
  size = pocet_jedincu
)
data_raw = data_raw %>% left_join(id_unique,by=join_by(Id_pojistence==.))

data_raw$aprox_datum_narozeni = as.Date(paste0(data_raw$Rok_narozeni,"-",data_raw$aprox_mesic_narozeni,"-1"))
data_raw$vek = time_length(data_raw$Datum_udalosti-data_raw$aprox_datum_narozeni,"years")
data_raw$vek_31_50 = cut(data_raw$vek,breaks = c(-Inf, 30, 50,Inf),labels = c("<31", "31-50", ">50"))

(celkem_receptu_31_50 = data_raw %>% 
  filter(!is.na(Equiv_sloucenina),vek_31_50=="31-50") %>% 
  summarise(n=n()) %>% 
  pull(n))

#pocet predpisu pro ockovane ve skupine 31-50 v obdobi 1.1.2020-1.1.2023
(pocet_predpisu_ockovani_obdobi = data_raw %>% 
  filter(vek_31_50=="31-50",ockovany==1,!is.na(Equiv_sloucenina),Datum_udalosti>=as.Date("2020-1-1"),
         Datum_udalosti<=as.Date("2023-1-1")) %>% 
  nrow())

#pocet neplatnych zaznamu - jedine, co jsem nasel, bylo par radku s abnormalne vysokym poctem baleni v predpise

#pocet prvopredpisu v obodbi 1.1.2020-1.1.2023
(pocet_prvopredpisu_v_obdobi = data_raw %>% filter(prvni_predpis==1,Datum_udalosti>=as.Date("2020-1-1"),
                                                   Datum_udalosti<=as.Date("2023-1-1")) %>% 
    nrow())

#CISTENI DAT ###################################################################
data_clean = data_raw[is.na(data_raw$Datum_umrti),]

data_clean$Posledni_ukonceni_pojisteni = as.Date(data_clean$Posledni_ukonceni_pojisteni)
data_clean$Posledni_zahajeni_pojisteni = as.Date(data_clean$Posledni_zahajeni_pojisteni)
data_clean$Posledni_ukonceni_pojisteni[is.na(data_clean$Posledni_ukonceni_pojisteni)]=as.Date("9999-1-1")

#odstraneni lidi, kteri se pojistili pozde (po 2018)
data_clean = data_clean[data_clean$Posledni_zahajeni_pojisteni<=as.Date("2018-1-1"),]

#odstraneni lidi, kteri skoncili pojisteni moc brzo (driv nez 2023)
data_clean = data_clean[data_clean$Posledni_ukonceni_pojisteni>=as.Date("2023-1-1"),]


#v datech je jeden predpis s pocet_baleni = 98872.0, podle predchozich predpisu ma byt pravdepodobne = 2
data_clean$Pocet_baleni[data_clean$Pocet_baleni==98872.0]=2
data_clean$Pocet_baleni[data_clean$Pocet_baleni==9709.00]=6
data_clean$Pocet_baleni[data_clean$Pocet_baleni==502]=2
data_clean$Pocet_baleni[data_clean$Pocet_baleni==200]=2
data_clean$Pocet_baleni[data_clean$Pocet_baleni==0]=1

data_clean = data_clean %>% mutate(síla = parse_number(síla))

data_clean$Equiv_prepocet = 
  data_clean$Prednison_equiv*data_clean$síla*data_clean$Pocet_baleni*data_clean$Pocet_v_baleni

#celkem ockovanych 31-50 v analyze
(celkem_ockovanych_31_50_v_analyze = data_clean %>% filter(ockovany==1,vek_31_50=="31-50") %>% 
    summarise(n = length(unique(Id_pojistence))) %>% pull(n))

data_clean$vakcinovan = 0

#datumy prvniho, druheho a tretiho ockovani pro jednotlive pojistence
{
  prvni_vakcinace = data_clean %>% 
    filter(Typ_udalosti=="vakcinace") %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_prvniho_ockovani = min(Datum_udalosti),.groups = "drop")
  
  data_clean = data_clean %>% 
    left_join(prvni_vakcinace,by="Id_pojistence") %>% 
    mutate(vakcinovan = 
             ifelse(!is.na(datum_prvniho_ockovani)&Datum_udalosti>=datum_prvniho_ockovani,
                    1,0))
  data_clean$vakcinovan = as.factor(data_clean$vakcinovan)
  #rm(prvni_vakcinace)
  
  druha_vakcinace = data_clean %>% 
    filter(Typ_udalosti=="vakcinace",Datum_udalosti>datum_prvniho_ockovani) %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_druheho_ockovani = min(Datum_udalosti),.groups="drop")
  
  data_clean = data_clean %>% 
    left_join(druha_vakcinace,by="Id_pojistence")
  #rm(druha_vakcinace)
  
  treti_vakcinace = data_clean %>% 
    filter(Typ_udalosti=="vakcinace",Datum_udalosti>datum_druheho_ockovani) %>% 
    group_by(Id_pojistence) %>% 
    summarise(datum_tretiho_ockovani = min(Datum_udalosti),.groups = "drop")
  
  data_clean = data_clean %>% 
    left_join(treti_vakcinace,by="Id_pojistence")
  #rm(treti_vakcinace)
}


data_clean$vek_31_50=NULL
data_clean$vek_kategorie = cut(data_clean$vek,breaks = c(-Inf, 11, 29, 49, 69, Inf),
                         labels = c("0-11", "12-29", "30-49", "50-69", "70+"))

write.csv(data_clean,"ozp_cleaned.csv",row.names = FALSE)
