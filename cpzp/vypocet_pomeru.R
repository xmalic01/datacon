library(tidyverse)

data = read.csv("data_ready.csv")

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
data$datum_prvniho_predpisu = as.Date(data$datum_prvniho_predpisu)
data$Pohlavi=as.factor(data$Pohlavi)
data$vek_pri_prvnim_ockovani = as.factor(data$vek_pri_prvnim_ockovani)
data$vek_pri_druhem_ockovani = as.factor(data$vek_pri_druhem_ockovani)
data$vek_pri_tretim_ockovani = as.factor(data$vek_pri_tretim_ockovani)

################################################################################
datumy_ockovani_cutoff_1 = data.frame(
  vek_pri_prvnim_ockovani = as.factor(c("0-11","12-29","30-49","50-69","70+")),
  cutoff_start_1 = as.Date(c("2021-12-13","2021-6-4","2021-5-10","2021-4-14","2021-3-1")),
  cutoff_end_1 = as.Date(c("2022-1-28","2021-8-13","2021-6-18","2021-6-4","2021-4-23"))  
)

datumy_ockovani_cutoff_2 = data.frame(
  vek_pri_druhem_ockovani = as.factor(c("0-11","12-29","30-49","50-69","70+")),
  cutoff_start_2 = as.Date(c("2022-1-3","2021-7-13","2021-6-20","2021-5-21","2021-3-28")),
  cutoff_end_2 = as.Date(c("2022-3-1","2021-9-3","2021-7-30","2021-7-21","2021-7-1"))
)

datumy_ockovani_cutoff_3 = data.frame(
  vek_pri_tretim_ockovani = as.factor(c("0-11","12-29","30-49","50-69","70+")),
  cutoff_start_3 = as.Date(c("2022-1-3","2022-1-1","2021-12-13","2021-11-29","2021-10-18")),
  cutoff_end_3=as.Date(c("2022-3-1","2022-2-11","2022-1-28","2022-1-21","2021-12-17"))
)

data$vlna1 = as.factor(ifelse(data$pocet_vakcinaci>=1,1,0))
data$vlna2 = as.factor(ifelse(data$pocet_vakcinaci>=2,2,
                              ifelse(data$pocet_vakcinaci==1,1,0)))
data$vlna3 = as.factor(ifelse(data$pocet_vakcinaci>=3,2,
                              ifelse(data$pocet_vakcinaci>=1,1,0)))



data = data %>% left_join(datumy_ockovani_cutoff_1,by="vek_pri_prvnim_ockovani")
data = data %>% left_join(datumy_ockovani_cutoff_2,by="vek_pri_druhem_ockovani")
data = data %>% left_join(datumy_ockovani_cutoff_3,by="vek_pri_tretim_ockovani")


# VYPOCET POMERU ###############################################################
{
  case_control_1 = data %>%
    select(Id_pojistence,Equiv_prepocet,Pohlavi,datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1,
           vek_pri_prvnim_ockovani,vlna1,Datum_udalosti,datum_prvniho_predpisu) %>%
    filter(between(datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1)) %>%
    group_by(Id_pojistence,Pohlavi,vek_pri_prvnim_ockovani,vlna1) %>%
    summarise(
              mesic_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani - days(30),
                                                      datum_prvniho_ockovani)],na.rm = TRUE),
              mesic_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani + days(1),
                                                    datum_prvniho_ockovani + days(30))],na.rm = TRUE),
              pulrok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani - days(180),
                                                            datum_prvniho_ockovani)],na.rm = TRUE),
              pulrok_po = sum(Equiv_prepocet[between(Datum_udalosti, datum_prvniho_ockovani + days(1),
                                                          datum_prvniho_ockovani + days(180))],na.rm = TRUE),
              rok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani - days(360),
                                                               datum_prvniho_ockovani)],na.rm = TRUE),
              rok_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani + days(1),
                                                  datum_prvniho_ockovani + days(360))],na.rm = TRUE),
              dva_roky_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani-days(730),
                                                         datum_prvniho_ockovani)],na.rm=TRUE),
              dva_roky_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_prvniho_ockovani+days(1),
                                                       datum_prvniho_ockovani+days(730))],na.rm = TRUE),
              datum_predpisu = datum_prvniho_predpisu[which.min(is.na(datum_prvniho_predpisu))],
              datum_ockovani = datum_prvniho_ockovani[which.min(is.na(datum_prvniho_ockovani))],
              dny_do_prvniho_predpisu = as.numeric(datum_predpisu-datum_ockovani),
              .groups = "drop")
}
{
  case_control_2 = data %>%
    select(Id_pojistence,Pohlavi,Equiv_prepocet,Datum_udalosti,datum_druheho_ockovani,
           vek_pri_druhem_ockovani,vlna2,cutoff_start_2,cutoff_end_2,datum_prvniho_predpisu,
           datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1,pocet_predpisu_kortikoidu) %>%
    filter(between(datum_druheho_ockovani,cutoff_start_2,cutoff_end_2),
           between(datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1)) %>%
    group_by(Id_pojistence,Pohlavi,vek_pri_druhem_ockovani,vlna2) %>%
    summarise(
              mesic_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani - days(30),
                                                      datum_druheho_ockovani)],na.rm = TRUE),
              mesic_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani + days(1),
                                                    datum_druheho_ockovani + days(30))],na.rm = TRUE),
              pulrok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani - days(180),
                                                       datum_druheho_ockovani)],na.rm = TRUE),
              pulrok_po = sum(Equiv_prepocet[between(Datum_udalosti, datum_druheho_ockovani + days(1),
                                                     datum_druheho_ockovani + days(180))],na.rm = TRUE),
              rok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani - days(365),
                                                    datum_druheho_ockovani)],na.rm = TRUE),
              rok_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani + days(1),
                                                  datum_druheho_ockovani + days(365))],na.rm = TRUE),
              dva_roky_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani-days(730),
                                                         datum_druheho_ockovani)],na.rm = TRUE),
              dva_roky_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_druheho_ockovani+days(1),
                                                       datum_druheho_ockovani+days(730))],na.rm = TRUE),
              datum_predpisu = datum_prvniho_predpisu[which.min(is.na(datum_prvniho_predpisu))],
              datum_ockovani = datum_druheho_ockovani[which.min(is.na(datum_druheho_ockovani))],
              dny_do_prvniho_predpisu = as.numeric(datum_predpisu-datum_ockovani),
              .groups = "drop")
}
{
  case_control_3 = data %>%
    select(Id_pojistence,Pohlavi,Equiv_prepocet,Datum_udalosti,datum_tretiho_ockovani,
           datum_prvniho_predpisu,vek_pri_tretim_ockovani,vlna3,cutoff_start_3,cutoff_end_3,
           datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1,datum_druheho_ockovani,
           cutoff_start_2,cutoff_end_2,pocet_predpisu_kortikoidu) %>%
    filter(between(datum_tretiho_ockovani,cutoff_start_3,cutoff_end_3),
           if_else(!is.na(datum_druheho_ockovani),between(datum_druheho_ockovani,cutoff_start_2,cutoff_end_2),
                   between(datum_prvniho_ockovani,cutoff_start_1,cutoff_end_1))) %>%
    group_by(Id_pojistence,Pohlavi,vek_pri_tretim_ockovani,vlna3) %>%
    summarise(
      mesic_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani - days(30),
                                              datum_tretiho_ockovani)],na.rm = TRUE),
      mesic_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani + days(1),
                                            datum_tretiho_ockovani + days(30))],na.rm = TRUE),
      pulrok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani - days(180),
                                               datum_tretiho_ockovani)],na.rm = TRUE),
      pulrok_po = sum(Equiv_prepocet[between(Datum_udalosti, datum_tretiho_ockovani + days(1),
                                             datum_tretiho_ockovani + days(180))],na.rm = TRUE),
      rok_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani - days(360),
                                            datum_tretiho_ockovani)],na.rm = TRUE),
      rok_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani + days(1),
                                          datum_tretiho_ockovani + days(360))],na.rm = TRUE),
      dva_roky_pred = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani-days(730),
                                                 datum_tretiho_ockovani)],na.rm=TRUE),
      dva_roky_po = sum(Equiv_prepocet[between(Datum_udalosti,datum_tretiho_ockovani+days(1),
                                               datum_tretiho_ockovani+days(730))],na.rm=TRUE),
      datum_predpisu = datum_prvniho_predpisu[which.min(is.na(datum_prvniho_predpisu))],
      datum_ockovani = datum_tretiho_ockovani[which.min(is.na(datum_tretiho_ockovani))],
      dny_do_prvniho_predpisu = as.numeric(datum_predpisu-datum_ockovani),
      .groups = "drop")
}

write_csv2(case_control_1,"prvni_vlna_ozp.csv")
write_csv2(case_control_2,"druha_vlna_ozp.csv")
write_csv2(case_control_3,"treti_vlna_ozp.csv")
