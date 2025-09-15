library(tidyverse)
library(timeDate)

data = read.csv("cpzp_cleaned.csv")

data$Datum_udalosti = as.Date(data$Datum_udalosti)
data$ockovany = as.factor(data$ockovany)
data$vakcinovan = as.factor(data$vakcinovan)
data$vek_kategorie = as.factor(data$vek_kategorie)

#ROZDELENI NA PRACOVNI DNY A VIKENDY
# 1) get the years you need (roky znam)
years = 2015:2023

# 2) build full holiday list
holiday_dates <- as.Date(unlist(
  lapply(years, function(y) {
    easter_sun  <- as.Date(format(Easter(y), "%Y-%m-%d")) # date of Easter Sunday
    c(
      as.Date(paste0(y, "-01-01")),               # New Year's Day
      easter_sun + 1,                             # Easter Monday 
      easter_sun - 2,                             # Velky patek
      as.Date(paste0(y, "-05-01")),               # Labour Day
      as.Date(paste0(y, "-05-08")),               # Victory Day
      as.Date(paste0(y, "-07-05")),               # Saints Cyril & Methodius
      as.Date(paste0(y, "-07-06")),               # Jan Hus Day
      as.Date(paste0(y, "-09-28")),               # Statehood Day
      as.Date(paste0(y, "-10-28")),               # Independent Czechoslovakia Day
      as.Date(paste0(y, "-11-17")),               # Struggle for Freedom & Democracy Day
      as.Date(paste0(y, "-12-24")),               # Christmas Eve
      as.Date(paste0(y, "-12-25")),               # Christmas Day
      as.Date(paste0(y, "-12-26")),               # Second Day of Christmas
      as.Date(paste0(y, "-12-31"))                # silvestr
    )
  })))

# VEKOVE KATEGORIE 
cr_kortikoidy_vek = data %>% 
  filter(!is.na(Equiv_prepocet)) %>% 
  group_by(Datum_udalosti,ockovany,vakcinovan,vek_kategorie,Pohlavi) %>% 
  # summarise(ekviv_prepocet_abs = sum(Equiv_prepocet),.groups = "drop")
  reframe(ekviv_prepocet_abs = sum(Equiv_prepocet),
          pocet_lidi = n_distinct(Id_pojistence))

cr_kortikoidy_vek$ukazatel = as.factor(as.numeric(cr_kortikoidy_vek$ockovany) + 
                                         as.numeric(cr_kortikoidy_vek$vakcinovan))
levels(cr_kortikoidy_vek$ukazatel) = c("NN","ZN","O")

#prepocet na osobu
cr_kortikoidy_vek$ekviv_prepocet_rel = 
  cr_kortikoidy_vek$ekviv_prepocet_abs / cr_kortikoidy_vek$pocet_lidi

cr_kortikoidy_vek = cr_kortikoidy_vek %>%
  mutate(
    day_type = case_when(
      Datum_udalosti %in% holiday_dates                       ~ "S",
      weekdays(Datum_udalosti) %in% c("sobota","neděle")      ~ "V",
      TRUE                                                    ~ "PD"
    ),
    day_type = factor(day_type)
  )

# GRAFY ########################################################################
cr_kortikoidy_vek %>% group_by(Datum_udalosti,ukazatel) %>% 
  filter(day_type=="PD") %>% 
  reframe(ekviv_prepocet_abs = sum(ekviv_prepocet_abs),
          ekviv_prepocet_rel = sum(ekviv_prepocet_abs)/sum(pocet_lidi),
          day_type=day_type,
          ukazatel = ukazatel) %>% 

  ggplot(aes(x=Datum_udalosti,y=ekviv_prepocet_abs,color=ukazatel)) + 
  geom_point()+
  labs(title = "Časová řada - kortikoidy",
                    x = "Datum",
                    y = "Množství kortikoidů")+
  scale_x_date(date_labels = "%b %Y",date_breaks = "4 months",guide = guide_axis(angle=60))+
  geom_vline(xintercept = as.Date(c("2015-1-1","2016-1-1","2017-1-1","2018-1-1","2019-1-1",
                                    "2020-1-1","2021-1-1","2022-1-1","2023-1-1","2024-1-1")),
             linetype="dashed",color="gray50")+
  theme_minimal()

# write.csv(cr_kortikoidy_vek,"cpzp_casova_rada.csv",row.names = FALSE)

{
  cr_kortikoidy_vek %>% group_by(Datum_udalosti,ukazatel,vek_kategorie) %>% 
    filter(day_type=="PD") %>% 
    summarise(ekviv_prepocet_celkem = sum(ekviv_prepocet_abs),
              pocet_lidi_celkem = sum(pocet_lidi),
              ekviv_prepocet_rel_celkem = ekviv_prepocet_celkem/pocet_lidi_celkem,
              .groups = "drop") %>% 
  
  ggplot(aes(x = Datum_udalosti, y = ekviv_prepocet_celkem,color = ukazatel)) +
    geom_point(alpha = 0.5) +
    labs(title = "Věkové kategorie, celkové počty",
         x = "Datum",
         y = "Množství kortikoidů",
         color = "Status") +
    theme_minimal() +
    theme(legend.position = "inside",legend.position.inside = c(0.8, 0.2))+
    facet_wrap(~vek_kategorie)
  }
