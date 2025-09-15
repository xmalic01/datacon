library(tidyverse)

df = read.csv2("prvni_vlna_ozp.csv")

df = df %>% filter(!vek_pri_prvnim_ockovani %in% c("0-11","70+"))
df$vlna1 = factor(df$vlna1,labels=c("neočkovaní","očkovaní"))
df$vek_pri_prvnim_ockovani = as.factor(df$vek_pri_prvnim_ockovani)
df$datum_ockovani = as.Date(df$datum_ockovani)
df$datum_predpisu = as.Date(df$datum_predpisu)

# CASOVA RADA ##################################################################
date_range = df %>% 
  group_by(datum_predpisu,vlna1,vek_pri_prvnim_ockovani) %>% 
  summarise(n=n(),.groups = "drop") %>% 
  rename(
    status = vlna1,
    vek_kategorie = vek_pri_prvnim_ockovani
  ) %>% drop_na() %>% 
  group_by(status,vek_kategorie) %>%
  complete(datum_predpisu = seq.Date(min(df$datum_predpisu,na.rm=TRUE),max(df$datum_predpisu,na.rm = TRUE),by="day"),
           fill = list(n=0))

ggplot(date_range) +
  geom_col(aes(x=datum_predpisu,y=n,fill = status))+
  theme_minimal()+
  facet_grid(status~vek_kategorie,scales="fixed")+
  labs(
    title = "OZP: datumy prvopředpisů",
    x = "Datum prvního předpisu",
    y = "Množství předpisů"
  )+
  theme(
    strip.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1))

# POMERY #######################################################################
df$index = if_else(df$dny_do_prvniho_predpisu > 0, 1,-1)

ratios = df %>% 
  group_by(vek_pri_prvnim_ockovani,vlna1) %>%
  summarise(pomer = sum(index>0,na.rm = TRUE)/sum(index<0,na.rm = TRUE),
            log_pomer = log2(pomer),
            N=n(),
            .groups = "drop") %>% 
  rename(AGE_GROUP = vek_pri_prvnim_ockovani,
         TREATMENT = vlna1)

#sloupcovej graf - nad sebou - cisla
{
  ggplot(ratios, aes(
    x = AGE_GROUP, 
    y = pomer, 
    fill = AGE_GROUP
  )) +
    geom_col(aes(group = AGE_GROUP), position = position_dodge(width = 0.8)) +
    geom_text(aes(label = N),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +  # puts count just above each bar
    facet_wrap(~TREATMENT, scales = "fixed",ncol=1) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 0.5, linetype = "dashed",color="gray")+
    # coord_cartesian(ylim = c(0.0, 1.75)) +
    labs(
      title = "OZP: Poměry prvopředpisů",
      x = "Věková kategorie",
      y = "Poměr po / před",
      fill = ""
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  }
