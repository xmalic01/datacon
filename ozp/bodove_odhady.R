library(tidyverse)
# library(brms)
library(patchwork)   # optional for combining plots
# library(scales)
library(gridExtra)

# df = read.csv2("prvni_vlna_CPZP.csv",row.names = 1)
# df = read.csv2("druha_vlna_CPZP.csv",row.names = 1)
# df = read.csv2("treti_vlna_CPZP.csv",row.names = 1)

df = read.csv2("prvni_vlna_OZP.csv",row.names = 1)
df = read.csv2("druha_vlna_OZP.csv",row.names = 1)
df = read.csv2("treti_vlna_OZP.csv",row.names = 1)


df = df %>% filter(!vek_pri_prvnim_ockovani %in% c("0-11","70+"))
df = df %>% filter(!vek_pri_druhem_ockovani %in% c("0-11","70+"))
df = df %>% filter(!vek_pri_tretim_ockovani %in% c("0-11","70+"))

df$vlna1 = factor(df$vlna1,labels=c("neočkovaní","očkovaní"))
df$vlna2 = factor(df$vlna2,labels=c("neočkovaní","očkovaní jednou","očkovaní dvakrát"))
df$vlna3 = factor(df$vlna3,labels = c("neočkovaní","primární","booster"))

# df$Pohlavi = factor(df$Pohlavi,labels = c("M","Ž"))

df$vek_pri_prvnim_ockovani = as.factor(df$vek_pri_prvnim_ockovani)
df$vek_pri_druhem_ockovani = as.factor(df$vek_pri_druhem_ockovani)
df$vek_pri_tretim_ockovani = as.factor(df$vek_pri_tretim_ockovani)

#v druhe vlne nebyla ani jedna zena ve veku 12-29, ktera byla ockovana jednou, ktera mela nejake kortikoidy
# df$tyden_pred[df$vek_pri_druhem_ockovani=="12-29"&df$Pohlavi=="Ž"&df$vlna2=="očkovaní jednou"] =
# 12/length(df$tyden_pred[df$vek_pri_druhem_ockovani=="12-29"&df$Pohlavi=="Ž"&df$vlna2=="očkovaní jednou"])
# zjistil jsem, ze druhy nejmensi soucet byl 25, tak jsem to nastavil tak, aby i tady byl soucet 25

# df %>% filter(vek_pri_druhem_ockovani=="12-29",Pohlavi=="Ž",vlna2=="očkovaní jednou") %>%
#   nrow()

df = df[df$vlna2!="očkovaní jednou",]

{
# grid.arrange(
# df %>% filter(mesic_po>0,mesic_pred>0) %>% 
# ggplot()+
#   geom_histogram(aes(x=mesic_po),fill="limegreen")+
#   xlim(0,2000)+
#   ylim(0,1250)+
#   labs(title="Spotřeba kortikidů",
#        y="počet lidí",
#        x="množství kortikoidů měsíc po očkování")+
#   theme_minimal(),
# 
# df %>% filter(mesic_po>0,mesic_pred>0) %>% 
#   ggplot()+
#   geom_histogram(aes(x=mesic_pred),fill="skyblue")+
#   xlim(0,2000)+
#   ylim(0,1250)+
#   labs(y="počet lidí",
#       x="množství kortikoidů měsíc před očkováním")+
#   theme_minimal()
# )


#   df %>% filter(mesic_pred>0) %>% 
#   ggplot()+
#   geom_histogram(aes(x=log2(mesic_pred)))+
#   theme_minimal()
# 
# df %>% 
#   summarise(ratio = sum(mesic_po)/sum(mesic_pred)) %>% 
#   pull(ratio)
# 
# df %>% 
#   group_by(vek_pri_druhem_ockovani,Pohlavi,vlna2) %>% 
#   summarise(tyden = sum(tyden_po)) %>% 
#   pull(tyden)
}
  
# RATIOS WIDE ##################################################################
ratios = df %>% 
  group_by(vek_pri_prvnim_ockovani,vlna1) %>%
  # group_by(vek_pri_druhem_ockovani,vlna2) %>%
  # group_by(vek_pri_tretim_ockovani,vlna3) %>%
  summarise(mesic = sum(mesic_po)/sum(mesic_pred),
         pulrok = sum(pulrok_po)/sum(pulrok_pred),
         rok = sum(rok_po)/sum(rok_pred),
         dva_roky = sum(dva_roky_po)/sum(dva_roky_pred),
         .groups = "drop")

# ratios = ratios %>% 
#   rename(vek = vek_pri_druhem_ockovani)
# 
# write.csv(ratios,"ratios_ozp_druha.csv")

# AI SCRIPT ####################################################################
#ggplot prej radsi pracuje s long tabulkama nez wide
ratios_long <- ratios %>%
  pivot_longer(
    cols = c(mesic, pulrok, rok, dva_roky),
    names_to = "TimeWindow",
    values_to = "Ratio"
  ) %>%
  mutate(
    TimeWindow = factor(TimeWindow, levels = c("mesic", "pulrok", "rok", "dva_roky"),
                        labels=c("měsíc","půl roku","rok", "dva roky")),
    TREATMENT = vlna1,
    # TREATMENT = vlna2,
    # TREATMENT = vlna3,
    # GENDER = factor(Pohlavi),
    AGE_GROUP = vek_pri_prvnim_ockovani,
    # AGE_GROUP = vek_pri_druhem_ockovani,
    # AGE_GROUP = vek_pri_tretim_ockovani,
    RatioCentered = log2(Ratio)
  )

group_counts <- df %>%
  group_by(vek_pri_prvnim_ockovani, vlna1) %>%
  # group_by(vek_pri_druhem_ockovani, vlna2) %>%
  # group_by(vek_pri_tretim_ockovani, vlna3) %>%
  summarise(N = n(), .groups = "drop")

ratios_long <- ratios_long %>%
  left_join(group_counts, by = c("vek_pri_prvnim_ockovani", "vlna1"))
  # left_join(group_counts, by = c("vek_pri_druhem_ockovani", "vlna2"))
  # left_join(group_counts, by = c("vek_pri_tretim_ockovani", "vlna3"))


#sloupcovej graf - ocko / neocko vedle sebe
{
  # ggplot(ratios_long, aes(
  #   x = vek_pri_prvnim_ockovani, 
  #   y = Ratio, 
  #   fill = Pohlavi
  # )) +
  #   geom_col(
  #     aes(group = interaction(Pohlavi, vlna1)),
  #     position = position_dodge(width = 0.8)
  #   ) +
  #   facet_wrap(~ TimeWindow + vlna1, nrow = 1) +
  #   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  #   coord_cartesian(ylim = c(0.5, 1.5)) +
  #   labs(
  #     title = "Group-Level After/Before Ratios by Age × Gender × Treatment",
  #     x = "Age Group",
  #     y = "After / Before"
  #   ) +
  #   theme_minimal() +
  #   theme(
  #     strip.text = element_text(size = 10),
  #     axis.text.x = element_text(angle = 45, hjust = 1)
  #   )
}

#sloupcovej graf - nad sebou 
{
  ggplot(ratios_long, aes(
    x = AGE_GROUP,
    y = Ratio,
    fill = AGE_GROUP
  )) +
    geom_col(
      aes(group = AGE_GROUP),
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(TREATMENT ~ TimeWindow, scales = "fixed") +  # two rows: treatment, columns: time windows
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    # coord_cartesian(ylim = c(0.0,3.5)) +
    labs(
      title = "ČPZP: První vlna očkování",
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

#sloupcovej graf - nad sebou - cisla
{
  ggplot(ratios_long, aes(
    x = AGE_GROUP, 
    y = Ratio, 
    fill = AGE_GROUP
  )) +
    geom_col(aes(group = AGE_GROUP), position = position_dodge(width = 0.8)) +
    geom_text(aes(label = N),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +  # puts count just above each bar
    facet_grid(TREATMENT ~ TimeWindow, scales = "fixed") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
    coord_cartesian(ylim = c(0.0, 1.6)) +
    labs(
      # title = "ČPZP: První vlna očkování",
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

#sloupcovej graf - nad sebou - centrovano (log2)
{
  ggplot(ratios_long, aes(
    x = AGE_GROUP, 
    y = RatioCentered, 
    fill = AGE_GROUP
  )) +
    geom_col(
      aes(group = AGE_GROUP),
      position = position_dodge(width = 0.8)
    ) +
    facet_grid(TREATMENT ~ TimeWindow, scales = "fixed") +  # two rows: treatment, columns: time windows
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    # coord_cartesian(ylim = c(-2.2, 2.1)) +
    labs(
      title = "ČPZP: První vlna očkování",
      x = "Věková kategorie",
      y = "Logaritmus poměru po / před",
      fill = "Pohlaví"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

#sloupcovej graf - nad sebou - log - cisla
{
  ggplot(ratios_long, aes(
    x = AGE_GROUP, 
    y = RatioCentered, 
    fill = AGE_GROUP
  )) +
    geom_col(aes(group = AGE_GROUP), position = position_dodge(width = 0.8)) +
    geom_text(aes(label = N),
              position = position_dodge(width = 0.8),
              vjust = -0.5, size = 3) +  # puts count just above each bar
    facet_grid(TREATMENT ~ TimeWindow, scales = "fixed") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    # coord_cartesian(ylim =  c(-2.2, 2.1)) +
    labs(
      title = "První vlna očkování",
      x = "Věková kategorie",
      y = "Logaritmus poměru po / před",
      fill = "Pohlaví"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
}

#carovej graf
{
  ggplot(ratios_long, aes(
    x = TimeWindow, 
    y = Ratio, 
    group = interaction(AGE_GROUP, TREATMENT),
    color = AGE_GROUP,
    linetype = TREATMENT
  )) +
    # scale_linetype_manual(values = c(
    #   "neočkovaní" = "solid",      # solid line
    #   "očkovaní jednou" = "dashed",     # dashed line
    #   "očkovaní třikrát" = "dotted"      # dotted line
    # )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ AGE_GROUP,ncol=4) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    # coord_cartesian(ylim = c(0.5, 2.5)) +
    labs(
      title = "OZP: První vlna očkování",
      x = "Časové okno",
      y = "Poměr po / před",
      linetype = "",
      color="AGE GROUP"
    ) +
    theme_minimal()
}

#carovej graf - logaritmus
{
  ggplot(ratios_long, aes(
    x = TimeWindow, 
    y = RatioCentered, 
    group = interaction(AGE_GROUP, TREATMENT),
    color = AGE_GROUP,
    linetype = TREATMENT
  )) +
    # scale_linetype_manual(values = c(
    #   "neočkovaní" = "solid",      # solid line
    #   "očkovaní jednou" = "dotted",     # dashed line
    #   "očkovaní třikrát" = "dashed"      # dotted line
    # )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~ AGE_GROUP,ncol=4) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    coord_cartesian(ylim = c(-.75,.75)) +
    labs(
      # title = "ČPZP: První vlna očkování",
      x = "Časové okno",
      y = "Logaritmus poměru po / před",
      linetype = "",
      color="Věk. kategorie"
    ) +
    theme_minimal()+
    theme(
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      # legend.position = "none"
    )
}

