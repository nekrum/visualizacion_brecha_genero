library(curso.feminismo.datos)
library(ggplot2)
library(dplyr)

brecha_entidad <- brecha_data %>%
  select(
    `Entidad federativa`,
    `Ingreso promedio por hora trabajada de la población ocupada de mujeres`,
    `Ingreso promedio por hora trabajada de la población ocupada de hombres`
  ) %>%
  rename(
    entidad = `Entidad federativa`,
    ingreso_mujeres = `Ingreso promedio por hora trabajada de la población ocupada de mujeres`,
    ingreso_hombres = `Ingreso promedio por hora trabajada de la población ocupada de hombres`
  ) %>%
  group_by(entidad) %>%
  summarise(
    ingresos_mujeres = mean(ingreso_mujeres),
    ingresos_hombres = mean(ingreso_hombres),
    ingresos_mujeres_sd = sd(ingreso_mujeres),
    ingresos_hombres_sd = sd(ingreso_hombres)
  ) %>%
  arrange(ingresos_mujeres) #%>%
  #filter(entidad %in% c("Tabasco", "Campeche", "Oaxaca", "Chiapas", "Sinaloa"))

ggplot(brecha_entidad, aes(x=reorder(entidad, -ingresos_mujeres), y = ingresos_mujeres, fill=entidad)) +
  geom_col() +
  geom_errorbar(aes(
    ymin = ingresos_mujeres - ingresos_mujeres_sd,
    ymax = ingresos_mujeres + ingresos_mujeres_sd)
  ) +
  scale_fill_viridis_d(option = "E") +
  #scale_fill_manual(values = c("#637E6B", "#819C8A", "#6F1A07", "#A4B7AA", "#C6D2CA"), na.value="#BEBEBE") +
  # scale_fill_brewer(palette = colorRampPalette(RColorBrewer::brewer.pal(32, "PuOr"))())) +
  # scale_fill_brewer(palette = "PuOr") +
  coord_flip() +
  labs(x = "", y = "Media de ingreso por hora") +
  theme_minimal() +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
  theme( legend.position = "none")


ggplot(brecha_entidad, aes(x=reorder(entidad, -ingresos_mujeres), y = ingresos_mujeres, fill=entidad)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = ingresos_mujeres - ingresos_mujeres_sd,
    ymax = ingresos_mujeres + ingresos_mujeres_sd)
  ) +
  coord_flip() +
  labs(x = "", y = "Media de ingreso por hora") +
  theme_minimal() +
  theme( legend.position = "none")

