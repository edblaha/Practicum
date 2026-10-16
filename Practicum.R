library(tidyverse)
library(ggplot2)
library(haven)
library(srvyr)

cces <- read_dta("/Users/edwardblaha/DataWrangling/Practicum/CCES 2006-2024 Cumulative/cumulative_2006-2024.dta")


#dropping unnecessary variables to simplify data. Used codebook to identify pid7
cces <- cces |> select(pid7, year, vv_turnout_gvm, weight)

cces <- cces %>%
  mutate(pid7 = if_else(pid7 %in% 8:9, NA_real_, pid7))

cces <- cces %>%
  mutate(
    strong  = case_when(
      pid7 %in% c(1, 7) ~ 1,
      pid7 %in% 2:6     ~ 0,
    ),
    weak = case_when(
      pid7 %in% c(2, 6) ~ 1,
      pid7 %in% c(1, 3, 4, 5, 7) ~ 0,
    ),
    leaners = case_when(
      pid7 %in% c(3, 5) ~ 1,
      pid7 %in% c(1, 2, 4, 6, 7) ~ 0,
    ),
    indeps = case_when(
      pid7 == 4         ~ 1,
      pid7 %in% c(1, 2, 3, 5, 6, 7) ~ 0,
    )
  )


cces <- cces %>%
  mutate(
    identifiers = strong + weak,
    identvote = if_else(vv_turnout_gvm == 1, identifiers, NA),
    identnovote = if_else(vv_turnout_gvm == 2, identifiers, NA),
    turnout = case_when(
      vv_turnout_gvm == 1 ~ 1,
      vv_turnout_gvm == 2 ~ 0,
    )
  )

#delcaring weights
cces_svy <- cces %>%
  as_survey_design(weights = weight)


#Building dataset for figure 1
figure1_cumul <- cces_svy %>%
  filter(year %in% seq(2006, 2024, by = 2)) |> # even years: presidential + midterm
  group_by(year) |>
  summarise(
    strong  = survey_mean(strong, na.rm = TRUE, vartype = NULL),
    weak    = survey_mean(weak,   na.rm = TRUE, vartype = NULL),
    leaners = survey_mean(leaners,na.rm = TRUE, vartype = NULL),
    indeps  = survey_mean(indeps, na.rm = TRUE, vartype = NULL)
  )

#creating version with only presidential years 
cces_svy_pres <- cces_svy |>filter(year %in% seq(2008, 2024, by = 4), !is.na(turnout))


figure2_cumul <- cces_svy_pres %>%
  group_by(year, turnout) %>%
  summarise(
    identifiers = survey_mean(identifiers, na.rm = TRUE, vartype = NULL),
  )









Figure1_graphA <- ggplot(figure1_cumul, aes(x = year)) +
  #Next, our scales and axes
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, 0.1),
    minor_breaks = seq(0, 0.5, 0.05),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2024, 4),
    name = "Year"
  ) +
  
  # Strong: connected (solid line + filled points)
  
  geom_line(aes(y = strong, linetype = "Strong Identifiers"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = strong, shape = "Strong Identifiers"), size = 2.2, stroke = 0.8,
             color = "black", fill = "black") +
  
  # Weak: connected (dashed line + hollow circle points)
  geom_line(aes(y = weak, linetype = "Weak Identifiers"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = weak, shape = "Weak Identifiers"), size = 2.2, stroke = 0.9,
             color = "black", fill = NA) +
  
  #Titles and labeling
  labs(
    title = "Distribution of Party Identification, 2006-2024",
    subtitle = "Proportions of Cooperative Election Study Sample",
  ) +
  
  # Map linetypes and shapes to series (order matches legend order)
  scale_linetype_manual(
    values = c("Strong Identifiers" = "solid",
               "Weak Identifiers"   = "dashed"),
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    values = c("Strong Identifiers" = 16,   # filled circle
               "Weak Identifiers"   = 1),   # hollow circle
    guide = guide_legend(order = 1)
  ) +
  
  # Legend: one column, inside the plot
  guides(
    linetype = guide_legend(ncol = 1, title = NULL),
    shape    = guide_legend(ncol = 1, title = NULL)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.6, 0.2),  # tweak as desired
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3), # light box
    panel.grid.minor = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.3),
    plot.title = element_text(face = "bold"), 
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size=6),
    legend.margin      = margin(0.1, 0.1, 0.1, 0.1)
  ) 
Figure1_graphA







Figure1_graphB <-ggplot(figure1_cumul, aes(x = year)) +
#scales and axes 
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, 0.1),
    minor_breaks = seq(0, 0.5, 0.05),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(2008, 2024, 4),
    name = "Year"
  ) +
  # Leaners: dashed + hollow
  geom_line(aes(y = leaners, linetype = "Independent Leaners"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = leaners, shape = "Independent Leaners"),
             size = 2.2, stroke = 0.9, color = "black", fill = NA) +
  
  # Pure Independents: solid + filled
  geom_line(aes(y = indeps, linetype = "Pure Independents"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = indeps, shape = "Pure Independents"),
             size = 2.2, stroke = 0.8, color = "black", fill = "black") +
  
  
  labs(
    title = paste0("Distribution of Party Identification 2006-2024"), 
    subtitle = "Proportions of Cooperative Election Study Sample",
  ) +
  
  # Map linetypes and shapes to series (order matches your legend order)
  scale_linetype_manual(
    values = c("Independent Leaners" = "dashed",
               "Pure Independents"   = "solid"),
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    values = c("Independent Leaners" = 1,   # hollow circle
               "Pure Independents"   = 16), # filled circle
    guide = guide_legend(order = 1)
  ) +
  
  # Legend: one column, inside the plot (similar to position(5) ring(0))
  guides(
    linetype = guide_legend(ncol = 1, title = NULL),
    shape    = guide_legend(ncol = 1, title = NULL)
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.4, 0.8),  # tweak as desired
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3), # light box
    panel.grid.minor = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.3),
    plot.title = element_text(face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.text = element_text(size=6),
    legend.margin      = margin(0.1, 0.1, 0.1, 0.1)
  ) 

Figure1_graphB

library(patchwork)
Figure1_graphA / Figure1_graphB

Figure1 <- (Figure1_graphA +
               theme(
                 axis.title.x = element_blank(),
                 axis.text.x  = element_blank(),
                 axis.ticks.x = element_blank(),
                 plot.caption = element_blank()
               )
) /
  (Figure1_graphB +
     labs(title = NULL, subtitle = NULL) +
     theme(axis.title.x = element_blank())
  )

Figure1

#===============================================================================
#===============================================================================

Figure2<-ggplot() +
  
  # nonvoters
  geom_line(data = subset(figure2_cumul, turnout == 0),
            aes(x = year, y = identifiers, linetype = "Nonvoters"),
            linewidth = 0.7, color = "black") +
  geom_point(data = subset(figure2_cumul, turnout == 0),
             aes(x = year, y = identifiers, shape = "Nonvoters"),
             size = 2.2, stroke = 0.9, color = "black", fill = NA) +
  
  # voters
  geom_line(data = subset(figure2_cumul, turnout == 1),
            aes(x = year, y = identifiers, linetype = "Voters"),
            linewidth = 0.7, color = "black") +
  geom_point(data = subset(figure2_cumul, turnout == 1),
             aes(x = year, y = identifiers, shape = "Voters"),
             size = 2.2, stroke = 0.8, color = "black", fill = "black") +
  
  # axes, labels, scales
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    minor_breaks = seq(0, 1, 0.05),
    name = NULL  # ytitle("")
  ) +
  scale_x_continuous(
    breaks = seq(2004, 2024, 4),
    name = "Year"  # xtitle
  ) +
  
  # title/subtitle/note
  labs(
    title = "Party Identification Among Presidential Voters and Nonvoters",
    subtitle = "Proportions of Strong or Weak Identfiers in Cooperative Election Study Sample",
    caption = "Presidential Election Years Only Weighted",
    linetype = NULL, shape = NULL
  ) +
  
  # legend styling & ordering
  scale_linetype_manual(values = c("Nonvoters" = "dashed", "Voters" = "solid")) +
  scale_shape_manual(values = c("Nonvoters" = 1, "Voters" = 16)) +
  guides(
    linetype = guide_legend(ncol = 1, order = 1),
    shape    = guide_legend(ncol = 1, order = 1)
  ) +
  
  theme_minimal(base_size = 10) +
  theme(
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.5),
    panel.grid.minor = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.3),
    legend.position  = c(0.2, 0.2),  
    legend.background= element_rect(fill = "white", color = "grey70", linewidth = 0.3),
    plot.title       = element_text(face = "bold", size = 11),    
    plot.subtitle = element_text(size = 8)
  )

Figure2














