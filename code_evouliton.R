library(dplyr)
library(ggplot2)
library(viridis)

# Charger les données
data <- read.csv("salaries.csv")

# Ajouter une colonne 'category' en fonction des titres de poste
data <- data %>%
  mutate(category = case_when(
    grepl("scientist", job_title, ignore.case = TRUE) ~ "Scientist",
    grepl("engineer", job_title, ignore.case = TRUE) ~ "Engineer",
    grepl("manager", job_title, ignore.case = TRUE) ~ "Manager",
    grepl("analyst", job_title, ignore.case = TRUE) ~ "Analyst",
    TRUE ~ "Other"
  ))

# Remplacer les niveaux d'expérience par les nouveaux libellés
data$experience_level <- recode(data$experience_level,
                                "EN" = "Junior",
                                "MI" = "Intermediate",
                                "SE" = "Expert",
                                "EX" = "Director")

# Réorganiser l'ordre des niveaux d'expérience
data$experience_level <- factor(data$experience_level, 
                                levels = c("Junior", 
                                           "Intermediate", 
                                           "Expert", 
                                           "Director"))

# Calculer les salaires moyens par niveau d'expérience et catégorie
grouped_data <- data %>%
  group_by(experience_level, category) %>%
  summarise(average_salary = mean(salary_in_usd, na.rm = TRUE), .groups = 'drop')

# Visualisation
ggplot(grouped_data, aes(x = experience_level, y = average_salary, color = category, group = category)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_viridis(discrete = TRUE) +
  labs(
    title = "Évolution des salaires moyens par niveau d'expérience et catégorie",
    x = "Niveau d'expérience",
    y = "Salaire moyen annuel (USD)",
    color = "Catégorie"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 24, face = "bold"),  # Agrandir le titre
    axis.title.x = element_text(size = 16),  # Agrandir le titre de l'axe X
    axis.title.y = element_text(size = 16),  # Agrandir le titre de l'axe Y
    legend.title = element_text(size = 16)  # Agrandir le titre de la légende
  )

