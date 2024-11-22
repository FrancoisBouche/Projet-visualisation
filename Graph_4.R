# Charger les biblioth?ques n?cessaires
library(ggplot2)
library(ggmosaic)
library(dplyr)

# Charger les donn?es
data <- read.csv("salaries.csv")

# 1. Relation entre experience_level et company_size
# Mosa?que
ggplot(data) +
  geom_mosaic(aes(x = product(company_size), fill = experience_level), na.rm = TRUE) +
  labs(title = "R?partition de la taille des entreprises par niveau d'exp?rience",
       x = "Taille de l'entreprise", y = "Proportion", fill = "Niveau d'exp?rience") +
  theme_minimal()

# 2. Relation entre remote_ratio et employment_type
# Diagramme de barres empil?es
ggplot(data, aes(x = factor(remote_ratio), fill = employment_type)) +
  geom_bar(position = "fill") +
  labs(title = "R?partition du type d'emploi par niveau de t?l?travail",
       x = "Ratio de t?l?travail (%)", y = "Proportion", fill = "Type d'emploi") +
  theme_minimal()

# 3. Distribution du salaire en USD selon le niveau d'exp?rience
# Bo?te ? moustaches
ggplot(data, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot() +
  labs(title = "Distribution des salaires en USD par niveau d'exp?rience",
       x = "Niveau d'exp?rience", y = "Salaire en USD") +
  theme_minimal()

# 4. R?partition des intitul?s de poste selon la taille de l'entreprise
# Diagramme de Pareto des postes les plus courants
top_jobs <- data %>%
  count(job_title, company_size) %>%
  arrange(desc(n)) %>%
  top_n(10, n)

ggplot(top_jobs, aes(x = reorder(job_title, n), y = n, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Postes les plus courants par taille d'entreprise",
       x = "Intitul? du poste", y = "Nombre", fill = "Taille de l'entreprise") +
  theme_minimal()

top_jobs <- data %>%
  group_by(company_size, job_title) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(company_size) %>%
  mutate(proportion = count / sum(count)) %>%  # Calcul des proportions
  arrange(company_size, desc(proportion)) %>%
  group_by(company_size) %>%
  slice_max(order_by = proportion, n = 10)  # S?lectionne les 10 postes les plus fr?quents par proportion

ggplot(top_jobs, aes(x = reorder(job_title, proportion), y = proportion, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Postes les plus courants par taille d'entreprise (en proportion)",
       x = "Intitul? du poste", y = "Proportion", fill = "Taille de l'entreprise") +
  scale_y_continuous(labels = scales::percent) +  # Afficher l'axe Y en pourcentages
  theme_minimal()

table(data$job_title,data$company_size)
table(data$remote_ratio,data$company_size)
table(data$work_year)
table(data$experience_level)
table(data$company_size)


library(dplyr)
library(ggplot2)
library(sf)
library(maps)

# Charger les donn?es
data <- read.csv("salaries.csv")

# Cr?ation des cat?gories avec grepl
data <- data %>%
  mutate(
    category = case_when(
      grepl("scientist", job_title, ignore.case = TRUE) ~ "Scientist",
      grepl("engineer", job_title, ignore.case = TRUE) ~ "Engineer",
      grepl("manager", job_title, ignore.case = TRUE) ~ "Manager",
      grepl("analyst", job_title, ignore.case = TRUE) ~ "Analyst",
      TRUE ~ "Other"  # Par d?faut, tout ce qui ne correspond pas aux cat?gories ci-dessus
    )
  )

# Calcul des statistiques par cat?gorie
category_summary <- data %>%
  group_by(category, employee_residence) %>%  # Regrouper par cat?gorie et lieu de r?sidence
  summarise(
    avg_salary = mean(salary_in_usd, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  )

# Charger des donn?es g?ographiques
world <- map_data("world")

# Filtrer pour les pays pr?sents dans le jeu de donn?es
category_summary <- category_summary %>%
  mutate(employee_residence = toupper(employee_residence))  # Harmoniser les codes pays

# Associer les codes pays aux donn?es g?ographiques
world_data <- world %>%
  mutate(region = toupper(region))  # Harmoniser avec les codes pays

# Ajouter les moyennes salariales aux coordonn?es des pays
map_data <- category_summary %>%
  left_join(world_data, by = c("employee_residence" = "region"))

# Visualisation de la carte
ggplot(map_data) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey90", color = "white") +
  geom_point(aes(x = long, y = lat, size = avg_salary, color = category), alpha = 0.8) +
  scale_color_manual(values = c("Scientist" = "blue", "Engineer" = "red", "Manager" = "green", "Analyst" = "purple", "Other" = "orange")) +
  labs(
    title = "R?partition des cat?gories par pays",
    size = "Salaire moyen (USD)",
    color = "Cat?gorie"
  ) +
  theme_minimal()





# Installer les packages n?cessaires si besoin
if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
if (!require("countrycode")) install.packages("countrycode")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("sf")) install.packages("sf")
if (!require("plotly")) install.packages("plotly")  # Ajouter plotly pour l'interactivit?

# Charger les biblioth?ques
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(dplyr)
library(ggplot2)
library(sf)
library(plotly)

# Charger les donn?es
data <- read.csv("salaries.csv")

# ?tape 1 : Regroupement des intitul?s en cat?gories avec grepl
data <- data %>%
  mutate(
    category = case_when(
      grepl("scientist", job_title, ignore.case = TRUE) ~ "Scientist",
      grepl("engineer", job_title, ignore.case = TRUE) ~ "Engineer",
      grepl("manager", job_title, ignore.case = TRUE) ~ "Manager",
      grepl("analyst", job_title, ignore.case = TRUE) ~ "Analyst",
      TRUE ~ "Other"  # Par d?faut pour les intitul?s non identifi?s
    )
  )

# ?tape 2 : Conversion des codes ISO-2 en ISO-3
data <- data %>%
  mutate(
    employee_residence = countrycode(employee_residence, origin = "iso2c", destination = "iso3c")
  )

# ?tape 3 : Calcul des statistiques par pays et par cat?gorie
category_summary <- data %>%
  group_by(category, employee_residence) %>%
  summarise(
    avg_salary = mean(salary_in_usd, na.rm = TRUE),  # Salaire moyen
    count = n(),  # Nombre de postes
    .groups = "drop"
  )

# ?tape 4 : Charger les coordonn?es des pays
countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  select(admin, iso_a3, geometry) %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],  # Longitude
    lat = st_coordinates(st_centroid(geometry))[, 2]   # Latitude
  ) %>%
  as.data.frame()

# ?tape 5 : Associer les donn?es de cat?gories aux coordonn?es
map_data <- category_summary %>%
  left_join(countries, by = c("employee_residence" = "iso_a3"))  # Associer les pays aux coordonn?es

# ?tape 6 : Cr?er la carte avec jitter
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")  # Contours des pays

# Cr?er la carte avec ggplot2
p <- ggplot() +
  # Fond de carte
  geom_sf(data = world, fill = "grey90", color = "white") +
  # Points pour chaque cat?gorie avec jitter pour ?viter l'empilement
  geom_point(
    data = map_data,
    aes(x = lon, y = lat, size = avg_salary, color = category),
    alpha = 0.8,
    position = position_jitter(width = 1, height = 1)  # D?calage l?ger des points
  ) +
  # Personnalisation des couleurs
  scale_color_manual(
    values = c(
      "Scientist" = "blue", 
      "Engineer" = "red", 
      "Manager" = "green", 
      "Analyst" = "purple", 
      "Other" = "orange"
    )
  ) +
  labs(
    title = "R?partition des cat?gories par pays",
    size = "Salaire moyen (USD)",
    color = "Cat?gorie"
  ) +
  theme_minimal()

# Convertir la carte ggplot2 en carte interactive avec plotly
interactive_map <- ggplotly(p)

# Afficher la carte interactive
interactive_map

table(data$employment_type)
# Charger les packages n?cessaires
library(dplyr)
library(ggplot2)
library(scales)

# Exemple de jeu de donn?es top_jobs (remplacer par vos propres donn?es)
top_jobs <- data %>%
  group_by(company_size, job_title) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(company_size) %>%
  mutate(proportion = count / sum(count)) %>%  # Calcul des proportions
  arrange(company_size, desc(proportion)) %>%
  group_by(company_size) %>%
  slice_max(order_by = proportion, n = 10)  # S?lectionne les 10 postes les plus fr?quents par proportion

# D?finir un vecteur de traduction pour les tailles d'entreprise en fran?ais
size_labels <- c("S" = "Petite entreprise (1-50 employ?s)", 
                 "M" = "Moyenne entreprise (51-250 employ?s)", 
                 "L" = "Grande entreprise (250+ employ?s)")

# Cr?er le graphique avec ggplot
ggplot(top_jobs, aes(x = reorder(job_title, proportion), y = proportion, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Postes les plus pr?sents par taille d'entreprise (en proportion)",
       x = "Intitul? du poste", y = "Proportion", fill = "Taille de l'entreprise") +
  scale_y_continuous(labels = scales::percent) +  # Afficher l'axe Y en pourcentages
  scale_fill_manual(values = c("S" = "lightblue", "M" = "lightgreen", "L" = "salmon"), 
                    labels = size_labels) +  # Personnalisation de la l?gende avec les labels en fran?ais
  theme_minimal() +
  theme(legend.title = element_blank(),  # Enlever le titre de la l?gende
        legend.position = "right")
  
# Charger les packages n?cessaires
library(dplyr)
library(ggplot2)
library(scales)
library(viridis)  # Palette Viridis pour daltoniens

# Exemple de jeu de donn?es top_jobs (remplacer par vos propres donn?es)
top_jobs <- data %>%
  group_by(company_size, job_title) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(company_size) %>%
  mutate(proportion = count / sum(count)) %>%  # Calcul des proportions
  arrange(company_size, desc(proportion)) %>%
  group_by(company_size) %>%
  slice_max(order_by = proportion, n = 8)  # S?lectionne les 8 postes les plus fr?quents par proportion

# D?finir un vecteur de traduction pour les tailles d'entreprise en fran?ais
size_labels <- c("S" = "Petite entreprise (1-50 employés)", 
                 "M" = "Moyenne entreprise (51-250 employés)", 
                 "L" = "Grande entreprise (250+ employés)")

# Cr?er le graphique avec ggplot
ggplot(top_jobs, aes(x = reorder(job_title, proportion), y = proportion, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Postes les plus présents par taille d'entreprise (en proportion)",
       x = "Intitulé du poste", y = "Proportion", fill = "Taille de l'entreprise") +
  scale_y_continuous(
    labels = scales::percent,  # Affiche les proportions en pourcentage
    breaks = c(0.05, 0.15, 0.25)  # Ajoute les breaks sp?cifiques demand?s
  ) +
  scale_fill_viridis_d(option = "D", labels = size_labels) +  # Utilise la palette Viridis
  theme_minimal() +
  theme(
    legend.title = element_blank(),  # Enl?ve le titre de la l?gende
    legend.position = "right",
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  # Aligne le titre ? gauche
    axis.text.y = element_text(size = 12),  # Augmente la taille des noms de job sur l'axe Y
    axis.text.x = element_text(size = 10),  # Ajuste la taille des labels de l'axe des proportions
    axis.title = element_text(size = 14, face = "bold")  # Augmente la taille des titres des axes
  )



