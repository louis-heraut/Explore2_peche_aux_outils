# Copyright 2024 Louis Héraut (louis.heraut@inrae.fr)*1
#
# *1   INRAE, France
#
# This file is part of Explore2 R toolbox.
#
# Explore2 R toolbox is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# Explore2 R toolbox is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Explore2 R toolbox.
# If not, see <https://www.gnu.org/licenses/>.


# Chemin du dossier téléchargé à changer
setwd("~/Documents/bouleau/INRAE/project/Explore2_project/Explore2_peche_aux_outils/")


## 0. INFO ___________________________________________________________
# 0.1. Library _______________________________________________________
if(!require(ncdf4)) install.packages("ncdf4")
if(!require(lubridate)) install.packages("lubridate")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(ggh4x)) install.packages("ggh4x")
if(!require(latex2exp)) install.packages("latex2exp")
if(!require(sf)) install.packages("sf")
if(!require(remotes)) install.packages("remotes")
if(!require(dataSHEEP)) remotes::install_github("super-lou/dataSHEEP")

## 0.2. Source _______________________________________________________
Scripts = list.files("R", full.names=TRUE)
for (script in Scripts) {
    source(script)    
}

## 0.3. Chemins ______________________________________________________
figures_dir = "figures"
if (!dir.exists(figures_dir)) {
    dir.create(figures_dir)
}
shapefiles_dir = "shapefiles"

## 0.4. Narratifs ____________________________________________________
# La liste des narratifs ainsi que quelques données utiles les
# concernants
Storylines = list(
    vert=list(
        name="vert",
        EXP="(historical)|(rcp85)",
        GCM="HadGEM2-ES", RCM="ALADIN63", BC="ADAMONT",
        climate_chain=c(
            "historical|MOHC-HadGEM2-ES|CNRM-ALADIN63|MF-ADAMONT-SAFRAN-1980-2011",
            "rcp85|MOHC-HadGEM2-ES|CNRM-ALADIN63|MF-ADAMONT-SAFRAN-1980-2011",
            "historical-rcp85|HadGEM2-ES|ALADIN63|MF-ADAMONT"),
        color="#569A71", color_light="#BAD8C6",
        info="Réchauffement marqué et augmentation des précipitations"
    ),
    jaune=list(
        name="jaune",
        EXP="(historical)|(rcp85)",
        GCM="CNRM-CM5", RCM="ALADIN63", BC="ADAMONT",
        climate_chain=c(
            "historical|CNRM-CERFACS-CNRM-CM5|CNRM-ALADIN63|MF-ADAMONT-SAFRAN-1980-2011",
            "rcp85|CNRM-CERFACS-CNRM-CM5|CNRM-ALADIN63|MF-ADAMONT-SAFRAN-1980-2011",
            "historical-rcp85|CNRM-CM5|ALADIN63|MF-ADAMONT"),
        color="#EECC66", color_light="#F8EBC2",
        info="Changements futurs relativement peu marqués"
    ),
    orange=list(
        name="orange",
        EXP="(historical)|(rcp85)",
        GCM="EC-EARTH", RCM="HadREM3-GA7", BC="ADAMONT",
        climate_chain=c(
            "historical|ICHEC-EC-EARTH|MOHC-HadREM3-GA7-05|MF-ADAMONT-SAFRAN-1980-2011",
            "rcp85|ICHEC-EC-EARTH|MOHC-HadREM3-GA7-05|MF-ADAMONT-SAFRAN-1980-2011",
            "historical-rcp85|EC-EARTH|HadREM3-GA7-05|MF-ADAMONT"),
        color="#E09B2F", color_light="#F3D7AC",
        info="Fort réchauffement et fort assèchement en été (et en annuel)"
    ),
    violet=list(
        name="violet",
        EXP="(historical)|(rcp85)",
        GCM="HadGEM2-ES", RCM="CCLM4-8-17", BC="ADAMONT",
        climate_chain=c(
            "historical|MOHC-HadGEM2-ES|CLMcom-CCLM4-8-17|MF-ADAMONT-SAFRAN-1980-2011",
            "rcp85|MOHC-HadGEM2-ES|CLMcom-CCLM4-8-17|MF-ADAMONT-SAFRAN-1980-2011",
            "historical-rcp85|HadGEM2-ES|CCLM4-8-17|MF-ADAMONT"),
        color="#791F5D", color_light="#E9A9D5",
        info="Fort réchauffement et forts contrastes saisonniers en précipitations"
    )
)

### 0.5. Périodes typiques ___________________________________________
historical = c("1976-01-01", "2005-08-31")
Horizons = list(H1=c("2021-01-01", "2050-12-31"),
                H2=c("2041-01-01", "2070-12-31"),
                H3=c("2070-01-01", "2099-12-31"))


## 1. PROJECTIONS ____________________________________________________
### 1.1. Obtenir les URLs ____________________________________________
# Une liste préformatée des URLs disponibles sur DRIAS-Eau est
# disponible dans le fichier URL_DRIAS_projections.csv
URL = dplyr::as_tibble(read.table(
                 file=file.path("robot",
                                "URL_DRIAS_projections.csv"),
                 header=TRUE,
                 sep=",", quote='"'))

# Le tibble URL contient donc l'ensemble des combinaisons utiles pour
# faire une sous sélection des URLs disponibles
EXP = unique(URL$EXP)
GCM = unique(URL$GCM)
RCM = unique(URL$RCM)
BC = unique(URL$BC)
HM = unique(URL$HM)
Variables = unique(URL$variable)

### 1.2. Filtrer les URLs ____________________________________________
# Soit en utilisant le tibble URL avec dplyr pour une combinaison
# spécifique ...
URL_filtered = dplyr::filter(URL,
                             EXP == "rcp85" &
                             grepl("ADAMONT", BC) &
                             HM %in% c("J2000", "GRSD"))

# ou pour filtrer les narratifs Explore2
URL_filtered = dplyr::tibble()
for (storyline in Storylines) {
    URL_filtered_tmp = dplyr::filter(URL,
                                     grepl(storyline$EXP, EXP) &
                                     grepl(storyline$GCM, GCM) &
                                     grepl(storyline$RCM, RCM) &
                                     grepl(storyline$BC, BC))
    URL_filtered = dplyr::bind_rows(URL_filtered, URL_filtered_tmp)
}
URL_filtered = dplyr::filter(URL_filtered,
                             HM %in% c("J2000", "GRSD"))
Urls = URL_filtered$url

# Soit en lisant directement un fichier txt qui contient une liste des
# URLs récupérés par le biais de la plateforme DRIAS-Eau
Urls = readLines("manual_URL_DRIAS_projections.txt")

### 1.3. Obtenir les NetCDFs _________________________________________
get_DRIAS_netcdf(Urls, "DRIAS_projections")

### 1.4. Lire un NetCDF ______________________________________________
# Obtenir l'ensemble des chemins des NetCDFs téléchargés
Paths = list.files(file.path("DRIAS_projections"),
                   recursive=TRUE,
                   full.names=TRUE)

# Le package ncdf4 permet de lire un NetCDF
NC = nc_open(Paths[1])

# Dans un premier temps, il est possible d'obtenir la liste des
# attributs globaux ...
ncatt_get(NC, "")
# ou un seul spécifiquement
ncatt_get(NC, "", "hy_institute_id")$value
# De la même manière, il est possible d'obtenir les attributs
# d'une dimension ou d'une variable
ncatt_get(NC, "time", "units")$value

# Dans un second temps, il est possible d'obtenir une variable
# contenue dans la liste des variables disponibles
names(NC$var)
# Donc par exemple pour obtenir le code des stations :
Codes_NC = ncvar_get(NC, "code")

# De cette manière, pour la Dore à Dora ...
code = "K298191001"
# il faut chercher son indice dans ce NetCDF
id = match(code, Codes_NC)
# Cet id permet donc d'aller chercher les informations d'intérêts dans
# ce NetCDF et uniquement dans celui-ci
ncvar_get(NC, "topologicalSurface_model")[id]

# Cependant, il est plus périlleux de vouloir répéter l'opération
# précédente pour obtenir l'entierté de la matrice des débits.
# Il est donc conseillé de n'en tier que la partie souhaité en
# utilisant les fonctionnalités internes aux NetCDFs
# On part de l'indice id pour la première dimension "station" et on
# part du début de la dimension seconde dimension "time"
start = c(id, 1)
# On compte un seul pas sur la dimension "station" et on prend
# l'entiereté de la dimension "time"
count = c(1, -1)
# Ainsi, on peut obtenir les débits de la Dore à Dora pour ce NetCDF
debit = ncvar_get(NC, "debit",
                  start=start,
                  count=count)

# et son vecteur temps associé
Date = ncvar_get(NC, "time") + as.Date("1950-01-01")

# Cette fonction reprend ce procédé avec une liste de station pour
# obtenir un tibble près à être traiter
code = "K298191001"
data = read_DRIAS_netcdf(Paths, Codes=code)

# La variables Codes peut correspondre à une liste de code comme :
# Codes = c("K297031001", "K298191001", "K299401001"), ou à une
# expression régulière.
# Donc "*" utilisera tous les codes du NetCDF et par exemple "^K20",
# cherchera tous les codes commençant par cette expression.

### 1.5. Afficher les projections ____________________________________
# Pour la Dora à Dora
# Calcul de la date minimale parmis les chaînes de modélisation
min_date = data %>%
    dplyr::filter(EXP == "historical") %>%
    dplyr::group_by(chain) %>%
    dplyr::summarise(min=min(date, na.rm=TRUE))
min_date = max(min_date$min, na.rm=TRUE)

# Pareil pour la date maximale
max_date = data %>%
    dplyr::filter(EXP != "historical") %>%
    dplyr::group_by(chain) %>%
    dplyr::summarise(max=max(date, na.rm=TRUE))
max_date = min(max_date$max, na.rm=TRUE)

# Filtrage des données sur la période de disponibilité
data = dplyr::filter(data,
                     min_date <= date &
                     date <= max_date)

# Pour chaque narratif, calcule de la médiane des sur les modèles
# hydrologiques
data_med = data %>%
    dplyr::group_by(climate_chain, date) %>%
    dplyr::summarise(debit=median(debit, na.rm=TRUE),
                     .groups="drop")

# Pour chaque narratif
for (storyline in Storylines) {
    data_med_storyline = data_med %>%
        dplyr::filter(climate_chain %in% storyline$climate_chain)

    # Définition du plot et du theme
    plot = ggplot() +
        theme_minimal() +
        theme(panel.grid.major.x=element_blank(),
              panel.grid.minor.x=element_blank(),
              axis.line.x=element_line(color="grey65",
                                       linewidth=0.5,
                                       lineend="round"),
              axis.ticks.x=element_line(color="grey80"),
              axis.ticks.length=unit(0.2, "cm"),
              plot.title=element_text(color=storyline$color)) +
        
        # Titre
        ggtitle(TeX(paste0(code, " - \\textbf{",
                           storyline$info, "}"))) +
        
        # Affichage des chaînes de modélisation
        geom_line(data=data,
                  aes(x=date,
                      y=debit,
                      group=chain),
                  color="grey65",
                  linewidth=0.8,
                  alpha=0.15,
                  lineend="round") +
        # et de la médiane du narratif concerné
        geom_line(data=data_med_storyline,
                  aes(x=date,
                      y=debit),
                  color=storyline$color,
                  linewidth=0.25,
                  alpha=1,
                  lineend="round")

    # Mise en forme des axes
    plot = plot +
        xlab(NULL) +
        scale_x_date(
            breaks=get_breaks,
            minor_breaks=get_minor_breaks,
            guide="axis_minor",
            date_labels="%Y",
            expand=c(0, 0)) +
        ylab(TeX("débit \\small{m$^{3}$.s$^{-1}$}")) +
        scale_y_sqrt(limits=c(0, NA),
                     expand=c(0, 0))
    
    # Sauvegarde
    ggsave(plot=plot,
           filename=file.path(figures_dir,
                              paste0("DRIAS_projection_",
                                     code, "_",
                                     storyline$name, ".pdf")),
           width=50, height=10, units="cm")
}


## 2. INDICATEURS ____________________________________________________
### 2.1. Obtenir les URLs ____________________________________________
# Une liste préformatée des URLs disponibles sur DRIAS-Eau est
# disponible dans le fichier URL_DRIAS_indicateurs.csv
URL = dplyr::as_tibble(read.table(
                 file=file.path("robot",
                                "URL_DRIAS_indicateurs.csv"),
                 header=TRUE,
                 sep=",", quote='"'))

# Le tibble URL contient donc l'ensemble des combinaisons utiles pour
# faire une sous sélection des URLs disponibles
EXP = unique(URL$EXP)
BC = unique(URL$BC)
HM = unique(URL$HM)
Indicateurs = unique(URL$indicateur)

### 2.2. Filtrer les URLs ____________________________________________
# Soit en utilisant le tibble URL avec dplyr
URL_filtered = dplyr::filter(URL,
                             EXP == "rcp85" &
                             BC == "MF-ADAMONT" &
                             HM %in% c("J2000", "GRSD") &
                             indicateur == "Debit-VCN10_Saisonnier")
Urls = URL_filtered$url

# Soit en lisant directement un fichier txt qui contient une liste des
# URLs récupérés par le biais de la plateforme DRIAS-Eau
Urls = readLines("manual_URL_DRIAS_indicateurs.txt")

### 2.3. Obtenir les NetCDFs _________________________________________
get_DRIAS_netcdf(Urls, "DRIAS_indicateurs")

### 2.4. Lire un NetCDF ______________________________________________
# Obtenir l'ensemble des chemins des NetCDFs téléchargés
Paths = list.files(file.path("DRIAS_indicateurs"),
                   recursive=TRUE,
                   full.names=TRUE)

# Le NetCDF peut être ouvert de la même manière
NC = nc_open(Paths[1])
# Et l'ensemble des commandes vu précédemment restes valides
# Par conséquent, pour obtenir un tableau de donnée prêt à traiter pour la Dore à Dora
code = "K298191001"
data = read_DRIAS_netcdf(Paths, code)
# Avec des infos sur la variable
# Nom dans le NetCDF
variable = "VCN10"
# Nom de la variable à afficher
variable_to_display = "VCN10 estival"
# Nom de la moyenne de cette variable
mean_variable = paste0("mean", variable)
# Nom de la variable de changement
delta_variable = paste0("delta", variable)
# Est ce que cette variable est à normalisable ?
to_normalise = TRUE

### 2.5. Afficher les indicateurs ____________________________________
#### 2.5.1. en série annuelle ________________________________________
# Définition du plot et du theme
plot = ggplot() +
    theme_minimal() +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          plot.margin=margin(5, 20, 5, 5),
          axis.line.x=element_line(color="grey65",
                                   linewidth=0.5,
                                   lineend="round"),
          axis.ticks.x=element_line(color="grey80"),
          axis.ticks.length=unit(0.2, "cm"),
          plot.title=element_text(color="grey30")) +

    # Titre
    ggtitle(TeX(paste0(code, " - \\textbf{",
                       variable_to_display,
                       "} \\small{m$^{3}$.s$^{-1}$}"))) +

    # Affichage des chaînes de modélisation
    geom_line(data=data,
              aes(x=date,
                  y=get(variable),
                  group=chain),
              color="grey65",
              linewidth=0.4,
              alpha=0.15,
              lineend="round")

# Pour chaque narratif, affichage de la médiane sur les modèles
# hydrologiques
for (storyline in Storylines) {
    data_storyline = data %>%
        dplyr::filter(climate_chain %in% storyline$climate_chain)
    
    data_storyline_med = data_storyline %>%
        dplyr::group_by(climate_chain,
                        date) %>%
        dplyr::summarise(!!variable:=median(get(variable),
                                            na.rm=TRUE),
                         .groups="drop")

    plot = plot +
        geom_line(data=data_storyline_med,
                  aes(x=date,
                      y=get(variable)),
                  color=storyline$color,
                  linewidth=0.6,
                  alpha=1,
                  lineend="round")
}

# Mise en forme des axes
plot = plot +
    xlab(NULL) +
    scale_x_date(
        breaks=get_breaks,
        minor_breaks=get_minor_breaks,
        guide="axis_minor",
        date_labels="%Y",
        expand=c(0, 0)) +
    ylab(NULL) +
    scale_y_continuous(limits=c(0, NA),
                       expand=c(0, 0))

# Sauvegarde
ggsave(plot=plot,
       filename=file.path(figures_dir,
                          paste0("DRIAS_indicateurs_",
                                 code, "_",
                                 gsub(" ", "_", variable_to_display),
                                 ".pdf")),
       width=30, height=10, units="cm")

#### 2.5.2. en changement annuel _____________________________________
# Filtrage de la partie historique et moyenne de la variable sur
# cette période pour chaque chaîne
data_historical = data %>%
    dplyr::group_by(chain) %>%
    dplyr::filter(historical[1] <= date &
                  date <= historical[2]) %>%
    dplyr::summarise(!!mean_variable:=mean(get(variable), na.rm=TRUE))

# Jointure avec les anciennes données
data = dplyr::left_join(data, data_historical, by="chain")

# Calcul du changement par rapport à la période de référence
# historique
data[[delta_variable]] =
    (data[[variable]] - data[[mean_variable]]) /
    data[[mean_variable]] * 100

# Définition du plot et du theme
plot = ggplot() +
    theme_minimal() +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          plot.margin=margin(5, 20, 5, 5),
          axis.line.x=element_line(color="grey65",
                                   linewidth=0.5,
                                   lineend="round"),
          axis.ticks.x=element_line(color="grey80"),
          axis.ticks.length=unit(0.2, "cm"),
          plot.title=element_text(color="grey30")) +

    # Titre
    ggtitle(TeX(paste0(code, " - changement de \\textbf{",
                       variable_to_display,
                       "}"))) +

    # Ligne du zéro
    annotate("line",
             x=c(min(data$date), max(data$date)),
             y=0,
             color="grey20",
             linewidth=0.6,
             linetype="dashed",
             lineend="round") +

    # Affichage des séries de changements
    geom_line(data=data,
              aes(x=date,
                  y=get(delta_variable),
                  group=chain),
              color="grey65",
              linewidth=0.4,
              alpha=0.15,
              lineend="round")

# Pour chaque narratif, affichage de la médiane des changements
# sur les modèles hydrologiques
for (storyline in Storylines) {
    data_storyline = data %>%
        dplyr::filter(climate_chain %in% storyline$climate_chain)

    data_storyline_med = data_storyline %>%
        dplyr::group_by(climate_chain, date) %>%
        dplyr::summarise(!!delta_variable:=
                             median(get(delta_variable), na.rm=TRUE),
                         .groups="drop")
    
    plot = plot +
        geom_line(data=data_storyline_med,
                  aes(x=date,
                      y=get(delta_variable)),
                  color=storyline$color,
                  linewidth=0.6,
                  alpha=1,
                  lineend="round")
}

# Mise en forme des axes
plot = plot +
    xlab(NULL) +
    scale_x_date(
        breaks=get_breaks,
        minor_breaks=get_minor_breaks,
        guide="axis_minor",
        date_labels="%Y",
        expand=c(0, 0)) +
    ylab(NULL) +
    scale_y_continuous(limits=c(-100, NA),
                       labels=delta_labels,
                       expand=c(0, 0))

# Sauvegarde
ggsave(plot=plot,
       filename=file.path(figures_dir,
                          paste0("DRIAS_indicateurs_",
                                 code, "_",
                                 "delta_",
                                 gsub(" ", "_", variable_to_display),
                                 ".pdf")),
       width=30, height=10, units="cm")

#### 2.5.3. en carte de changements par horizon ______________________
# Extraction de d'avantage de point
data = read_DRIAS_netcdf(Paths, Codes="K")

# Filtrage des points où les deux modèles hydrologiques choisis
# contiennent de la donnée
n_limits = 2
Codes_selection = data %>%
    dplyr::group_by(code) %>%
    dplyr::summarise(n=length(unique(HM))) %>%
    dplyr::filter(n_limits <= n)
Codes_selection = Codes_selection$code
data = dplyr::filter(data, code %in% Codes_selection)

# Obtention des métadonnées de position
NC = ncdf4::nc_open(Paths[1])
Codes_NC = ncdf4::ncvar_get(NC, "code")
L93_X = ncdf4::ncvar_get(NC, "L93_X")
L93_Y = ncdf4::ncvar_get(NC, "L93_Y")
Id = match(Codes_selection, Codes_NC)
meta = dplyr::tibble(code=Codes_selection,
                     L93_X=L93_X[Id],
                     L93_Y=L93_Y[Id])

# Choix de l'horizon
horizon = Horizons$H3

# Calcule des moyennes sur la période de référence et sur la
# période future par chaîne et par point
data_delta =
    dplyr::full_join(

               # Moyenne par chaîne sur la partie historique
               # de référence
               data %>%
               dplyr::filter(historical[1] <= date &
                             date <= historical[2]) %>%
               # Grouper par EXP, GCM, RCM, BC, HM pour conserver
               # ces colonnes aussi
               dplyr::group_by(code, chain,
                               EXP, GCM, RCM, BC, HM) %>%
               dplyr::summarise(historical=mean(get(variable),
                                                na.rm=TRUE),
                                .groups="drop"),

               # Moyenne par chaîne sur l'horizon choisi
               data %>%
               dplyr::filter(horizon[1] <= date &
                             date <= horizon[2]) %>%
               dplyr::group_by(code, chain) %>%
               dplyr::summarise(horizon=mean(get(variable),
                                             na.rm=TRUE),
                                .groups="drop"),

               by=c("code", "chain"))


# Calcule des changements relatifs ou absolues selon si la variable
# doit être normalisé
if (to_normalise) {
    data_delta$delta =
        (data_delta$horizon - data_delta$historical) /
        data_delta$historical * 100
} else {
    data_delta$delta =
        data_delta$horizon - data_delta$historical
}

# Moyenne par étape des changements par chaînes
# sur les modèles hydrologiques
data_delta = data_delta %>%
    group_by(code, EXP, GCM, RCM, BC) %>%
    summarise(mean_delta=mean(delta),
              .groups="drop")
# puis sur les corrections de biais
data_delta = data_delta %>%
    group_by(code, EXP, GCM, RCM) %>%
    summarise(mean_delta=mean(mean_delta),
                   .groups="drop")
# et enfin sur les couples GCM / RCM
data_delta = data_delta %>%
    group_by(code, EXP) %>%
    summarise(mean_delta=mean(mean_delta),
              .groups="drop")

# Ajout des métadonnées
data_delta = dplyr::left_join(data_delta, meta, by="code")

# Chargement des shapefiles
# Facteur de simplification
tolerance = 1000
# France
france_path = file.path(shapefiles_dir, "france")
france = sf::st_read(france_path)
france = sf::st_union(france)
france = sf::st_transform(france, 2154)
france = sf::st_simplify(france,
                         preserveTopology=TRUE,
                         dTolerance=tolerance)

# Grands bassins hydrologiques
bassin_path = file.path(shapefiles_dir, "bassin")
bassin = sf::st_read(bassin_path)
bassin = sf::st_transform(bassin, 2154)
bassin = sf::st_simplify(bassin,
                         preserveTopology=TRUE,
                         dTolerance=tolerance*0.6)

# Cours d'eau
cours_eau_path = file.path(shapefiles_dir, "cours_eau_Explore2")
cours_eau = sf::st_read(cours_eau_path)
cours_eau = sf::st_transform(cours_eau, 2154)
cours_eau = sf::st_simplify(cours_eau,
                            preserveTopology=TRUE,
                            dTolerance=tolerance*0.4)

# Title
title = ggplot() + theme_void() +
    annotate("text",
             x=0, y=1, hjust=0, vjust=1,
             label=TeX(paste0("Changement de \\textbf{",
                              variable_to_display,
                              "} en fin de siècle")),
             color="grey20") +
    scale_x_continuous(expand=c(0, 0),
                       limits=c(0, 1)) + 
    scale_y_continuous(expand=c(0, 0),
                       limits=c(0, 1))

# Utilisation d'un système de coordonnées fixes 
cf = coord_fixed()
cf$default = TRUE
# Nouveau plot pour une carte
map = ggplot() + theme_void() + cf

# Limites de la France
xlim = c(90000, 1250000)
ylim = c(6040000, 7120000)

# Échelle de la carte
xmin = gpct(62, xlim, shift=TRUE)
xint = c(0, 50*1E3, 100*1E3, 250*1E3)
ymin = gpct(5, ylim, shift=TRUE)
ymax = ymin + gpct(1.3, ylim)
size = 2.6
sizekm = 2.5
linewidth = 0.4

map = map +
    geom_line(aes(x=c(xmin, max(xint)+xmin),
                  y=c(ymin, ymin)),
              color="grey50", linewidth=linewidth,
              lineend="round") +
    annotate("text",
             x=max(xint)+xmin+gpct(1, xlim), y=ymin,
             vjust=0, hjust=0, label="km",
             color="grey50", size=sizekm)
for (x in xint) {
    map = map +
        annotate("segment",
                 x=x+xmin, xend=x+xmin, y=ymin, yend=ymax,
                 color="grey50", linewidth=linewidth,
                 lineend="round") +
        annotate("text",
                 x=x+xmin, y=ymax+gpct(0.5, ylim),
                 vjust=0, hjust=0.5, label=x/1E3,
                 fontface="bold",
                 color="grey50", size=size)
}

# Obtention de la palette sur mesure
reverse = FALSE
name = "ground_8"
Palette_level = c(4, 3, 2, 1, 1, 2, 3, 4)
Palette = get_IPCC_Palette(name, reverse=reverse)

# Calcule de limites de la palette
prob = 0.9
min_delta = quantile(data_delta$mean_delta,
                     prob, na.rm=TRUE)
max_delta = quantile(data_delta$mean_delta,
                     1-prob, na.rm=TRUE)

res = compute_colorBin(min_delta, max_delta,
                       colorStep=length(Palette),
                       center=0, include=FALSE)
bin = res$bin
upBin = res$upBin
lowBin = res$lowBin

# Obtention des couleurs pour chaque delta
data_delta$fill = get_colors(data_delta$mean_delta,
                             upBin=upBin,
                             lowBin=lowBin,
                             Palette=Palette)

# Obtention des couches d'affichage de chaque point
palette_match = match(data_delta$fill, Palette)
data_delta$level = Palette_level[palette_match]
Levels = as.numeric(levels(factor(data_delta$level)))

# Affichage des shapefiles
map = map +
    # Affichage du fond de carte de la France
    geom_sf(data=france,
            color=NA, fill="grey99") +
    # Affichage du contour des bassins
    geom_sf(data=bassin,
            color="grey85",
            fill=NA, size=0.2) +
    # des cours d'eau
    geom_sf(data=cours_eau,
            color="DarkTurquoise",
            alpha=1, fill=NA,
            linewidth=0.3, na.rm=TRUE) +
    # et de la France
    geom_sf(data=france,
            color="grey50",
            fill=NA, linewidth=0.45)

# Affichage des changements par couche
for (l in Levels) {
    data_delta_level = dplyr::filter(data_delta, level==l)
    map = map +
        geom_point(data=data_delta_level,
                   aes(x=L93_X, y=L93_Y),
                   fill=data_delta_level$fill,
                   color="transparent",
                   shape=21, size=1.4)
}

# Redécoupage de la carte
map = map +
    coord_sf(xlim=xlim, ylim=ylim,
             expand=FALSE)

# Affichage de la palette
label = delta_labels(bin)
cb = panel_colorbar_circle(bin,
                           Palette,
                           size_circle=3,
                           d_line=0.2,
                           linewidth=0.35,
                           d_space=0.15,
                           d_text=0.5,
                           text_size=2.8,
                           label=label,
                           ncharLim=4,
                           colorText="grey50",
                           colorLine="grey50",
                           on_circle=FALSE,
                           margin=margin(t=-1, r=0,
                                         b=1.5, l=7, "cm"))

# Mise en forme des graphs
plan = matrix(c("title", "title", "title",
                "map", "map", "map",
                "map", "map", "cb",
                "map", "map", "map"),
              byrow=TRUE, ncol=3)

herd = bring_grass()
herd = plan_of_herd(herd, plan)

herd = add_sheep(herd,
                 sheep=title,
                 id="title",
                 height=1)

herd = add_sheep(herd,
                 sheep=map,
                 id="map",
                 height=14)

herd = add_sheep(herd,
                 sheep=cb,
                 id="cb",
                 height=9)

paper_size = c(width=15, height=15)
plot = return_to_sheepfold(herd,
                           paper_size=paper_size,
                           page_margin=c(t=0.5, r=0.5,
                                         b=0.5, l=0.5))$plot

# Sauvegarde
ggsave(plot=plot,
       filename=file.path(figures_dir,
                          paste0("DRIAS_indicateurs_",
                                 "carte_",
                                 "delta_",
                                 gsub(" ", "_", variable_to_display),
                                 ".pdf")),
       width=paper_size[1],
       height=paper_size[2], units="cm")
