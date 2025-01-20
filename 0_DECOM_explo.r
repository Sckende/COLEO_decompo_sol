# ---------------------------- #
#### Decomposition sol - Complete data gabarit from Coleo ####
# ---------------------------- #

decom <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/COLEO_decompo_sol/2025-01_decomposition_sol-donnees_completes.csv",
    header = TRUE,
    sep = ",",
    dec = "."
)
library(readxl)
decom2 <- read_excel("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/COLEO_decompo_sol/2025-01_decomposition_sol-donnees_completes.xlsx")
head(decom)
summary(decom)
sapply(decom, class)
names(decom)
# Class conversion
col <- c("obs_soil_decomposition_start_weight", "obs_soil_decomposition_end_weight_with_bag", "obs_soil_decomposition_end_weight_tea", "obs_soil_decomposition_shading")
decom[col] <- sapply(decom[col], as.numeric)

# ---------------------------- #
#### Decomposition sol - simplified data & calculated indices from Coleo ####
# ---------------------------- #

s_decom <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/COLEO_decompo_sol/2025-01_decomposition_sol-simple_data.csv",
    h = TRUE, dec = "."
)
head(s_decom)
summary(s_decom)

table(s_decom$site_type)
summary(s_decom$jour_sol)
hist(s_decom$jour_sol[s_decom$site_type == "toundrique"])

#### Interactive ggplot - lat vs v_decomp ####
library(ggplot2)
library(plotly)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(sf)
library(lubridate)


# Text preparation
p <- s_dec %>%
    mutate(v_decomp = round(v_decomp, 2)) %>%
    mutate(fact_stab = round(fact_stab, 2)) %>%
    mutate(text = paste(
        "Region: ", region,
        "\nNom site: ", site_name,
        "\nCode site: ", site_code,
        "\nType site: ", site_type,
        "\nNb de jour dans sol: ", jour_sol,
        "\nVitesse de decomposition: ", v_decomp,
        "\nFacteur de stabilite: ", fact_stab
    ))

dec_plot <- ggplot(p, aes(x = lat_centro, y = v_decomp, size = jour_sol, color = site_type, text = text)) +
    geom_point(alpha = 0.7) +
    # scale_size(range = c(1.4, 19), name="Population (M)") +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    theme_ipsum() +
    theme(legend.position = "none")

# turn ggplot interactive with plotly
int_plt1 <- ggplotly(dec_plot, tooltip = "text")
int_plt1

#### Interactive ggplot - lat vs fact_stab ####
stab_plot <- ggplot(p, aes(x = lat_centro, y = fact_stab, size = jour_sol, color = site_type, text = text)) +
    geom_point(alpha = 0.7) +
    # scale_size(range = c(1.4, 19), name="Population (M)") +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    theme_ipsum() +
    theme(legend.position = "none")

# turn ggplot interactive with plotly
int_plt2 <- ggplotly(stab_plot, tooltip = "text")
int_plt2

#### Interactive ggplot - v_decomp vs fact_stab ####
dec_stab_plot <- ggplot(p, aes(x = v_decomp, y = fact_stab, size = jour_sol, color = site_type, text = text)) +
    geom_point(alpha = 0.7) +
    # scale_size(range = c(1.4, 19), name="Population (M)") +
    scale_color_viridis(discrete = TRUE, guide = FALSE) +
    theme_ipsum() +
    theme(legend.position = "none")

# turn ggplot interactive with plotly
int_plt3 <- ggplotly(dec_stab_plot, tooltip = "text")
int_plt3


#### Interactive map with decomposition speed ####
qc <- st_transform(st_read("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/QUEBEC_CR_NIV_01.gpkg"), crs = st_crs(4326))

map <- p %>%
    ggplot() +
    geom_sf(data = qc, fill = "grey", alpha = 0.3) +
    geom_point(aes(
        x = lon_centro, y = lat_centro, size = v_decomp, color = v_decomp, text = text,
        alpha = v_decomp
    )) +
    #   scale_size_continuous(range = c(1, 9)) +
    scale_color_viridis_c(option = "inferno", direction = -1, trans = "log") +
    scale_alpha_continuous(trans = "log") +
    theme_void() +
    theme(legend.position = "none")

int_map <- ggplotly(map, tooltip = "text")
int_map


#### Interactive map with stability factor ####
map <- p %>%
    ggplot() +
    geom_sf(data = qc, fill = "grey", alpha = 0.3) +
    geom_point(aes(
        x = lon_centro, y = lat_centro, size = fact_stab, color = fact_stab, text = text,
        alpha = fact_stab
    )) +
    #   scale_size_continuous(range = c(1, 9)) +
    scale_color_viridis_c(option = "inferno", direction = -1, trans = "log") +
    scale_alpha_continuous(trans = "log") +
    theme_void() +
    theme(legend.position = "none")

int_map <- ggplotly(map, tooltip = "text")
int_map


summary(lm(p$v_decomp ~ p$fact_stab))
summary(lm(p$fact_stab ~ p$lat_centro))
summary(lm(p$v_decomp ~ p$lat_centro))
summary(lm(p$v_decomp ~ p$site_type))
summary(lm(p$fact_stab ~ p$site_type))

# ------------------------------------------------ #
#### MELCCFP data & comparison with coleo data ####
# ---------------------------------------------- #

melccfp <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/COLEO_decompo_sol/MELCCFP_raw_data.csv", header = TRUE, dec = ".")

# Class conversion
col <- c("stabilisation", "decomposition")
melccfp[col] <- sapply(melccfp[col], as.numeric)

summary(melccfp)
# retrieve basal information
infos <- decom2[, c("sites_site_code", "landmarks_lat", "landmarks_lon", "campaigns_opened_at", "campaigns_closed_at", "obs_soil_decomposition_date_end", "obs_soil_decomposition_bag_no")]

melccfp2 <- left_join(melccfp, infos, by = join_by(vert == obs_soil_decomposition_bag_no))

melccfp3 <- melccfp2[!is.na(melccfp2$sites_site_code), ]
dim(melccfp2)
sites_melccfp <- unique(melccfp2$sites_site_code)
sites_coleo <- unique(s_decom$site_code)

length(sites_melccfp)
length(sites_coleo)
extra_sites <- sites_coleo[!(sites_coleo %in% sites_melccfp)]

# retrait des sites non présents dans les donnees du melccfp
s_decom2 <- s_decom[!(s_decom$site_code %in% extra_sites), ]
names(s_decom2)
names(melccfp2)

melccfp3$campaigns_opened_at <- ymd(melccfp3$campaigns_opened_at)
melccfp3$campaigns_closed_at <- ymd(melccfp3$campaigns_closed_at)
melccfp3$obs_soil_decomposition_date_end <- ymd(melccfp3$obs_soil_decomposition_date_end)
melccfp3$site_campaigns_year <- paste0(melccfp3$sites_site_code, "-", year(melccfp3$campaigns_opened_at))

lis <- split(melccfp3, melccfp3$site_campaigns_year)
length(lis)
#### *** restart here - mean of v and s per site per year *** ####
# ---------------------------- #
#### Environmental data ####
# ---------------------------- #

# from CHELSA - bio1 (mean annual air temperature), bio5 (mean daily maximum air temperature of the warmest month), bio6 (mean daily minimum air temperature of the coldest month), bio12 (annual precipitation amount) & GHMTS (Global Human of Terrestrial Systems)
env <- readRDS("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/env_data/site_climate.RDS")
head(env)
