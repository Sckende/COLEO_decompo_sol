#### Decomposition sol - Complete data gabarit ####
decom <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/2025-01-16_decomposition_sol-complete_data.csv",
    h = TRUE
)
head(decom)
summary(decom)
sapply(decom, class)
names(decom)
# Class conversion
col <- c("obs_soil_decomposition_start_weight", "obs_soil_decomposition_end_weight_with_bag", "obs_soil_decomposition_end_weight_tea", "obs_soil_decomposition_shading")
decom[col] <- sapply(decom[col], as.numeric)

#### Decomposition sol - simplified data & calculated indices ####
s_dec <- read.csv("/home/local/USHERBROOKE/juhc3201/BdQc/COLEO/Data/2025-01-16_decomposition_sol-simple_data.csv",
    h = TRUE, row.names = NULL
)
head(s_dec)
names(s_dec) <- c("region", "site_name", "site_type", "site_code", "lat_centro", "lon_centro", "date_pose", "date_recup", "jour_sol", "v_decomp", "fact_stab", "pds_ini_tv", "pds_fin_sac_tv", "pds_fin_tv", "pds_ini_rooi", "pds_fin_sac_rooi", "pds_fin_rooi")
summary(s_dec)

table(s_dec$site_type)
summary(s_dec$jour_sol)
hist(s_dec$jour_sol[s_dec$site_type == "toundrique"])

#### Interactive ggplot - lat vs v_decomp ####
library(ggplot2)
library(plotly)
library(dplyr)
library(plotly)
library(viridis)
library(hrbrthemes)
library(sf)

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
