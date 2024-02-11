# 1. PACKAGES
#------------

install.packages("remotes")
remotes::install_github(
    "eurostat/restatapi"
)

libs <- c(
    "restatapi",
    "tidyverse",
    "giscoR",
    "sf",
    "classInt"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(
    any(installed_libs == F){
        install.packages(
            libs[!installed_libs],
            dependencies = T
        )
    }
)

invisible(
    lapply(
        libs, library,
        character.only = T
    )
)

# 2. GET NUTS & COUNTRY BOUNDARIES
#---------------------------------

nuts3_sf <- giscoR::gisco_get_nuts(
    nuts_level = "3",
    resolution = "3",
    year = "2021"
)

countries_sf <- giscoR::gisco_get_countries(
    resolution = "3",
    region = c("Asia", "Europe")
)

# 3. FILTER COUNTRIES
#--------------------

non_eu_list <- c(
    "AM", "AZ", "BA",
    "BY", "GE", "MD",
    "RU", "UA", "XK"
)

eu_list <- c(
    unique(
        nuts3_sf$CNTR_CODE
    )
)

eu_sf <- countries_sf |>
    dplyr::filter(
        CNTR_ID %in% eu_list
    )

non_eu_sf <- countries_sf |>
    dplyr::filter(
        CNTR_ID %in% non_eu_list
    )

# 4. MEDIAN AGE DATA
#--------------------

indicator_df <- restatapi::get_eurostat_data(
    id = "demo_r_pjanind3",
    filters = c("MEDAGEPOP", "YR"),
    date_filter = c(2021:2022),
    exact_match = T,
    label = F
)

head(indicator_df)

# 5. FILTER MEDIAN AGE DATA
#--------------------------

indicator_filtered_df <- indicator_df |>
    dplyr::select(
        1, 4, 5
    ) |>
    dplyr::rename(
        "NUTS_ID" = "geo"
    )

head(indicator_filtered_df)

# 6. DF TO WIDE FORMAT
#---------------------

indicator_wide_df <- indicator_filtered_df |>
    tidyr::pivot_wider(
        names_from = time,
        values_from = values
    )

head(indicator_wide_df)

# 7. REPLACE MISSING VALUES
#--------------------------

indicator_df_final <- indicator_wide_df |>
    dplyr::mutate(
        values = dplyr::if_else(
            is.na(`2022`),
            `2021`,
            `2022`
        )
    ) |>
    dplyr::select(NUTS_ID, values)

head(indicator_df_final)

# 8. MERGE NUTS3 SF AND DF
#-------------------------

mapping_sf <- nuts3_sf |>
    dplyr::left_join(
        indicator_df_final,
        by = "NUTS_ID"
    )

head(mapping_sf)

# 9. BREAKS, COLOR & BOUNDING BOX
#--------------------------------

ni <- classInt::classIntervals(
    mapping_sf$values,
    n = 6,
    style = "equal"
)$brks

brk <- ni |>
    append(
        max(mapping_sf$values)
    ) |>
    head(-1)

breaks <- c(
    min(mapping_sf$values), brk
) |>
tail(-1)

cols <- hcl.colors(
    n = 7,
    palette = "viridis",
    rev = T
)

crs_lambert <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

xmin <- -10.66
ymin <- 34.5
xmax <- 45
ymax <- 71

bbox <- sf::st_sfc(
    sf::st_polygon(
        list(
            cbind(
                c(
                    xmin,
                    xmax,
                    xmax,
                    xmin,
                    xmin
                ),
                c(
                    ymin,
                    ymin,
                    ymax,
                    ymax,
                    ymin
                )
            )
        )
    ), crs = 4326
)

lambert_bbox <- sf::st_transform(
    bbox,
    crs = crs_lambert
)

bb <- sf::st_bbox(lambert_bbox)

# 10. MAP
#--------

p <- ggplot(data = mapping_sf) +
    geom_sf(
        data = subset(
            eu,
            CNTR_ID == "RS"
        ),
        fill = "grey80",
        color = "white",
        size = .15
    ) +
    geom_sf(
        mapping = aes(
            fill = values
        ),
        color = NA,
        size = 0
    ) +
    geom_sf(
        data = eu_sf,
        color = "white",
        size = .15,
        fill = "transparent"
    ) +
    geom_sf(
        data = non_eu_sf,
        color = "white",
        size = .15,
        fill = "grey80"
    ) +
    coord_sf(
        crs = crs_lambert,
        xlim = c(bb["xmin"], bb["xmax"]),
        ylim = c(bb["ymin"], bb["ymax"]),
    ) +
    scale_fill_gradientn(
        name = "Median age (years)",
        colors = cols,
        breaks = breaks,
        labels = round(breaks, 0),
        limits = c(
            min(mapping_sf$values),
            max(mapping_sf$values)
        ),
        na.value = "grey80"
    ) +
    guides(
        fill = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2.5, units = "mm"),
            barwidth = unit(50, units = "mm"),
            title.position = "top",
            title.hjust = .5,
            label.position = "bottom",
            label.hjust = .5,
            nrow = 1,
            byrow = T
        )
    ) +
    theme_void() +
    theme(
        legend.position = c(.35, .95),
        legend.title = element_text(
            size = 11, color = "grey10"
        ),
        legend.text = element_text(
            size = 10, color = "grey10"
        ),
        legend.spacing.y = unit(.25, "cm"),
        plot.title = element_text(
            size = 26, face = "bold",
            color = "grey40", hjust = .5,
            vjust = 5
        ),
        plot.caption = element_text(
            size = 10, color = "grey60",
            hjust = .5, vjust = 3
        ),
        plot.margin = unit(
            c(
                t = 0, r = -2, 
                b = -2, l = -2
            ), "lines"
        )
    ) +
    labs(
        caption = "©2024 Milos Popovic (https://milospopovic.net) Data: Eurostat",
        title = "Median age in 2022"
    )

