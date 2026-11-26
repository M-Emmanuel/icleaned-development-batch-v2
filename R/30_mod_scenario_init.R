# Updated column names for crop and livestock tables ---------------------------
# for better presentation in Shiny data table
feedtype_colnames <- c(
  "", # For the delete_row column
  "Feed",
  "Crop",
  "Source type",
  "Intercropping (yes / no)",
  "IF intercropping, fraction of field occupied by this crop",
  "Cut-and-carry (fraction)",
  "Land cover",
  "Slope",
  "Length of slope (m)",
  "Management grassland",
  "Main product removal (fraction)",
  "Crop residue removal from field (fraction)",
  "Crop residue burnt (fraction)",
  "DM content (%)",
  "ME content (MJ/kg DM)",
  "CP content (% DM)",
  "water regime",
  "Cultivation period",
  "rice ecosystem type",
  "rice organic amendment",
  "Grassland change factor",
  "Land cover C Factor",
  "Slope P Factor",
  "Main product dry yield (t DM/ha)",
  "Residue dry yield (t DM/ha)",
  "Main product N content (kg N/kg DM)",
  "Residue N content (kg N/kg DM)",
  "Kc: Initial",
  "Kc: MidSeason",
  "Kc: Late",
  "Category",
  "Trees/ha",
  "Trees DBH",
  "Trees annual growth (kg)",
  "Trees annual removal (kg)",
  "Number of trees per hectare with a diameter at breast height of less than 25cm",
  "Average diameter at breast height of trees with a DBH of less than 25cm",
  "Diameter at breast height increase in trees with a DBH of less than 25cm (cm/year)",
  "Number of trees per hectare with a diameter at breast height of 25-50cm",
  "Average diameter at breast height of trees with a DBH of 25-50cm",
  "Diameter at breast height increase in trees with a DBH of 25-50cm (cm/year)",
  "Number of trees per hectare with a diameter at breast height more than 50cm",
  "Average diameter at breast height of trees with a DBH more than 50cm",
  "Diameter at breast height increase in trees with a DBH of more than 50cm (cm/year)",
  "Time it takes for tree to mature (years)",
  "Tree breast diameter (cm)"
)

livestock_table_colnames <- c(
  "", # For the delete_row column
  "Category",
  "Species",
  "Animal class",
  "Number",
  "Average annual milk production (kg/animal)",
  "Average annual wool production (kg/animal)",
  "Average annual live weight gain (kg/animal)",
  "Time spent in stable (fraction of day)",
  "Collection of manure in stable (fraction)",
  "Manure management in stable",
  "Share 1 stable",
  "Second manure management in stable (optional)",
  "Share of the stable manure under the second management (fraction)",
  "Time spent in non-roofed enclosure  (fraction of day)",
  "Collection of manure in non-roofed enclosure (fraction)",
  "Manure management in non-roofed enclosure",
  "Share 1 non_roofed_enclosure",
  "Second manure management in non-roofed enclosure (optional)",
  "Share of the non-roofed enclosure manure under the second management (fraction)",
  "Time spent grazing  pasture/fields on-farm (fraction of day)",
  "Collection of manure in fields/pasture (fraction)",
  "Manure management during on-farm grazing",
  "Share 1 onfarm_grazing",
  "Second manure management in on-farm grazing (optional)",
  "Share of the on-farm grazing manure under the second management (fraction)",
  "Time spent grazing off-farm (fraction of day)",
  "Manure management during off-farm grazing",
  "Share 1 offfarm_grazing",
  "Second manure management in off-farm grazing (optional)",
  "Share of the off-farm grazing manure under the second management (fraction)",
  "Distance stable / enclosure to pasture (km)",
  "Fraction of collected manure used on the farm",
  "Fraction of collected manure being sold",
  "Average Body weight (kg)",
  "The live bodyweight at weaning (kgs)",
  "The live bodyweight of sheep and goat at 1-year old or at slaughter 
  (live-weight) if slaughtered prior to 1 year of age (kgs)",
  "The mature body weight of an adult animal (kgs)",
  "Work animals (hours/day/animal)",
  "Litter size  (pigs)",
  "Piglets relying on milk (% of the litter)",
  "Lactation length (small ruminants/pigs, days)",
  "Proportion growth piglets covered by milk (%)",
  "LW gain piglets (kg/day)",
  "Crude Protein/Lysine requirement maintenance (kg/day)",
  "Crude Protein/ Lysine requirement pregnancy (kg)",
  "Crude Protein requirement  (kg/kg milk)",
  "Crude Protein/ Lysine requirement growth (kg/kg LWG)",
  "Parturition interval (years)",
  "Protein content milk (%)",
  "Fat content milk (%)",
  "Energy content milk",
  "Energy content meat",
  "Protein content meat (%)",
  "Carcass fraction",
  "Average N content manure",
  "Meat product",
  "Milk product",
  "Enteric methane category (IPCC Tier 2, Ym)",
  "Manure methane category (IPCC Table 10.16, Bo)",
  "Manure N loss category (IPCC Table 10.22)",
  "Pasture N2O group (IPCC Table 11.1, EF3PRP)"
)

crop_inputs_table_colnames <- c(
  "Feed",
  "Crop",
  "Fraction collected manure used as fertilizer",            
  "Urea (kg/ha)",
  "NPK (kg/ha)",
  "DAP (kg/ha)",
  "Ammonium nitrate (kg/ha)",
  "Ammonium sulfate (kg/ha)",
  "N solutions (kg/ha)",
  "Ammonia (kg/ha)"
)

# Initialize data frames -------------------------------------------------------

seasons_initialization <- data.frame(
  Season = character(),
  Days = numeric(),
  stringsAsFactors = FALSE
)

fertilizers_initialization <- data.frame(
  fertilizer_code = character(),
  fertilizer_desc = character(),
  fraction = numeric(),
  percentage_n = numeric(),
  stringsAsFactors = FALSE
)

livestock_data_initialization <- data.frame(
  livetype_code = character(),
  livetype_desc = character(),
  species = character(),
  animal_class = character(),
  herd_composition = numeric(),
  annual_milk = numeric(),
  annual_wool = numeric(),
  annual_growth = numeric(),
  time_in_stable = numeric(),
  manure_in_stable = numeric(),
  manureman_stable = character(),
  manure_share_1_stable = numeric(),
  manureman_2_stable = character(),
  manure_share_2_stable = numeric(),
  time_in_non_roofed_enclosure = numeric(),
  manure_in_non_roofed_enclosure = numeric(),
  manureman_non_roofed_enclosure = character(),
  manure_share_1_non_roofed_enclosure = numeric(),
  manureman_2_non_roofed_enclosure = character(),
  manure_share_2_non_roofed_enclosure = numeric(),
  time_in_onfarm_grazing = numeric(),
  manure_in_field = numeric(),
  manureman_onfarm_grazing = character(),
  manure_share_1_onfarm_grazing = numeric(),
  manureman_2_onfarm_grazing = character(),
  manure_share_2_onfarm_grazing = numeric(),
  time_in_offfarm_grazing = numeric(),
  manureman_offfarm_grazing = character(),
  manure_share_1_offfarm_grazing = numeric(),
  manureman_2_offfarm_grazing = character(),
  manure_share_2_offfarm_grazing = numeric(),
  distance_to_pasture = numeric(),
  manure_onfarm_fraction = numeric(),
  manure_sales_fraction = numeric(),
  body_weight = numeric(),
  body_weight_weaning = numeric(),
  body_weight_year_one = numeric(),
  adult_weight = numeric(),
  work_hour = numeric(),
  litter_size = numeric(),
  piglets_relying_on_milk = numeric(),
  lactation_length = numeric(),
  proportion_growth_piglets_milk = numeric(),
  lw_gain_piglets = numeric(),
  cp_maintenance = numeric(),
  cp_lys_pregnancy = numeric(),
  cp_lactmilk = numeric(),
  cp_lys_growth = numeric(),
  birth_interval = numeric(),
  protein_milkcontent = numeric(),
  fat_milkcontent = numeric(),
  energy_milkcontent = numeric(),
  energy_meatcontent = numeric(),
  protein_meatcontent = numeric(),
  carcass_fraction = numeric(),
  n_manure_content = numeric(),
  meat_product = character(),
  milk_product = character(),
  ipcc_ef_category_t2 = character(),
  ipcc_manure_ch4_category = character(),
  ipcc_n_loss_category = character(),
  ipcc_prp_category = character(),
  stringsAsFactors = FALSE
)

# Column order of the livestock table: the time in each place first, then the
# manure of each place (shown only for places where the animals spend time)
# (per place: system option 1, its fraction, system option 2, its fraction, collection)
livestock_place_blocks <- list(
  stable = c("manureman_stable", "manure_share_1_stable", "manureman_2_stable", "manure_share_2_stable",
             "manure_in_stable"),
  non_roofed_enclosure = c("manureman_non_roofed_enclosure", "manure_share_1_non_roofed_enclosure",
                           "manureman_2_non_roofed_enclosure", "manure_share_2_non_roofed_enclosure",
                           "manure_in_non_roofed_enclosure"),
  onfarm_grazing = c("manureman_onfarm_grazing", "manure_share_1_onfarm_grazing", "manureman_2_onfarm_grazing",
                     "manure_share_2_onfarm_grazing", "manure_in_field"),
  offfarm_grazing = c("manureman_offfarm_grazing", "manure_share_1_offfarm_grazing",
                      "manureman_2_offfarm_grazing", "manure_share_2_offfarm_grazing")
)
livestock_place_labels <- c(stable = "in stable", non_roofed_enclosure = "in non-roofed enclosure",
                            onfarm_grazing = "during on-farm grazing", offfarm_grazing = "during off-farm grazing")
local({
  time_cols <- paste0("time_in_", names(livestock_place_blocks))
  # species, animal class and the IPCC categories are livestock parameters:
  # they come last, together (orange section)
  categories <- c("species", "animal_class", "ipcc_ef_category_t2", "ipcc_manure_ch4_category",
                  "ipcc_n_loss_category", "ipcc_prp_category")
  first <- setdiff(names(livestock_data_initialization)[seq_len(match("annual_growth", names(livestock_data_initialization)))],
                   categories)
  manure <- c(unlist(livestock_place_blocks, use.names = FALSE), "manure_onfarm_fraction", "manure_sales_fraction")
  order <- c(first, time_cols, "distance_to_pasture", manure)
  order <- c(order, setdiff(names(livestock_data_initialization), c(order, categories)), categories)
  # labels follow their columns (label k belongs to column k)
  label_of <- stats::setNames(livestock_table_colnames, names(livestock_data_initialization))
  livestock_data_initialization <<- livestock_data_initialization[, order]
  labels <- unname(label_of[order])
  # the two management options of each place and their fractions
  for (place in names(livestock_place_blocks)) {
    where <- livestock_place_labels[[place]]
    labels[match(c(paste0("manureman_", place), paste0("manure_share_1_", place),
                   paste0("manureman_2_", place), paste0("manure_share_2_", place)), order)] <- c(
      paste("Manure management", where, "- option 1"),
      paste("Fraction of the manure", where, "under option 1"),
      paste("Manure management", where, "- option 2 (optional)"),
      paste("Fraction of the manure", where, "under option 2"))
  }
  labels[1] <- ""
  livestock_table_colnames <<- labels
})

feedtype_initialization <- data.frame(
  crop_code = numeric(),
  feed_item_code = numeric(),
  feed_item_name = character(),
  crop_name = character(),
  source_type = character(),
  intercrop = numeric(),
  intercrop_fraction = numeric(),
  cut_carry_fraction = numeric(),
  land_cover_desc = character(),
  slope_desc = character(),
  slope_length = numeric(),
  grassman_desc = character(),
  main_product_removal = numeric(),
  residue_removal = numeric(),
  residue_burnt = numeric(),
  dm_content = numeric(),
  me_content = numeric(),
  cp_content = numeric(),    
  water_regime = character(),     
  cultivation_period = numeric(),
  ecosystem_type = character(),
  organic_amendment = character(),
  grassman_change_factor = numeric(),
  landcover_c_factor = numeric(),
  slope_p_factor = numeric(),
  dry_yield = numeric(),
  residue_dry_yield = numeric(),
  main_n = numeric(),
  residue_n = numeric(),
  kc_initial = numeric(),
  kc_midseason = numeric(),
  kc_late = numeric(),
  category = character(),
  trees_ha = numeric(),
  trees_dhb = numeric(),
  trees_growth = numeric(),
  trees_removal = numeric(),
  trees_ha_dbh25 = numeric(),
  average_dbh25 = numeric(),
  increase_dbh25 = numeric(),
  trees_ha_dbh2550 = numeric(),
  average_dbh2550 = numeric(),
  increase_dbh2550 = numeric(),
  trees_ha_dbh50 = numeric(),
  average_dbh50 = numeric(),
  increase_dbh50 = numeric(),
  time_horizon = numeric(),
  diameter_breast = numeric(),
  
  # These ones are available in the json but not in the DT
  fraction_as_manure = character(),
  n_fertilizer = character(),
  land_cover = character(),
  slope = character(),
  grassman = character(),
  
  stringsAsFactors = FALSE
)

crop_inputs_data_initialization <- data.frame(
  Feed = character(),
  Crop = character(),
  fraction_as_fertilizer = numeric(),
  urea = numeric(),
  npk = numeric(),
  dap = numeric(),
  ammonium_nitrate = numeric(),
  ammonium_sulfate = numeric(),
  n_solutions = numeric(),
  ammonia = numeric(),
  stringsAsFactors = FALSE
)

# Columns synced from the parameters database into the scenario state ---------
# When the parameters database changes (or a scenario is loaded), these columns
# are refreshed from the matching row in the parameters DB (matched by code).
# User-edited columns that are NOT listed here are preserved as-is.

feedtype_sync_cols_lkp_crops <- c(
  "crop_name", "dry_yield", "residue_dry_yield", "main_n", "residue_n",
  "kc_initial", "kc_midseason", "kc_late", "category",
  "trees_ha", "trees_dhb", "trees_growth", "trees_removal",
  "trees_ha_dbh25", "average_dbh25", "increase_dbh25",
  "trees_ha_dbh2550", "average_dbh2550", "increase_dbh2550",
  "trees_ha_dbh50", "average_dbh50", "increase_dbh50",
  "time_horizon", "diameter_breast"
)

# (crop_code too: the crop of a feed is the one the parameters database gives it)
feedtype_sync_cols_lkp_feeditem <- c(
  "feed_item_name", "crop_code", "dm_content", "me_content", "cp_content"
)

# Named vector: target column name = source column name (enables renaming).
livestock_sync_cols_lkp_livetype <- c(
  "livetype_desc"                  = "livetype_desc",
  "body_weight"                    = "body_weight",
  "litter_size"                    = "litter_size",
  "lactation_length"               = "lactation_length",
  "proportion_growth_piglets_milk" = "proportion_growth_piglets_milk",
  "lw_gain_piglets"                = "lw_gain_piglets",
  "cp_maintenance"                 = "cp_maintenance",
  "cp_lys_pregnancy"               = "cp_lys_pregnancy",
  "cp_lactmilk"                    = "cp_lactmilk",
  "cp_lys_growth"                  = "cp_lys_growth",
  "birth_interval"                 = "birth_interval",
  "protein_milkcontent"            = "protein_milkcontent",
  "fat_milkcontent"                = "fat_milkcontent",
  "energy_milkcontent"             = "energy_milkcontent",
  "energy_meatcontent"             = "energy_meatcontent",
  "protein_meatcontent"            = "protein_meatcontent",
  "carcass_fraction"               = "carcass_fraction",
  "n_manure_content"               = "n_manure_content",
  "meat_product"                   = "meat_product",
  "milk_product"                   = "milk_product",
  "species"                        = "species",
  "animal_class"                   = "animal_class",
  "ipcc_ef_category_t2"            = "ipcc_ef_category_t2",
  "ipcc_manure_ch4_category"       = "ipcc_manure_ch4_category",
  "ipcc_n_loss_category"           = "ipcc_n_loss_category",
  "ipcc_prp_category"              = "ipcc_prp_category"
)
# (the sync works both ways: these columns edited in a scenario are written
# back to the scenario's parameters database, see push_parameter_value())

# Define the list of inputs for saving & loading -------------------------------

numeric_inputs <- c(
  "purchased_manure", "purchased_compost", "purchased_organic_n", 
  "purchased_bedding", "waste_production_milk", "waste_production_meat", 
  "waste_distribution_milk", "waste_distribution_meat", "waste_processing_milk", 
  "waste_processing_meat", "waste_consume_milk", "waste_consume_meat", 
  "annual_prec", "rain_length", "soil_k_value", "soil_n", "soil_c", 
  "soil_clay", "soil_bulk", "soil_depth", "et", "cropland_system_ipcc", 
  "cropland_tillage_ipcc", "cropland_orgmatter_ipcc", "grassland_management_ipcc", 
  "grassland_implevel_ipcc", "grassland_toarable", "arable_tograssland"
)

text_inputs <- c(
  "farm_name", "climate_zone", "climate_zone_2", "soil_description", 
  "cropland_system", "cropland_tillage", "cropland_orgmatter", 
  "grassland_management", "grassland_implevel"
)

manure_cols <- c(
  "manureman_stable",
  "manureman_non_roofed_enclosure",
  "manureman_onfarm_grazing",
  "manureman_offfarm_grazing",
  # a second system for a share of the manure of the place (optional)
  "manureman_2_stable",
  "manureman_2_non_roofed_enclosure",
  "manureman_2_onfarm_grazing",
  "manureman_2_offfarm_grazing"
)
manure_second_cols <- grep("^manureman_2_", manure_cols, value = TRUE)
manure_share_cols <- sub("^manureman_2_", "manure_share_2_", manure_second_cols)
manure_share_1_cols <- sub("^manureman_2_", "manure_share_1_", manure_second_cols)
manure_none_label <- "None"

# Named vector for hard coded percentage_n values ------------------------------
fertilizer_percentages <- c(
  "Urea" = 46,
  "DAP" = 18,
  "Ammonium nitrate" = 34.5,
  "Ammonium sulfate" = 21,
  "N solutions" = 32,
  "Ammonia" = 82
)

# Fertilizer name to column mapping --------------------------------------------
# Maps fertilizer names from the 'Farm > Fertilizer' tab to their corresponding
# column names in the 'Crop Inputs' dataframe. Used to dynamically identify
# and control column editability.
fertilizer_column_mapping <- c(
  "Urea" = "urea",
  "NPK" = "npk",
  "DAP" = "dap",
  "Ammonium nitrate" = "ammonium_nitrate",
  "Ammonium sulfate" = "ammonium_sulfate",
  "N solutions" = "n_solutions",
  "Ammonia" = "ammonia"
)

# Rice settings: the values of IPCC tables 5.12-5.14, which the model looks the
# factors up with (the same lists as the batch Excel template's dropdowns) ------
rice_ipcc_fallback <- list(
  ecosystem_type = c("Irrigated-Continuously flooded", "intermittently flooded-single aeration",
                     "Intermittently flooded-multiple aeration", "Rainfed-regular rainfed",
                     "Rainfed-drought prone", "Rainfed-deep water", "Upland", "None"),
  water_regime = c("non-flooded pre-season <180 days( often in double cropping of rice)",
                   "non-flooded pre-season >180 days (single rice crop following a dry fallow period)",
                   "flooded pre-season (>30 days)", "flooded pre-season (<30 days)", "None"),
  organic_amendment = c("straw incorporated in soil shortly (<30 days) before cultivation",
                        "straw incorporated in soil long (>30 days) before cultivation",
                        "Compost", "Farm yard manure", "green manure", "None")
)
rice_ipcc_lists <- tryCatch(cleaned::rice_options(), error = function(e) rice_ipcc_fallback)
water_regime_options <- rice_ipcc_lists$water_regime
rice_ecosystem_options <- rice_ipcc_lists$ecosystem_type
rice_organic_amendment_options <- rice_ipcc_lists$organic_amendment
