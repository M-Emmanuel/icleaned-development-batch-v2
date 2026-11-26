# Livestock species, classes and IPCC Tier 2 categories ------------------------
#
# These vocabularies mirror the CLEANED package (?livestock_species_classes,
# ?ipcc_livestock_categories). Each livestock type in the parameters database
# (lkp_livetype) carries these fields, and CLEANED selects its energy and
# emission equations from them instead of from the livestock name, so users can
# add or rename livestock types.

livestock_species_classes <- list(
  Cattle = c("Cows", "Adult males", "Young stock"),
  Buffalo = c("Cows", "Adult males", "Young stock"),
  Sheep = c("Ewes", "Breeding rams", "Fattening rams", "Lambs"),
  Goats = c("Does", "Breeding bucks", "Fattening bucks", "Kids"),
  Pigs = c("Lactating/pregnant sows", "Dry sows/boars", "Growers")
)

ipcc_livestock_categories <- list(
  ipcc_ef_category_t2 = c("Dairy cows", "Non-dairy", "Sheep", "Goats", "N/A"),
  ipcc_manure_ch4_category = c("Dairy cattle", "Other cattle", "Buffalo",
                               "Sheep", "Goats", "Swine"),
  ipcc_n_loss_category = c("Dairy Cow", "Other Cattle", "Swine", "Poultry",
                           "Other animals"),
  ipcc_prp_category = c("Cattle, pigs and poultry", "Sheep and other animals")
)

# Meat and milk products follow from the species (labels only: the quantities
# come from growth, carcass fraction and milk; results are grouped by species)
livestock_species_products <- data.frame(
  species = c("Cattle", "Buffalo", "Sheep", "Goats", "Pigs"),
  meat_product = c("beef", "buffalo", "mutton", "goat", "pork"),
  milk_product = c("cow milk", "buffalo milk", "sheep milk", "goat milk", "N/A"),
  stringsAsFactors = FALSE
)
livestock_product_columns <- c("meat_product", "milk_product")

# Set meat_product and milk_product from the species (rows with a known species)
livestock_fill_products <- function(d) {
  if (!is.data.frame(d) || !nrow(d) || !"species" %in% names(d)) return(d)
  k <- match(as.character(d$species), livestock_species_products$species)
  for (cl in livestock_product_columns) {
    v <- if (cl %in% names(d)) as.character(d[[cl]]) else rep(NA_character_, nrow(d))
    v[!is.na(k)] <- livestock_species_products[[cl]][k[!is.na(k)]]
    d[[cl]] <- v
  }
  d
}

# Columns chosen from a dropdown (never typed) in the livestock types table
livetype_choice_columns <- c("species", "animal_class",
                             names(ipcc_livestock_categories))

# Labels shown for these columns
livetype_category_labels <- c(
  species = "Species",
  animal_class = "Animal class",
  ipcc_ef_category_t2 = "Enteric methane category (IPCC Tier 2, Ym)",
  ipcc_manure_ch4_category = "Manure methane category (IPCC Table 10.16, Bo)",
  ipcc_n_loss_category = "Manure N loss category (IPCC Table 10.22)",
  ipcc_prp_category = "Pasture N2O group (IPCC Table 11.1, EF3PRP)"
)

# Tier 1 fields no longer used (CLEANED only uses Tier 2)
livetype_dropped_columns <- c(
  "ipcc_meth_ef_t1", "ipcc_meth_man", "ipcc_meth_exc",
  "ipcc_ef_category_t1", "ipcc_meth_man_category", "ipcc_n_exc_category"
)

# Choices for one dropdown column; animal classes depend on the species
livetype_column_choices <- function(column, species = NULL) {
  if (column == "species") return(names(livestock_species_classes))
  if (column == "animal_class") {
    if (!is.null(species) && !is.na(species) && species %in% names(livestock_species_classes)) {
      return(livestock_species_classes[[species]])
    }
    return(unique(unlist(livestock_species_classes)))
  }
  ipcc_livestock_categories[[column]]
}

# Default categories of the livestock types shipped with iCLEANED, used to fill
# in databases and study objects created before these fields existed.
legacy_livestock_categories <- data.frame(
  livetype_desc = c(
    "Cattle - Cows (local)", "Cattle - Cows (improved)",
    "Cattle - Cows (high productive)", "Cattle - Adult male",
    "Cattle - Steers/heifers", "Cattle - Steers/heifers (improved)",
    "Cattle - Calves", "Cattle - Calves (improved)",
    "Buffalo - Cows", "Buffalo - Adult male", "Buffalo - Steers/heifers",
    "Buffalo - Calves",
    "Sheep - Ewes", "Sheep - Breeding Rams", "Sheep - Fattening Rams",
    "Sheep - Lambs",
    "Goats - Does", "Goats - Breeding Bucks", "Goats - Fattening Bucks",
    "Goats - Kids",
    "Pigs - lactating/pregnant sows", "Pigs - dry sows/boars", "Pigs - growers"
  ),
  species = c(rep("Cattle", 8), rep("Buffalo", 4), rep("Sheep", 4),
              rep("Goats", 4), rep("Pigs", 3)),
  animal_class = c(
    rep("Cows", 3), "Adult males", rep("Young stock", 4),
    "Cows", "Adult males", rep("Young stock", 2),
    "Ewes", "Breeding rams", "Fattening rams", "Lambs",
    "Does", "Breeding bucks", "Fattening bucks", "Kids",
    "Lactating/pregnant sows", "Dry sows/boars", "Growers"
  ),
  ipcc_ef_category_t2 = c(
    rep("Dairy cows", 3), rep("Non-dairy", 5), "Dairy cows",
    rep("Non-dairy", 3), rep("Sheep", 4), rep("Goats", 4), rep("N/A", 3)
  ),
  ipcc_manure_ch4_category = c(
    rep("Dairy cattle", 3), rep("Other cattle", 5),
    rep("Buffalo", 4), rep("Sheep", 4), rep("Goats", 4), rep("Swine", 3)
  ),
  ipcc_n_loss_category = c(
    rep("Dairy Cow", 3), rep("Other Cattle", 5),
    rep("Other animals", 12), rep("Swine", 3)
  ),
  ipcc_prp_category = c(
    rep("Cattle, pigs and poultry", 9), NA,
    rep("Cattle, pigs and poultry", 2),
    rep("Sheep and other animals", 8),
    rep("Cattle, pigs and poultry", 3)
  ),
  stringsAsFactors = FALSE
)

#' Add missing species, class and IPCC category columns to livestock rows
#'
#' Works for both the parameters database table (lkp_livetype) and the
#' livestock of a study object. Old column names are renamed, missing values
#' are filled from the defaults of the shipped livestock types (matched by
#' name), and the unused Tier 1 columns are dropped. Rows that cannot be
#' matched keep NA so the user can choose the categories.
#'
#' @param data A data frame or data.table with a `livetype_desc` column.
#' @return A data.frame with the category columns.
fill_livestock_categories <- function(data) {
  if (is.null(data)) return(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Old parameters databases name the Tier 2 enteric field differently
  if ("ipcc_meth_ef_t2" %in% names(data) && !"ipcc_ef_category_t2" %in% names(data)) {
    names(data)[names(data) == "ipcc_meth_ef_t2"] <- "ipcc_ef_category_t2"
  }
  data <- data[, setdiff(names(data), c(livetype_dropped_columns, "ipcc_meth_ef_t2")), drop = FALSE]

  legacy_idx <- match(data$livetype_desc, legacy_livestock_categories$livetype_desc)
  for (column in livetype_choice_columns) {
    values <- if (column %in% names(data)) as.character(data[[column]]) else rep(NA_character_, nrow(data))
    values[!is.na(values) & trimws(values) == ""] <- NA_character_
    fill <- is.na(values) & !is.na(legacy_idx)
    values[fill] <- legacy_livestock_categories[[column]][legacy_idx[fill]]
    data[[column]] <- values
  }
  data
}

#' Order the columns of the livestock types table
#'
#' Puts species and class right after the name and the IPCC categories last.
order_livetype_columns <- function(data) {
  first <- c("livetype_code", "livetype_desc", "species", "animal_class")
  last <- names(ipcc_livestock_categories)
  middle <- setdiff(names(data), c(first, last))
  data[, c(intersect(first, names(data)), middle, intersect(last, names(data))), drop = FALSE]
}

# Manure management systems ----------------------------------------------------
# Labels that do not match the IPCC tables used by CLEANED (no MCF/EF3 found)
manure_system_label_fixes <- c(
  "Pasture / range / paddock" = "Pasture/Range/Paddock",
  "Anaer digester, Low leak, HQ stor, HQ tec." = "Anaer digester, Low leak, HQ stor, HQ tec"
)

#' Replace manure system labels that CLEANED cannot match
normalise_manure_labels <- function(data, columns = manure_cols) {
  for (column in intersect(columns, names(data))) {
    values <- as.character(data[[column]])
    fix <- values %in% names(manure_system_label_fixes)
    values[fix] <- unname(manure_system_label_fixes[values[fix]])
    data[[column]] <- values
  }
  data
}

#' Remove duplicate or unmatched manure systems from lkp_manureman
migrate_lkp_manureman <- function(data) {
  if (is.null(data) || !"manureman_desc" %in% names(data)) return(data)
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  data$manureman_desc <- normalise_manure_labels(
    data.frame(x = data$manureman_desc, stringsAsFactors = FALSE), "x"
  )$x
  data[!duplicated(data$manureman_desc), , drop = FALSE]
}

#' Read one parameters database table, upgrading older table layouts
read_parameters_table <- function(path) {
  data <- data.table::fread(path)
  name <- tools::file_path_sans_ext(basename(path))
  if (name == "lkp_livetype") {
    data <- data.table::as.data.table(order_livetype_columns(fill_livestock_categories(data)))
  } else if (name == "lkp_manureman") {
    data <- data.table::as.data.table(migrate_lkp_manureman(data))
  }
  data
}

#' Check livestock types for missing or inconsistent categories
#'
#' @return Character vector of problems (empty when all rows are valid).
validate_livetype_categories <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(character(0))
  problems <- character(0)
  name <- ifelse(is.na(data$livetype_desc) | data$livetype_desc == "",
                 paste("code", data$livetype_code), data$livetype_desc)
  for (column in livetype_choice_columns) {
    values <- if (column %in% names(data)) as.character(data[[column]]) else rep(NA, nrow(data))
    allowed <- if (column == "animal_class") unique(unlist(livestock_species_classes)) else livetype_column_choices(column)
    bad <- is.na(values) | !values %in% allowed
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "<strong>%s</strong> is missing or invalid for: %s.",
        livetype_category_labels[[column]], paste(name[bad], collapse = ", ")
      ))
    }
  }
  if (all(c("species", "animal_class") %in% names(data))) {
    wrong <- !mapply(function(sp, cl) !is.na(sp) && cl %in% livestock_species_classes[[sp]],
                     as.character(data$species), as.character(data$animal_class))
    wrong <- wrong & !is.na(data$animal_class)
    if (any(wrong)) {
      problems <- c(problems, sprintf(
        "The <strong>animal class</strong> does not belong to the species for: %s.",
        paste(name[wrong], collapse = ", ")
      ))
    }
  }
  dup_names <- unique(name[duplicated(name)])
  if (length(dup_names) > 0) {
    problems <- c(problems, sprintf(
      "These livestock names are used more than once: %s. Please give each livestock type a unique name.",
      paste(dup_names, collapse = ", ")
    ))
  }
  problems
}
