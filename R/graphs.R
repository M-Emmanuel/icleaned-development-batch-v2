
wrap_labels <- function(labels, width) {
  # Split labels by spaces and insert line breaks if they exceed the width
  sapply(labels, function(x) {
    paste(strwrap(x, width = width), collapse = "\n")
  })
}

# Define the color palette
color_palette <- c(
  "#005275", "#94C859", "#593D23", "#E5D98B", "#80C5E2", "#009ADB",
  "#28B463", "#C70039", "#900C3F", "#581845", "#1F618D", "#FF5733",
  "#D68910", "#7D3C98", "#F4D03F", "#27AE60", "#2E86C1", "#AF7AC5",
  "#CD6155", "#2874A6"
)

#' Colours for n categories (feeds, seasons)
#'
#' Uses the iCLEANED palette for the first 20 categories and adds further
#' distinct colours when there are more, so plots work for any number of feeds.
#'
#' @param n Number of categories
#' @return Character vector of n hex colours
category_colors <- function(n) {
  if (n <= length(color_palette)) return(color_palette[seq_len(n)])
  c(color_palette, grDevices::hcl.colors(n - length(color_palette), palette = "Dark 3"))
}

# iCLEANED theme
cleaned_theme <- theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "#CDCDCD"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
    axis.line = element_line(colour = "#707070", linewidth = 0.5),
    axis.text.y = element_text(
      hjust = 1, # Align text to the right (for x-axis)
      face = "plain", # Normal (not bold or italic)
      size = 12, # Font size in points
      color = "#000000", # Text color
      lineheight = 1.2, # Line height (equivalent to 12px/10px)
      margin = margin(r = 15)
    ),
    axis.text.x = element_text(
      hjust = 1, # Align text to the right (for y-axis)
      face = "plain", # Normal (not bold or italic)
      size = 12, # Font size in points
      color = "#000000", # Text color
      lineheight = 1.2,
      margin = margin(t = 8)
    ),
  )

#' Build char plot of GHG emission
#' 
#' @param data_table data table to plot
plot_ghg_emissions <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$sources_and_sinks_formatted <- wrap_labels(
    data_table$sources_and_sinks, width = 10
  )
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$t_CO2e_per_ha, 2), "</span>", "<i> ", unit, " </i><br>",
    '<span class="text-center">', data_table$sources_and_sinks, "</span></div>"
  )
  
  # Adding interactive tooltips to the graph
  p <- data_table %>% 
    ggplot(aes(x = sources_and_sinks_formatted, y = t_CO2e_per_ha)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = 0.6
    ) + 
    labs(x = "", y = "", title = "") +
    cleaned_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(data_table$t_CO2e_per_ha >= 0)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    )
  } else {
    p <- p + geom_hline(
      yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
    ) # Dashed line at y = 0
  }
  
  if (png) {
    p <- p + labs(title = "GHG Emissions", y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}


#' Build char plot of N Balance
#' 
#' @param data_table data table to plot
plot_n_balance <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$feed_formatted <- wrap_labels(data_table$feed, width = 13)
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$nbalance_kg_n_total, 2), "</span>", "<i> ", unit,
    " </i> <br>", '<span class="text-center">', data_table$feed, "</span></div>"
  )
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(data_table$feed))
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.27  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  p <- data_table %>%
    ggplot2::ggplot(aes(x = feed_formatted, y = nbalance_kg_n_total)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = bar_width
    ) +
    geom_bar(stat = "identity", fill = "#005275", width = bar_width) +
    labs(x = "", y = "", title = "") +
    cleaned_theme +
    theme(
      axis.text.x = element_text(
        angle = 50, hjust = 1, vjust = 1, lineheight = 0.7
      )
    )
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(data_table$nbalance_kg_n_total >= 0)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    )
  } else {
    p <- p + geom_hline(
      yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
    ) # Dashed line at y = 0
  }
  
  # if PNG then add title and unit
  if (png) {
    p <- p + labs(title = "N Balance", y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}

#' Build char plot of land required
#' 
#' @param data_table data table to plot
plot_land_req <- function(data_table, unit, png = FALSE) {
  
  # Dynamically wrap x-axis labels
  data_table$feed_formatted <- wrap_labels(data_table$feed, width = 13)
  
  # Get unique season names
  unique_seasons <- unique(data_table$season_name)
  
  # Assign colors to each season based on the order of unique seasons
  season_colors <- setNames(
    category_colors(length(unique_seasons)), unique_seasons
  )
  
  data_table$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(data_table$area_feed_total, 2), "</span>", "<i> ", unit, " </i> <br>",
    '<span class="text-center">', data_table$feed, "</span> <br>",
    'Season: <span class="lead fw-bold text-center" style="font-size: 13px;">',
    data_table$season_name, "</span></div>"
  )
  
  # now I wnat to add the season name in the bar tooltip
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(data_table$feed))
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.27  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  p <- data_table %>%
    ggplot(aes(x = feed_formatted, y = area_feed_total, fill = season_name)) +
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", width = bar_width
    ) +
    labs(x = "", y = "", fill = "Seasons", title = "") +
    cleaned_theme +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 50, hjust = 1, vjust = 1, lineheight = 0.7
      )
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1))  # No space below, 10% space at top
    ) +
    scale_fill_manual(values = season_colors)  # Set bar colors
  
  # if PNG then add title, unit and legend
  if (png) {
    p <- p + labs(title = "Land Required", y = unit) + 
      theme(legend.position = "right")
  }
  
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}

#' Prepare the slices of the water-use-per-feed pie
#'
#' Feeds without water use are left out. Feeds below `min_share` of the total
#' are merged into one "Other feeds" slice (when there are at least two of
#' them), so the pie stays readable for any number of feeds. Used by both the
#' chart and its legend so they always match.
#'
#' @param data_table Water use per feed item (columns `feed`, `feed_water_use`)
#' @param min_share Smallest share shown as its own slice (default 2%)
#' @param other_label Label of the merged slice
#'
#' @return A data frame with one row per slice: `feed`, `feed_water_use`,
#'   `share`, `color`, `members` (feeds in the slice, with their shares).
water_feed_slices <- function(data_table, min_share = 0.02,
                              other_label = "Other feeds") {
  feeds <- unique(data_table$feed)
  use <- vapply(feeds, function(f) {
    sum(data_table$feed_water_use[data_table$feed == f], na.rm = TRUE)
  }, numeric(1))
  keep <- is.finite(use) & use > 0
  feeds <- feeds[keep]
  use <- use[keep]
  if (length(feeds) == 0) {
    return(data.frame(feed = character(0), feed_water_use = numeric(0),
                      share = numeric(0), color = character(0),
                      members = character(0), stringsAsFactors = FALSE))
  }
  # Largest feeds first, so the pie reads clockwise from the biggest slice
  ord <- order(-use)
  feeds <- feeds[ord]
  use <- use[ord]
  share <- use / sum(use)
  small <- share < min_share
  if (sum(small) < 2) small[] <- FALSE   # a single small feed keeps its own slice

  slices <- data.frame(
    feed = feeds[!small], feed_water_use = unname(use[!small]),
    share = unname(share[!small]), stringsAsFactors = FALSE
  )
  slices$color <- category_colors(nrow(slices))
  slices$members <- slices$feed
  if (any(small)) {
    ord <- order(-share[small])
    slices <- rbind(slices, data.frame(
      feed = other_label,
      feed_water_use = sum(use[small]),
      share = sum(share[small]),
      color = "#9E9E9E",
      members = paste0(feeds[small][ord], ": ",
                       format_percent(share[small][ord]), collapse = "<br>"),
      stringsAsFactors = FALSE
    ))
  }
  slices
}

# Percentages with one decimal (0.1% for very small non-zero shares)
format_percent <- function(x) {
  ifelse(x > 0 & x < 0.001, "<0.1%", sprintf("%.1f%%", 100 * x))
}

#' Build pie chart of water use per feed
#'
#' Slices are labelled outside the pie with a pointer line; small feeds are
#' merged into "Other feeds" (see `water_feed_slices()`), whose tooltip lists
#' them.
#'
#' @param data_table Water use per feed item
#' @param unit Unit shown in the tooltip
#' @param png TRUE for the downloadable image (title and legend added)
plot_water_feed <- function(data_table, unit, png = FALSE) {

  slices <- water_feed_slices(data_table)
  if (nrow(slices) == 0) {
    # No feed uses water: show an empty chart instead of an error
    return(ggiraph::girafe(ggobj = ggplot() + theme_void(), width_svg = 11, height_svg = 8))
  }
  total <- sum(slices$feed_water_use)

  # Slice positions: clockwise from 12 o'clock
  slices$ymax <- cumsum(slices$feed_water_use)
  slices$ymin <- slices$ymax - slices$feed_water_use
  slices$ymid <- (slices$ymin + slices$ymax) / 2
  angle <- 2 * pi * slices$ymid / total
  slices$hjust <- ifelse(sin(angle) >= 0, 0, 1)   # right half left-aligned

  # Labels sit outside the pie; consecutive narrow slices alternate between
  # two distances so their labels do not overlap
  narrow <- slices$share < 0.06
  run <- ave(narrow, cumsum(!narrow), FUN = cumsum)
  slices$label_x <- ifelse(narrow & run %% 2 == 0, 1.36, 1.16)
  slices$label <- format_percent(slices$share)

  slices$bar_tooltip <- paste0(
    '<div class="text-center px-1" style="font-size: 13px;">',
    '<span class="lead fw-bold text-center" style="font-size: 13px;">',
    slices$label, "</span> <i>", unit, "</i><br>",
    '<span class="text-center">',
    ifelse(slices$feed == slices$members, slices$feed,
           paste0("<b>", slices$feed, "</b><br>", slices$members)),
    "</span></div>"
  )

  slices$feed <- factor(slices$feed, levels = slices$feed)
  feed_colors <- setNames(slices$color, levels(slices$feed))

  p <- ggplot(slices) +
    geom_rect_interactive(
      aes(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, fill = feed,
          tooltip = bar_tooltip, data_id = feed),
      color = "white", linewidth = 0.3
    ) +
    # Pointer from the edge of each slice to its label
    geom_segment(
      aes(x = 1.01, xend = label_x - 0.04, y = ymid, yend = ymid),
      color = "#9E9E9E", linewidth = 0.3
    ) +
    geom_text_interactive(
      aes(x = label_x, y = ymid, label = label, hjust = hjust,
          tooltip = bar_tooltip, data_id = feed),
      color = "#333333",
      size = 5,
      family = "Merriweather",
      fontface = "bold"
    ) +
    coord_polar(theta = "y", start = 0, clip = "off") +
    scale_x_continuous(limits = c(0, 1.5)) +
    scale_fill_manual(values = feed_colors) +
    labs(x = "", y = "", title = "") +
    theme_void()

  # if PNG then add title, unit and legend
  if (png) {
    p <- p + labs(title = "Water Use per Feed", fill = NULL) +
      theme(legend.position = "bottom")
    p
  } else {
    p <- p + theme(
      legend.position = "none", # The legend is shown next to the chart
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 11,
    height_svg = 8
  )
}

#' Build char plot for scenario comparison
#' 
#' @param data_table data table to plot
plot_comparison <- function(
    datos, tt, y_title, title, unit, png = FALSE, relative = FALSE
) {
  
  # Check the number of bars (unique x-axis values)
  num_bars <- length(unique(datos[, 2]))
  
  datos$bar_tooltip <- paste(
    '<div class="text-center px-1" style="font-size: 13px;">
    <span class="lead fw-bold text-center" style="font-size: 13px;">', 
    round(datos[[tt]], 2), "</span> <i>",
    ifelse(relative, "%", unit),
    "</i> <br>",
    '<span class="text-center">', datos[["scenario"]], "</span></div>"
  )
  
  # Set bar width based on the number of bars
  bar_width <- if (num_bars == 1) {
    0.1  # Narrow width for 1 bar
  } else if (num_bars >= 2 && num_bars <= 5) {
    0.2  # Slightly wider for 2-5 bars
  } else if (num_bars >= 6 && num_bars <= 12) {
    0.5  # Moderate width for 6-12 bars
  } else {
    0.4  # Default width for 13-20 bars
  }
  
  # divide values by 100 if relative
  if (relative) {
    datos[[tt]] <- datos[[tt]] / 100
  }
  
  # Readable scenario names: underscores become spaces and long names wrap
  # onto several lines; the wrap width shrinks as the number of bars grows
  horizontal <- num_bars > 8
  wrap_width <- if (horizontal) 30 else max(8, floor(70 / max(num_bars, 1)))
  scenario_names <- unique(as.character(datos[["scenario"]]))
  scenario_labels <- setNames(
    wrap_labels(gsub("_", " ", scenario_names), width = wrap_width),
    scenario_names
  )
  datos$scenario_label <- factor(
    scenario_labels[as.character(datos[["scenario"]])],
    levels = unique(scenario_labels)
  )

  p <- ggplot(datos, aes(x = scenario_label, y = .data[[tt]])) + 
    geom_bar_interactive(
      aes(tooltip = bar_tooltip), 
      stat = "identity", fill = "#005275", width = bar_width
    ) +
    labs(x = "", y = "", title = "") +
    cleaned_theme

  if (horizontal) {
    # Many scenarios: horizontal bars, names on the left, first scenario on top
    p <- p +
      scale_x_discrete(limits = rev) +
      coord_flip() +
      theme(
        axis.text.y = element_text(hjust = 1, vjust = 0.5, margin = margin(r = 8)),
        axis.text.x = element_text(hjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#CDCDCD")
      )
  } else {
    p <- p + theme(axis.text.x = element_text(hjust = 0.5, vjust = 1, lineheight = 1))
  }
  
  # Conditionally apply scale_y_continuous if all values are non-negative
  if (all(datos[, tt] >= 0, na.rm = TRUE)) {
    p <- p + scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),  # No space below, 10% space at top
      labels = ifelse(relative, scales::percent, scales::comma)
    )
  } else {
    p <- p +
      geom_hline(
        yintercept = 0, linetype = "FF", color = "#707070", linewidth = 0.5
      ) + # Dashed line at y = 0
      scale_y_continuous(labels = ifelse(
        relative, scales::percent, scales::comma
      ))
  }
  
  # if plot is made for png, then add title and unit
  if (png) {
    p <- p + labs(title = title, y = unit)
  }
  
  # Create the interactive plot
  ggiraph::girafe(
    ggobj = p,
    options = list(
      ggiraph::opts_toolbar(
        hidden = c("selection", "zoom", "misc")
      ),
      ggiraph::opts_tooltip(
        use_fill = TRUE
      )
    ),
    width_svg = 7,
    height_svg = 7
  )
}
