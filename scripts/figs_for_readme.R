# Script to create example plots and save them to images

# loading library
library(exploreARTIS)

# Load environment variables
load("R/sysdata.rda")

# Bar chart visualizing seafood trade volumes by exporter
p <- plot_bar(mini_artis, bar_group = "exporter_iso3c")
ggsave(filename = "images/all_trade_export_bar.png", p)


# Bar chart visualizing seafood trade volumes by exporter and filling by export source
p <- plot_bar(mini_artis, bar_group = "exporter_iso3c", fill_type = "dom_source")
ggsave(filename = "images/all_trade_export_dom_source_bar.png", p)


# Line graph of trade by exporter
p <- plot_ts(mini_artis, artis_var = "exporter_iso3c")
ggsave(filename = "images/line_all_trade.png", p)


# Area graph of trade by exporter
p <- plot_ts(mini_artis, artis_var = "exporter_iso3c", plot.type = "stacked")
ggsave(filename = "images/line_stacked_all_trade.png", p)


# Sankey plot of all seafood trade
p <- plot_sankey(mini_artis, cols = c("sciname", "exporter_iso3c", "importer_iso3c"))
ggsave(filename = "images/sankey_all_trade.png", p)


# Chord diagram of all seafood trade
png("images/all_trade_chord.png")
plot_chord(mini_artis, region_colors = region7_palette)
dev.off()

# Chord diagram of all seafood trade with individual country highlighted
png("images/all_trade_chord_vnm.png")
p <- plot_chord(mini_artis, focal_country = "VNM", region_colors = region7_palette)
dev.off()


# Map of global seafood exports 
p <- plot_map(mini_artis, country_fill = "importer_iso3c", flow_arrows = TRUE)
ggsave(filename = "images/all_map_flows.png", p)


# Map of seafood exports from Chile
p <- mini_artis %>%
  filter(exporter_iso3c == "CHL") %>%
  plot_map(country_fill = "importer_iso3c", flow_arrows = TRUE, 
           arrow_label = "Trade (live t)", fill_label = "Import (live t)")
ggsave(filename = "images/chl_map_flows.png", p)


# Area graph facetted by production type
p <- plot_ts(mini_artis, artis_var = "importer_iso3c", plot.type = "stacked", 
        facet_variable = "method", facet_values = c("capture", "aquaculture"))
ggsave(filename = "images/area_plot_facetted.png", p)

# Bar graph facetted by production type
p <- plot_bar(mini_artis, bar_group = "importer_iso3c", facet_variable = "method", facet_values = c("capture", "aquaculture"))
ggsave(filename = "images/bar_plot_facetted.png", p)


