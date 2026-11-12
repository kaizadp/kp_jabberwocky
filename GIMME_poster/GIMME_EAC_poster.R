library(tidyverse)

# BRAM GRAPH --------------------------------------------------------------

# Approximate redraw of the updated GIMME network
# Creates:
#   gimme_network_updated.png
#   gimme_network_updated.svg


library(ggplot2)
library(grid)


# Nodes

nodes <- data.frame(
  id = paste0("PP_", sprintf("%04d", 1:30)),
  
  x = c(
    39.5, 45.8, 52.7, 60.2, 40.2, 46.5, 55.8, 66.0, 71.5,
    33.0, 29.5, 24.2, 32.0, 39.8, 48.0, 57.5, 66.3, 72.5,
    36.3, 23.7, 31.5, 27.7, 51.8, 46.8, 57.8, 62.5, 38.7, 49.0,
    57.5, 63.5
  ),
  
  y = c(
    86.5, 87.8, 86.8, 83.5, 76.0, 78.2, 76.2, 77.0, 70.0,
    80.0, 69.0, 58.5, 54.0, 60.5, 67.0, 67.0, 63.0, 55.0,
    45.0, 37.0, 36.0, 23.0, 50.0, 32.5, 36.5, 36.0, 19.0, 16.5,
    13.5, 21.5
  ),
  
  cluster = c(
    "Glucose metabolism",
    "Urease operon",
    "Urease operon",
    "Urease operon",
    "Urease operon",
    "Urease operon",
    "Urease operon",
    "Unknown",
    "Glucose metabolism",
    
    "ETF complex",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    "ATP synthase",
    
    "Urease operon",
    "Urease operon",
    "ETF complex",
    "ETF complex",
    
    "Co-fit neighbor",
    "Glucose metabolism",
    "Glucose metabolism",
    "Unknown",
    "Co-fit neighbor",
    "Co-fit neighbor",
    "Glucose metabolism",
    "ATP synthase"
  ),
  
  stringsAsFactors = FALSE
)

# Approximate labels. Replace these with confirmed IDs if available.
nodes$label <- nodes$id


# Approximate directed edges

edges <- data.frame(
  from = c(
    "PP_0004", "PP_0003",
    "PP_0010", "PP_0011", "PP_0012", "PP_0013",
    "PP_0014", "PP_0015", "PP_0016", "PP_0017",
    "PP_0020", "PP_0020", "PP_0021",
    "PP_0023", "PP_0023", "PP_0023",
    "PP_0024", "PP_0024", "PP_0024", "PP_0024",
    "PP_0025", "PP_0025", "PP_0025",
    "PP_0026", "PP_0026", "PP_0026",
    "PP_0027", "PP_0027",
    "PP_0028", "PP_0028",
    "PP_0029", "PP_0029",
    "PP_0030", "PP_0030"
  ),
  
  to = c(
    "PP_0003", "PP_0002",
    "PP_0011", "PP_0012", "PP_0013", "PP_0014",
    "PP_0015", "PP_0016", "PP_0017", "PP_0018",
    "PP_0021", "PP_0022", "PP_0022",
    "PP_0024", "PP_0025", "PP_0026",
    "PP_0025", "PP_0026", "PP_0028", "PP_0030",
    "PP_0026", "PP_0028", "PP_0030",
    "PP_0028", "PP_0029", "PP_0030",
    "PP_0024", "PP_0025",
    "PP_0024", "PP_0026",
    "PP_0025", "PP_0028",
    "PP_0026", "PP_0029"
  ),
  
  stringsAsFactors = FALSE
)

# Add coordinates to edges
edges <- merge(
  edges,
  nodes[, c("id", "x", "y")],
  by.x = "from",
  by.y = "id",
  all.x = TRUE,
  sort = FALSE
)

names(edges)[names(edges) %in% c("x", "y")] <- c("x_from", "y_from")

edges <- merge(
  edges,
  nodes[, c("id", "x", "y")],
  by.x = "to",
  by.y = "id",
  all.x = TRUE,
  sort = FALSE
)

names(edges)[names(edges) %in% c("x", "y")] <- c("x_to", "y_to")


# Colors

cluster_colors <- c(
  "ATP synthase"       = "#F2C63D",
  "Co-fit neighbor"    = "#A86FA0",
  "ETF complex"        = "#4E9E4F",
  "Glucose metabolism" = "#F58220",
  "Unknown"            = "#F58220",
  "Urease operon"      = "#68B3B0"
)


# Plot

#p <- 
ggplot() +
  
  geom_segment(data = edges,
               aes(x = x_from, y = y_from, xend = x_to, yend = y_to),
               # colour = "#A6A6A6",
               linewidth = 1, alpha = 1, lineend = "round",
               arrow = arrow(length = unit(0.12, "cm"), type = "closed")) +
  
  geom_point(data = nodes,
             aes(x = x, y = y, fill = cluster),
             shape = 21, colour = "black", stroke = 1, size = 10) +
  
  #  geom_text(
  #    data = nodes,
  #    aes(
  #      x = x,
  #      y = y - 2.5,
  #      label = label
  #    ),
  #    colour = "#9A9A9A",
  #    size = 2.0,
  #    lineheight = 0.75
  #  ) +
  
  scale_fill_manual(
    values = cluster_colors,
    breaks = c(
      "ATP synthase",
      "Co-fit neighbor",
      "ETF complex",
      "Glucose metabolism",
      "Unknown",
      "Urease operon"
    ),
    drop = FALSE
  ) +
  
  scale_x_continuous(
    limits = c(5, 82),
    expand = c(0, 0)
  ) +
  
  
  labs(
    title = "Co-fitness Network of 27 P. putida Genes Across 5 Functional Clusters",
    fill = NULL) +
  
  theme_void(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    legend.position = c(0, 0.7),
    legend.direction = "vertical",
    legend.justification = "left",
    legend.text = element_text(size = 9.5),
    legend.key = element_blank(),
    plot.margin = margin(8, 12, 8, 12)
  )


# Save files

ggsave(
  filename = "gimme_network_updated.png",
  plot = p,
  width = 12.8,
  height = 8.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "gimme_network_updated.svg",
  plot = p,
  width = 12.8,
  height = 8.5,
  units = "in",
  bg = "white"
)

print(p)



# SNEHA GRAPH -------------------------------------------------------------


# Compact redraw: Phosphatase Gene Family Network
# Outputs PNG and SVG in the current working directory.

nodes <- read.table(text = '
id|label|group|x|y
EC3619|EC 3.6.1.9\\nNTP Pyrophosphatase|EC|50|88
PP1657|PP_1657\\nNTP Pyrophosphohydrolase|Putida|38|77
P11_2526|P11NB4_2526\\nDiadenosine Tetraphosphatase|fluorescens|70|84
GFF5040|GFF5040\\nDiadenosine Tetraphosphatase|psRCH2|84|84
Psvr0624|Psvr_0624\\nInorg. Pyrophosphatase|Syringae|60|70
EC3617|EC 3.6.1.7\\nInorg. Pyrophosphatase|fluorescens|47|58
PP5146|PP_5146\\nRNA Pyrophosphohydrolase|Putida|32|52
PP0658|PP_0658\\nInorg. Pyrophosphatase|Putida|50|43
P11_2068|P11NB4_2068\\nInorg. Pyrophosphatase|WCS417|39|39
Psvr1481|Psvr_1481\\nInorg. Pyrophosphatase|Syringae|69|40
PP0309|PP_0309\\nDiadenosine Tetraphosphatase|Putida|80|72
Psvr4029|Psvr_4029\\nDiadenosine Tetraphosphatase|Syringae|93|55
EC3641|EC 3.6.1.41\\nInorganic Pyrophosphatase|EC|63|27
GFF5051|GFF5051\\nInorg. Pyrophosphatase|WCS417|76|27
GFF4858|GFF4858\\nInorg. Pyrophosphatase|Syringae|63|18
P11_1442|P11NB4_1442\\ncAMP Phosphodiesterase|fluorescens|39|18
Psvr0649|Psvr_0649\\ncAMP Phosphodiesterase|Syringae|51|11
', sep = "|", header = TRUE, stringsAsFactors = FALSE)

edges <- read.table(text = '
from|to
PP1657|EC3619
P11_2526|PP0309
GFF5040|PP0309
Psvr0624|PP0658
EC3617|PP0658
P11_2068|PP0658
Psvr1481|PP0658
Psvr4029|PP0309
PP0309|PP0658
PP0309|EC3641
PP0309|GFF5051
EC3641|PP0658
GFF5051|PP0658
PP0658|EC3617
PP0658|Psvr1481
PP0658|Psvr0649
PP0658|GFF4858
P11_1442|Psvr0649
Psvr0649|GFF4858
GFF4858|EC3641
', sep = "|", header = TRUE, stringsAsFactors = FALSE)

edges$x  <- nodes$x[match(edges$from, nodes$id)]
edges$y  <- nodes$y[match(edges$from, nodes$id)]
edges$x2 <- nodes$x[match(edges$to, nodes$id)]
edges$y2 <- nodes$y[match(edges$to, nodes$id)]

cols <- c(
  EC = "#A66D9F", Putida = "#F58220", Syringae = "#E65355",
  WCS417 = "#69B3B0", fluorescens = "#F0C43E", psRCH2 = "#69AFAE"
)

ggplot() +
  geom_segment(
    data = edges,
    aes(x = x, y = y, xend = x2, yend = y2),
    linewidth = 1, alpha = 1,
    arrow = arrow(length = unit(.13, "cm"), type = "closed")
  ) +
  geom_point(
    data = nodes,
    aes(x, y, fill = group),
    shape = 21, colour = "black", stroke = 1, size = 10
  ) +
#  geom_text(
#    data = nodes,
#    aes(x, y - 3.2, label = label),
#    size = 2.25, colour = "#555555", lineheight = .8
#  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(15, 101), expand = c(0, 0)) +
  scale_y_continuous(limits = c(5, 96), expand = c(0, 0)) +
  labs(title = "Phosphatase Gene Family Network (Pseudomonas KG)", fill = NULL) +
  theme_void(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0, size = 16, face = "bold"),
    legend.position = c(0, 0.7),
    legend.direction = "vertical",
    legend.justification = "left",
    legend.text = element_text(size = 9.5),
    legend.key = element_blank(),
    plot.margin = margin(8, 12, 8, 12)
  )

ggsave("phosphatase_network.png", p, width = 12, height = 8.5,
       units = "in", dpi = 600, bg = "white")

ggsave("phosphatase_network.svg", p, width = 12, height = 8.5,
       units = "in", bg = "white")

print(p)

  
  

# HEATMAP -----------------------------------------------------------------

# Fitness heatmap from the digitized table
# Rows = genes; columns = media; fill = fitness
# Outputs: fitness_heatmap.png and fitness_heatmap.pdf


library(ggplot2)

# Exact media names and order
media <- c(
  "Propandiamine (N)",
  "L-leucine (N)",
  "D-glucose (C)",
  "M9-glucose Fermentation",
  "Urea (N)",
  "NH4Cl (N)",
  "Nitrate (N)",
  "Myristic (C)",
  "Oleic (C)",
  "Acetate (C)",
  "4-Guanidobutyric (C)",
  "Adenine (N)"
)

# Exact gene names and order
genes <- c(
  "PP_1011 (glk)",
  "PP_1015 (unknown)",
  "PP_1016 (ABC-bind)",
  "PP_1017 (ABC-bind)",
  "PP_1018 (ABC-ATPase)"
#  "PP_1023 (pgl)",
#  "PP_1689 (unknown)",
#  "PP_2842 (ureD)",
#  "PP_2843 (ureA)",
#  "PP_2844 (ureB)",
#  "PP_2845 (ureC)",
#  "PP_2846 (ureE)",
#  "PP_2847 (ureF)",
#  "PP_2848 (ureF2)",
#  "PP_2849 (ureG)"
)

# Approximate digitized fitness values.
# Rows correspond to genes; columns correspond to media.
fitness <- matrix(
  c(
    -3.50,-2.05,-1.55,-1.80,-0.60,-0.80,-0.80,NA,NA,NA,NA,-0.50,
    -3.30,-1.45,-0.90,-1.20,NA,NA,NA,NA,NA,NA,-0.50,-0.65,
    -3.20,-1.35,-0.80,-1.15,NA,-0.40,-0.45,NA,NA,NA,NA,-0.60,
    -3.40,-1.50,-1.00,-1.65,NA,-0.45,NA,NA,-0.40,NA,NA,NA,
    -2.70,-1.45,-0.85,-1.30,NA,NA,NA,NA,NA,-2.45,NA,NA,
    -3.40,-1.55,-0.80,-1.75,-0.70,NA,-0.20,NA,NA,NA,-0.85,-0.80,
    -1.00,-0.35,-0.45,-0.25,-0.20,-0.20,NA,-1.30,-0.70,NA,NA,-0.55,
    -3.50,-0.20,-0.25,-0.35,NA,-0.35,NA,NA,NA,NA,NA,NA,
    -3.45,-0.25,-0.30,-0.25,NA,NA,NA,NA,NA,NA,NA,NA,
    -3.35,-0.25,-0.25,-0.25,NA,NA,NA,NA,NA,NA,NA,NA,
    -3.40,-0.25,-0.25,-0.25,NA,NA,NA,NA,NA,NA,NA,NA,
    -3.45,-0.25,-0.25,-0.25,NA,NA,NA,NA,NA,NA,NA,NA,
    -3.40,-0.30,-0.30,-0.25,NA,NA,NA,NA,NA,-2.40,NA,NA,
    -3.35,-0.25,-0.25,-0.25,NA,NA,NA,NA,NA,-1.00,NA,NA,
    -3.40,-0.25,-0.25,-0.25,NA,NA,NA,NA,NA,-1.55,NA,-0.55
  ),
  nrow = length(genes),
  ncol = length(media),
  byrow = TRUE,
  dimnames = list(genes, media)
)

# Convert matrix to long format for ggplot
heatmap_data <- expand.grid(
  Gene = genes,
  Media = media,
  stringsAsFactors = FALSE
)

heatmap_data$Fitness <- as.vector(fitness)

heatmap_data$Gene <- factor(
  heatmap_data$Gene,
  levels = rev(genes)
)

heatmap_data$Media <- factor(
  heatmap_data$Media,
  levels = media
)

# Create heatmap
p = ggplot(
  heatmap_data,
  aes(x = Media, y = Gene, fill = Fitness)
) +
  geom_tile(
    colour = "white",
    linewidth = 0.8
  ) +
  scale_fill_gradientn(
    colours = PNWColors::pnw_palette("Bay"), 
    limits = c(-3.6, 0),
    breaks = c(-3.5, -3, -2, -1, 0),
    na.value = "grey90",
    name = "Fitness"
  ) +
  scale_x_discrete(
    labels = media
  ) +
  labs(
 #   title = "Fitness of Pseudomonas putida Genes Across Media",
    x = NULL,
    y = NULL
  ) +
  
# theme_minimal(base_family = "Arial") +
# theme(
#   plot.title = element_text(hjust = 0, size = 16, face = "bold"),
#   legend.position = "right",
#   legend.direction = "vertical",
#   legend.justification = "left",
#   legend.text = element_text(size = 9.5),
#   legend.key = element_blank(),
#   plot.margin = margin(8, 12, 8, 12)
# )
#
  theme_minimal(base_family = "Arial") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    panel.grid = element_blank(),
    legend.position = "right"  )

# Display plot

# Save high-resolution PNG
ggsave(
  "fitness_heatmap.png",
  plot = p,
  width = 16,
  height = 8.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

# Save vector PDF
ggsave(
  "fitness_heatmap.pdf",
  plot = p,
  width = 14,
  height = 8.5,
  units = "in",
  bg = "white"
)  


#

# BRC plot 1 --------------------------------------------------------------

# Standalone recreation of the Module A / Module B gene network
# Outputs: module_network.png and module_network.pdf

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

set.seed(42)


# Gene positions

nodes <- rbind(
  data.frame(
    gene = c(
      "pcaG", "pcaB", "ics", "pcaK", "vanA", "frmA", "pobR",
      "vdh", "pcaH", "pobA", "vanB", "ech", "pcaC", "pcaQ"
    ),
    module = "Module A",
    x = c(0.0, -1.45, -1.00, -1.25, -0.35, 0.05, 0.95,
          1.15, 1.05, 0.10, -0.30, 0.55, 1.65, 0.35),
    y = c(1.55, 0.85, 0.25, -0.65, -0.45, -1.35, -1.25,
          -0.55, 0.05, 0.40, 0.10, 0.75, 0.55, 1.45)
  ),
  data.frame(
    gene = c(
      "pcaD", "pcaJ", "catB", "benB", "benC", "benA",
      "catC", "catR", "benR", "pcaR", "pcaI", "pcaF"
    ),
    module = "Module B",
    x = c(7.00, 5.50, 5.95, 5.75, 6.85, 7.25,
          8.25, 5.45, 6.40, 6.25, 7.55, 8.45),
    y = c(1.55, 0.85, 1.15, 0.15, 0.60, 0.05,
          0.25, -0.55, -0.85, -1.25, -1.05, 0.90)
  )
)


# Create dense within-module edges

make_edges <- function(d, density, seed = 42) {
  pairs <- t(combn(d$gene, 2))
  edges <- data.frame(
    from = pairs[, 1],
    to = pairs[, 2],
    stringsAsFactors = FALSE
  )
  
  keep_n <- round(nrow(edges) * density)
  
  set.seed(seed)
  edges <- edges[sample(seq_len(nrow(edges)), keep_n), ]
  
  edges$x    <- d$x[match(edges$from, d$gene)]
  edges$y    <- d$y[match(edges$from, d$gene)]
  edges$xend <- d$x[match(edges$to, d$gene)]
  edges$yend <- d$y[match(edges$to, d$gene)]
  edges
}

edges_a <- make_edges(
  subset(nodes, module == "Module A"),
  density = 0.86,
  seed = 10
)

edges_b <- make_edges(
  subset(nodes, module == "Module B"),
  density = 1.00,
  seed = 20
)

edges <- rbind(edges_a, edges_b)


# Circular module boundaries

circle <- function(cx, cy, r, n = 300) {
  data.frame(
    x = cx + r * cos(seq(0, 2 * pi, length.out = n)),
    y = cy + r * sin(seq(0, 2 * pi, length.out = n))
  )
}

circle_a <- circle(cx = 0, cy = 0.1, r = 2.25)
circle_b <- circle(cx = 6.9, cy = 0.1, r = 2.25)


# Node colors

nodes$node_color <- ifelse(
  nodes$gene %in% c(
    "pcaB", "pcaC", "pcaD", "pcaJ", "pcaF", "pcaI", "pcaR"
  ),
  "#B27A00",
  ifelse(nodes$module == "Module A", "#247D74", "#87498D")
)


# Plot

p <- ggplot() +
  
  # Module backgrounds
  annotate(
    "polygon",
    x = circle_a$x,
    y = circle_a$y,
    fill = "#EEF5F4",
    colour = NA
  ) +
  
  annotate(
    "polygon",
    x = circle_b$x,
    y = circle_b$y,
    fill = "#F7F1F8",
    colour = NA
  ) +
  
  # Module boundaries
  geom_path(
    data = circle_a,
    aes(x, y),
    colour = "#A8D1CD",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  geom_path(
    data = circle_b,
    aes(x, y),
    colour = "#D2A9D5",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  # Dense network edges
  geom_segment(
    data = edges,
    aes(x = x, y = y, xend = xend, yend = yend),
    colour = "#A8AAA8",
    linewidth = 0.28,
    alpha = 0.38
  ) +
  
  # Module titles
  annotate(
    "text",
    x = 0, y = 2.72,
    label = "Module A",
    colour = "#247D74",
    fontface = "bold",
    size = 8
  ) +
  
  annotate(
    "text",
    x = 0, y = 2.38,
    label = "protocatechuate / methoxy arm",
    colour = "#666666",
    size = 4.2
  ) +
  
  annotate(
    "text",
    x = 6.9, y = 2.72,
    label = "Module B",
    colour = "#87498D",
    fontface = "bold",
    size = 6
  ) +
  
  annotate(
    "text",
    x = 6.9, y = 2.38,
    label = "benzoate + shared confluence core",
    colour = "#666666",
    size = 4.2
  ) +
  
  # Central separator
  annotate(
    "segment",
    x = 3.45, xend = 3.45,
    y = -2.05, yend = 2.35,
    colour = "#B8AA96",
    linetype = "dotted",
    linewidth = 0.55
  ) +
  
  annotate(
    "text",
    x = 3.45, y = 2.55,
    label = "the benzoate seam",
    colour = "#9B907F",
    family = "mono",
    size = 3.8
  ) +
  
  # Nodes
  geom_point(
    data = nodes,
    aes(x, y, colour = node_color),
    shape = 21,
    fill = "#FFFFFF",
    size = 7,
    stroke = 1.2
  ) +
  
  # Gene labels
  geom_text(
    data = nodes,
    aes(x, y - 0.27, label = gene, colour = node_color),
    fontface = "italic",
    size = 3.4,
    show.legend = FALSE
  ) +
  
  scale_colour_identity() +
  
#  # Module summaries
#  annotate(
#    "text",
#    x = 0, y = -2.38,
#    label = "14 genes  •  density 0.86  •  benzoate-dispensable",
#    colour = "#9B907F",
#    family = "mono",
#    size = 3.8
#  ) +
#  
#  annotate(
#    "text",
#    x = 6.9, y = -2.38,
#    label = "12 genes  •  density 1.00  •  benzoate-required",
#    colour = "#9B907F",
#    family = "mono",
#    size = 3.8
#  ) +
#  
#  coord_equal(
#    xlim = c(-2.7, 9.6),
#    ylim = c(-2.75, 3.0),
#    expand = FALSE
#  ) +
#  
#  labs(
#    title = NULL,
#    x = NULL,
#    y = NULL
#  ) +
  
  theme_void(base_family = "Arial") +
  
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    plot.margin = margin(18, 18, 18, 18)
  )

print(p)


# Export

ggsave(
  "module_A_B_gene_network.png",
  plot = p,
  width = 14,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  "module_A_B_gene_network.pdf",
  plot = p,
  width = 14,
  height = 7,
  units = "in",
  bg = "white"
)

#
#




# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Separate Module A and Module B gene networks
# No benzoate seam or dashed center line
# Nodes: black outline with 20% transparent color fill
# Outputs:
#   module_A_network.png
#   module_B_network.png
#   module_A_B_networks.png
#   module_A_B_networks.pdf

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

library(ggplot2)
library(patchwork)

# ------------------------------------------------------------
# Module A nodes
# ------------------------------------------------------------

module_a <- data.frame(
  gene = c(
    "pcaG", "pcaB", "ics", "pcaK", "vanA", "frmA", "pobR",
    "vdh", "pcaH", "pobA", "vanB", "ech", "pcaC", "pcaQ"
  ),
  x = c(
    -0.55, -1.45, -1.05, -1.35, -0.45, 0.00, 0.90,
    1.10,  1.00,  0.05, -0.30, 0.55, 1.55, 0.35
  ),
  y = c(
    1.45,  0.85,  0.25, -0.65, -0.45, -1.35, -1.20,
    -0.55,  0.05,  0.40,  0.10, 0.75, 0.55, 1.35
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Module B nodes
# ------------------------------------------------------------

module_b <- data.frame(
  gene = c(
    "pcaD", "pcaJ", "catB", "benB", "benC", "benA",
    "catC", "catR", "benR", "pcaR", "pcaI", "pcaF"
  ),
  x = c(
    0.10, -1.35, -0.90, -1.05, 0.05, 0.45,
    1.45, -1.40, -0.45, -0.60, 0.75, 1.65
  ),
  y = c(
    1.55,  0.85,  1.15,  0.15, 0.60, 0.05,
    0.25, -0.55, -0.85, -1.25, -1.05, 0.90
  ),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Node colors
# ------------------------------------------------------------

module_a$node_colour <- ifelse(
  module_a$gene %in% c("pcaB", "pcaC"),
  "#B27A00",
  "#247D74"
)

module_b$node_colour <- ifelse(
  module_b$gene %in% c("pcaD", "pcaF", "pcaI", "pcaJ", "pcaR"),
  "#B27A00",
  "#87498D"
)

# ------------------------------------------------------------
# Create approximate within-module edges
# ------------------------------------------------------------

make_edges <- function(nodes, density = 1, seed = 1) {
  all_pairs <- t(combn(nodes$gene, 2))
  
  edges <- data.frame(
    from = all_pairs[, 1],
    to = all_pairs[, 2],
    stringsAsFactors = FALSE
  )
  
  if (density < 1) {
    set.seed(seed)
    keep <- runif(nrow(edges)) <= density
    edges <- edges[keep, ]
  }
  
  edges$x1 <- nodes$x[match(edges$from, nodes$gene)]
  edges$y1 <- nodes$y[match(edges$from, nodes$gene)]
  edges$x2 <- nodes$x[match(edges$to, nodes$gene)]
  edges$y2 <- nodes$y[match(edges$to, nodes$gene)]
  
  edges
}

edges_a <- make_edges(
  nodes = module_a,
  density = 0.86,
  seed = 10
)

edges_b <- make_edges(
  nodes = module_b,
  density = 1,
  seed = 20
)

# ------------------------------------------------------------
# Create circular background
# ------------------------------------------------------------

make_background <- function(fill_colour, radius = 2.0) {
  theta <- seq(0, 2 * pi, length.out = 400)
  
  data.frame(
    x = radius * cos(theta),
    y = radius * sin(theta),
    fill_colour = fill_colour
  )
}

background_a <- make_background("#EEF6F5")
background_b <- make_background("#F8F2F8")

# ------------------------------------------------------------
# Plot function
# ------------------------------------------------------------

make_network_plot <- function(
    nodes,
    edges,
    background,
    title,
    subtitle,
    title_colour
) {
  ggplot() +
    
    # Module-specific pale background
    geom_polygon(
      data = background,
      aes(x = x, y = y),
      fill = background$fill_colour[1],
      colour = NA
    ) +
    
    # Network edges
    geom_segment(
      data = edges,
      aes(
        x = x1,
        y = y1,
        xend = x2,
        yend = y2
      ),
      colour = "#A9ADAC",
      linewidth = 0.35,
      alpha = 0.42
    ) +
    
    # Nodes: black outline and 20% transparent color fill
    geom_point(
      data = nodes,
      aes(
        x = x,
        y = y,
        fill = grDevices::adjustcolor(
          node_colour,
          alpha.f = 1
        )
      ),
      shape = 21,
      colour = "black",
      size = 7,
      stroke = 1.2,
      show.legend = FALSE
    ) +
    
    scale_fill_identity() +
    
    # Gene labels
    geom_text(
      data = nodes,
      aes(
        x = x,
        y = y - 0.28,
        label = gene,
        colour = node_colour
      ),
      fontface = "bold.italic",
      size = 5,
      show.legend = FALSE
    ) +
    
    scale_colour_identity() +
    
    labs(
      title = title,
      subtitle = subtitle
    ) +
    
    coord_equal(
      xlim = c(-2.15, 2.15),
      ylim = c(-2.05, 2.35),
      expand = FALSE
    ) +
    
    theme_void(base_family = "Arial") +
    
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 18,
        colour = title_colour,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 11,
        colour = "#666666",
        margin = margin(b = 12)
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(18, 18, 18, 18)
    )
}

# ------------------------------------------------------------
# Generate separate graphs
# ------------------------------------------------------------

plot_a <- make_network_plot(
  nodes = module_a,
  edges = edges_a,
  background = background_a,
  title = "Module A",
  subtitle = "protocatechuate / methoxy arm",
  title_colour = "#247D74"
)

plot_b <- make_network_plot(
  nodes = module_b,
  edges = edges_b,
  background = background_b,
  title = "Module B",
  subtitle = "benzoate + shared confluence core",
  title_colour = "#87498D"
)

# Display separately
print(plot_a)
print(plot_b)

# ------------------------------------------------------------
# Save each module separately
# ------------------------------------------------------------

ggsave(
  filename = "module_A_network.png",
  plot = plot_a,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "module_B_network.png",
  plot = plot_b,
  width = 7,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

# ------------------------------------------------------------
# Save side-by-side version
# ------------------------------------------------------------

combined_plot <- plot_a | plot_b

print(combined_plot)

ggsave(
  filename = "module_A_B_networks.png",
  plot = combined_plot,
  width = 14,
  height = 7,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = "module_A_B_networks.pdf",
  plot = combined_plot,
  width = 14,
  height = 7,
  units = "in",
  bg = "white"
)


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# Recreate the network-style figure
# install.packages(c("ggplot2", "dplyr", "tibble", "grid"))

library(ggplot2)
library(dplyr)
library(tibble)
library(grid)

# -----------------------------
# Colors
# -----------------------------
teal  <- "#2D827B"
teal2 <- "#8FC5BF"
gold  <- "#C58A17"
gold2 <- "#F5E5B1"
gray  <- "#A9A59A"
dark  <- "#504C43"

# -----------------------------
# Positions of small gene nodes
# -----------------------------
nodes <- tribble(
  ~name,   ~x,   ~y,   ~group,
  "pcaQ",  0.35, 4.05, "teal",
  "frmA",  1.10, 5.55, "teal",
  "pcaH",  2.05, 5.25, "teal",
  "pobA",  2.75, 5.72, "teal",
  "vanA",  3.25, 4.60, "teal",
  "pcsB",  0.05, 3.05, "teal",
  "pcaB",  1.40, 1.95, "teal",
  "ech",   2.35, 1.85, "teal",
  "pcaG",  2.75, 2.45, "teal",
  "vdh",   3.55, 3.02, "teal",
  
  "pcab",  1.10, 4.50, "gold",
  "pcaI",  1.25, 3.70, "gold",
  "pcaJ",  2.20, 3.58, "gold",
  "pcaD",  3.05, 3.78, "gold",
  "pcaC",  0.90, 2.82, "gold",
  "pcaR",  1.95, 2.72, "gold"
)

# -----------------------------
# Positions of target proteins
# -----------------------------
targets <- tribble(
  ~target,     ~x,   ~y,   ~group,
  "PP_1378",   7.95, 8.05, "gold",
  "PP_1615",   7.95, 4.75, "teal",
  "PP_3621–23",7.95, 1.72, "teal",
  "PP_3130",   7.95, 0.15, "gray"
)

# -----------------------------
# Connections
# -----------------------------
edges <- tribble(
  ~from,  ~target,      ~group, ~width, ~alpha,
  "pcab", "PP_1378",    "gold", 1.5,    0.75,
  "pcaI", "PP_1378",    "gold", 2.2,    0.82,
  "pcaJ", "PP_1378",    "gold", 1.5,    0.75,
  "pcaD", "PP_1378",    "gold", 1.2,    0.72,
  "pcaR", "PP_1378",    "gold", 1.0,    0.70,
  
  "pobA", "PP_1615",    "teal", 1.1,    0.70,
  "vanA", "PP_1615",    "teal", 1.5,    0.75,
  "pcaH", "PP_1615",    "teal", 0.9,    0.65,
  "pcaB", "PP_1615",    "teal", 1.1,    0.65,
  "pcaG", "PP_1615",    "teal", 0.8,    0.65,
  
  "frmA", "PP_3621–23", "teal", 0.7,    0.55,
  "pcaH", "PP_3621–23", "teal", 0.8,    0.55,
  "pcaB", "PP_3621–23", "teal", 0.7,    0.55,
  
  "pcaG", "PP_3130",    "gray",  0.7,    0.55,
  "pcaD", "PP_3130",    "gray",  0.7,    0.55
) %>%
  left_join(nodes %>% select(from = name, x, y),
            by = "from") %>%
  left_join(targets %>% select(target, xend = x, yend = y),
            by = "target")

# -----------------------------
# Dashed ellipse surrounding the seed genes
# -----------------------------
theta <- seq(0, 2 * pi, length.out = 500)

ellipse <- tibble(
  x = 1.65 + 2.35 * cos(theta),
  y = 3.95 + 2.65 * sin(theta)
)

# -----------------------------
# Target descriptions
# -----------------------------
target_text <- tribble(
  ~x,   ~y,   ~label,
  8.55, 8.18, "PP_1378",
  8.55, 7.88, "core β-ketoadipate transporter",
  8.55, 7.58, "13/17 seeds · r̄ 0.81 · peak 0.96",
  
  8.55, 4.93, "PP_1615",
  8.55, 4.63, "methoxy-arm co-regulator",
  8.55, 4.33, "6/17 seeds · r̄ 0.70 · peak 0.87",
  
  8.55, 1.90, "PP_3621–23",
  8.55, 1.60, "Mo aldehyde oxidoreductase",
  8.55, 1.30, "2–3/17 seeds · r̄ 0.60 · peak 0.64",
  
  8.55, 0.25, "PP_3130",
  8.55, -0.05, "periplasmic accessory (speculative)",
  8.55, -0.35, "2/17 seeds · r̄ 0.57"
)

# -----------------------------
# Plot
# -----------------------------
p <- ggplot() +
  
  # Background
  theme_minimal(base_size = 12) +
  
  # Dashed seed-region ellipse
  geom_polygon(
    data = ellipse,
    aes(x, y),
    fill = "#D6D0BE",
    color = "grey",
    linewidth = 0.8, alpha = 0.1,
    linetype = "dashed"
  ) +
  
  # Network edges
  geom_segment(
    data = edges,
    aes(
      x = x, y = y,
      xend = xend, yend = yend,
      color = group,
      linewidth = width,
      alpha = alpha
    ),
    lineend = "round"
  ) +
  
  # Small nodes
  geom_point(
    data = nodes,
    aes(x, y, fill = group, color = group),
    shape = 21,
    size = 7,
    stroke = 1.2
  ) +
  
  # Small node labels
  geom_text(
    data = nodes,
    aes(
      x = x,
      y = y - 0.20,
      label = name,
      color = group
    ),
    family = "mono",
    size = 5,
    fontface = "bold.italic"
  ) +
  
  # Large target nodes
  geom_point(
    data = targets,
    aes(x, y, fill = group, color = group),
    shape = 21,
    size = 12,
    stroke = 1.5
  ) +
  
  ##    # Target labels and descriptions
  ##    geom_text(
  ##      data = target_text,
  ##      aes(x, y, label = label),
  ##      hjust = 0,
  ##      family = "mono",
  ##      color = dark,
  ##      size = 3.5
  ##    ) +
  ##    
  ##    # Make target names larger and colored
  ##    geom_text(
  ##      data = target_text %>%
  ##        filter(label %in% c("PP_1378", "PP_1615", "PP_3621–23", "PP_3130")),
  ##      aes(x, y, label = label, color = ifelse(label == "PP_1378", "gold", "teal")),
  ##      hjust = 0,
  ##      family = "mono",
  ##      fontface = "bold",
  ##      size = 4.2
  ##    ) +
  ##    
  ##    # Figure title
  ##    annotate(
  ##      "text",
  ##      x = 0.55, y = 7.15,
  ##      label = "The confirmed funnel",
  ##      hjust = 0,
  ##      fontface = "bold",
  ##      color = dark,
  ##      size = 6
  ##    ) +
  ##    
  ##    annotate(
  ##      "text",
  ##      x = 0.55, y = 6.82,
  ##      label = "aromatic-specific seeds · Fig 4 · z < −3σ",
  ##      hjust = 0,
  ##      family = "mono",
  ##      color = "#8E897D",
  ##      size = 3.5
  ##    ) +
  ##    
  ##    # Center annotation
  ##    annotate(
  ##      "text",
  ##      x = 4.45, y = 3.95,
  ##      label = "co-fitness ≥ 0.55 → transferred role",
  ##      color = "#A59D8A",
  ##      family = "mono",
  ##      fontface = "italic",
  ##      size = 3.8
  ##    ) +
  ##    
  ##    # Information badges
  ##    annotate(
  ##      "label",
  ##      x = 8.55, y = 7.25,
  ##      label = "■ inside pca operon · +10 bp to pcaB",
  ##      hjust = 0,
  ##      family = "mono",
  ##      color = "#176C68",
  ##      fill = "#F3FBF9",
  ##      label.size = 0.5,
  ##      size = 3.2
  ##    ) +
  ##    
  ##    annotate(
  ##      "label",
  ##      x = 8.55, y = 4.02,
  ##      label = "■ divergent from frmA · 105 bp",
  ##      hjust = 0,
  ##      family = "mono",
  ##      color = "#176C68",
  ##      fill = "#F3FBF9",
  ##      label.size = 0.5,
  ##      size = 3.2
  ##    ) +
  
  # Scales
  scale_color_manual(
    values = c(teal = teal, gold = gold, gray = "#817B6E")
  ) +
  scale_fill_manual(
    values = c(teal = "#D6F0EC", gold = "#FFF3C9", gray = "#E9E6DC")
  ) +
  scale_linewidth_identity() +
  scale_alpha_identity() +
  
  coord_cartesian(
    xlim = c(-0.35, 12.3),
    ylim = c(-0.75, 8.55),
    clip = "off"
  ) +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 30, 15, 15)
  )

p

# Save high-resolution output
ggsave(
  "confirmed_funnel.png",
  p,
  width = 14,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)
