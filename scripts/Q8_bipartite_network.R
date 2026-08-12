# =============================================================
# Q8: Bipartite Document-Token Network
# (requires dtms_final_matrix from Q3, token_network$group from Q7)
# =============================================================

library(igraph)

bipartite_matrix <- dtms_final_matrix
dim(bipartite_matrix)
rownames(bipartite_matrix)
colnames(bipartite_matrix)

bipartite_dtm_df <- as.data.frame(bipartite_matrix)
bipartite_dtm_df$Document <- rownames(bipartite_matrix)

# Reshape wide DTM into document-token-weight edge list
bipartite_edges <- data.frame()
for (i in 1:nrow(bipartite_dtm_df)) {
  for (j in 1:(ncol(bipartite_dtm_df) - 1)) {
    temp_row <- data.frame(
      Document = bipartite_dtm_df$Document[i],
      Token = colnames(bipartite_dtm_df)[j],
      Weight = bipartite_dtm_df[i, j]
    )
    bipartite_edges <- rbind(bipartite_edges, temp_row)
  }
}

bipartite_edges <- bipartite_edges[bipartite_edges$Weight != 0, ]
rownames(bipartite_edges) <- NULL
head(bipartite_edges, 20)
dim(bipartite_edges)

bipartite_network <- graph_from_data_frame(bipartite_edges, directed = FALSE)
bipartite_network
vcount(bipartite_network); ecount(bipartite_network)

bipartite_types <- bipartite_mapping(bipartite_network)$type
V(bipartite_network)$type <- bipartite_types

data.frame(Node = V(bipartite_network)$name, Type = V(bipartite_network)$type)

# ---- Basic bipartite plot ----
set.seed(123)
bipartite_layout <- layout_with_fr(bipartite_network)

pdf(file = "Q8_Basic_Bipartite_Network.pdf", height = 8, width = 10)
plot(bipartite_network, layout = bipartite_layout, vertex.label = V(bipartite_network)$name,
     vertex.label.cex = 0.6, vertex.size = 12,
     vertex.shape = ifelse(V(bipartite_network)$type, "circle", "square"),
     vertex.color = ifelse(V(bipartite_network)$type, "lightgreen", "pink"),
     edge.color = "grey80", edge.width = 1,
     main = "Basic Bipartite Network of Documents and Tokens")
legend("bottomleft", legend = c("Documents", "Tokens"), pch = c(22, 21),
       pt.bg = c("pink", "lightgreen"), pt.cex = 2, bty = "n", title = "Node Type")
dev.off()

# ---- Node attributes for improved plot ----
document_nodes <- rownames(bipartite_matrix)
token_nodes <- colnames(bipartite_matrix)

V(bipartite_network)$Node_Type <- ifelse(V(bipartite_network)$name %in% document_nodes, "Document", "Token")

V(bipartite_network)$Genre <- NA
V(bipartite_network)$Genre[V(bipartite_network)$Node_Type == "Document" & grepl("skincare", V(bipartite_network)$name)] <- "Skincare"
V(bipartite_network)$Genre[V(bipartite_network)$Node_Type == "Document" & grepl("sports", V(bipartite_network)$name)] <- "Sports"
V(bipartite_network)$Genre[V(bipartite_network)$Node_Type == "Document" & grepl("tech", V(bipartite_network)$name)] <- "Tech"

V(bipartite_network)$Token_Group <- NA
V(bipartite_network)$Token_Group[V(bipartite_network)$Node_Type == "Token" &
  V(bipartite_network)$name %in% c("skin", "product", "cleanser", "face", "dri", "spot")] <- "Skincare"
V(bipartite_network)$Token_Group[V(bipartite_network)$Node_Type == "Token" &
  V(bipartite_network)$name %in% c("goal", "shot", "final", "season", "score", "minut", "first")] <- "Sports"
V(bipartite_network)$Token_Group[V(bipartite_network)$Node_Type == "Token" &
  V(bipartite_network)$name %in% c("human", "learn", "system", "requir", "specif", "general")] <- "Tech"
V(bipartite_network)$Token_Group[V(bipartite_network)$Node_Type == "Token" & is.na(V(bipartite_network)$Token_Group)] <- "General"

V(bipartite_network)$Plot_Group <- ifelse(
  V(bipartite_network)$Node_Type == "Document", V(bipartite_network)$Genre, V(bipartite_network)$Token_Group
)

group_colours <- c("Skincare" = "pink", "Sports" = "gold", "Tech" = "skyblue", "General" = "grey80")
V(bipartite_network)$color <- group_colours[V(bipartite_network)$Plot_Group]
V(bipartite_network)$shape <- ifelse(V(bipartite_network)$Node_Type == "Document", "square", "circle")
V(bipartite_network)$size <- 7 + degree(bipartite_network) * 1.2
E(bipartite_network)$width <- E(bipartite_network)$Weight / max(E(bipartite_network)$Weight) * 6

set.seed(456)
bipartite_layout_clean <- layout_with_fr(bipartite_network, weights = E(bipartite_network)$Weight,
                                          niter = 3000, grid = "nogrid")
bipartite_layout_clean <- norm_coords(bipartite_layout_clean, xmin = -1.5, xmax = 1.5, ymin = -1.2, ymax = 1.2)

pdf(file = "Q8_Improved_Bipartite_Network.pdf", height = 10, width = 13)
par(mar = c(1, 1, 4, 1))
plot(bipartite_network, layout = bipartite_layout_clean, vertex.label = V(bipartite_network)$name,
     vertex.label.cex = 0.6, vertex.label.color = "black", vertex.label.dist = 0.45,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     vertex.color = V(bipartite_network)$color, vertex.frame.color = "black",
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     edge.curved = 0.05, main = "Improved Bipartite Network of Documents and Tokens")
legend("bottomleft", legend = c("Document", "Token"), pch = c(22, 21), pt.bg = "white",
       col = "black", pt.cex = 2, bty = "n", title = "Node Type")
legend("topright", legend = names(group_colours), pch = 21, pt.bg = group_colours,
       col = "black", pt.cex = 2, bty = "n", title = "Genre / Token Group")
dev.off()

# ---- Overall network measures ----
bipartite_network_measures <- data.frame(
  Measure = c("Number of nodes", "Number of edges", "Number of document nodes", "Number of token nodes",
              "Average path length", "Diameter", "Density", "Transitivity"),
  Value = c(
    vcount(bipartite_network), ecount(bipartite_network),
    sum(V(bipartite_network)$Node_Type == "Document"), sum(V(bipartite_network)$Node_Type == "Token"),
    mean_distance(bipartite_network, directed = FALSE, weights = NA),
    diameter(bipartite_network, directed = FALSE, weights = NA),
    edge_density(bipartite_network), transitivity(bipartite_network)
  )
)
bipartite_network_measures

# ---- Centrality ----
E(bipartite_network)$distance_weight <- 1 / E(bipartite_network)$Weight

bipartite_degree <- degree(bipartite_network)
bipartite_strength <- strength(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_betweenness <- betweenness(bipartite_network, directed = FALSE, weights = E(bipartite_network)$distance_weight)
bipartite_closeness <- closeness(bipartite_network, weights = E(bipartite_network)$distance_weight, normalized = TRUE)
bipartite_eigenvector <- eigen_centrality(bipartite_network, directed = FALSE, weights = E(bipartite_network)$Weight)$vector

bipartite_centrality_table <- data.frame(
  Node = V(bipartite_network)$name, Node_Type = V(bipartite_network)$Node_Type,
  Genre = V(bipartite_network)$Genre, Token_Group = V(bipartite_network)$Token_Group,
  Degree = bipartite_degree, Strength = bipartite_strength,
  Betweenness = bipartite_betweenness, Closeness = bipartite_closeness, Eigenvector = bipartite_eigenvector
)
bipartite_centrality_table

ranked_bipartite_centrality_table <- data.frame(
  Rank = 1:5,
  Ranked_by_Degree = bipartite_centrality_table$Node[order(-bipartite_centrality_table$Degree)][1:5],
  Ranked_by_Strength = bipartite_centrality_table$Node[order(-bipartite_centrality_table$Strength)][1:5],
  Ranked_by_Betweenness = bipartite_centrality_table$Node[order(-bipartite_centrality_table$Betweenness)][1:5],
  Ranked_by_Closeness = bipartite_centrality_table$Node[order(-bipartite_centrality_table$Closeness)][1:5],
  Ranked_by_Eigenvector = bipartite_centrality_table$Node[order(-bipartite_centrality_table$Eigenvector)][1:5]
)
ranked_bipartite_centrality_table

# ---- Community detection ----
bipartite_edge_betweenness_community <- cluster_edge_betweenness(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_fast_greedy_community <- cluster_fast_greedy(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_leading_eigen_community <- cluster_leading_eigen(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_label_prop_community <- cluster_label_prop(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_louvain_community <- cluster_louvain(bipartite_network, weights = E(bipartite_network)$Weight)
bipartite_optimal_community <- cluster_optimal(bipartite_network, weights = E(bipartite_network)$Weight)

pdf(file = "Q8_Overall_Bipartite_Community_Detection_Algorithms.pdf", height = 12, width = 12)
par(mfrow = c(3, 2), mar = c(1, 1, 4, 1))
plot(bipartite_edge_betweenness_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Edge Betweenness", cex.main = 1.6)
plot(bipartite_fast_greedy_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Fast Greedy", cex.main = 1.6)
plot(bipartite_leading_eigen_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Leading Eigenvector", cex.main = 1.6)
plot(bipartite_label_prop_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Label Propagation", cex.main = 1.6)
plot(bipartite_louvain_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Louvain", cex.main = 1.6)
plot(bipartite_optimal_community, bipartite_network, layout = bipartite_layout_clean, vertex.label = NA,
     vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     main = "Optimal", cex.main = 1.6)
dev.off()

bipartite_fast_greedy_membership <- membership(bipartite_fast_greedy_community)
bipartite_community_table <- data.frame(
  Node = V(bipartite_network)$name, Node_Type = V(bipartite_network)$Node_Type,
  Genre = V(bipartite_network)$Genre, Token_Group = V(bipartite_network)$Token_Group,
  Community = bipartite_fast_greedy_membership
)
bipartite_community_table

pdf(file = "Q8_Fast_Greedy_Bipartite_Community_Detection.pdf", height = 10, width = 13)
par(mar = c(1, 1, 5, 1))
plot(bipartite_fast_greedy_community, bipartite_network, layout = bipartite_layout_clean,
     vertex.label = V(bipartite_network)$name, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 0.55, vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     edge.curved = 0.05, main = "Fast Greedy Community Detection on Bipartite Network")
dev.off()

bipartite_optimal_membership <- membership(bipartite_optimal_community)
bipartite_optimal_table <- data.frame(
  Node = V(bipartite_network)$name, Node_Type = V(bipartite_network)$Node_Type,
  Genre = V(bipartite_network)$Genre, Token_Group = V(bipartite_network)$Token_Group,
  Community = bipartite_optimal_membership
)
bipartite_optimal_table

pdf(file = "Q8_Optimal_Bipartite_Community_Detection.pdf", height = 10, width = 13)
par(mar = c(1, 1, 5, 1))
plot(bipartite_optimal_community, bipartite_network, layout = bipartite_layout_clean,
     vertex.label = V(bipartite_network)$name, vertex.label.cex = 0.6, vertex.label.color = "black",
     vertex.label.dist = 0.55, vertex.size = V(bipartite_network)$size, vertex.shape = V(bipartite_network)$shape,
     edge.width = E(bipartite_network)$width, edge.color = adjustcolor("grey70", alpha.f = 0.45),
     edge.curved = 0.05, main = "Optimal Community Detection on Bipartite Network")
dev.off()
