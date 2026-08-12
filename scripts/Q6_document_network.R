# =============================================================
# Q6: Document Single-Mode Network
# (requires dtms_matrix from Q3, sentiment_qdap from Q5)
# =============================================================

library(igraph)

dim(dtms_matrix)
rownames(dtms_matrix)
colnames(dtms_matrix)

# Binary DTM (present/absent)
binary_dtms_matrix <- dtms_matrix
binary_dtms_matrix[binary_dtms_matrix > 0] <- 1

# Document-document shared term matrix
shared_terms_matrix <- binary_dtms_matrix %*% t(binary_dtms_matrix)
diag(shared_terms_matrix) <- 0
dim(shared_terms_matrix)
shared_terms_matrix[1:6, 1:6]

document_network <- graph_from_adjacency_matrix(
  shared_terms_matrix, mode = "undirected", weighted = TRUE, diag = FALSE
)
document_network
vcount(document_network); ecount(document_network)

# ---- Basic network plot ----
set.seed(123)
pdf(file = "Q6_Basic_Document_Network.pdf", height = 7, width = 9)
plot(document_network, vertex.label = V(document_network)$name, vertex.label.cex = 0.6,
     vertex.size = 18, edge.color = "grey80", main = "Basic Document Single-Mode Network")
dev.off()

# ---- Genre labelling and colours ----
V(document_network)$Genre <- ifelse(
  grepl("skincare", V(document_network)$name), "Skincare",
  ifelse(grepl("sports", V(document_network)$name), "Sports", "Tech")
)

genre_colours <- c("Skincare" = "pink", "Sports" = "gold", "Tech" = "skyblue")
V(document_network)$color <- genre_colours[V(document_network)$Genre]

data.frame(Document = V(document_network)$name, Genre = V(document_network)$Genre,
           Colour = V(document_network)$color)

# ---- Overall network measures ----
network_measures_final <- data.frame(
  Measure = c("Number of nodes", "Number of edges", "Average path length",
              "Diameter", "Density", "Transitivity"),
  Value = c(
    vcount(document_network), ecount(document_network),
    mean_distance(document_network, directed = FALSE, weights = NA),
    diameter(document_network, directed = FALSE, weights = NA),
    edge_density(document_network), transitivity(document_network)
  )
)
network_measures_final

# ---- Centrality measures ----
document_degree <- degree(document_network)
document_strength <- strength(document_network, weights = E(document_network)$weight)
document_betweenness <- betweenness(document_network, directed = FALSE, weights = E(document_network)$weight)
document_closeness <- closeness(document_network, weights = E(document_network)$weight, normalized = TRUE)
document_eigenvector <- eigen_centrality(document_network, directed = FALSE, weights = E(document_network)$weight)$vector

centrality_table <- data.frame(
  Document = V(document_network)$name, Genre = V(document_network)$Genre,
  Degree = document_degree, Strength = document_strength,
  Betweenness = document_betweenness, Closeness = document_closeness,
  Eigenvector = document_eigenvector
)
centrality_table

ranked_centrality_table <- data.frame(
  Rank = 1:5,
  Ranked_by_Degree = centrality_table$Document[order(-centrality_table$Degree)][1:5],
  Ranked_by_Strength = centrality_table$Document[order(-centrality_table$Strength)][1:5],
  Ranked_by_Betweenness = centrality_table$Document[order(-centrality_table$Betweenness)][1:5],
  Ranked_by_Closeness = centrality_table$Document[order(-centrality_table$Closeness)][1:5],
  Ranked_by_Eigenvector = centrality_table$Document[order(-centrality_table$Eigenvector)][1:5]
)
ranked_centrality_table

# ---- Improved network plot ----
set.seed(123)
layout_document <- layout_with_fr(document_network)
V(document_network)$size <- 6 + (degree(document_network) * 1.2)
E(document_network)$width <- E(document_network)$weight / max(E(document_network)$weight) * 5
E(document_network)$color <- "grey80"

pdf(file = "Q6_Improved_Document_Network.pdf", height = 7, width = 9)
plot(document_network, layout = layout_document, vertex.color = V(document_network)$color,
     vertex.size = V(document_network)$size, vertex.label = V(document_network)$name,
     vertex.label.cex = 0.65, vertex.label.color = "black",
     edge.width = E(document_network)$width, edge.color = E(document_network)$color,
     main = "Improved Document Single-Mode Network")
legend("bottomleft", legend = names(genre_colours), pch = 21, pt.bg = genre_colours,
       pt.cex = 2, bty = "n", title = "Genre")
dev.off()

# ---- Community detection ----
edge_betweenness_community <- cluster_edge_betweenness(document_network, weights = E(document_network)$weight)
fast_greedy_community <- cluster_fast_greedy(document_network, weights = E(document_network)$weight)
leading_eigen_community <- cluster_leading_eigen(document_network, weights = E(document_network)$weight)
label_prop_community <- cluster_label_prop(document_network, weights = E(document_network)$weight)
louvain_community <- cluster_louvain(document_network, weights = E(document_network)$weight)
optimal_community <- cluster_optimal(document_network, weights = E(document_network)$weight)

pdf(file = "Q6_Overall_Community_Detection_Algorithms.pdf", height = 12, width = 12)
par(mfrow = c(3, 2), mar = c(1, 1, 4, 1))
plot(edge_betweenness_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Edge Betweenness", cex.main = 1.6)
plot(fast_greedy_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Fast Greedy", cex.main = 1.6)
plot(leading_eigen_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Leading Eigenvector", cex.main = 1.6)
plot(label_prop_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Label Propagation", cex.main = 1.6)
plot(louvain_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Louvain", cex.main = 1.6)
plot(optimal_community, document_network, layout = layout_document, vertex.label = NA,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Optimal", cex.main = 1.6)
dev.off()

evaluate_community <- function(community_object, algorithm_name) {
  community_membership <- membership(community_object)
  community_table <- data.frame(
    Document = V(document_network)$name, Actual_Genre = V(document_network)$Genre,
    Community = community_membership
  )
  confusion_matrix <- table(Actual_Genre = community_table$Actual_Genre, Community = community_table$Community)
  accuracy <- sum(apply(confusion_matrix, 1, max)) / sum(confusion_matrix)
  confusion_df <- as.data.frame.matrix(confusion_matrix)
  confusion_df <- cbind(Actual_Genre = rownames(confusion_df), confusion_df)
  print(algorithm_name); print(community_table); print(confusion_matrix)
  print(paste0("Accuracy = ", round(accuracy * 100, 2), "%"))
  list(membership = community_membership, table = community_table,
       confusion_matrix = confusion_matrix, accuracy = accuracy)
}

fast_greedy_results <- evaluate_community(fast_greedy_community, "Fast_Greedy")
louvain_results <- evaluate_community(louvain_community, "Louvain")
optimal_results <- evaluate_community(optimal_community, "Optimal")

pdf(file = "Q6_Fast_Greedy_Community_Detection.pdf", height = 7, width = 9)
plot(fast_greedy_community, document_network, layout = layout_document,
     vertex.label = V(document_network)$name, vertex.label.cex = 0.65,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Fast Greedy Community Detection on Document Network")
dev.off()

pdf(file = "Q6_Louvain_Community_Detection.pdf", height = 7, width = 9)
plot(louvain_community, document_network, layout = layout_document,
     vertex.label = V(document_network)$name, vertex.label.cex = 0.65,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Louvain Community Detection on Document Network")
dev.off()

pdf(file = "Q6_Optimal_Community_Detection.pdf", height = 7, width = 9)
plot(optimal_community, document_network, layout = layout_document,
     vertex.label = V(document_network)$name, vertex.label.cex = 0.65,
     vertex.size = V(document_network)$size, edge.width = E(document_network)$width,
     edge.color = "grey75", main = "Optimal Community Detection on Document Network")
dev.off()

# ---- Attach sentiment to document nodes ----
V(document_network)$SentimentQDAP <- sentiment_qdap$SentimentQDAP[match(V(document_network)$name, sentiment_qdap$Document)]
V(document_network)$PositivityQDAP <- sentiment_qdap$PositivityQDAP[match(V(document_network)$name, sentiment_qdap$Document)]
V(document_network)$NegativityQDAP <- sentiment_qdap$NegativityQDAP[match(V(document_network)$name, sentiment_qdap$Document)]

document_sentiment_network_table <- data.frame(
  Document = V(document_network)$name, Genre = V(document_network)$Genre,
  SentimentQDAP = V(document_network)$SentimentQDAP,
  PositivityQDAP = V(document_network)$PositivityQDAP,
  NegativityQDAP = V(document_network)$NegativityQDAP
)
document_sentiment_network_table

V(document_network)$sentiment_border <- ifelse(
  V(document_network)$SentimentQDAP > 0.05, "darkgreen",
  ifelse(V(document_network)$SentimentQDAP < 0, "firebrick3", "grey35")
)

pdf(file = "Q6_Improved_Document_Network_With_Clear_Sentiment.pdf", height = 7, width = 9)
plot(document_network, layout = layout_document, vertex.color = V(document_network)$color,
     vertex.frame.color = V(document_network)$sentiment_border, vertex.frame.width = 4,
     vertex.size = V(document_network)$size, vertex.label = V(document_network)$name,
     vertex.label.cex = 0.65, vertex.label.color = "black",
     edge.width = E(document_network)$width, edge.color = "grey80",
     main = "Improved Document Network with Sentiment")
legend("bottomleft", legend = names(genre_colours), pch = 21, pt.bg = genre_colours,
       col = "black", pt.cex = 2, bty = "n", title = "Genre")
legend("topright", legend = c("Positive sentiment", "Neutral / low sentiment", "Negative sentiment"),
       pch = 21, pt.bg = "white", col = c("darkgreen", "grey35", "firebrick3"),
       pt.cex = 2, lwd = 3, bty = "n", title = "Sentiment Border")
dev.off()
