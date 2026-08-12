# =============================================================
# Q7: Token Single-Mode Network
# (requires dtms_matrix from Q3, sentiment_qdap from Q5)
# =============================================================

library(igraph)

binary_dtms_matrix <- dtms_matrix
binary_dtms_matrix[binary_dtms_matrix > 0] <- 1

# Token-token co-occurrence matrix
token_cooccur_matrix <- t(binary_dtms_matrix) %*% binary_dtms_matrix
diag(token_cooccur_matrix) <- 0
dim(token_cooccur_matrix)
token_cooccur_matrix[1:10, 1:10]

token_network <- graph_from_adjacency_matrix(
  token_cooccur_matrix, mode = "undirected", weighted = TRUE, diag = FALSE
)
token_network
vcount(token_network); ecount(token_network)

set.seed(123)
token_layout <- layout_with_fr(token_network)

pdf(file = "Q7_Basic_Token_Network.pdf", height = 7, width = 9)
plot(token_network, layout = token_layout, vertex.label = V(token_network)$name,
     vertex.label.cex = 0.8, vertex.size = 18, edge.color = "grey80",
     main = "Basic Token Single-Mode Network")
dev.off()

E(token_network)$distance_weight <- 1 / E(token_network)$weight

token_network_measures <- data.frame(
  Measure = c("Number of nodes", "Number of edges", "Average path length",
              "Diameter", "Density", "Transitivity"),
  Value = c(
    vcount(token_network), ecount(token_network),
    mean_distance(token_network, directed = FALSE, weights = NA),
    diameter(token_network, directed = FALSE, weights = NA),
    edge_density(token_network), transitivity(token_network)
  )
)
token_network_measures

# ---- Centrality ----
token_degree <- degree(token_network)
token_strength <- strength(token_network, weights = E(token_network)$weight)
token_betweenness <- betweenness(token_network, directed = FALSE, weights = E(token_network)$distance_weight)
token_closeness <- closeness(token_network, weights = E(token_network)$distance_weight, normalized = TRUE)
token_eigenvector <- eigen_centrality(token_network, directed = FALSE, weights = E(token_network)$weight)$vector

token_centrality_table <- data.frame(
  Token = V(token_network)$name, Degree = token_degree, Strength = token_strength,
  Betweenness = token_betweenness, Closeness = token_closeness, Eigenvector = token_eigenvector
)
token_centrality_table

ranked_token_centrality_table <- data.frame(
  Rank = 1:5,
  Ranked_by_Degree = token_centrality_table$Token[order(-token_centrality_table$Degree)][1:5],
  Ranked_by_Strength = token_centrality_table$Token[order(-token_centrality_table$Strength)][1:5],
  Ranked_by_Betweenness = token_centrality_table$Token[order(-token_centrality_table$Betweenness)][1:5],
  Ranked_by_Closeness = token_centrality_table$Token[order(-token_centrality_table$Closeness)][1:5],
  Ranked_by_Eigenvector = token_centrality_table$Token[order(-token_centrality_table$Eigenvector)][1:5]
)
ranked_token_centrality_table

# ---- Manual topic groups for improved visual ----
token_groups <- rep("General", vcount(token_network))
names(token_groups) <- V(token_network)$name
token_groups[c("skin", "product", "cleanser", "face", "dri", "spot")] <- "Skincare"
token_groups[c("goal", "shot", "final", "season", "score", "minut", "first")] <- "Sports"
token_groups[c("human", "learn", "system", "requir", "specif", "general")] <- "Tech"

token_group_colours <- c("Skincare" = "pink", "Sports" = "gold", "Tech" = "skyblue", "General" = "grey80")
V(token_network)$group <- token_groups
V(token_network)$color <- token_group_colours[V(token_network)$group]

V(token_network)$size <- 6 + (degree(token_network) * 0.9)
E(token_network)$width <- E(token_network)$weight / max(E(token_network)$weight) * 8
E(token_network)$color <- "grey75"

pdf(file = "Q7_Improved_Token_Network.pdf", height = 7, width = 9)
plot(token_network, layout = token_layout, vertex.color = V(token_network)$color,
     vertex.size = V(token_network)$size, vertex.label = V(token_network)$name,
     vertex.label.cex = 0.8, vertex.label.color = "black",
     edge.width = E(token_network)$width, edge.color = E(token_network)$color,
     main = "Improved Token Single-Mode Network")
legend("bottomleft", legend = names(token_group_colours), pch = 21, pt.bg = token_group_colours,
       pt.cex = 2, bty = "n", title = "Token Group")
dev.off()

# ---- Community detection ----
token_fast_greedy_community <- cluster_fast_greedy(token_network, weights = E(token_network)$weight)
token_louvain_community <- cluster_louvain(token_network, weights = E(token_network)$weight)
token_optimal_community <- cluster_optimal(token_network, weights = E(token_network)$weight)
token_edge_betweenness_community <- cluster_edge_betweenness(token_network, weights = E(token_network)$weight)
token_leading_eigen_community <- cluster_leading_eigen(token_network, weights = E(token_network)$weight)
token_label_prop_community <- cluster_label_prop(token_network, weights = E(token_network)$weight)

pdf(file = "Q7_Fast_Greedy_Token_Community_Detection.pdf", height = 7, width = 9)
plot(token_fast_greedy_community, token_network, layout = token_layout,
     vertex.label = V(token_network)$name, vertex.label.cex = 0.8,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Fast Greedy Community Detection on Token Network")
dev.off()

pdf(file = "Q7_Louvain_Token_Community_Detection.pdf", height = 7, width = 9)
plot(token_louvain_community, token_network, layout = token_layout,
     vertex.label = V(token_network)$name, vertex.label.cex = 0.8,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Louvain Community Detection on Token Network")
dev.off()

pdf(file = "Q7_Optimal_Token_Community_Detection.pdf", height = 7, width = 9)
plot(token_optimal_community, token_network, layout = token_layout,
     vertex.label = V(token_network)$name, vertex.label.cex = 0.8,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Optimal Community Detection on Token Network")
dev.off()

pdf(file = "Q7_Overall_Token_Community_Detection_Algorithms.pdf", height = 12, width = 12)
par(mfrow = c(3, 2), mar = c(1, 1, 4, 1))
plot(token_edge_betweenness_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Edge Betweenness", cex.main = 1.6)
plot(token_fast_greedy_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Fast Greedy", cex.main = 1.6)
plot(token_leading_eigen_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Leading Eigenvector", cex.main = 1.6)
plot(token_label_prop_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Label Propagation", cex.main = 1.6)
plot(token_louvain_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Louvain", cex.main = 1.6)
plot(token_optimal_community, token_network, layout = token_layout, vertex.label = NA,
     vertex.size = V(token_network)$size, edge.width = E(token_network)$width,
     edge.color = "grey75", main = "Optimal", cex.main = 1.6)
dev.off()

# ---- Token-level sentiment (average sentiment of documents containing token) ----
sentiment_for_dtm <- sentiment_qdap[match(rownames(binary_dtms_matrix), sentiment_qdap$Document), ]

data.frame(DTM_Document = rownames(binary_dtms_matrix), Sentiment_Document = sentiment_for_dtm$Document,
           SentimentQDAP = sentiment_for_dtm$SentimentQDAP)

token_sentiment <- numeric(ncol(binary_dtms_matrix))
token_positivity <- numeric(ncol(binary_dtms_matrix))
token_negativity <- numeric(ncol(binary_dtms_matrix))
token_doc_count <- numeric(ncol(binary_dtms_matrix))

for (i in 1:ncol(binary_dtms_matrix)) {
  docs_with_token <- binary_dtms_matrix[, i] > 0
  token_sentiment[i] <- mean(sentiment_for_dtm$SentimentQDAP[docs_with_token], na.rm = TRUE)
  token_positivity[i] <- mean(sentiment_for_dtm$PositivityQDAP[docs_with_token], na.rm = TRUE)
  token_negativity[i] <- mean(sentiment_for_dtm$NegativityQDAP[docs_with_token], na.rm = TRUE)
  token_doc_count[i] <- sum(docs_with_token)
}

token_sentiment_table <- data.frame(
  Token = colnames(binary_dtms_matrix),
  Token_Group = V(token_network)$group[match(colnames(binary_dtms_matrix), V(token_network)$name)],
  Document_Count = token_doc_count,
  Average_SentimentQDAP = token_sentiment,
  Average_PositivityQDAP = token_positivity,
  Average_NegativityQDAP = token_negativity
)
token_sentiment_table

V(token_network)$Average_SentimentQDAP <- token_sentiment_table$Average_SentimentQDAP[match(V(token_network)$name, token_sentiment_table$Token)]
V(token_network)$Average_PositivityQDAP <- token_sentiment_table$Average_PositivityQDAP[match(V(token_network)$name, token_sentiment_table$Token)]
V(token_network)$Average_NegativityQDAP <- token_sentiment_table$Average_NegativityQDAP[match(V(token_network)$name, token_sentiment_table$Token)]

V(token_network)$sentiment_border <- ifelse(
  V(token_network)$Average_SentimentQDAP > 0.05, "darkgreen",
  ifelse(V(token_network)$Average_SentimentQDAP < 0, "firebrick3", "grey35")
)

pdf(file = "Q7_Improved_Token_Network_With_Clear_Sentiment.pdf", height = 7, width = 9)
plot(token_network, layout = token_layout, vertex.color = V(token_network)$color,
     vertex.frame.color = V(token_network)$sentiment_border, vertex.frame.width = 5,
     vertex.size = V(token_network)$size, vertex.label = V(token_network)$name,
     vertex.label.cex = 0.85, vertex.label.color = "black",
     edge.width = E(token_network)$width, edge.color = "grey75",
     main = "Improved Token Network with Sentiment")
legend("bottomleft", legend = names(token_group_colours), pch = 21, pt.bg = token_group_colours,
       col = "black", pt.cex = 2, bty = "n", title = "Token Group")
legend("topright", legend = c("Positive sentiment", "Neutral / low sentiment", "Negative sentiment"),
       pch = 21, pt.bg = "white", col = c("darkgreen", "grey35", "firebrick3"),
       pt.cex = 2, lwd = 4, bty = "n", title = "Sentiment Border")
dev.off()
