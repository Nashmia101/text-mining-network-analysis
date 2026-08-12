# =============================================================
# Q4: Hierarchical Clustering using Cosine Distance
# (requires dtms / dtms_final_matrix from Q3_text_processing_dtm.R)
# =============================================================

dtms_final_matrix <- as.matrix(dtms)

# Cosine distance between documents
distmatrix <- proxy::dist(dtms_final_matrix, method = "cosine")

# Ward's method hierarchical clustering
fit <- hclust(distmatrix, method = "ward.D")

png("Q4_Cluster_Dendrogram_3_Clusters.png", width = 2600, height = 1700, res = 200)
par(mar = c(12, 6, 6, 2), cex.main = 1.6, cex.lab = 1.3, font.main = 2, font.lab = 2)
plot(fit, hang = -1, main = "Cluster Dendrogram using Cosine Distance",
     xlab = "Documents", ylab = "Height", sub = "", cex = 0.8)
rect.hclust(fit, k = 3, border = c("red", "blue", "darkgreen"))
dev.off()

clusters <- cutree(fit, k = 3)
clusters

actual_genre <- ifelse(
  grepl("skincare", names(clusters)), "Skincare",
  ifelse(grepl("sports", names(clusters)), "Sports",
         ifelse(grepl("tech", names(clusters)), "Tech", NA))
)

cluster_membership <- data.frame(
  document = names(clusters),
  actual_genre = actual_genre,
  cluster = as.numeric(clusters)
)
cluster_membership

confusion_matrix <- table(
  Actual_Genre = cluster_membership$actual_genre,
  Cluster = cluster_membership$cluster
)
confusion_matrix

cluster_genre_mapping <- apply(confusion_matrix, 2, function(x) names(which.max(x)))
cluster_genre_mapping

correct_classifications <- sum(apply(confusion_matrix, 2, max))
total_documents <- sum(confusion_matrix)
overall_accuracy <- correct_classifications / total_documents
overall_accuracy
round(overall_accuracy * 100, 2)

cluster_accuracy <- apply(confusion_matrix, 2, function(x) max(x) / sum(x))
cluster_accuracy
round(cluster_accuracy * 100, 2)

confusion_matrix_df <- as.data.frame.matrix(confusion_matrix)
confusion_matrix_df

cluster_accuracy_df <- data.frame(
  Cluster = names(cluster_accuracy),
  Assigned_Genre = cluster_genre_mapping,
  Accuracy = round(as.numeric(cluster_accuracy) * 100, 2)
)
cluster_accuracy_df

overall_accuracy_df <- data.frame(
  Measure = "Overall clustering accuracy",
  Accuracy = round(overall_accuracy * 100, 2)
)
overall_accuracy_df

# Top words per cluster
get_top_words <- function(cluster_number, top_n = 5) {
  cluster_docs <- names(clusters[clusters == cluster_number])
  cluster_matrix <- dtms_final_matrix[cluster_docs, , drop = FALSE]
  cluster_freq <- colSums(cluster_matrix)
  cluster_freq_sorted <- sort(cluster_freq, decreasing = TRUE)
  head(cluster_freq_sorted, top_n)
}

cluster1_words <- get_top_words(1, 5)
cluster2_words <- get_top_words(2, 5)
cluster3_words <- get_top_words(3, 5)
cluster1_words
cluster2_words
cluster3_words

common_words_table <- data.frame(
  Cluster = c(1, 2, 3),
  Assigned_Genre = c(cluster_genre_mapping["1"], cluster_genre_mapping["2"], cluster_genre_mapping["3"]),
  Common_Words = c(
    paste(names(cluster1_words), collapse = ", "),
    paste(names(cluster2_words), collapse = ", "),
    paste(names(cluster3_words), collapse = ", ")
  )
)
common_words_table

get_top_words_df <- function(cluster_number, top_n = 10) {
  cluster_docs <- names(clusters[clusters == cluster_number])
  cluster_matrix <- dtms_final_matrix[cluster_docs, , drop = FALSE]
  cluster_freq <- colSums(cluster_matrix)
  cluster_freq_sorted <- sort(cluster_freq, decreasing = TRUE)
  top_words <- head(cluster_freq_sorted, top_n)
  data.frame(Cluster = cluster_number, Term = names(top_words), Frequency = as.numeric(top_words))
}

cluster1_word_pattern <- get_top_words_df(1, 10)
cluster2_word_pattern <- get_top_words_df(2, 10)
cluster3_word_pattern <- get_top_words_df(3, 10)

cluster_word_patterns <- rbind(cluster1_word_pattern, cluster2_word_pattern, cluster3_word_pattern)
cluster_word_patterns
