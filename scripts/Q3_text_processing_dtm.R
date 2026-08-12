# =============================================================
# Q3: Text Processing and Document-Term Matrix (DTM)
# =============================================================

library(tm)
library(slam)
library(SnowballC)
library(proxy)
library(wordcloud)
library(RColorBrewer)

# Point to the folder that holds our text documents
cname <- file.path(".", "corpus")
print(dir(cname))
length(dir(cname))

# Load all the documents into a Corpus object
docs <- Corpus(DirSource(cname))
summary(docs)
writeLines(as.character(docs[[1]]))

# ---- Raw DTM (before preprocessing) ----
dtm_raw <- DocumentTermMatrix(docs)
dim(dtm_raw)
inspect(dtm_raw)

freq_raw <- colSums(as.matrix(dtm_raw))
freq_raw_sorted <- sort(freq_raw, decreasing = TRUE)
head(freq_raw_sorted, 80)

top50_raw <- head(freq_raw_sorted, 50)

png("Top50_Frequent_Terms_Before_Preprocessing.png", width = 2400, height = 1800, res = 200)
par(mar = c(18, 7, 6, 2), cex.axis = 1.2, cex.lab = 1.6, cex.main = 1.8, font.lab = 2, font.main = 2)
barplot(top50_raw, col = "tan", border = "grey40", las = 2, cex.names = 1.15,
        main = "Top 50 Frequent Terms Before Preprocessing", ylab = "Frequency")
dev.off()

# ---- Basic preprocessing ----
docs_clean <- docs
docs_clean <- tm_map(docs_clean, removeNumbers)
docs_clean <- tm_map(docs_clean, removePunctuation)
docs_clean <- tm_map(docs_clean, content_transformer(tolower))
docs_clean <- tm_map(docs_clean, removeWords, stopwords("english"))
docs_clean <- tm_map(docs_clean, stripWhitespace)
docs_clean <- tm_map(docs_clean, stemDocument, language = "english")

writeLines(as.character(docs_clean[[1]]))

dtm_clean <- DocumentTermMatrix(docs_clean)
dim(dtm_clean)

freq_clean <- colSums(as.matrix(dtm_clean))
freq_clean_sorted <- sort(freq_clean, decreasing = TRUE)
head(freq_clean_sorted, 80)
inspect(dtm_clean)

top50_clean <- head(freq_clean_sorted, 50)
png("Top50_Frequent_Terms_After_Basic_Preprocessing.png", width = 2400, height = 1800, res = 200)
par(mar = c(18, 7, 6, 2), cex.axis = 1.2, cex.lab = 1.6, cex.main = 1.8, font.lab = 2, font.main = 2)
barplot(top50_clean, col = "tan", border = "grey40", las = 2, cex.names = 1.15,
        main = "Top 50 Frequent Terms After Basic Preprocessing", ylab = "Frequency")
dev.off()

# ---- Custom stop word removal ----
custom_stopwords <- c(
  "can", "use", "may", "make", "will", "need", "also", "just", "often", "time",
  "like", "two", "take", "differ", "last", "best", "includ", "look", "day", "even",
  "put", "say", "without", "end", "set", "new", "actual", "still", "three", "side"
)

docs_custom <- tm_map(docs_clean, removeWords, custom_stopwords)
docs_custom <- tm_map(docs_custom, stripWhitespace)

dtm_custom <- DocumentTermMatrix(docs_custom)
dim(dtm_custom)
inspect(dtm_custom)

dtm_custom_matrix <- as.matrix(dtm_custom)

# Remove apostrophe fragments (e.g. "'s", "'re")
apostrophe_terms <- colnames(dtm_custom_matrix)[grepl("'|'|'", colnames(dtm_custom_matrix))]
apostrophe_terms
apostrophe_freq <- colSums(dtm_custom_matrix[, apostrophe_terms, drop = FALSE])
sort(apostrophe_freq, decreasing = TRUE)

remove_apostrophe_terms <- c("'s", "'re", "'ve", "'m", "won't", "doesn't", "don't", "isn't")
dtm_custom_matrix <- dtm_custom_matrix[, !colnames(dtm_custom_matrix) %in% remove_apostrophe_terms]
dim(dtm_custom_matrix)

freq_custom <- colSums(dtm_custom_matrix)
freq_custom_sorted <- sort(freq_custom, decreasing = TRUE)
head(freq_custom_sorted, 80)

top50_custom <- head(freq_custom_sorted, 50)

# Remove blank term artefact
blank_term <- names(top50_custom)[49]
blank_term
nchar(blank_term)
dtm_custom_matrix <- dtm_custom_matrix[, colnames(dtm_custom_matrix) != blank_term]

freq_custom <- colSums(dtm_custom_matrix)
freq_custom_sorted <- sort(freq_custom, decreasing = TRUE)
top50_custom <- head(freq_custom_sorted, 50)

data.frame(
  position = 1:length(top50_custom),
  term = names(top50_custom),
  frequency = as.numeric(top50_custom)
)

# Sparsity check
total_cells_custom <- nrow(dtm_custom_matrix) * ncol(dtm_custom_matrix)
zero_cells_custom <- sum(dtm_custom_matrix == 0)
sparsity_custom <- zero_cells_custom / total_cells_custom
sparsity_custom
round(sparsity_custom * 100, 2)

# ---- Sparse term removal to build final reduced DTM ----
dtm_custom_clean <- as.simple_triplet_matrix(dtm_custom_matrix)
class(dtm_custom_clean) <- c("DocumentTermMatrix", "simple_triplet_matrix")
attr(dtm_custom_clean, "weighting") <- attr(dtm_custom, "weighting")

dtms <- removeSparseTerms(dtm_custom_clean, 0.762)
dim(dtms)
inspect(dtms)
colnames(as.matrix(dtms))

dtms_matrix <- as.matrix(dtms)
total_cells_sparse <- nrow(dtms_matrix) * ncol(dtms_matrix)
zero_cells_sparse <- sum(dtms_matrix == 0)
sparsity_sparse <- zero_cells_sparse / total_cells_sparse
sparsity_sparse
round(sparsity_sparse * 100, 2)

dtms_final_matrix <- as.matrix(dtms)
dim(dtms_final_matrix)
colnames(dtms_final_matrix)
write.csv(dtms_final_matrix, "A3_Final_DTM.csv")

# ---- Word cloud + frequency chart of final retained terms ----
freq_sparse <- colSums(dtms_matrix)
freq_sparse_sorted <- sort(freq_sparse, decreasing = TRUE)
freq_sparse_sorted

set.seed(123)
wordcloud(
  words = names(freq_sparse_sorted),
  freq = freq_sparse_sorted,
  max.words = length(freq_sparse_sorted),
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(5, 1.5)
)

barplot(
  rev(freq_sparse_sorted),
  horiz = TRUE, las = 1, col = "tan", border = "grey40",
  main = "Most Frequent Words", xlab = "Frequency", cex.names = 0.9
)
