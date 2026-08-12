# =============================================================
# Q5: Sentiment Analysis using QDAP
# (requires docs from Q3_text_processing_dtm.R)
# =============================================================

install.packages("SentimentAnalysis")
library(SentimentAnalysis)

sentiment_results <- analyzeSentiment(docs)
head(sentiment_results)

sentiment_df <- as.data.frame(sentiment_results)
sentiment_df$Document <- names(docs)

sentiment_df$Genre <- ifelse(
  grepl("skincare", sentiment_df$Document), "Skincare",
  ifelse(grepl("sports", sentiment_df$Document), "Sports",
         ifelse(grepl("tech", sentiment_df$Document), "Tech", NA))
)

sentiment_qdap <- sentiment_df[, c("Document", "Genre", "WordCount",
                                    "SentimentQDAP", "PositivityQDAP", "NegativityQDAP")]
sentiment_qdap

# Boxplots by genre
pdf(file = "Q5_QDAP_Boxplots_By_Genre.pdf", height = 6, width = 10)
par(mfrow = c(2, 2))
boxplot(WordCount ~ Genre, data = sentiment_qdap, frame = TRUE,
        main = "Word Count by Genre", xlab = "Genre", ylab = "Word Count")
boxplot(SentimentQDAP ~ Genre, data = sentiment_qdap, frame = TRUE,
        main = "SentimentQDAP by Genre", xlab = "Genre", ylab = "SentimentQDAP")
boxplot(PositivityQDAP ~ Genre, data = sentiment_qdap, frame = TRUE,
        main = "PositivityQDAP by Genre", xlab = "Genre", ylab = "PositivityQDAP")
boxplot(NegativityQDAP ~ Genre, data = sentiment_qdap, frame = TRUE,
        main = "NegativityQDAP by Genre", xlab = "Genre", ylab = "NegativityQDAP")
dev.off()

# Mean / SD / variance by genre
qdap_mean_variability <- aggregate(
  cbind(SentimentQDAP, PositivityQDAP, NegativityQDAP) ~ Genre,
  data = sentiment_qdap,
  FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Variance = var(x, na.rm = TRUE))
)
qdap_mean_variability <- do.call(data.frame, qdap_mean_variability)
colnames(qdap_mean_variability) <- c(
  "Genre", "Sentiment_Mean", "Sentiment_SD", "Sentiment_Variance",
  "Positivity_Mean", "Positivity_SD", "Positivity_Variance",
  "Negativity_Mean", "Negativity_SD", "Negativity_Variance"
)
qdap_mean_variability[, -1] <- round(qdap_mean_variability[, -1], 4)
qdap_mean_variability

# Split by genre
skincare_sent <- sentiment_qdap[sentiment_qdap$Genre == "Skincare", "SentimentQDAP"]
sports_sent   <- sentiment_qdap[sentiment_qdap$Genre == "Sports", "SentimentQDAP"]
tech_sent     <- sentiment_qdap[sentiment_qdap$Genre == "Tech", "SentimentQDAP"]

skincare_pos <- sentiment_qdap[sentiment_qdap$Genre == "Skincare", "PositivityQDAP"]
sports_pos   <- sentiment_qdap[sentiment_qdap$Genre == "Sports", "PositivityQDAP"]
tech_pos     <- sentiment_qdap[sentiment_qdap$Genre == "Tech", "PositivityQDAP"]

skincare_neg <- sentiment_qdap[sentiment_qdap$Genre == "Skincare", "NegativityQDAP"]
sports_neg   <- sentiment_qdap[sentiment_qdap$Genre == "Sports", "NegativityQDAP"]
tech_neg     <- sentiment_qdap[sentiment_qdap$Genre == "Tech", "NegativityQDAP"]

length(skincare_sent); length(sports_sent); length(tech_sent)

# ---- SentimentQDAP: two-sided Welch t-tests ----
sent_skincare_sports <- t.test(skincare_sent, sports_sent, alternative = "two.sided")
sent_skincare_tech   <- t.test(skincare_sent, tech_sent, alternative = "two.sided")
sent_sports_tech     <- t.test(sports_sent, tech_sent, alternative = "two.sided")

sentiment_ttest_table <- data.frame(
  Comparison = c("Skincare vs Sports", "Skincare vs Tech", "Sports vs Tech"),
  Mean_First_Genre = c(mean(skincare_sent), mean(skincare_sent), mean(sports_sent)),
  Mean_Second_Genre = c(mean(sports_sent), mean(tech_sent), mean(tech_sent)),
  T_Value = c(sent_skincare_sports$statistic, sent_skincare_tech$statistic, sent_sports_tech$statistic),
  DF = c(sent_skincare_sports$parameter, sent_skincare_tech$parameter, sent_sports_tech$parameter),
  P_Value = c(sent_skincare_sports$p.value, sent_skincare_tech$p.value, sent_sports_tech$p.value)
)
sentiment_ttest_table$Significant_At_0.05 <- ifelse(sentiment_ttest_table$P_Value < 0.05, "Yes", "No")
sentiment_ttest_table[, 2:6] <- round(sentiment_ttest_table[, 2:6], 4)
sentiment_ttest_table

# ---- PositivityQDAP: two-sided Welch t-tests ----
pos_skincare_sports <- t.test(skincare_pos, sports_pos, alternative = "two.sided")
pos_skincare_tech   <- t.test(skincare_pos, tech_pos, alternative = "two.sided")
pos_sports_tech     <- t.test(sports_pos, tech_pos, alternative = "two.sided")

positivity_ttest_table <- data.frame(
  Comparison = c("Skincare vs Sports", "Skincare vs Tech", "Sports vs Tech"),
  Mean_First_Genre = c(mean(skincare_pos), mean(skincare_pos), mean(sports_pos)),
  Mean_Second_Genre = c(mean(sports_pos), mean(tech_pos), mean(tech_pos)),
  T_Value = c(pos_skincare_sports$statistic, pos_skincare_tech$statistic, pos_sports_tech$statistic),
  DF = c(pos_skincare_sports$parameter, pos_skincare_tech$parameter, pos_sports_tech$parameter),
  P_Value = c(pos_skincare_sports$p.value, pos_skincare_tech$p.value, pos_sports_tech$p.value)
)
positivity_ttest_table$Significant_At_0.05 <- ifelse(positivity_ttest_table$P_Value < 0.05, "Yes", "No")
positivity_ttest_table[, 2:6] <- round(positivity_ttest_table[, 2:6], 4)
positivity_ttest_table

# NOTE: report text states one-sided PositivityQDAP tests were used
# (alternative = "greater") to test whether the first genre's mean exceeds the
# second's — update `alternative` above to "greater" to reproduce Table 5.3 exactly.

# ---- NegativityQDAP: two-sided Welch t-tests ----
neg_skincare_sports <- t.test(skincare_neg, sports_neg, alternative = "two.sided")
neg_skincare_tech   <- t.test(skincare_neg, tech_neg, alternative = "two.sided")
neg_sports_tech     <- t.test(sports_neg, tech_neg, alternative = "two.sided")

negativity_ttest_table <- data.frame(
  Comparison = c("Skincare vs Sports", "Skincare vs Tech", "Sports vs Tech"),
  Mean_First_Genre = c(mean(skincare_neg), mean(skincare_neg), mean(sports_neg)),
  Mean_Second_Genre = c(mean(sports_neg), mean(tech_neg), mean(tech_neg)),
  T_Value = c(neg_skincare_sports$statistic, neg_skincare_tech$statistic, neg_sports_tech$statistic),
  DF = c(neg_skincare_sports$parameter, neg_skincare_tech$parameter, neg_sports_tech$parameter),
  P_Value = c(neg_skincare_sports$p.value, neg_skincare_tech$p.value, neg_sports_tech$p.value)
)
negativity_ttest_table$Significant_At_0.05 <- ifelse(negativity_ttest_table$P_Value < 0.05, "Yes", "No")
negativity_ttest_table[, 2:6] <- round(negativity_ttest_table[, 2:6], 4)
negativity_ttest_table
