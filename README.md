# Text Mining and Network Analysis of Skincare, Sports, and Technology Documents

An R-based text mining pipeline that classifies and analyses a 21-document corpus spanning three genres (skincare, sports, technology) using document-term matrix construction, hierarchical clustering, QDAP sentiment analysis, and single-mode, token-mode, and bipartite network analysis with multiple community detection algorithms.

## Overview

The corpus consists of 21 documents (7 per genre) collected from online sources and stored as individual `.txt` files. The pipeline processes raw text into a cleaned document-term matrix, then applies clustering, sentiment scoring, and three complementary network representations to examine how genre-specific vocabulary shapes document and term relationships.

## Pipeline

1. **Text Processing and DTM Construction** (`Q3_text_processing_dtm.R`)
   Raw corpus (21 docs, 2,174 terms, 93% sparsity) is cleaned via number/punctuation removal, lowercasing, stopword removal, and stemming, reducing it to 1,512 terms. A custom stopword list and apostrophe-artefact cleanup further refine the vocabulary, and `removeSparseTerms()` (threshold 0.763) produces a final 27-term reduced DTM used for all downstream analysis.

2. **Hierarchical Clustering** (`Q4_clustering.R`)
   Ward's method with cosine distance clusters all 21 documents into three groups, achieving 100% classification accuracy against the true genre labels with no misclassifications.

3. **Sentiment Analysis** (`Q5_sentiment_analysis.R`)
   QDAP dictionary-based sentiment scoring (via the `SentimentAnalysis` package) compares overall sentiment, positivity, and negativity across genres, with Welch t-tests testing whether differences are statistically significant. Technology documents scored significantly higher on positivity than skincare (p = 0.0271); overall sentiment differences between genres were not statistically significant at the 0.05 level.

4. **Document Single-Mode Network** (`Q6_document_network.R`)
   Documents are linked by shared retained terms into a weighted, undirected graph. Centrality analysis (degree, strength, betweenness, closeness, eigenvector) identifies the most influential documents per genre, and Fast Greedy, Louvain, and Optimal community detection each recover the three genre clusters with 100% accuracy.

5. **Token Single-Mode Network** (`Q7_token_network.R`)
   A term-term co-occurrence network built from the binary DTM reveals genre-specific vocabulary clusters and bridging terms (e.g. "goal," "better," "work") that connect otherwise distinct topic areas.

6. **Bipartite Document-Token Network** (`Q8_bipartite_network.R`)
   A two-mode network connecting documents directly to their retained tokens gives the clearest picture of genre-vocabulary relationships, identifying four communities (three genre clusters plus one overlapping group of general/bridging terms).

## Key Findings

- Hierarchical clustering and community detection both achieve 100% accuracy in separating documents into their correct genre, confirming that skincare, sports, and technology use distinct, well-separated vocabulary.
- Network analysis surfaces relationships clustering alone can't show: central documents (`skincare_3.txt`, `sports_1.txt`, `tech_7.txt`), bridging terms across genres, and a small overlap between skincare and technology vocabulary in the bipartite network.
- Technology documents show the most positive and most variable sentiment; skincare documents are more consistently problem-focused in tone.

## Tech Stack

R, `tm`, `SnowballC`, `proxy`, `igraph`, `SentimentAnalysis`, `wordcloud`

## Repository Structure

├── corpus/ # 21 source .txt documents (7 skincare, 7 sports, 7 tech)
├── scripts/
│ ├── Q3_text_processing_dtm.R
│ ├── Q4_clustering.R
│ ├── Q5_sentiment_analysis.R
│ ├── Q6_document_network.R
│ ├── Q7_token_network.R
│ └── Q8_bipartite_network.R
└── README.md

## Notes

Scripts are numbered to match the corresponding analysis question and are meant to be run in sequence (Q3 → Q4/Q5/Q6/Q7/Q8), since later scripts depend on objects created in Q3 (`dtms`, `dtms_final_matrix`) and Q5 (`sentiment_qdap`).
