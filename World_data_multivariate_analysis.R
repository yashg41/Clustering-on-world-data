install.packages("readxl")
# Load the readxl library
library(readxl)


# Specify the path to your Excel file

excel_file <- "DATA_MV.xlsx"

# Read the Excel file
df <- read_excel(excel_file)

# View the structure of the data frame
str(df)

# View the first few rows of the data frame
head(df)

df_clean <- df[complete.cases(df) | rowSums(is.na(df)) <= 5, ]

# View the structure of the cleaned data frame
str(df_clean)

# View the first few rows of the cleaned data frame
head(df_clean)

#converting columns 

# Function to convert values
convert_k_to_numeric <- function(x) {
  # Extract numeric part and multiply by 1000
  numeric_value <- as.numeric(sub("k", "", x)) * 1000
  return(numeric_value)
}

# Apply function to column 6
df_clean[,6]
df_clean[,14]
df_clean[,6] <- sapply(df_clean[,6], convert_k_to_numeric)
df_clean[,14] <- sapply(df_clean[,14], convert_k_to_numeric)
df_clean[,6]
df_clean[,14]

df_clean[188,9]
df_clean[,-1]<- lapply(df_clean[,-1], function(x) gsub("−", "-", x))
df_clean[188,9]

df_clean[,-1]<- lapply(df_clean[,-1], as.numeric)
df_clean[,-1] <- as.data.frame(df_clean[,-1])
df_clean[,-1]

# Load the corrplot library
library(corrplot)


# Compute the correlation matrix
correlation_matrix <- cor(df_clean[,-1], use = "pairwise.complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix,method = "color", tl.cex = 0.4)

# Load the corrplot package
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)

# Customize the correlation plot
corrplot(
  correlation_matrix, 
  method = "color", 
  type = "upper",  # Display only the upper triangle of the correlation matrix
  tl.cex = 0.6,    # Adjust the size of the text labels
  diag = TRUE,    # Exclude diagonal elements
  col = colorRampPalette(c("#FFFFFF", "#7CAE00", "#2B4817"))(100),  # Custom color palette
  addCoef.col = "black",  # Color of correlation coefficient text
  tl.col = "black",       # Color of variable names
  tl.srt = 45,            # Rotate variable names for better readability
  number.cex = 0.5,        # Adjust the size of correlation coefficient text
  mar = c(0,0,0,0),
  )
# Load the necessary package
if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)

# Remove the first column from df_clean
df_clean_subset <- df_clean[, -1]

# Create a pair plot
pairs(df_clean_subset)

# Divide variables into segments
num_variables <- ncol(df_clean_subset)
segments <- split(1:num_variables, ceiling(seq_along(1:num_variables)/7))  # Divides variables into segments of 4

# Create pair plots for each segment
for (segment in segments) {
  pairs(df_clean_subset[, segment])
}
df_clean_og<-df_clean
df_clean<-df_clean[,-1]

summary(df_clean)

# Function to calculate similarity between two rows
calculate_similarity <- function(row1, row2) {
  # Get indices of columns with missing values in both rows
  missing_cols_row1 <- which(is.na(row1))
  missing_cols_row2 <- which(is.na(row2))
  
  # Remove columns with missing values from both rows
  non_missing_indices <- setdiff(1:length(row1), c(missing_cols_row1, missing_cols_row2))
  non_missing_row1 <- row1[non_missing_indices]
  non_missing_row2 <- row2[non_missing_indices]
  
  # Calculate Euclidean distance between non-missing values
  euclidean_distance <- sqrt(sum((non_missing_row1 - non_missing_row2)^2))
  
  # Calculate Euclidean similarity (inverse of distance)
  similarity_score <- 1 / (1 + euclidean_distance)
  return(similarity_score)
}
similarity_matrix <- matrix(NA, nrow(df_clean), nrow(df_clean))

# Calculate similarity scores between each pair of rows
for (i in 1:nrow(df_clean)) {
  for (j in 1:nrow(df_clean)) {
    similarity_matrix[i, j] <- calculate_similarity(df_clean[i, ], df_clean[j, ])
  }
}
similarity_matrix


# Find top 5 most similar rows for each row
top_10_similar_rows <- lapply(1:nrow(df_clean), function(i) {
  top_10_indices <- order(similarity_matrix[i, ], decreasing = TRUE)[1:10]
  return(top_10_indices)
})


# Fill missing values in each row using similar rows
for (i in 1:nrow(df_clean)) {
  missing_indices <- which(is.na(df_clean[i, ]))  # Get indices of missing values in the current row
  for (missing_index in missing_indices) {
    similar_rows <- df_clean[top_10_similar_rows[[i]], ]  # Get similar rows
    similar_values <- similar_rows[, missing_index]  # Get values of the missing column from similar rows
    similar_values <- as.numeric(similar_values[[1]])
    df_clean[i, missing_index] <- mean(similar_values, na.rm = TRUE)  # Fill missing value with mean of similar values
  }
}
summary(df_clean)
df_clean
str(df_clean)
df_clean_og_subset <- df_clean_og[, c("COUNTRY", "Cell phones","Children and elderly (per 100 adults)",'GDP 10 yrs')]
merged_df <- merge(df_clean_og_subset, df_clean, by = c("Cell phones", "Children and elderly (per 100 adults)",'GDP 10 yrs'))
merged_df
numeric_cols <- merged_df[, sapply(merged_df, is.numeric)]
# Perform PCA on df_cleaned
pca_result <- prcomp(df_clean, scale. = TRUE)
pca_result
# Summary of PCA
summary(pca_result)

# Scree plot
screeplot(pca_result, type = "lines")

# Biplot
biplot(pca_result, scale = 0, col.var = "blue", cex = 0.7, cex.axis = 0.8)
# Adjust the margins and size of the plot
par(mar = c(5, 5, 2, 2))  # Set the margins
options(repr.plot.width=8, repr.plot.height=6)  # Set the plot size

# Create the biplot
biplot(pca_result, col = c("blue", "red"), cex = 0.7)

# Add title and axis labels
title(main = "Biplot of PCA Results", font.main = 1)
xlabel <- paste("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)", sep = "")
abline(h = 0, v = 0, col = "gray", lty = 2)
text(x = c(max(pca_result$ind$coord[,1]) * 1.1, min(pca_result$ind$coord[,1]) * 1.1),
     y = c(0, 0),
     labels = c(xlabel, ylabel),
     pos = 4, col = "black")




df_clean
str(df_clean)
df_clean_og_subset <- df_clean_og[, c("COUNTRY", "Cell phones","Children and elderly (per 100 adults)",'GDP 10 yrs')]
merged_df <- merge(df_clean_og_subset, df_clean, by = c("Cell phones", "Children and elderly (per 100 adults)",'GDP 10 yrs'))
merged_df
# Select the numeric columns for clustering (excluding non-numeric and key columns)
numeric_cols <- merged_df[, sapply(merged_df, is.numeric)]
# Perform hierarchical clustering
# Perform hierarchical clustering
hc_result <- hclust(dist(numeric_cols), method = "complete")

# Plot dendrogram with country names on the x-axis
plot(hc_result, hang = -1, cex = 0.6, main = "Dendrogram of Hierarchical Clustering", labels = merged_df$COUNTRY)

# Perform hierarchical clustering
hc_result <- hclust(dist(numeric_cols), method = "complete")

# Cut the dendrogram to obtain clusters
clusters <- cutree(hc_result, k = 5)

# Add cluster labels to merged dataframe
merged_df$Cluster <- clusters

# Print the counts of countries in each cluster
table(merged_df$Cluster)

# Plot dendrogram with clusters
plot(hc_result, hang = -1, cex = 0.6, main = "Dendrogram of Hierarchical Clustering with 5 Clusters", labels = merged_df$COUNTRY)
rect.hclust(hc_result, k = 5, border = 2:6)  # Highlight clusters with rectangles







# Perform k-means clustering
k <- 5  # Specify the number of clusters
set.seed(123)  # Set seed for reproducibility
kmeans_result <- kmeans(numeric_cols, centers = k)
k_means_merged_df<-merged_df
# Add cluster labels to the merged_df
k_means_merged_df$Cluster <- kmeans_result$cluster

# Print the counts of countries in each cluster
table(k_means_merged_df$Cluster)

# Display the first few rows of the merged data frame with cluster labels
head(k_means_merged_df)


# Load the necessary library for plotting
library(ggplot2)

# Plot clusters with respect to index
ggplot(k_means_merged_df, aes(x = rownames(k_means_merged_df), y = Cluster, color = factor(Cluster))) +
  geom_point() +
  labs(x = "Index", y = "Cluster", title = "Cluster Plot") +
  theme_minimal() +
  theme(axis.text.x = element_blank())  # Remove x-axis labels for better visualization

# Perform factor analysis


# Calculate the correlation matrix
cor_matrix <- cor(df_clean)

# Perform factor analysis using the correlation matrix
factor_analysis <- factanal(
  covmat = cor_matrix,     # Provide the correlation matrix
  factors = 6,             # Number of factors to extract
  rotation = "varimax"     # Type of rotation, varimax is a common choice
)

# Print the results
print(factor_analysis)

# Assuming you have performed PCA and stored the results in pca_result

# Load the necessary library for clustering
library(cluster)
numeric_cols <- merged_df[, sapply(merged_df, is.numeric)]
# Perform PCA on df_cleaned
pca_result <- prcomp(numeric_cols, scale. = TRUE)
pca_result
# Summary of PCA
summary(pca_result)

# Scree plot
screeplot(pca_result, type = "lines")

# Biplot
biplot(pca_result, scale = 0, col.var = "blue", cex = 0.7, cex.axis = 0.8)
# Adjust the margins and size of the plot
par(mar = c(5, 5, 2, 2))  # Set the margins
options(repr.plot.width=8, repr.plot.height=6)  # Set the plot size

# Create the biplot
biplot(pca_result, col = c("blue", "red"), cex = 0.7)

# Add title and axis labels
title(main = "Biplot of PCA Results", font.main = 1)
xlabel <- paste("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)", sep = "")
abline(h = 0, v = 0, col = "gray", lty = 2)
text(x = c(max(pca_result$ind$coord[,1]) * 1.1, min(pca_result$ind$coord[,1]) * 1.1),
     y = c(0, 0),
     labels = c(xlabel, ylabel),
     pos = 4, col = "black")


# Extract the principal components from the PCA result
principal_components <- pca_result$x

# Choose the number of clusters (you can use various methods to determine this, such as silhouette analysis, elbow method, etc.)
num_clusters <- 5 # Example: Assume you want to create 3 clusters

# Perform clustering using K-means algorithm
kmeans_result <- kmeans(principal_components, centers = num_clusters)

# Get the cluster assignments for each data point
cluster_assignments <- kmeans_result$cluster

# Print cluster centers
print(kmeans_result$centers)

# Print cluster assignments
print(cluster_assignments)
# Assuming you have a dataset df_clean with IDs
# Assuming cluster_assignments are obtained from clustering

# Load the necessary library for plotting
library(ggplot2)
library(ggrepel)

# Combine principal components with cluster assignments and IDs
data <- data.frame(principal_components, cluster = as.factor(cluster_assignments), id = 1:nrow(principal_components))

# Plot the clusters along with IDs
ggplot(data, aes(x = PC1, y = PC2, color = cluster, label = merged_df$COUNTRY)) +
  geom_point() +  # Plot points
   # Add labels without overlapping
  labs(title = "Cluster Plot") +  # Add title
  theme_minimal()  # Set plot theme


# Combine principal components with cluster assignments, IDs, and country names
data <- data.frame(principal_components, cluster = as.factor(cluster_assignments), id = 1:nrow(principal_components), country = merged_df$COUNTRY)

# Plot the clusters along with IDs and country names
ggplot(data, aes(x = PC1, y = PC2, color = cluster, label = country)) +
  geom_point() +  # Plot points
    # Add labels without overlapping
  labs(title = "Cluster Plot") +  # Add title
  geom_text_repel() +
  theme_minimal()  # Set plot theme

