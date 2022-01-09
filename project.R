# ====================
# Load data
# ====================

# Create dataframe
data <- data.frame(AlgorithmA = c(120, 29, 24.2),
                   AlgorithmB = c(120, 23, 19.2))
# Add rownames 
row.names(data) <- c("Visitors", "Customers", "Conversion %")

data


