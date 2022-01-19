library(xtable)

# ====================
# Define functions
# ====================

# Create jpg file path for plots by providing a plot name
getJpgFilePath <- function(plot_name) {
  plots_path <- "plots"
  file_type <- "jpg"
  path <- paste(paste(plots_path, plot_name, sep="/"), file_type, sep=".")
  
}


# ====================
# Load data
# ====================

# Create dataframe
data <- data.frame(AlgorithmA = c(120, 29, 24.2),
                   AlgorithmB = c(120, 23, 19.2))
# Add rownames 
row.names(data) <- c("Visitors", "Customers", "Conversion %")

data


# ====================
# Plot
# ====================

# Rearrange data for plot 
data_plot <- data.frame(Visitors = c( unlist(data[2, ]),unlist(data[1, ] - data[2,])),
                       Algorithm = c("A", "B", "A", "B"),
                        Conversion =  c("Converted", "Converted", "Not converted", "Not converted"))
data_plot


# Make plot 
jpeg(file= getJpgFilePath("bar_stacked"))
ggplot(data_plot, aes(fill=Conversion, y=Visitors, x=Algorithm)) + 
  geom_bar(position="stack", stat="identity")
dev.off()


# ====================
# Test significance
# ====================

# Rearrange data for testing 
data_new <- data.frame(Success = unlist(data[2, ]),
                       Fail = unlist(data[1, ] - data[2,]))
row.names(data_new) <- c("Algorithm A", "Algortithm B")
data_new

# Perform a chisq-test
chisq.test(data_new)

# ========= 
# a
# =========

# Get customer numbers and vistor numbers as lists 
customers <- unlist(data[2,])
visitors  <- unlist(data[1,])

# Two Proportion Z-Test
prop.test(customers, visitors, correct=FALSE, alternative = "two.sided", conf.level=0.95)


# ========= 
# b
# =========

alpha <- 0.05


# ========= 
# c
# =========
n <- 120
# Calculate probabilities
p1 <- data[2,1]/data[1,1];p1
p2 <- data[2,2]/data[1,2];p2

proptest <- power.prop.test(n, p1, p2, alternative = "two.sided", sig.level=alpha); proptest

beta <- 1 - proptest$power;beta




 
# =========

# Calculate p-value with example from: https://www.geeksforgeeks.org/ab-testing-with-r-programming/
conv_rate_A <- data$AlgorithmA[3]/100; conv_rate_A
conv_rate_B <-  data$AlgorithmB[3]/100; conv_rate_B
visitors_A <-  data$AlgorithmA[1]; visitors_A
visitors_B <- data$AlgorithmB[1]; visitors_B
conversion_A <-  data$AlgorithmA[2]; conversion_A
conversion_B <- data$AlgorithmB[2]; conversion_B

# Uplift is calculated
uplift <- (conv_rate_A - conv_rate_B) / conv_rate_B * 100
uplift

p_pool <- (conversion_A + conversion_B) / (visitors_A + visitors_B); p_pool

SE_pool <- sqrt(p_pool * (1 - p_pool) * ((1 / visitors_A) + (1 / visitors_B))); SE_pool

MOE <- SE_pool * qnorm(0.975); MOE

d_hat <- conv_rate_A - conv_rate_B; d_hat

z_score <- d_hat / SE_pool; z_score

p_value <- pnorm(q = -z_score, mean = 0, sd = 1) * 2; p_value

ci <- c(d_hat - MOE, d_hat + MOE) ; ci

