### Horse relation to ASW and PCA

setwd("/Users/beheard/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/4. Semester/02445_ProjectStatisticalEvaluation/Project")

data = read.table("horse_data23.txt")
data$horse = as.factor(data$horse)
data$lameLeg = as.factor(data$lameLeg)
data

### Horse's effect on A,S,W
colors = c("aquamarine","brown1","cadetblue1","chartreuse3","chocolate1","darkgoldenrod1","deeppink1","lightpink1","navajowhite1")
boxplot(data$A ~ data$horse, xlab = "Horse", ylab = "A",col=colors, main = "Horse relation to A")
boxplot(data$S ~ data$horse, xlab = "Horse", ylab = "S",col=colors, main = "Horse relation to S")     
boxplot(data$W ~ data$horse, xlab = "Horse", ylab = "W",col=colors, main = "Horse relation to W")

### PCA plot in relation to horse
boxplot(data$pc1 ~ data$horse, col = colors, xlab = "Horse", ylab = "PC1", main = "PC1")
boxplot(data$pc2 ~ data$horse, col = colors, xlab = "Horse", ylab = "PC2", main = "PC2")
boxplot(data$pc3 ~ data$horse, col = colors, xlab = "Horse", ylab = "PC3", main = "PC3")
boxplot(data$pc4 ~ data$horse, col = colors, xlab = "Horse", ylab = "PC4", main = "PC4")

