setwd("/Users/beheard/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/4. Semester/02445_ProjectStatisticalEvaluation/Project")

data = read.table("horse_data23.txt")
data$horse = as.factor(data$horse)
data$lameLeg = as.factor(data$lameLeg)
data


### DONT FUCK WITH THE COLORS! IT WORKS

#Correlation of AW
plot(data$A, data$W, col = data$lameLeg, main = "Correlation of A and W", xlab = "A", ylab = "W")
legend("topleft", legend = c("left:fore", "left:hind", "none", "right:fore", "right:hind"),fill=c(1,2,3,4,5))

#Correlation of PC3-PC4
plot(data$pc3, data$pc4, col = data$lameLeg, main = "Correlation of PC3 and PC4", xlab = "PC3", ylab = "PC4")
legend("topleft", legend = c("left:fore", "left:hind", "none", "right:fore", "right:hind"),fill=c(1,2,3,4,5))


