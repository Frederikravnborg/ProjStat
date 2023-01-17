setwd("/Users/beheard/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/4. Semester/02445_ProjectStatisticalEvaluation/Project")
setwd("/Users/frederikravnborg/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/DTU-Frederik’s MacBook Pro/ProjStat/Project, Group/ProjStat")

data = read.table("horse_data23.txt")
dDia <- data
for (i in 1:85){
  if(dDia$lameLeg[i] == "right:fore" || dDia$lameLeg[i] == "left:hind"){
    dDia$lameLeg[i] <- "rflh"}
  else if (dDia$lameLeg[i] == "left:fore" || dDia$lameLeg[i] == "right:hind"){
    dDia$lameLeg[i] <- "lfrh"}
}
dDia$lameLeg <- as.factor(dDia$lameLeg)
dDia$horse <- as.factor(dDia$horse)

data$horse = as.factor(data$horse)
data$lameLeg = as.factor(data$lameLeg)
dDia

### DONT FUCK WITH THE COLORS! IT WORKS

#Correlation of AW
plot(data$A, data$W, col = data$lameLeg, main = "Correlation of A and W", xlab = "A", ylab = "W")
legend("topleft", legend = c("left:fore", "left:hind", "none", "right:fore", "right:hind"),fill=c(1,2,3,4,5))

#Correlation of PC3-PC4
plot(data$pc3, data$pc4, col = data$lameLeg, main = "Correlation of PC3 and PC4", xlab = "PC3", ylab = "PC4")
legend("topleft", legend = c("left:fore", "left:hind", "none", "right:fore", "right:hind"),fill=c(1,2,3,4,5))

# Correlation of right:fore and left:hind
plot(subset(data, lameLeg == "right:fore")$A, subset(data, lameLeg == "right:fore")$W)

par(mfrow=c(1,2))
#Correlation of AW
library("ggplot2")
require(gridExtra)

p1 <- ggplot(dDia,aes(x=A, y=W, group=lameLeg)) + geom_point() + geom_point(aes(color=lameLeg)) + scale_color_manual(values=c(6,3,7) ) + ggtitle("Correlation of A and W")

#Correlation of PC
p2 <- ggplot(dDia,aes(x=pc3, y=pc4, group=lameLeg)) + geom_point() + geom_point(aes(color=lameLeg)) + scale_color_manual(values=c(6,3,7) ) + ggtitle("Correlation of PC3 and PC4")

grid.arrange(p1, p2, ncol=2)


