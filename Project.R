# Create a new row with the specified values
new_row <- data.frame(
  gender = "female",
  race = "group A",
  parentaledu = "master's degree",
  lunch="standard",
  prepcourse = "completed",
  mathS = 63.6,
  readingS = 79.6,
  writingS = 79.2
)
# Add the new row to the data dataframe
StudentPerformance <- rbind(StudentPerformance, new_row)
data=StudentPerformance

data$AvgScore=(data$mathS+data$readingS+data$writingS)/3
data$Parentaledu <- ifelse(data$parentaledu %in% c("some high school","high school" ), "School",
                           ifelse(data$parentaledu %in% c("associate's degree","some college"), "Baseline College", "Quality College"))


hist(data$readingS,prob=TRUE,col='sky blue',xlab='Reading Scores',main='Histogram of Reading Scores')
curve(dnorm(x,mean(data$readingS),sd(data$readingS)),add=TRUE,lwd=2)
hist(data$mathS,prob=TRUE,col='sky blue',xlab='Math Scores',main='Histogram of Math Scores')
curve(dnorm(x,mean(data$mathS),sd(data$mathS)),add=TRUE,lwd=2)
hist(data$writingS,prob=TRUE,col='sky blue',xlab='Writing Scores',main='Histogram of Writing Scores')
curve(dnorm(x,mean(data$writingS),sd(data$writingS)),add=TRUE,lwd=2)
hist(data$AvgScore,prob=TRUE,col='sky blue',xlab='Average Scores',main='Histogram of Average Scores')
curve(dnorm(x,mean(data$AvgScore),sd(data$AvgScore)),add=TRUE,lwd=2)
unique(data$parentaledu)
hist(data[data$gender=='female',]$AvgScore,prob=TRUE)
summary(data[data$gender=='female',]$AvgScore)
hist(data[data$gender=='male',]$AvgScore,prob=TRUE)
summary(data[data$gender=='male',]$AvgScore)
hist(data[data$gender=='female',]$mathS,prob=TRUE)
summary(data[data$gender=='female',]$mathS)
hist(data[data$gender=='male',]$mathS,prob=TRUE)
summary(data[data$gender=='male',]$mathS)
hist(data[data$prepcourse=='none',]$AvgScore,prob=TRUE)
summary(data[data$prepcourse=='none',]$AvgScore)
hist(data[data$prepcourse=='completed',]$AvgScore,prob=TRUE)
summary(data[data$prepcourse=='completed',]$AvgScore)

library(ggplot2)
# Create boxplot of initial vs final levels of Parental education average scores
boxplotf <- ggplot(data, aes(x = Parentaledu, y = AvgScore, fill = parentaledu)) +
  geom_boxplot() +
  labs(x = "Parental Education Initial and Final levels", y = "Average Score", fill = "Parental Education Initial levels") +
  ggtitle("Boxplot of Average Scores by levels of Parental Education") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the boxplot
print(boxplotf)

# Create boxplot of initial vs final levels of Parental education math scores
boxplotf1 <- ggplot(data, aes(x = Parentaledu, y = mathS, fill = parentaledu)) +
  geom_boxplot() +
  labs(x = "Parental Education Initial and Final levels", y = "Math Score", fill = "Parental Education Initial levels") +
  ggtitle("Boxplot of Math Scores by levels of Parental Education") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the boxplot
print(boxplotf1)

# Create boxplot of initial vs final levels of Parental education reading scores
boxplotf2 <- ggplot(data, aes(x = Parentaledu, y = readingS, fill = parentaledu)) +
  geom_boxplot() +
  labs(x = "Parental Education Initial and Final levels", y = "Reading Score", fill = "Parental Education Initial levels") +
  ggtitle("Boxplot of Reading Scores by levels of Parental Education") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the boxplot
print(boxplotf2)

# Create boxplot of initial vs final levels of Parental education writing scores
boxplotf3 <- ggplot(data, aes(x = Parentaledu, y = writingS, fill = parentaledu)) +
  geom_boxplot() +
  labs(x = "Parental Education Initial and Final levels", y = "Writing Score", fill = "Parental Education Initial levels") +
  ggtitle("Boxplot of Writing Scores by levels of Parental Education") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the boxplot
print(boxplotf3)
scatter_plot1 <- ggplot(data, aes(x = mathS, y = readingS, color = gender)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black",lwd=1) +  # Add y = x line
  labs(x = "Math Score", y = "Reading Score", color = "Gender") +
  ggtitle("Scatter Plot of Math vs. Reading Scores by Gender") +
  theme_dark()
# Display the scatter plot
print(scatter_plot1)

scatter_plot2 <- ggplot(data, aes(x = writingS, y = readingS, color = gender)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black",lwd=1) +  # Add y = x line
  labs(x = "Writing Score", y = "Reading Score", color = "Gender") +
  ggtitle("Scatter Plot of Writing vs. Reading Scores by Gender") +
  theme_dark()
# Display the scatter plot
print(scatter_plot2)

scatter_plot3 <- ggplot(data, aes(x = mathS, y = writingS, color = gender)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black",lwd=1) +  # Add y = x line
  labs(x = "Math Score", y = "Writing Score", color = "Gender") +
  ggtitle("Scatter Plot of Math vs. Writing Scores by Gender") +
  theme_dark()
# Display the scatter plot
print(scatter_plot3)

#boxplot of average scores with prep course and gender
boxplot1 <- ggplot(data, aes(x = prepcourse, y = AvgScore, fill = gender)) +
  geom_boxplot() +
  labs(x = "Preparatory Course", y = "Average Score", fill = "Gender") +
  ggtitle("Boxplot of Average Scores by Preparatory Course") +
  theme_minimal()
# Display the boxplot
print(boxplot1)

boxplot2 <- ggplot(data, aes(x = prepcourse, y = mathS, fill = gender)) +
  geom_boxplot() +
  labs(x = "Preparatory Course", y = "Math Score", fill = "Gender") +
  ggtitle("Boxplot of Math Scores by Preparatory Course") +
  theme_minimal()

# Display the boxplot
print(boxplot2)

boxplot3 <- ggplot(data, aes(x = prepcourse, y = readingS, fill = gender)) +
  geom_boxplot() +
  labs(x = "Preparatory Course", y = "Reading Score", fill = "Gender") +
  ggtitle("Boxplot of Reading Scores by Preparatory Course") +
  theme_minimal()

# Display the boxplot
print(boxplot3)

boxplot4 <- ggplot(data, aes(x = prepcourse, y = writingS, fill = gender)) +
  geom_boxplot() +
  labs(x = "Preparatory Course", y = "Writing Score", fill = "Gender") +
  ggtitle("Boxplot of Writing Scores by Preparatory Course") +
  theme_minimal()

# Display the boxplot
print(boxplot4)
boxplot2 <- ggplot(data, aes(x = lunch, y = AvgScore, fill = Parentaledu)) +
  geom_boxplot() +
  labs(x = "Lunch", y = "Average Score", fill = "Parental Education") +
  ggtitle("Boxplot of Average Scores by Lunch") +
  theme_minimal()
# Display the boxplot
print(boxplot2)
# Compute correlation matrix
correlation_matrix <- cor(data[, c("mathS", "readingS","writingS")])
colnames(correlation_matrix) <- c("Math Scores", "Writing Scores", "Reading Scores")
rownames(correlation_matrix) <- c("Math Scores", "Writing Scores", "Reading Scores")
# Plot correlation heatmap
library(corrplot)
heatmap_plot <- corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust", addrect = 4)
# Display the correlation heatmap
print(heatmap_plot)

scatter_plot4 <- ggplot(data, aes(x = mathS, y = writingS, color = race)) +
  geom_point() +
  labs(x = "Math Score", y = "Writing Score", color = "Race") +
  ggtitle("Scatter Plot of Math vs. Writing Scores by Race") +
  theme_dark()
# Display the scatter plot
print(scatter_plot4)

scatter_plot5 <- ggplot(data, aes(x = mathS, y = readingS, color = race)) +
  geom_point() +
  labs(x = "Math Score", y = "Reading Score", color = "Race") +
  ggtitle("Scatter Plot of Math vs. Reading Scores by Race") +
  theme_dark()
# Display the scatter plot
print(scatter_plot5)

scatter_plot6 <- ggplot(data, aes(x = writingS, y = readingS, color = race)) +
  geom_point() +
  labs(x = "Writing Score", y = "Reading Score", color = "Race") +
  ggtitle("Scatter Plot of Writing vs. Reading Scores by Race") +
  theme_dark()
# Display the scatter plot
print(scatter_plot6)

boxplot5 <- ggplot(data, aes(x = race, y = writingS, fill = race)) +
  geom_boxplot() +
  labs(x = "Race", y = "Writing Score", fill = "Race") +
  ggtitle("Boxplot of Writing Scores by Race") +
  theme_minimal()
# Display the boxplot
print(boxplot5)

data$gender <- factor(data$gender, levels = c("male", "female"))
data$race <- factor(data$race, levels = c("group A", "group B", "group C", "group D", "group E"))
data$Parentaledu<-factor(data$Parentaledu,levels=c("School","Baseline College","Quality College"))
data$prepcourse<-factor(data$prepcourse,levels=c("none","completed"))
library(tidyverse)
library(magrittr)
library(dplyr)
library(conflicted)
library(agricolae)

set.seed(105)
s_data1<- data %>%
  group_by(gender, Parentaledu,race,prepcourse) %>%
  sample_n(1) %>%
  ungroup()
#Four-way Anova using gender,Parentaledu,race,prep course with response as math scores
anova_result1 <- aov(s_data1$mathS ~ s_data1$gender*s_data1$race*s_data1$Parentaledu*s_data1$prepcourse- s_data1$gender:s_data1$race:s_data1$Parentaledu:s_data1$prepcourse
                      ,data=s_data1)
summary(anova_result1)
fisher_lsd_test1 <- LSD.test(anova_result1,"s_data1$prepcourse", alpha = 0.05)
print(fisher_lsd_test1)
#keeping math scores as response variables, 2 way layout on parentaledu and gender for each level of race
data1=s_data1[s_data1$race=='group A',]

s_data11<- data1 %>%
  group_by(Parentaledu,gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result11 <- aov(s_data11$mathS ~ s_data11$Parentaledu*s_data11$gender
                  ,data=s_data11)
summary(anova_result11)

data2=s_data1[s_data1$race=='group B',]
#install.packages("agricolae")  # Install agricolae package
library(agricolae)
s_data12<- data2 %>%
  group_by(Parentaledu,gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result12 <- aov(s_data12$mathS ~ s_data12$Parentaledu*s_data12$gender
                      ,data=s_data12)
summary(anova_result12)
fisher_lsd_test12 <- LSD.test(anova_result12, "s_data12$Parentaledu", alpha = 0.05)
print(fisher_lsd_test12)


data3=s_data1[s_data1$race=='group C',]

s_data13<- data3 %>%
  group_by(Parentaledu,gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result13 <- aov(s_data13$mathS ~ s_data13$Parentaledu*s_data13$gender
                      ,data=s_data13)
summary(anova_result13)
data31=data3[data3$gender=='male',]
s_data131<- data31 %>%
  group_by(Parentaledu) %>%
  sample_n(2) %>%
  ungroup()
anova_result131 <- aov(s_data131$mathS ~ s_data131$Parentaledu
                      ,data=s_data131)
summary(anova_result131)
fisher_lsd_test131 <- LSD.test(anova_result131,"s_data131$Parentaledu", alpha = 0.05)
print(fisher_lsd_test131)

data32=data3[data3$gender=='female',]
s_data132<- data32 %>%
  group_by(Parentaledu) %>%
  sample_n(2) %>%
  ungroup()
anova_result132 <- aov(s_data132$mathS ~ s_data132$Parentaledu
                       ,data=s_data132)
summary(anova_result132)

data4=s_data1[s_data1$race=='group D',]

s_data14<- data4 %>%
  group_by(Parentaledu,gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result14 <- aov(s_data14$mathS ~ s_data14$Parentaledu*s_data14$gender
                      ,data=s_data14)
summary(anova_result14)

data5=s_data1[s_data1$race=='group E',]

s_data15<- data5 %>%
  group_by(Parentaledu,gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result15 <- aov(s_data15$mathS ~ s_data15$Parentaledu*s_data15$gender
                      ,data=s_data15)
summary(anova_result15)



#Four-way Anova using gender,Parentaledu,race,prep course with response as reading scores
anova_result2 <- aov(s_data1$readingS ~ s_data1$gender*s_data1$Parentaledu*s_data1$race*s_data1$prepcourse-s_data1$gender:s_data1$race:s_data1$Parentaledu:s_data1$prepcourse
                     ,data = s_data1)
summary(anova_result2)
fisher_lsd_test2 <- LSD.test(anova_result2,"s_data1$prepcourse", alpha = 0.05)
print(fisher_lsd_test2)
#keeping reading scores as response variables, 2 way layout on gender and parental education for each level of race
#group A:
anova_result21 <- aov(s_data11$readingS ~ s_data11$Parentaledu*s_data11$gender
                      ,data=s_data11)
summary(anova_result21)
#group B:
anova_result22 <- aov(s_data12$readingS ~ s_data12$Parentaledu*s_data12$gender
                      ,data=s_data12)
summary(anova_result22)
fisher_lsd_test22 <- LSD.test(anova_result22,"s_data12$Parentaledu", alpha = 0.05)
print(fisher_lsd_test22)

#group C:
anova_result23 <- aov(s_data13$readingS ~ s_data13$Parentaledu*s_data13$gender
                      ,data=s_data13)
summary(anova_result23)

#group D:
anova_result24 <- aov(s_data14$readingS ~ s_data14$Parentaledu*s_data14$gender
                      ,data=s_data14)
summary(anova_result24)

#group E:
anova_result25 <- aov(s_data15$readingS ~ s_data15$Parentaledu*s_data15$gender
                      ,data=s_data15)
summary(anova_result25)



#Four-way Anova using gender,Parentaledu,race,prep course with response as writing scores
anova_result3 <- aov(s_data1$writingS ~ s_data1$gender*s_data1$race*s_data1$Parentaledu*s_data1$prepcourse-s_data1$gender:s_data1$race:s_data1$Parentaledu:s_data1$prepcourse
                     ,data = s_data1)
summary(anova_result3)
fisher_lsd_test3 <- LSD.test(anova_result3,"s_data1$prepcourse", alpha = 0.05)
print(fisher_lsd_test3)
#keeping writing scores as response variables, 2 way layout on gender and parental education for each level of race
#group A:
anova_result31 <- aov(s_data11$writingS ~ s_data11$Parentaledu*s_data11$gender
                      ,data=s_data11)
summary(anova_result31)
#group B:
anova_result32 <- aov(s_data12$writingS ~ s_data12$Parentaledu*s_data12$gender
                      ,data=s_data12)
summary(anova_result32)
#best level of parentaledu for B
fisher_lsd_test32 <- LSD.test(anova_result32,"s_data12$Parentaledu", alpha = 0.05)
print(fisher_lsd_test32)
#group C:
anova_result33 <- aov(s_data13$writingS ~ s_data13$Parentaledu*s_data13$gender
                      ,data=s_data13)
summary(anova_result33)
#for males:
anova_result331 <- aov(s_data131$writingS ~ s_data131$Parentaledu
                       ,data=s_data131)
summary(anova_result331)
#for females:
anova_result332 <- aov(s_data132$writingS ~ s_data132$Parentaledu
                       ,data=s_data132)
summary(anova_result332)

#for School parentaledu:
data310=data3[data3$Parentaledu=='School',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result333 <- aov(s_data1310$writingS ~ s_data1310$gender
                       ,data=s_data1310)
summary(anova_result333)
fisher_lsd_test333 <- LSD.test(anova_result333,"s_data1310$gender", alpha = 0.1)
print(fisher_lsd_test333)
#for Baseline college parentaledu:
data310=data3[data3$Parentaledu=='Baseline College',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result334 <- aov(s_data1310$writingS ~ s_data1310$gender
                       ,data=s_data1310)
summary(anova_result334)
#for Quality college parentaledu:
data310=data3[data3$Parentaledu=='Quality College',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result335<- aov(s_data1310$writingS ~ s_data1310$gender
                       ,data=s_data1310)
summary(anova_result335)

#group D:
anova_result34 <- aov(s_data14$writingS ~ s_data14$Parentaledu*s_data14$gender
                      ,data=s_data14)
summary(anova_result34)

#group E:
anova_result35 <- aov(s_data15$writingS ~ s_data15$Parentaledu*s_data15$gender
                      ,data=s_data15)
summary(anova_result35)


#Four-way Anova using gender,Parentaledu,race,prep course with response as average scores
anova_result4 <- aov(s_data1$AvgScore ~ s_data1$gender*s_data1$race*s_data1$Parentaledu*s_data1$prepcourse-s_data1$gender:s_data1$race:s_data1$Parentaledu:s_data1$prepcourse
                     ,data = s_data1)
summary(anova_result4)
fisher_lsd_test4 <- LSD.test(anova_result4,"s_data1$prepcourse", alpha = 0.05)
print(fisher_lsd_test4)
#keeping average scores as response variables, 2 way layout on gender and parental education for each level of race
#group A:
anova_result41 <- aov(s_data11$AvgScore ~ s_data11$Parentaledu*s_data11$gender
                      ,data=s_data11)
summary(anova_result41)
#for group B:
anova_result42 <- aov(s_data12$AvgScore ~ s_data12$Parentaledu*s_data12$gender
                      ,data=s_data12)
summary(anova_result42)
fisher_lsd_test42 <- LSD.test(anova_result42,"s_data12$Parentaledu", alpha = 0.05)
print(fisher_lsd_test42)
#for group C:
anova_result43 <- aov(s_data13$AvgScore ~ s_data13$Parentaledu*s_data13$gender
                      ,data=s_data13)
summary(anova_result43)
#for males:
anova_result431 <- aov(s_data131$AvgScore ~ s_data131$Parentaledu
                       ,data=s_data131)
summary(anova_result431)
#for females:
anova_result432 <- aov(s_data132$AvgScore ~ s_data132$Parentaledu
                       ,data=s_data132)
summary(anova_result432)
#for School parentaledu:
data310=data3[data3$Parentaledu=='School',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result433 <- aov(s_data1310$AvgScore ~ s_data1310$gender
                       ,data=s_data1310)
summary(anova_result433)
fisher_lsd_test433 <- LSD.test(anova_result433,"s_data1310$gender", alpha = 0.1)
print(fisher_lsd_test433)
#for Baseline college parentaledu:
data310=data3[data3$Parentaledu=='Baseline College',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result434 <- aov(s_data1310$AvgScore ~ s_data1310$gender
                       ,data=s_data1310)
summary(anova_result434)
#for Quality college parentaledu:
data310=data3[data3$Parentaledu=='Quality College',]
s_data1310<- data310 %>%
  group_by(gender) %>%
  sample_n(2) %>%
  ungroup()
anova_result435<- aov(s_data1310$AvgScore ~ s_data1310$gender
                      ,data=s_data1310)
summary(anova_result435)
#group D
anova_result44 <- aov(s_data14$AvgScore ~ s_data14$Parentaledu*s_data14$gender
                      ,data=s_data14)
summary(anova_result44)
#group E
anova_result45 <- aov(s_data15$AvgScore ~ s_data15$Parentaledu*s_data15$gender
                      ,data=s_data15)
summary(anova_result45)
#for those with Parentaledu level 'School'
data1=s_data1[s_data1$Parentaledu=='School',]
set.seed(105)
s_data11<- data1 %>%
  group_by(gender,prepcourse) %>%
  sample_n(2) %>%
  ungroup()
anova_result46 <- aov(s_data11$AvgScore ~ s_data11$gender*s_data11$prepcourse
                      ,data=s_data11)
summary(anova_result46)
#for those with Parentaledu level 'Baseline College'
data1=s_data1[s_data1$Parentaledu=='Baseline College',]
set.seed(105)
s_data11<- data1 %>%
  group_by(gender,prepcourse) %>%
  sample_n(2) %>%
  ungroup()
anova_result47 <- aov(s_data11$AvgScore ~ s_data11$gender*s_data11$prepcourse
                      ,data=s_data11)
summary(anova_result47)
fisher_lsd_test471 <- LSD.test(anova_result47,"s_data11$prepcourse", alpha = 0.05)
print(fisher_lsd_test471)
fisher_lsd_test472 <- LSD.test(anova_result47,"s_data11$gender", alpha = 0.1)
print(fisher_lsd_test472)
#for those with Parentaledu level 'Quality College'
data1=s_data1[s_data1$Parentaledu=='Quality College',]
set.seed(105)
s_data11<- data1 %>%
  group_by(gender,prepcourse) %>%
  sample_n(2) %>%
  ungroup()
anova_result48 <- aov(s_data11$AvgScore ~ s_data11$gender*s_data11$prepcourse
                      ,data=s_data11)
summary(anova_result48)
