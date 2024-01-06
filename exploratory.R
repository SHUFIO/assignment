#Title: what impact students'experience?
#Data Preparation
library(clusterSim)
library(FactoMineR)
library(factoextra)
#Check current working directory and set up
getwd()
#Set working session
#Read the related CSV and show the first row
data<-read.csv("001.csv", sep=",", dec=".", header=TRUE)
head(data)
#Check if columns are numeric since PCA requirements
sapply(data, is.numeric)
#All of these variables are numeric
#Check if there is missing values
sort(colSums(is.na(data)))
#Shows no missing values
#KMO test to check if dataset support PCA
library(psych)
KMO(data)
#Overall  MSA=0.99 and it greater than 0.8
#Standardise the data before PCA
scaled.data<-scale(data,center=TRUE, scale=TRUE)
head(scaled.data)
#Calculate the covariance matrix
cov.data<-cov(scaled.data)
head(cov.data)
#Calculate the eigenvalues of covariance matrix
eigenvalues<-eigen(cov.data)$values
print(eigenvalues)
#Calculate the eigenvectors of covariance matrix
eigenvectors<-eigen(cov.data)$vectors
print(eigenvectors)
#Calculate Principle Components
pca_results<-prcomp(scaled.data, center=TRUE, scale=TRUE)
summary(pca_results)
#Extract the eigenvectors from the rotation matrix
eigenvectors<-pca_results$rotation
#Check the math behind PCA
result_matrix<-scaled.data%*%eigenvectors
#Extract the scores of principal components
scores<-pca_results$x
head(scores)
#Visualize scoresplot
plot(pca_results$x[,1],pca_results$x[,2])
#Visualize our components in 2 dimensional space
#PC1-PC2
fviz_pca_var(pca_results,col.var="black")
#As result, attention and difficulty contributes negatively on our dimension1 and all of the quality indicators contribute negatively to the dimension 2.
#PCA biplot
biplot(pca_results)
#How many components to extract?
#Method1:Select the smallest number of components that together hold about 80-90% of the total variances.
#As it can be seen that PC1~PC5 can hold greater than 80% of total variance
#Method2: Use Kaiser criterian, where we retain components with a variance(eigenvalue) greater than the average of eigenvalues
get_eig(pca_results)
#As it can be seen above only first 5 components greater than 1
#Method3:Use a screeplot
fviz_eig(pca_results, addlables=TRUE, ylim=c(0,75))
#Method4: Parallel analysis
library(paran)
paran(scaled.data)
#As a result, we retain 2 components.

#PC1 primarily measures the impact of instructors to students' experience
#PC2 primarily measures the quality of courses to students' experience.
# ANOVA
#Transform certain variable into categorical
data$instr<-as.factor(data$instr)
data$class<-as.factor(data$class)
data$nb.repeat<-as.factor(data$nb.repeat)
data$attendance<-as.factor(data$attendance)
data$difficulty<-as.factor(data$difficulty)
#Check parametric assumptions
#assumption1:normality
#method1:shapiro.test
shapiro_test_result<-shapiro.test(scores[,1])
shapiro_test_result<-shapiro.test((scores[,2])
#method2: qqplot(qualitatively check)
qqnorm(scores[,1])
qqline(scores[,1], col=2)
qqnorm(scores[,2])
qqline(scores[,2], col=2)
#As a result, the qqplot appears as roughly a straight line showing normal distribution
#assumption2: homogeneity of variances
#method1:LeveneTest(gold standard)
library(car)
leveneTest(scores[,1]~data$instr)
#Levene's test for homogeneity of variance was significant[F(2,5817)=10.225,p<0.001]
leveneTest(scores[,2]~data$instr)
leveneTest(scores[,1]~data$class)
leveneTest(scores[,2]~data$class)
#method2:box plot (qualitatively check)
boxplot(scores[,1]~data$instr)
boxplot(scores[,2]~data$instr)
boxplot(scores[,1]~data$class)
boxplot(scores[,2]~data$class)

# ANOVA Test: PC1&instructors
fit<-aov(scores[,1]~data$instr)
summary(fit)
library(lsr)
etaSquared(fit)
#Report:according to ANOVA, there is a significant but very small effect of instructors on PC1
#PC1[F(2,5817)=60.91,p<0.001, eta-squared=0.02051292]

#since ANOVA test is an omnibus test,we shall Run a Tukey HSD test to see where the differences are
TukeyHSD(fit)
#Plotting the graph with intervals CI
library(sjPlot)
model<-lm(scores[,1]~instr, data=data)
plot_model(model,type="pred",terms=c("instr"))
#In conclusion, instr2 does not differ significantly from instr1 in terms of PC1

#ANOVA Test: PC2&classes
fit2<-aov(scores[,2]~data$class)
summary(fit2)
etaSquared(fit2)
#Run TukeyHSD test
TukeyHSD(fit2)
#Check with plots
model2<-lm(scores[,2]~class, data=data)
plot_model(model2,type="pred", terms=c("class"))

