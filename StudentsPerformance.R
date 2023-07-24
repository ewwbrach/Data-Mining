
# Załadowanie bibliotek
library(readr)
library(readxl)
library(Rcmdr)
library(psych)
library(dummies)
library(corrplot)
library(Hmisc)
library(caret)
library(MLmetrics)

# Ładowanie danych


data <- read_csv("C:/Users/acer/Desktop/StudentsPerformance.csv",comment = "#", skip = 2,show_col_types = FALSE)
data<-as.data.frame(data)
names(data)<-c("gender","race","parent_lvl_edu","lunch","test_prep_course","math_score","reading_score","writing_score")


View(data)


sapply(data, function(x)(sum(is.na(x)))) # NA counts


summary(data)


attach(data)


boxplot(math_score)+title("Boxplot of math score") 
hist(math_score) 
shapiro.test(math_score)                                


boxplot(reading_score)+title("Boxplot of reading score") 
hist(reading_score) 
shapiro.test(reading_score)                                


boxplot(writing_score)+title("Boxplot of writing score") 
hist(writing_score) 
shapiro.test(writing_score)                                




boxplot(math_score~test_prep_course,data=data, col=rainbow(7), xlab="Test Preparation Course", ylab="Math Score")

boxplot(reading_score~test_prep_course,data=data, col=rainbow(9), xlab="Test Preparation Course", ylab="Reading Score")

boxplot(writing_score~test_prep_course,data=data, col=rainbow(5), xlab="Test Preparation Course", ylab="Writing Score")



library(tidyverse)   
library(ggplot2)


boxplot(math_score~gender,data=data, col=rainbow(3), xlab="Gender", ylab="Math Score")   

 
boxplot(reading_score~gender,data=data, col=rainbow(9), xlab="Gender", ylab="Reading Score") 
  


boxplot(writing_score~gender,data=data, col=rainbow(9), xlab="Gender", ylab="Writing Score") 
  


mutate(data, score=rowMeans(data[,6:8])) -> data_new  #let's calculate average score.

x <- subset(data_new, (parent_lvl_edu=="bachelor's degree" | parent_lvl_edu=="some college"  
                       | parent_lvl_edu=="master's degree" | parent_lvl_edu=="associate's degree" 
                       | parent_lvl_edu=="high school"  | parent_lvl_edu=="some high school") )  
model<-aov(data_new$score~data_new$parent_lvl_edu,data = x)
summary(model)  
TukeyHSD(model)
ggplot(data=data_new, aes(y=score, x= parent_lvl_edu))+geom_boxplot(aes(fill=parent_lvl_edu))+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ggtitle("Average Score V/S Parent Education")+xlab("Parent Education Level")+ylab("Average Score")



ggplot(data, aes(x = math_score, y = reading_score)) + geom_point()    # math.score & reading.sccore have a positive correlation.
m1<-lm(math_score~ reading_score, data = data )
summary(m1)
ggplot(data, aes(x = writing_score, y = reading_score)) + geom_point() # writing.score & reading.score have a positive correlation.
m2<-lm(writing_score~reading_score, data=data)
summary(m2)
ggplot(data, aes(x = math_score, y = writing_score)) + geom_point()    # math.score & reading.sccore have a positive correlation.
m3<-lm(writing_score~math_score,data=data)
summary(m3)


#------------------------------------------------------------------------------------------------------
data <- data %>% mutate(percent = (math_score + reading_score+writing_score)/3)

TukeyHSD(model)
ggplot(data=data, aes(x=race, y=percent ))+
  geom_boxplot(aes(fill=race))+theme(axis.text.x = element_text(angle = 90, hjust = 1))
  scale_y_continuous(limits=c(0,110),breaks = seq(0,110,10))

 


#------------------------------------------------------------------------------
library(psych)
library(naivebayes)
#-------------------------- Naive Bayes -----------------------
#-------------------------- gender --------------------------

stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$gender)
boxplot(math_score~gender)
boxplot(writing_score~gender)
boxplot(reading_score~gender)
#podzbiory
#sets<-sample(nrow(data),996)
sets <- sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]



#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,1]
test_stud_class<-data[-sets,1]



#model z wyliczanymi prawdopodobienstwami apriori 

nb_stud1<-naive_bayes(data.gender ~.,train_stud)
summary(nb_stud1)


nb_stud1_pred<-predict(nb_stud1, test_stud[,-4],type="class")
nb_stud1_pred_prob<-predict(nb_stud1,test_stud,type="prob")

get_cond_dist(nb_stud1)

t_nb_stud1<-table(test_stud[,4],nb_stud1_pred)
confusionMatrix(t_nb_stud1)
acc_nb_stud_1<-mean(test_stud[,4] == nb_stud1_pred)




#-------------------------------race/ethnicity-------------------------------------
boxplot(math_score~race)
boxplot(reading_score~race)
boxplot(writing_score~race)
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$race)

sets <- sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,2]
test_stud_class<-data[-sets,2]

nb_stud3<-naive_bayes(data.race ~.,train_stud)
summary(nb_stud3)

nb_stud3_pred<-predict(nb_stud3, test_stud[,-4],type="class")
nb_stud3_pred_prob<-predict(nb_stud3,test_stud,type="prob")

get_cond_dist(nb_stud3)

t_nb_stud3<-table(test_stud[,4],nb_stud3_pred)
acc_nb_stud_3<-mean(test_stud[,4] == nb_stud3_pred)
confusionMatrix(t_nb_stud3)



#---------------------------------------parent_lvl_edu--------------------------------
boxplot(math_score~parent_lvl_edu)
boxplot(reading_score~parent_lvl_edu)
boxplot(writing_score~parent_lvl_edu)
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$parent_lvl_edu)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,3]
test_stud_class<-data[-sets,3]

nb_stud5<-naive_bayes(data.parent_lvl_edu ~.,train_stud)
summary(nb_stud5)

nb_stud5_pred<-predict(nb_stud5, test_stud[,-4],type="class")
nb_stud5_pred_prob<-predict(nb_stud5,test_stud,type="prob")

get_cond_dist(nb_stud5)

t_nb_stud5<-table(test_stud[,4],nb_stud5_pred)
acc_nb_stud_5<-mean(test_stud[,4] == nb_stud5_pred)

confusionMatrix(t_nb_stud5)

#--------------------------------------------lunch-------------------------------------------------
boxplot(math_score~lunch)
boxplot(reading_score~lunch)
boxplot(writing_score~lunch)

stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$lunch)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,4]
test_stud_class<-data[-sets,4]

nb_stud7<-naive_bayes(data.lunch ~.,train_stud)
summary(nb_stud7)

nb_stud7_pred<-predict(nb_stud7, test_stud[,-4],type="class")
nb_stud7_pred_prob<-predict(nb_stud7,test_stud,type="prob")

get_cond_dist(nb_stud7)

t_nb_stud7<-table(test_stud[,4],nb_stud7_pred)
acc_nb_stud_7<-mean(test_stud[,4] == nb_stud7_pred)

confusionMatrix(t_nb_stud7)

#-------------------------------------------test_prep_course-------------------------------------------
boxplot(math_score~test_prep_course)
boxplot(reading_score~test_prep_course)
boxplot(writing_score~test_prep_course)
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$test_prep_course)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,5]
test_stud_class<-data[-sets,5]

nb_stud9<-naive_bayes(data.test_prep_course ~.,train_stud)
summary(nb_stud9)

nb_stud9_pred<-predict(nb_stud9, test_stud[,-4],type="class")
nb_stud9_pred_prob<-predict(nb_stud9,test_stud,type="prob") #ROC

get_cond_dist(nb_stud9)

t_nb_stud9<-table(test_stud[,4],nb_stud9_pred)
acc_nb_stud_9<-mean(test_stud[,4] == nb_stud9_pred)

confusionMatrix(t_nb_stud9)


#------------------------------------------- K-NN ----------------------------------------------------------
#------------------------------------------- gender --------------------------------------------------------
library(class)
library(psych)
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$gender)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,1]
test_stud_class<-data[-sets,1]


model_stud_g3<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=8)
summary(model_stud_g3)

#wyliczenie macierzy pomy?ek i jako?ci modelu
t_stud_g3<-table(test_stud_class,model_stud_g3)
confusionMatrix(t_stud_g3)
acc_ir_g3<-mean(test_stud_class == model_stud_g3)

model_stud_g5<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

t_stud_g5<-table(test_stud_class,model_stud_g5)
confusionMatrix(t_stud_g5)
acc_ir_g5<-mean(test_stud_class == model_stud_g5)


model_stud_g7<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=8)

t_stud_g7<-table(test_stud_class,model_stud_g7)
confusionMatrix(t_stud_g7)
acc_ir_g7<-mean(test_stud_class == model_stud_g7)


#-------------------------------------------- race/enthicity ---------------------------------------------------
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$race)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,2]
test_stud_class<-data[-sets,2]


model_stud_r3<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=9)

#wyliczenie macierzy pomy?ek i jako?ci modelu
t_stud_r3<-table(test_stud_class,model_stud_r3)
confusionMatrix(t_stud_r3)
acc_ir_r3<-mean(test_stud_class == model_stud_r3)

model_stud_r5<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

t_stud_r5<-table(test_stud_class,model_stud_r5)
confusionMatrix(t_stud_r5)
acc_ir_r5<-mean(test_stud_class == model_stud_r5)


model_stud_r7<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=7)

t_stud_r7<-table(test_stud_class,model_stud_r7)
confusionMatrix(t_stud_r7)
acc_ir_r7<-mean(test_stud_class == model_stud_r7)

#-------------------------------------------- parent_lvl_edu ---------------------------------------------------------
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$parent_lvl_edu)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,3]
test_stud_class<-data[-sets,3]


model_stud_p3<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=15)

#wyliczenie macierzy pomy?ek i jako?ci modelu
t_stud_p3<-table(test_stud_class,model_stud_p3)
confusionMatrix(t_stud_p3)
acc_ir_p3<-mean(test_stud_class == model_stud_p3)

model_stud_p5<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

t_stud_p5<-table(test_stud_class,model_stud_p5)
confusionMatrix(t_stud_p5)
acc_ir_p5<-mean(test_stud_class == model_stud_p5)


model_stud_p7<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=7)

t_stud_p7<-table(test_stud_class,model_stud_p7)
confusionMatrix(t_stud_p7)
acc_ir_p7<-mean(test_stud_class == model_stud_p7)

#----------------------------------------------- lunch --------------------------------------------------------------------------
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$lunch)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,4]
test_stud_class<-data[-sets,4]


model_stud_l3<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

#wyliczenie macierzy pomy?ek i jako?ci modelu
t_stud_l3<-table(test_stud_class,model_stud_l3)
confusionMatrix(t_stud_l3)
acc_ir_l3<-mean(test_stud_class == model_stud_l3)

model_stud_l5<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

t_stud_l5<-table(test_stud_class,model_stud_l5)
confusionMatrix(t_stud_l5)
acc_ir_l5<-mean(test_stud_class == model_stud_l5)


model_stud_l7<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=7)

t_stud_l7<-table(test_stud_class,model_stud_l7)
confusionMatrix(t_stud_l7)
acc_ir_l7<-mean(test_stud_class == model_stud_l7)

#------------------------------------------- test_prep_course -------------------------------------------------------------------
stand <-function(x) { (x -mean(x))/(sd(x))}
students_std<-as.data.frame(lapply(data[,c(6,7,8)],stand))
students_std_sp<-data.frame(students_std,data$test_prep_course)

#podzbiory
sets<-sample(1:nrow(data), 0.75 * nrow(data))
train_stud<-students_std_sp[sets,]
test_stud <-students_std_sp[-sets,]

#klasy poszczeg?lnych obserwacji
train_stud_class<-data[sets,5]
test_stud_class<-data[-sets,5]


model_stud_t3<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=3)

#wyliczenie macierzy pomy?ek i jako?ci modelu
t_stud_t3<-table(test_stud_class,model_stud_t3)
confusionMatrix(t_stud_t3)
acc_ir_t3<-mean(test_stud_class == model_stud_t3)

model_stud_t5<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=5)

t_stud_t5<-table(test_stud_class,model_stud_t5)
confusionMatrix(t_stud_t5)
acc_ir_t5<-mean(test_stud_class == model_stud_t5)


model_stud_t7<-knn(train=train_stud[, 1:3],test=test_stud[,1:3], cl=train_stud_class,k=7)

t_stud_t7<-table(test_stud_class,model_stud_t7)
confusionMatrix(t_stud_t7)
acc_ir_t7<-mean(test_stud_class == model_stud_t7)
