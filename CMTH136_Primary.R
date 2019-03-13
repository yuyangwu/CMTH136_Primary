rawData<-read.csv("/Users/Ashley/Downloads/Suicide_Rate_Data.csv")
Data_clean<- rawData[rawData$Unemployed!=-9999 & rawData$Uninsured!=-2222,]
attach(Data_clean)
sd(Population_Size)
boxplot(Suicide, horizontal = TRUE)
Data_clean$Unempl_n<-Unemployed/Population_Size
Data_clean$MD_n<-Major_Depression/Population_Size
Data_clean$UI_n<- Uninsured/Population_Size
Data_clean$Drug_n<- Recent_Drug_Use/Population_Size
attach(Data_clean)
boxplot(Data_clean$Suicide, horizontal = TRUE, xlab="Suicide rate", main="boxplot of suicide rate")
hist(Data_clean$Unempl_n,xlab ="Unemployment Rate",main="Unemployment")
hist(MD_n, xlab ="Major Depression Rate",main="Major Depression")
hist(Drug_n, xlab ="Drug Usage Rate",main="Drug Usage")
hist(UI_n, xlab ="Uninsured Rate",main="Uninsured")
hist(Prim_Care_Phys_Rate, xlab ="Primary Care per 100000 ppl",main="Primary Care")
hist(Suicide,xlab="Suicide per 100000 ppl", main="Suicide Rate")
suicide_level <- cut(Suicide,c(0,10,20,100))
levels(suicide_level)<- c("low","medium","high")
Data_clean$suicide_l <- suicide_level
Dataset<-c("Unempl_n","MD_n","UI_n", "Drug_n","Prim_Care_Phys_Rate")
Dataset2<-c("suicide_l","Suicide","Unemployed", "Major_Depression","Uninsured", "Recent_Drug_Use","Prim_Care_Phys_Rate","Unempl_n","MD_n","UI_n", "Drug_n")
attach(Data_clean)
Data_sum <- Data_clean[Dataset2]

count(suicide_l)
#Correlation between independent variables.
Dataset_cor <- Data_clean[Dataset]
cor_indep<-cor(Dataset_cor)
symnum(cor_indep)
#No independent variables highly correlated with each other. 

library(nnet)
#Suicide with primary health care rate
M_prim <- multinom(suicide_l ~ Prim_Care_Phys_Rate, reflevel="low")
summary(M_prim)
z_prim <- summary(M_prim)$coefficients/summary(M_prim)$standard.errors
p_prim<- (1 - pnorm(abs(z_prim), 0, 1))*2
p_prim
M_prim_pred <- predict(M_prim,type="probs")
#Graph
plot(NA,xlim = c(min(Prim_Care_Phys_Rate), max(Prim_Care_Phys_Rate)), ylim = c(0, 1),xlab = "Prim_Care_Rate", ylab = "Predicted Probability", main = "Primary Care Rate")
lines(Data_clean$Prim_Care_Phys_Rate, M_prim_pred[, 1], col = "green", lty = 1)
lines(Data_clean$Prim_Care_Phys_Rate, M_prim_pred[, 2], col = "blue", lty = 2)
lines(Data_clean$Prim_Care_Phys_Rate, M_prim_pred[, 3], col = "red", lty = 3)
text(50, 0.4, "low", col = "green")
text(300, 0.2, "high", col = "red")
text(70, 0.85, "medium", col = "blue")

#Suicide with Uninsured rate
M_unin <- multinom(suicide_l ~ UI_n)
summary(M_unin)
z_unin <- summary(M_unin)$coefficients/summary(M_unin)$standard.errors
p_unin<- (1 - pnorm(abs(z_unin), 0, 1))*2
p_unin
M_unin <- predict(M_unin,type="probs")
#Graph
plot(NA,xlim = c(min(UI_n), max(UI_n)), ylim = c(0, 1),xlab = "Uninsured_rate", ylab = "Predicted Probability", main="Uninsured Rate")
lines(Data_clean$UI_n, M_unin[, 1], col = "green", lty = 1)
lines(Data_clean$UI_n, M_unin[, 2], col = "blue", lty = 2)
lines(Data_clean$UI_n, M_unin[, 3], col = "red", lty = 3)
text(0.5, 0.4, "low", col = "green", pos = 1)
text(300, 0.2, "high", col = "red")
text(22, 0.85, "medium", col = "blue")

#Suicide with Unemloyed rate
M_unem <- multinom(suicide_l ~ Unempl_n)
summary(M_unem)
z_unem <- summary(M_unem)$coefficients/summary(M_unem)$standard.errors
z_unem
p_unem<- (1 - pnorm(abs(z_unem), 0, 1))*2
p_unem

#Suicide with Major Depression rate
M_madp <- multinom(suicide_l ~ MD_n)
summary(M_madp)
z_madp <- summary(M_madp)$coefficients/summary(M_madp)$standard.errors
p_madp<- (1 - pnorm(abs(z_madp), 0, 1))*2
p_madp
M_madp <- predict(M_madp,type="probs")
#Graph
plot(NA,xlim = c(min(MD_n), max(MD_n)), ylim = c(0, 1),xlab = "Major_Depression_rate", ylab = "Predicted Probability", main="Major Depression Rate")
lines(Data_clean$MD_n, M_madp[, 1], col = "green", lty = 1)
lines(Data_clean$MD_n, M_madp[, 2], col = "blue", lty = 2)
lines(Data_clean$MD_n, M_madp[, 3], col = "red", lty = 3)
text(35, 0.4, "y==low", col = "green")
text(300, 0.2, "y==high", col = "red")
text(22, 0.85, "y==medium", col = "blue")

#Suicide with Recent drug use rate
M_drug <- multinom(suicide_l ~ Drug_n)
summary(M_drug)
z_drug <- summary(M_drug)$coefficients/summary(M_drug)$standard.errors
p_drug<- (1 - pnorm(abs(z_drug), 0, 1))*2
p_drug
M_drug <- predict(M_drug,type="probs")
#Graph
plot(NA,xlim = c(min(Drug_n), max(Drug_n)), ylim = c(0, 1),xlab = "Drug_Use_rate", ylab = "Predicted Probability", main="Drug Usage Rate")
lines(Data_clean$Drug_n, M_drug[, 1], col = "green", lty = 1)
lines(Data_clean$Drug_n, M_drug[, 2], col = "blue", lty = 2)
lines(Data_clean$Drug_n, M_drug[, 3], col = "red", lty = 3)
text(35, 0.4, "y==low", col = "green")
text(300, 0.2, "y==high", col = "red")
text(22, 0.85, "y==medium", col = "blue")


#Unemployment rate is not significant, so we only use the other 4 variables for prediction model.

Model_mlg <- multinom(suicide_l ~ Prim_Care_Phys_Rate + MD_n+ UI_n + Drug_n)
summary(Model_mlg)
z <- summary(Model_mlg)$coefficients/summary(Model_mlg)$standard.errors
p<- (1 - pnorm(abs(z), 0, 1))*2
p

#Another m regression model with chi square and other numebrs
longdata = mlogit.data(Data_sum, choice = "suicide_l", shape = "wide")
model = mlogit(suicide_l ~ 1 | Prim_Care_Phys_Rate + UI_n +Drug_n +MD_n,data = longdata, reflevel = "low")
summary(model)

head(fitted(Model_mlg))
mlg_Er<-table(predict(Model_mlg),suicide_l)
sum(diag(mlg_Er))/sum(mlg_Er)
mlg_Er
Model_pred <- predict(Model_mlg,type="probs")
correct<-as.data.frame(Model_pred)
head(Model_pred)

library(ggplot2)

pred_data<-as.data.frame(cbind(Dataset_cor,correct))
pred_data2<-as.data.frame(cbind(pred_data, Data_clean$suicide_l))

cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.text.y=element_blank(),
                axis.ticks=element_blank(),
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                text = element_text(size=20),
                legend.key = element_blank())

hist = ggplot(pred_data2, aes(correct$medium, color = suicide_l, fill = suicide_l))
hist +
  cleanup +
  geom_dotplot(binwidth = 0.01, position="jitter") +
  coord_cartesian(xlim = c(0,1)) +
  xlab("Likelihood of medium") +
  ylab("Frequency") +
  scale_color_manual(values = c("Blue", "Green", "Red"),
                     labels = c("low", "medium", "high"),
                     name = "suicide_rate_level") +
  scale_fill_manual(values = c("Blue", "Green", "Red"),
                    labels = c("low", "medium", "high"),
                    name = "suicide_rate_level")

