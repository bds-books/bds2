########################################################################################
# December 2025
#
# The following R code allows to replicate all the analyses and examples
# in the book "Advanced Basketball Data Science - With Applications in R"
# (by P. Zuccolotto, M. Manisera and M. Sandri), CRC Press.
#
# All the R codes run assume that the reader has downloaded all the supplementary
# material (data, functions, code, and input objects), which is organized with a
# rigorous folder structure within a main folder called Rcode.
# The individual script files are stored in the subfolder of the chapter they
# refer to, which must be set as the working directory.
# In addition, the pacman package must be installed beforehand.
#
# The supplementary material can be downloaded from the book website
# http://bdsports.unibs.it/bds2
#####################################################################################

# The code has been tested on Windows and macOS 
# using R version 4.5.2 and RStudio version 2025.09.2+418, 
# and on Ubuntu 25.10 (64-bit, Linux kernel 6.17) 
# using R version 4.5.1 and RStudio version 2025.09.2+418.

########################################################################

# The package {\tt pacman} has to be installed
# install.packages("pacman")

# set Chapter7 as working directory
# setwd("....")


############################################
############################################
# PART 2                                   #
# Decoding motion                          #                            
############################################
############################################

##############################################
# CHAPTER 7                                  #
# Tracking and analyzing ball trajectories   #
##############################################

######################
# 7.2 Ball detection data analysis
######################

######################
# 7.2.4 Modeling phase
######################


rm(list=ls())
graphics.off()

pacman::p_load(plotly,ggplot2,dplyr,cutpointr)
load(file="./Data/traj_stats.Rdata")


p <- 0.8
n <- nrow(traj_stats)
set.seed(8765)
idx <- sample(1:n, size = floor(p * n))
df_train <- traj_stats[idx, ]
df_test  <- traj_stats[-idx, ]


########
# Logistic
# regression
########

frml <- made_miss ~ angle_dir + dist0b
logreg <- glm(frml, data=df_train, family="binomial")
summary(logreg)

df_test$logreg_pred <- predict(logreg, newdata=df_test, type="response")

cut <- cutpointr(data=df_test, x=logreg_pred, class=made_miss, pos_class="Made",
                 method=minimize_metric, metric=roc01)          		 
optcut <- cut$optimal_cutpoint

T <- 0.5  
pred_class <- factor(ifelse(df_test$logreg_pred > T, 1, 0),
                     labels=c("Miss","Made"))

conf_matrix <- table(Predicted = pred_class, Actual = df_test$made_miss)
print(conf_matrix)

TP <- conf_matrix["Made", "Made"]
TN <- conf_matrix["Miss", "Miss"]
FP <- conf_matrix["Made", "Miss"]
FN <- conf_matrix["Miss", "Made"]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy    <- (TP + TN) / sum(conf_matrix)

T <- 0.37  
pred_class <- factor(ifelse(df_test$logreg_pred > T, 1, 0),
                     labels=c("Miss","Made"))

conf_matrix <- table(Predicted = pred_class, Actual = df_test$made_miss)
print(conf_matrix)

TP <- conf_matrix["Made", "Made"]
TN <- conf_matrix["Miss", "Miss"]
FP <- conf_matrix["Made", "Miss"]
FN <- conf_matrix["Miss", "Made"]

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy    <- (TP + TN) / sum(conf_matrix)


pacman::p_load(pROC)
roc_obj <- roc(df_test$made_miss, df_test$logreg_pred)
AUC <- auc(roc_obj)
ci_AUC <- ci.auc(roc_obj)

auc_label <- data.frame(x = 0.05, y = 0.95,
                        label = sprintf("AUC=%.3f (95%% CI: %.3f-%.3f)", AUC, ci_AUC[1], ci_AUC[3]))

roc_df <- data.frame(spec = roc_obj$specificities,  
                     sens = roc_obj$sensitivities)

p12 <- ggplot(roc_df, aes(x = 1 - spec, y = sens)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_abline(slope=1, intercept=0, linetype="dashed", 
              colour="grey", linewidth = 1) +
  geom_text(data=auc_label, aes(x=x, y=y, label=label), hjust = 0) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  theme_bw()

print(p12)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/ROC.pdf", width=6, height=6, paper="special")
print(p12)
dev.off()
#################################################################


########
# CART
########
pacman::p_load(rpart, rattle)


########## first CART
set.seed(8765)
tree_fit <- rpart(made_miss ~ angle_dir+distVM, data = df_train, method = "class", 
                  control = rpart.control(cp = 0.0001)) 

fancyRpartPlot(tree_fit,sub="")

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/CARTnoprun.pdf", width=8, height=8, paper="special")
fancyRpartPlot(tree_fit,sub="")
dev.off()
#################################################################

cpt <- tree_fit$cptable
p13 <- ggplot(cpt, aes(x = CP, y = xerror)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=xerror-xstd, ymax=xerror+xstd),
                width = 0.01) +
  scale_x_log10() +
  labs(x = "cp (Complexity Parameter)",
       y = "Cross-validated classification error") +
  theme_bw()

print(p13)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/cp.pdf", width=5, height=5, paper="special")
print(p13)
dev.off()
#################################################################

min_xerror <- min(cpt[,"xerror"])
min_index <- which.min(cpt[,"xerror"])
xstd_at_min <- cpt[min_index, "xstd"]
threshold <- min_xerror + xstd_at_min
index_1se <- which(cpt[,"xerror"] <= threshold)[1]
opt_cp_1se <- cpt[index_1se, "CP"]

pruned_tree <- prune(tree_fit, cp = opt_cp_1se)
fancyRpartPlot(pruned_tree, digits=3,sub="")


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/CARTprun1.pdf", width=8, height=8, paper="special")
fancyRpartPlot(pruned_tree, digits=3,sub="")
dev.off()
#################################################################

df_test$tree_pred <- predict(pruned_tree, newdata=df_test)[, 2]
roc_obj <- roc(df_test$made_miss, df_test$tree_pred)
AUC <- auc(roc_obj)
ci_AUC <- ci.auc(roc_obj)

#### alluvial plot
df_alluv <- df_test %>%
  mutate(angle_dir_3cat = cut(angle_dir, breaks=c(0, 0.671, 1.06, Inf), 
                              labels=c("on target", "bad", "very bad")),
         distVM_3cat = cut(distVM, breaks=c(-Inf, -1.06, 0.143, +Inf), 
                           labels=c("too long","good","too short"))
  ) %>%
  group_by(made_miss, angle_dir_3cat, distVM_3cat) %>%
  summarise(n = n())


pacman::p_load(ggalluvial)

p_alluv <- ggplot(data = df_alluv,
                  aes(axis1 = angle_dir_3cat, axis2 = distVM_3cat, axis3=made_miss, y=n)) +
  geom_alluvium(aes(fill = made_miss), show.legend=F) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits=c("angle_dir", "distVM", "made/miss"), expand=c(0.15, 0.05)) +
  theme_bw() %+replace% theme(panel.border=element_blank(), axis.text.x=element_text(size=12),
                              axis.text.y=element_blank(), axis.ticks.y=element_blank())

print(p_alluv)

####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/alluvial.pdf", width=8, height=8, paper="special")
print(p_alluv)
dev.off()
#################################################################


########## second CART

df_train$distVMcat <- cut(df_train$distVM, breaks=c(-Inf,-1.06,0.143,Inf),
                          labels=c("too long","good","too short"))

set.seed(8765)						
tree_fit <- rpart(distVMcat ~ angle+isp.feet, data=df_train,
                  parms = list(prior = c("too long" = 1/3, "good" = 1/3, "too short"=1/3)),
                  method="class", control=rpart.control(cp=0.0005))

cpt <- tree_fit$cptable
p14 <- ggplot(cpt, aes(x = CP, y = xerror)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = xerror - xstd, ymax = xerror + xstd), width = 0.01) +
  scale_x_log10() +
  labs(x = "cp (Complexity Parameter)", 
       y = "Cross-validated classification error") +
  theme_bw()

print(p14)

min_xerror <- min(cpt[,"xerror"])
min_index <- which.min(cpt[,"xerror"])
xstd_at_min <- cpt[min_index, "xstd"]
threshold <- min_xerror + xstd_at_min
index_1se <- which(cpt[,"xerror"] <= threshold)[1]
opt_cp_1se <- cpt[index_1se, "CP"]

pruned_tree <- prune(tree_fit, cp = opt_cp_1se)
fancyRpartPlot(pruned_tree, digits=3, sub="")


####################### save figures
# dir.create("./Figures", showWarnings=FALSE)

pdf(file="./Figures/CARTprun2.pdf", width=8, height=8, paper="special")
fancyRpartPlot(pruned_tree, digits=3, sub="")
dev.off()
#################################################################


df_test$distVMcat <- cut(df_test$distVM, breaks=c(-Inf,-1.06,0.143,Inf),
                         labels=c("too long","good","too short"))

df_test$tree_pred <- predict(pruned_tree, newdata=df_test)


pacman::p_load(multiROC)

ytrue <- as.data.frame(model.matrix(~df_test$distVMcat-1))
ypred <- as.data.frame(df_test$tree_pred)

data <- cbind(ytrue,ypred)

names(data) <- c("G1_true","G2_true","G3_true","G1_pred_tree","G2_pred_tree","G3_pred_tree")

res <- multi_roc(data)



