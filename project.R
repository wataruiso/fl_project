setwd("C:/R_data")
# install.packages("texreg")
# library(memisc)
library(texreg)
library(erer)
library(effects)
library(ordinal)
library(corrplot)
library(car)
library(MASS)
library(makedummies)
library(pglm)
source("functions-project.R")
# source("functions.R")

#---------------------
sheet16 <- read.csv("ssjda_data16.csv",sep=",")
sheet17 <- read.csv("ssjda_data17.csv",sep=",")
sheet18 <- read.csv("ssjda_data18.csv",sep=",")

col16 <- rbind(c("Q1","Q2.5","Q6","Q18","Q20","Q20SQ.1","Q20SQ.2","Q20SQ.3","Q20SQ.4","Q23_1","Q23_2","Q23_3","F1","F2.9","F3.7","F4","F5","F6_4","F7","area","size"),
                 c(4,0,1005,5,4,rep(0,4),rep(5,3),1003,1001,1001,1002,1120,1003,1005,1010,1005))
data16 <- process_data(sheet16, col16)

# Qsum16 <- apply(data16[,c("Q23_1","Q23_2","Q23_3")], 1, sum)
Qsum16 <- prcomp(data16[,c("Q23_1","Q23_2","Q23_3")], scale=T)
# summary(Qsum16)
# biplot(Qsum16)

result16x <- data.frame(
  interest=data16[,c("Q1")],
  lossEdu=data16[,c("Q2.5")],
  lossCon=data16[,c("Q18")],
  lossReason1=data16[,c("Q20SQ.1")],
  lossReason2=data16[,c("Q20SQ.2")],
  lossReason3=data16[,c("Q20SQ.3")],
  lossReason4=data16[,c("Q20SQ.4")],
  # safety=Qsum16
  safety=Qsum16$x[,1]
  )

control16 <- data16[, c("F1","F2.9","F3.7","F4","F5","F6_4","area","Q6","F7","size")]
dummies16 <- make_dummies_mat(control16)
y_var16 <- as.ordered(data16[,c("Q20")])

mat16 <- data.frame(y=y_var16,result16x,dummies16)

# reg16 <- polr(y~., data = mat16, method = c("logistic"))
# summary(reg16)
# vif(reg16)

M <- cor(mat16[,-1], method='spearman', use='pairwise.complete.obs')
corrplot(M, method = "circle")


#----------------------------
col17 <- rbind(c("Q1","Q2.5","Q7","Q16","Q18","Q18SQ.1","Q18SQ.2","Q18SQ.3","Q18SQ.4","Q21_1","Q21_2","Q21_3","F1","F2.9","F3.7","F4","F5","F6_4","F7","area","size"),
                 c(4,0,1005,5,4,rep(0,4),rep(5,3),1003,1001,1001,1002,1120,1003,1005,1010,1005))
data17 <- process_data(sheet17, col17)

# Qsum17 <- apply(data17[,c("Q21_1","Q21_2","Q21_3")], 1,sum)
Qsum17 <- prcomp(data17[,c("Q21_1","Q21_2","Q21_3")], scale=T)
# summary(Qsum17)
# biplot(Qsum17)

result17x <- data.frame(
  interest=data17[,c("Q1")],
  lossEdu=data17[,c("Q2.5")],
  lossCon=data17[,c("Q16")],
  lossReason1=data17[,c("Q18SQ.1")],
  lossReason2=data17[,c("Q18SQ.2")],
  lossReason3=data17[,c("Q18SQ.3")],
  lossReason4=data17[,c("Q18SQ.4")],
  # safety=Qsum17
  safety=Qsum17$x[,1]
  )

control17 <- data17[, c("F1","F2.9","F3.7","F4","F5","F6_4","area","Q7","F7","size")]
dummies17 <- make_dummies_mat(control17)
y_var17 <- as.ordered(data17[,c("Q18")])

mat17 <- data.frame(y=y_var17,result17x, dummies17)

reg17 <- polr(y~., data = mat17, method = c("logistic"))
summary(reg17)
vif(reg17)

M <- cor(mat17[,-1], method='spearman', use='pairwise.complete.obs')
corrplot(M, method = "circle")

#----------------------------
col18 <- rbind(c("Q1","Q2.5","Q8","Q19","Q20","Q20SQ.1","Q20SQ.2","Q20SQ.3","Q20SQ.4","Q21_1","Q21_2","Q21_3","F1","F2.9","F3.7","F4","F5","F6_4","F7","area","size"),
                 c(4,0,1005,5,4,rep(0,4),rep(5,3),1003,1001,1001,1002,1120,1003,1005,1010,1005))
data18 <- process_data(sheet18, col18)

# Qsum18 <- apply(data18[,c("Q21_1","Q21_2","Q21_3")], 1,sum)
Qsum18 <- prcomp(data18[,c("Q21_1","Q21_2","Q21_3")], scale=T)
# summary(Qsum18)
# biplot(Qsum18)


result18x <- data.frame(
  interest=data18[,c("Q1")],
  lossEdu=data18[,c("Q2.5")],
  lossCon=data18[,c("Q19")],
  lossReason1=data18[,c("Q20SQ.1")],
  lossReason2=data18[,c("Q20SQ.2")],
  lossReason3=data18[,c("Q20SQ.3")],
  lossReason4=data18[,c("Q20SQ.4")],
  # safety=Qsum18
  safety=Qsum18$x[,1]
  )


control18 <- data18[, c("F1","F2.9","F3.7","F4","F5","F6_4","area","Q8","F7","size")]
dummies18 <- make_dummies_mat(control18)
y_var18 <- as.ordered(data18[,c("Q20")])

mat18 <- data.frame(y=y_var18,result18x,dummies18)

reg18 <- polr(y~., data = mat18, method = c("logistic"))
summary(reg18)
vif(reg18)

M <- cor(mat18[,-1], method='spearman', use='pairwise.complete.obs')
corrplot(M, method = "circle")


pmat16 <- data.frame(mat16,"year17"=rep(0, nrow(mat16)),"year18"=rep(0, nrow(mat16)))
pmat17 <- data.frame(mat17,"year17"=rep(1, nrow(mat17)),"year18"=rep(0, nrow(mat17)))
pmat18 <- data.frame(mat18,"year17"=rep(0, nrow(mat18)),"year18"=rep(1, nrow(mat18)))

# pmat16 <- data.frame("year"=rep(2016, nrow(mat16)), mat16)
# pmat17 <- data.frame("year"=rep(2017, nrow(mat17)), mat17)
# pmat18 <- data.frame("year"=rep(2018, nrow(mat18)), mat18)


pmat <- rbind(pmat16,pmat17,pmat18)
pmat <- data.frame(
  pmat,
   wife=pmat$d4 * pmat$d6,
   d2_safe=pmat$d2 * pmat$safety
  )

pmat$y <- as.ordered(pmat$y)

polr_result <- polr(y~
                    interest
                    +lossEdu
                    +lossCon
                    +safety
                    # +solo_rea1
                    # +rel_rea1
                    # +mar_wife_rea1
                    # +solo_safe
                    # +d1_2
                    +d3
                    +d2
                    +d5
                    +d4
                    +d6
                    # +d8
                    # +d9
                    # +mar_wife_cons
                    # +mar_wife_safe
                    # +solo_inte
                    # +edu_safe
                    # +cons_inte
                    +year17
                    +year18
                    +d2_safe
                    +wife,
                    data=pmat, method = c("logistic"))

summary(polr_result)

polr_result$deviance

texreg::screenreg(polr_result)

pmat$y <- as.numeric(pmat$y)
M <- cor(pmat[,c(-1,-5:-8)], method='spearman', use='pairwise.complete.obs')
# M <- cor(pmat[,c("y","lossReason1","lossReason4")], method='spearman', use='pairwise.complete.obs')
corrplot(M, method = "circle")

g.eff <- Effect(focal.predictors = c("lossEdu"), polr_result)
g.eff
plot(g.eff, rug = FALSE)

mea <-  ocME(w = polr_result)
mea$out

# ctable <- coef(summary(polr_result))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# (ctable <- cbind(ctable, "p value" = p))

# interest/inte  食育への関心
# lossEdu/edu   今後力をいれたい食育→食品廃棄（0 or 1）
# lossCon/cons  食べ残しに対する意識
# safety/safe   食の安全性への意識
# lossReason(1~4)/rea  食品ロスの原因の主成分（項目1~4）
# d1    既婚
# d2　　単身世帯
# d3    子供がいる家庭ダミー
# d4　　女性
# d5　　高齢者（60歳以上）
# d6　　主婦
# d7   地域
  # 1	北海道【北海道】
  # 2	東北【青森，岩手，宮城，秋田，山形，福島】
  # 3	関東【茨城，栃木，群馬，埼玉，千葉，東京，神奈川】
  # 4	北陸【新潟，富山，石川，福井】
  # 5	東山【山梨，長野，岐阜】
  # 6	東海【静岡，愛知，三重】
  # 7	近畿【滋賀，京都，大阪，兵庫，奈良，和歌山】
  # 8	中国【鳥取，島根，岡山，広島，山口】
  # 9	四国【徳島，香川，愛媛，高知】
  # 10	九州【福岡，佐賀，長崎，大分，熊本，宮崎，鹿児島，沖縄】
# d8    一人食事ダミー 週4以上
# d9    暮らしのゆとり（コントロール、5段階評価、5点→ゆとりがある）
# d10　　都市サイズ　　（コントロール、5段階評価、5点→大都市）
# _   かけ算
# mar_wife  既婚＊主婦




# write.csv(basic_data, file="basic_for_reason.csv")
# write.csv(data17, file="data17.csv")


basic_data <- getBasicData(pmat)
write.csv(basic_data, file="basic_data.csv")

# solo <- pmat[pmat[,c("d2")] == 1,]
# solono <- pmat[pmat[,c("d2")] != 1,]
# 
# hist(as.numeric(solo$y))


coef <- round(coef(polr_result), 3)
(coef[1])
mode(polr_result)


more_loss <- pmat[pmat[,c("y")] > 2,]
more_loss <- more_loss[,c("y", "lossReason1", "lossReason2", "lossReason3", "lossReason4","interest","lossEdu","lossCon","safety","d2","d3","d4","d5","d6")]
more_loss <- data.frame(
  more_loss,
  wife=more_loss$d4 * more_loss$d6,
  safe_d2=more_loss$safety * more_loss$d2
  # rea1_d2=more_loss$lossReason1*more_loss$d2,
  # rea4_d2=more_loss$lossReason4*more_loss$d2,
  # inte_d2=more_loss$interest*more_loss$d2,
  # inte_d3=more_loss$interest*more_loss$d3
  # con_d2=more_loss$lossCon*more_loss$d2,
  # con_d3=more_loss$lossCon*more_loss$d3
)
reg <- glm(y~., data = more_loss, family = binomial(logit))
summary(reg)

logLik(reg)

M <- cor(more_loss[,-1], method='spearman', use='pairwise.complete.obs')
corrplot(M, method = "circle")

# barplot(as.numeric(t(pmat$y)), beside=F)
# length(pmat[pmat[,1]== 4,1])


# write.csv(data18, file="data18.csv")

ModelName <- reg
alpha <- 0.05
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR <- exp(x$coefficients[,1])
LowerCL <- exp(y[,1])
UpperCL <- exp(y[,2])
pvalue <- summary(reg)$coefficients[,"Pr(>|z|)"]
z <- cbind(OR, LowerCL, UpperCL, pvalue)
round(z, 5)
