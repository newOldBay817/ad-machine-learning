SETS = build_train_test(DATA2, round(.3*nrow(DATA2)))

TEST = SETS[[1]]
TRAIN = SETS[[2]]

FORMULA = FORMULA1
FORMULA = FORMULA2

LIN = lm(FORMULA, TRAIN)
LOG = glm(FORMULA, TRAIN, family = 'binomial')
TRE = rpart(FORMULA, TRAIN, method = "anova", control = rpart.control(n = 20, cp = 0.01))
PRE_LOG = predict(LOG, type = "response")
PRE_LIN = predict(LIN, type = "response")
PRE_TRE = predict(TRE, type = 'vector')
#TRAIN = as.data.frame(cbind(TRAIN$ID, TRAIN$X125, TRAIN$X125.1, TRAIN$X1.0, TRAIN$X1, TRAIN$AD, PRE_LIN, PRE_LOG, PRE_TRE))

library(ggplot2)

#GLM PLOTS
plot(TRAIN$X1.0, TRAIN$AD,main = 'Formula 2: Logarithhmic', xlab = 'a-ratio', ylab = 'AD', plot.window(c(0,15), c(0,1)))

plot(TRAIN$X125.1, TRAIN$AD,main = 'Formula 1: Logarithmic', xlab = 'Width', ylab = 'AD', plot.window(c(0,650), c(0,1)))
points(TRAIN$X1.0, PRE_LOG, col = "green")

# mid = mean(TRAIN$V3)
# ggplot(TRAIN, aes(PRE_LOG, V2, color = V3))+geom_point()+ggtitle("Formula 2: Linear Model")+labs(x = "Height", y = "AD")+scale_color_gradient2(midpoint = mid, low = "blue", mid = "white", high = "red")+ theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5))


#LIN PLOTS
# plot(TRAIN$X1.0, TRAIN$AD,plot.window(c(0,15), c(0,1)))
plot(TRAIN$X125.1, TRAIN$AD,main = 'Formula 1: OLS Linear', xlab = 'Width', ylab = 'AD', plot.window(c(0,650), c(0,1)))
plot(TRAIN$X1.0, TRAIN$AD,main = 'Formula 2: OLS Linear', xlab = 'a-ratio', ylab = 'AD', plot.window(c(0,15), c(0,1)))
# plot(TRAIN$X125.1,TRAIN$AD, plot.window(c(0,600), c(0,1)))
points(TRAIN$X1.0, PRE_LIN, col = 'green')

#DEC PLOTS
fancyRpartPlot(TRE, main = "Formula 1: Decision Tree", sub = "AD ~ Height + Width + Local")
fancyRpartPlot(TRE, main = "Formula 2: Decision Tree", sub = "AD ~ ARatio + Local")

