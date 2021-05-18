library(survivalmodels)
library(mlr3proba); library(mlr3); 
library(survival)
library(mlr3misc)
library(survival)
library(mlr3tuning)
library(mlr3benchmark)

head(gbcs)
dim(gbcs)
str(gbcs)
table(gbcs$censdead)

gbcs$grade <- as.factor(gbcs$grade)


try1 <- coxph(Surv(survtime, censdead) ~ grade, data = gbcs) 
summary(try1) 

gbcs2 <- gbcs[,c(5:12,15:16)]
head(gbcs2)
str(gbcs2)

gbcs2$age <- scale(gbcs2$age)
gbcs2$menopause <- gbcs2$menopause-1
gbcs2$hormone <- gbcs2$hormone-1
gbcs2$size <- scale(gbcs2$size)
gbcs2$grade <- as.factor(gbcs2$grade)
grade123 <- with(gbcs2, data.frame(model.matrix(~grade-1))) # 3 dummies for the grade
gbcs2$nodes <- scale(gbcs2$nodes)
gbcs2$prog_recp <- scale(gbcs2$prog_recp)
gbcs2$estrg_recp <- scale(gbcs2$estrg_recp)


head(gbcs2)

cov <- data.frame(gbcs2[,c(1:4, 6:10)], grade1 = grade123[,1], 
                  grade2 = grade123[,2], grade3 = grade123[,3])
head(cov)

# train/test split 
set.seed(123)
train_set = sample(nrow(cov), 0.8 * nrow(cov))
str(train_set)
test_set = setdiff(seq_len(nrow(cov)), train_set)

train_gbcs <- cov[train_set, ]
dim(train_gbcs)
head(train_gbcs)
table(train_gbcs$censdead)

test_gbcs <- cov[test_set, ]
dim(test_gbcs)
head(test_gbcs)
table(test_gbcs$censdead)


# ------------------------------------------------------------------------
task_gbcs = TaskSurv$new(id = "train_gbcs", backend = train_gbcs, time = "survtime", event = "censdead")
test_gbcs = TaskSurv$new(id = "test_gbcs", backend = test_gbcs, time = "survtime", event = "censdead")

NDml <- data.frame(age = 0, menopause = 1, hormone = 1,
                   size = 0, grade1 = c(1,0,0), grade2=c(0,1,0), grade3=c(0,0,1), 
                   nodes = 0, prog_recp=0, estrg_recp=0, survtime=1, censdead=1)
ND_gbcs = TaskSurv$new(id = "NDml", backend = NDml, time = "survtime", event = "censdead")


task_gbcs$nrow 
task_gbcs$feature_types

test_gbcs$nrow

learner.cox = lrn("surv.coxph") 

learner.cox$train(task_gbcs)
learner.cox$model 

prediction.cox = learner.cox$predict(test_gbcs) 
prediction.cox
prediction.cox$score() 

measure = lapply(c("surv.graf"), msr)
prediction.cox$score(measure)



cox.distr = ppl("distrcompositor", learner = lrn("surv.coxph"),
                estimator = "kaplan", form = "ph", overwrite = FALSE, graph_learner = TRUE)

temp <- cox.distr$train(task_gbcs)$predict(ND_gbcs)
temp
proba <- temp$distr$survival(surv_probs_Cox$time)

----

plot(surv_probs_Cox$time, proba$WeightDisc1, col = c("red"), type="l", ylim=c(0,1),
     xlab = "Follow-Up Time (days)", ylab = "Survival Probabilities")

lines(surv_probs_Cox$time, proba$WeightDisc1, col="red")
lines(surv_probs_Cox$time, proba$WeightDisc2, col="blue")
lines(surv_probs_Cox$time, proba$WeightDisc3, col="green")

#lines are the same

#can be adapted to everyt learner on mlr3


#### -------------------####

#trying new model Random Forest
library(mlr3learners)


task_gbcs = TaskSurv$new(id = "train_gbcs", backend = train_gbcs, time = "survtime", event = "censdead")
test_gbcs = TaskSurv$new(id = "test_gbcs", backend = test_gbcs, time = "survtime", event = "censdead")

NDml <- data.frame(age = 0, menopause = 1, hormone = 1,
                   size = 0, grade1 = c(1,0,0), grade2=c(0,1,0), grade3=c(0,0,1), 
                   nodes = 0, prog_recp=0, estrg_recp=0, survtime=1, censdead=1)
ND_gbcs = TaskSurv$new(id = "NDml", backend = NDml, time = "survtime", event = "censdead")


task_gbcs$nrow 
task_gbcs$feature_types

test_gbcs$nrow

learner.forest = lrn("surv.rfsrc")


learner.forest$train(task_gbcs)
learner.forest$model

prediction.forest = learner.forest$predict(test_gbcs) 
prediction.forest
prediction.forest$score()

measure = lapply(c("surv.graf"), msr)
prediction.cox$score(measure)

#surv.harrell_c 
#0.6982248 


