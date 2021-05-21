library(survivalmodels)
library(mlr3)
library(mlr3proba)  
library(survival)
library(mlr3misc)
library(survival)
library(mlr3tuning)
library(mlr3benchmark)
library(mlr3extralearners)
library(paradox)
library(mlr3learners)
#install_learners('surv.coxboost')
install_learners('surv.rfsrc')

#test/train split

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
#test/train split for mlr3


task_gbcs = TaskSurv$new(id = "train_gbcs", backend = train_gbcs, time = "survtime", event = "censdead")
test_gbcs = TaskSurv$new(id = "test_gbcs", backend = test_gbcs, time = "survtime", event = "censdead")

NDml <- data.frame(age = 0, menopause = 1, hormone = 1,
                   size = 0, grade1 = c(1,0,0), grade2=c(0,1,0), grade3=c(0,0,1), 
                   nodes = 0, prog_recp=0, estrg_recp=0, survtime=1, censdead=1)
ND_gbcs = TaskSurv$new(id = "NDml", backend = NDml, time = "survtime", event = "censdead")


task_gbcs$nrow 
task_gbcs$feature_types

test_gbcs$nrow

# ------------------------------------------------------------------------
# cox regression


learner.cox = lrn("surv.coxph") 

learner.cox$train(task_gbcs)
learner.cox$model 

prediction.cox = learner.cox$predict(test_gbcs) 
prediction.cox
prediction.cox$score() 

measure = lapply(c("surv.graf"), msr)
prediction.cox$score(measure)


#### -------------------####

#Random Forest

learner.forest = lrn("surv.rfsrc")

learner.forest$train(task_gbcs)
learner.forest$model

prediction.forest = learner.forest$predict(test_gbcs) 
prediction.forest
prediction.forest$score()

measure = lapply(c("surv.graf"), msr)
prediction.cox$score(measure)

#---------------------------------------
#dnnsurv model

dnn_model  <- dnnsurv(time_variable = "survtime", status_variable = "censdead", data = train_gbcs,
      early_stopping = TRUE, epochs = 100L, validation_split = 0.3)


y_pred = predict(dnn_model, test_gbcs, type ="all") 
y_pred$risk
y_pred$surv


#--------------------------
#dnn with mlr3

mlr_learners$get("surv.dnnsurv")

learner.dnnsurv = lrn("surv.dnnsurv")

learner.dnnsurv$train(task_gbcs)
learner.dnnsurv$model

learner.dnnsurv$param_set

prediction.dnnsurv = learner.dnnsurv$predict(test_gbcs) 
prediction.dnnsurv
prediction.dnnsurv$score()

#parameters setting

#ps = ParamSet$new(list(
#  ParamInt$new("epochs", default =100L),
#  ParamDbl$new("validation_split", default =0.3),
#  ParamLgl$new("early_stopping", default =TRUE)))


#at = AutoTuner$new(
#  learner = learner.dnnsurv,
#  resampling = rsmp("holdout"),
#  search_space = ps,
#  measure = msr("surv.cindex"),
#  terminator = trm("evals", n_evals = 2),
#  tuner = tnr("random_search"))

#surv.harrell_c with dnnsurv - 0.5224007
#surv.harrell_c with cox - 0.6897718 
#surv.harrell_c with random forest - 0.7154128 

