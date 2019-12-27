
rm(list=ls())
set.seed(42)

# Install from CRAN
#install.packages('rmarkdown')
#install.packages("tinytex")
#install.packages("pdflatex")
#install.packages("finalfit")
#install.packages("qwraps2")
library(magrittr)
library(qwraps2)
options(qwraps2_markup = "markdown")
#options(qwraps2_markup = "latex")
#######################################
#1 载入数据
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url,header=F)
head(data)
colnames(data) <- c(
  "age",
  "sex",
  "cp",
  "trestbps",
  "chol",
  "fbs",
  "restecg",
  "thalach",
  "exang",
  "oldpeak",
  "slope",
  "ca",
  "thal",
  "hd"
)
head(data)  
str(data)
#2 清洗数据
data[data == "?"] <- NA
#any(is.na(data$ca))
#data <- na.exclude(data)
data[data$sex == 0,]$sex <- "F"
data[data$sex == 1,]$sex <- "M"

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca)
data$ca <- as.factor(data$ca)

data$thal <- as.integer(data$thal)
data$thal <- as.factor(data$thal)

data$age <- as.integer(data$age)
data$age <- ifelse(test=data$age >= 50 ,yes=1,no=0)
data$age <- as.factor(data$age)

data$hd <- ifelse(test=data$hd == 0 ,yes="患病",no="未患病")
data$hd <- as.factor(data$hd)
str(data)
#3 构建表格 
our_summary1 <-
  list("年龄" =
         list(">=50" = ~ qwraps2::n_perc(.data$age == 1),
              "<50" = ~ qwraps2::n_perc(.data$age == 0)),
       "性别" =
         list("Female" = ~ qwraps2::n_perc(.data$sex == "F"),
              "Male"  = ~ qwraps2::n_perc(.data$sex == "M")),
       "血清胆汁" =
         list("min" = ~ min(.data$chol),
              "median" = ~ median(.data$chol),
              "max" = ~ max(.data$chol),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$chol)),
       "血压" =
         list("min" = ~ min(.data$trestbps),
              "max" = ~ max(.data$trestbps),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$trestbps)),
       "胸痛等级" =
         list("One" = ~ qwraps2::n_perc(.data$cp == 1),
              "Two"  = ~ qwraps2::n_perc(.data$cp == 2),
              "Three"  = ~ qwraps2::n_perc(.data$cp == 3),
              "Four"  = ~ qwraps2::n_perc(.data$cp == 4)),
       "血液循环" =
         list("One" = ~ qwraps2::n_perc(.data$ca == 2,na_rm = T),
              "Two"  = ~ qwraps2::n_perc(.data$ca == 3,na_rm =T),
              "Three"  = ~ qwraps2::n_perc(.data$ca == 4,na_rm =T),
              "Four"  = ~ qwraps2::n_perc(.data$ca == 5,na_rm =T))
       
)

whole <- summary_table(data, our_summary1)
whole

by_hd <- summary_table(dplyr::group_by(data, hd), our_summary1)
whole
by_hd %>% str
#dim(data)
#dim(data[complete.cases(data[,"ca"]),])
#4 添加p value
mpvals <-
  list(
       lm(chol ~ hd, data = data),
       lm(trestbps ~ hd,   data = data)) %>%
  lapply(aov) %>%
  lapply(summary) %>%
  lapply(function(x) x[[1]][["Pr(>F)"]][1]) %>%
  lapply(frmtp) %>%
  do.call(c, .)
mpvals

cpval_age <- frmtp(chisq.test(table(data$age, data$hd))$p.value)
cpval_sex <- frmtp(chisq.test(table(data$sex, data$hd))$p.value)
cpval_cp<- frmtp(chisq.test(table(data$cp, data$hd))$p.value)

cadata<-data[!(is.na(data$ca)),]# 过滤数据集用于卡方检验
cpval_ca<- frmtp(chisq.test(table(cadata$ca, cadata$hd))$p.value)


#增加pvalue column
both <- cbind(by_hd, "P-value" = "")
both[grepl("mean \\(sd\\)", rownames(both)), "P-value"] <- mpvals
both[grepl("Sex",rownames(both)), "P-value" ] <-cpval_sex
a <- capture.output(print(both))
a[grepl("年龄", a)] %<>% sub("&nbsp;&nbsp;\\ \\|$", paste(cpval_age, "|"), .)
a[grepl("性别", a)] %<>% sub("&nbsp;&nbsp;\\ \\|$", paste(cpval_sex, "|"), .)
a[grepl("胸痛等级", a)] %<>% sub("&nbsp;&nbsp;\\ \\|$", paste(cpval_cp, "|"), .)
a[grepl("血液循环", a)] %<>% sub("&nbsp;&nbsp;\\ \\|$", paste(cpval_ca, "|"), .)

#5 保存文件
cat(a, sep = "\n")
setwd("~/desktop/work/R")
write.table(a, file = "summary_table.md", sep = ",", quote = FALSE, row.names = F)

