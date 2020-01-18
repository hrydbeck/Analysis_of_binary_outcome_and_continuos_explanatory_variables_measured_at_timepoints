## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)


## ----include=FALSE------------------------------------------------------------------------------------------------------------
# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), 'bookdown', 'knitr', 'rmarkdown'
), 'packages.bib')


## ----Loading_libraries--------------------------------------------------------------------------------------------------------
library("tidyverse")
#library("readxl")
#http://www.sthda.com/english/wiki/cox-proportional-hazards-model
#install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
#install.packages(c("naniar"))
library(naniar)


## ----Reading in file----------------------------------------------------------------------------------------------------------
d <- read_delim("/Users/halfdanr/Desktop/bioinf_gu/data/sample_file_dot_as_decimal_time_point_num.csv", delim=";")


## ----logo, echo=FALSE, fig.cap="Wild data"------------------------------------------------------------------------------------
knitr::include_graphics("img/IMG_2229.png")


## ----tabl1, echo=FALSE--------------------------------------------------------------------------------------------------------
knitr::kable(
  head(d[1:5, 1:8], 10), booktabs = TRUE,
  caption = 'A table of the first 10 rows of the data.'
)


## ----fig1, echo=FALSE, fig.cap="Plotting histogram of C3"---------------------------------------------------------------------
ggplot(data = d) +
geom_histogram(mapping = aes(x = C3), binwidth = 0.05)


## ----Removing most common value-----------------------------------------------------------------------------------------------
options(digits=15)
val_to_repl <- as.numeric(names(head(sort(table(d$C3),decreasing=TRUE),1)))
d_temp <- d
d_temp <- d_temp %>% replace_with_na(replace = list(C3 = val_to_repl))


## ----fig2, echo=FALSE, fig.cap="Plotting histogram of C3 with most common value removed"--------------------------------------
ggplot(data = d_temp) +
geom_histogram(mapping = aes(x = C3), binwidth = 0.05) 


## ----A summary of the distribution of the variables---------------------------------------------------------------------------
summary(d)


## ----Omitting missing values--------------------------------------------------------------------------------------------------
d <- na.omit(d)


## ----Gather-------------------------------------------------------------------------------------------------------------------
d_long <- gather(d, "ck", "ck_value",c(C1:C17))
#G1 cannot be included since missing data
d_long <- gather(d_long, "gf", "gf_value",c(G1:G5))


## ----fig4, echo=FALSE, fig.cap="Histograms of growth facors. All data included gf"--------------------------------------------
#Histograms of growth facors
ggplot(data = d_long) +
  geom_histogram(mapping = aes(x = gf_value),binwidth = 100) +
  facet_wrap( ~ gf)


## ----Removing most common value from cytokine data----------------------------------------------------------------------------
d_test <- d
d_test <- na.omit(d_test)
#C1
val_to_repl <- as.numeric(names(head(sort(table(d$C1),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C1 = val_to_repl))
#C2
val_to_repl <- as.numeric(names(head(sort(table(d$C2),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C2 = val_to_repl))
#C3
val_to_repl <- as.numeric(names(head(sort(table(d$C3),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C3 = val_to_repl))
#C4
val_to_repl <- as.numeric(names(head(sort(table(d$C4),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C4 = val_to_repl))
#C5
val_to_repl <- as.numeric(names(head(sort(table(d$C5),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C5 = val_to_repl))


#C6
val_to_repl <- as.numeric(names(head(sort(table(d$C6),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C6 = val_to_repl))
#C7
val_to_repl <- as.numeric(names(head(sort(table(d$C7),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C7 = val_to_repl))
#C8
val_to_repl <- as.numeric(names(head(sort(table(d$C8),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C8 = val_to_repl))
#C9
val_to_repl <- as.numeric(names(head(sort(table(d$C9),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C9 = val_to_repl))
#C10
val_to_repl <- as.numeric(names(head(sort(table(d$C10),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C10 = val_to_repl))


#C11
val_to_repl <- as.numeric(names(head(sort(table(d$C11),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C11 = val_to_repl))
#C12
val_to_repl <- as.numeric(names(head(sort(table(d$C12),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C12 = val_to_repl))
#C13
val_to_repl <- as.numeric(names(head(sort(table(d$C13),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(C13 = val_to_repl))

#G1
val_to_repl <- as.numeric(names(head(sort(table(d$G1),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(G1 = val_to_repl))
#G2
val_to_repl <- as.numeric(names(head(sort(table(d$G2),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(G2 = val_to_repl))
#G3
val_to_repl <- as.numeric(names(head(sort(table(d$G3),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(G3 = val_to_repl))
#G4
val_to_repl <- as.numeric(names(head(sort(table(d$G4),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(G4 = val_to_repl))
#G5
val_to_repl <- as.numeric(names(head(sort(table(d$G5),decreasing=TRUE),1)))
d_test <- d_test %>% replace_with_na(replace = list(G5 = val_to_repl))

d_test_long <- gather(d_test, "ck_low", "ck_low_value",c(C1:C12))
d_test_long <- gather(d_test_long, "ck_high", "ck_high_value",c(C13:C17))
d_test_long <- gather(d_test_long, "gf", "gf_value",c(G1:G5))




## ----Plot with most common value removed ck-----------------------------------------------------------------------------------
#Histograms of Cytokines, low count values
ggplot(data = d_test_long) +
  geom_histogram(mapping = aes(x = ck_low_value),binwidth = 0.05) +
  facet_wrap( ~ ck_low)

#Histograms of cytokine, high count values
ggplot(data = d_test_long) +
  geom_histogram(mapping = aes(x = ck_high_value),binwidth = 0.05) +
  facet_wrap( ~ ck_high)


## ----Plot with most common value removed gf-----------------------------------------------------------------------------------
#Histograms of growth factors, low count values
ggplot(data = d_test_long) +
  geom_histogram(mapping = aes(x = gf_value),binwidth = 100) +
  facet_wrap( ~ gf)


## ----Gather for plots of covariation------------------------------------------------------------------------------------------
d_long2 <- gather(d, "ck_low", "ck_low_val",c(C2:C13))
d_long2 <- gather(d_long2, "ck_high", "ck_high_val",c(C14:C17))
d_long2 <- gather(d_long2, "gf", "gf_val",c(G3:G5))


## ----Plot covariation ck high count-------------------------------------------------------------------------------------------
ggplot(data = d_long2) +
  geom_point(mapping = aes(x = C1, y = ck_high_val, color = as.factor(Time_point_num))) +#
  facet_wrap( ~  ck_high)

## ----Plot covariation ck low count--------------------------------------------------------------------------------------------
ggplot(data = d_long2) +
  geom_point(mapping = aes(x = C1, y = ck_low_val, color = as.factor(Time_point_num))) +
  facet_wrap( ~  ck_low)

## ----Plot covariation gfs-----------------------------------------------------------------------------------------------------
ggplot(data = d_long2) +
  geom_point(mapping = aes(x = G1, y = gf_val, color = as.factor(Time_point_num))) +
  facet_wrap( ~  gf)


## -----------------------------------------------------------------------------------------------------------------------------
res.cox <- coxph(Surv(Time_point_num, outcome) ~ sex, data = d)
res.cox
summary(res.cox)


## -----------------------------------------------------------------------------------------------------------------------------
covariates <- names(d)[3:25]
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(Time_point_num, outcome)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = d)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))


## -----------------------------------------------------------------------------------------------------------------------------
res_filt <- res[as.numeric(res[,4]) <= 0.001, ] 
rownames(res_filt)
ind_var <- paste(rownames(res_filt),sep="",collapse=" + ") # havent figure out how to use tis as input
res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
res.cox <- coxph(Surv(Time_point_num, outcome) ~ C1 + C2 + C3 + C4 + C5 + C7 + C8 + C9 + C12 + C13 + G1 + G2 + G4 , data = d)
res.cox
summary(res.cox)


## -----------------------------------------------------------------------------------------------------------------------------
fit <- survfit(Surv(Time_point_num, outcome) ~ sex, data = d)
print(fit)
# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table
# Convert list to data frame
data_f <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower
)
head(data_f)



## -----------------------------------------------------------------------------------------------------------------------------
#mydata$Agecat2<-cut(mydata$Age, seq(0,30,5))
#https://www.r-bloggers.com/from-continuous-to-categorical/
mi <- min(d$C5, na.rm = TRUE)
ma <- max(d$C5, na.rm = TRUE)
d$C5_cat <- cut(d$C5, seq(mi,ma,1))
min(d$C5)
max(d$C5)

head(d)
fit <- survfit(Surv(Time_point_num, outcome) ~ d$C5_cat, data = d)
print(fit)
# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table
# Convert list to data frame
data_f <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower
)
head(data_f)


## -----------------------------------------------------------------------------------------------------------------------------
# Change color, linetype by strata, risk.table color by strata
#ggsurvplot(fit,
#           pval = TRUE, conf.int = TRUE,
#          risk.table = TRUE, # Add risk table
#           risk.table.col = "strata", # Change risk table color by groups
#           linetype = "strata", # Change line type by groups
#           surv.median.line = "hv", # Specify median survival
#           ggtheme = theme_bw(), # Change ggplot2 theme
#           palette = c("blue", "red", "green","yellow"))


## -----------------------------------------------------------------------------------------------------------------------------
mi <- min(d$C11, na.rm = TRUE)
ma <- max(d$C11, na.rm = TRUE)
d$C11_cat <- cut(d$C11, seq(mi,ma,0.6))


head(d)
fit <- survfit(Surv(Time_point_num, outcome) ~ d$C11_cat, data = d)
print(fit)
# Summary of survival curves
summary(fit)
# Access to the sort summary table
summary(fit)$table
# Convert list to data frame
data_f <- data.frame(time = fit$time,
                     n.risk = fit$n.risk,
                     n.event = fit$n.event,
                     n.censor = fit$n.censor,
                     surv = fit$surv,
                     upper = fit$upper,
                     lower = fit$lower
)
head(data_f)


## -----------------------------------------------------------------------------------------------------------------------------
# Change color, linetype by strata, risk.table color by strata
#ggsurvplot(fit,
#           pval = TRUE, conf.int = TRUE,
#           risk.table = TRUE, # Add risk table
#           risk.table.col = "strata", # Change risk table color by groups
#           linetype = "strata", # Change line type by groups
#           surv.median.line = "hv", # Specify median survival
#           ggtheme = theme_bw(), # Change ggplot2 theme
#           palette = c("blue", "red", "green","yellow"))

