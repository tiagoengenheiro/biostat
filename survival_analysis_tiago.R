library(tidyverse)
library(survival)
library(psych)
library(ggplot)
library(ggsurvplot)
#install packages survminer
install.packages("survminer")
library(survminer)

rm(list = ls())

#Load df_final_mortality.csv dataset
df <- read.csv("data/df_final_mortality.csv")


table(df$mortstat) # 0: alive, 1: dead

max(df$permth_int)
#png("images/survival_analysis/hist_mortstat_0.png")
hist(subset(df,mortstat==0)$permth_int, breaks=seq(0, 250, by=10))
#dev.off()

#png("images/survival_analysis/hist_mortstat_1.png")
hist(subset(df,mortstat==1)$permth_int,breaks=seq(0, 250, by=10))
#dev.off()

#save history plots


summary(km_fit1, times=c(100*(1:43)))
km_fit1
#png("images/survival_analysis/survival_analysis.png")
plot(km_fit1)
#dev.off()


library(survminer)
#ggsurvplot(km_fit1, data=df, risk.table=TRUE, conf.int=TRUE, ggtheme=theme_minimal())

km_fit2<-survfit(Surv(permth_int,mortstat)~Coffee,
                 data=df,
                 type='kaplan-meier')
plot(km_fit2)


sv_plot <- ggsurvplot(km_fit2, data=df, 
           #risk.table=TRUE, 
           conf.int=TRUE, 
           ggtheme=theme_bw(), 
           pval=TRUE, pval.method = TRUE,
           risk.table.col = "strata",
           xlab = "Time in months",   # customize X axis label.
           surv.median.line = "hv", #"#2E9FDF",
           #break.time.by = 200,
           #palette = c("#E7B800","#69C669","#64403E", "#C65751")
)
grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
} #Save ggplot

iqr_standardization <- function(df, col_name) {
  # Calculate Q1 (25th percentile) and Q3 (75th percentile)
  q1 <- quantile(df[[col_name]], 0.25, na.rm = TRUE)
  q3 <- quantile(df[[col_name]], 0.75, na.rm = TRUE)
  
  # Calculate IQR
  iqr <- q3 - q1
  
  # Apply IQR standardization to each data point in the column
  df[[col_name]] <- (df[[col_name]] - q1) / iqr
  
  return(df)
}

df <- iqr_standardization(df, "TotalCoffeeIntake")

ggsave("images/survival_analysis/survival_analysis_coffee.png",plot=sv_plot)


cox_reg1 <- coxph(Surv(permth_int,mortstat)~Coffee,data=df)

coffee_df <- with(df,
               data.frame(Coffee=c(0,1))
               )



fit <- survfit(cox_reg1, newdata = coffee_df)


cox_plot <- ggsurvplot(fit, data=df, 
           #risk.table=TRUE, 
           conf.int=TRUE, 
           ggtheme=theme_bw(), 
           #pval=TRUE, pval.method = TRUE,
           #risk.table.col = "strata",
           xlab = "Time in months",   # customize X axis label.
           surv.median.line = "hv", #"#2E9FDF",
           legend.labs = c("Coffee Consumers", "Non-Coffee Consumers"),
           #break.time.by = 200,
           #palette = c("#E7B800","#69C669","#64403E", "#C65751")
)

ggsave("images/survival_analysis/survival_analysis_coffee_cox_plot.png",plot=cox_plot)
