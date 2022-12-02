library(tidyverse)
#library(stats)
library(dplyr)
#library(Metrics)
#library(MLmetrics)
# library(rpart)
# library(rpart.plot)
# library(lmvar)
library(DT)
library(plotly)
library(tidymodels)
# theme_set(
#   theme_bw() +
#     theme(legend.position = "top")
# )

##### General functions statistics

RSE <- function(real,pred,data){
  
  RSE=sqrt(sum((real-pred)^2)/(nrow(data)-2))
  return(RSE)
}

#RSE(df_ad_4$Sales,df_ad_4$Predicted_Sales,df_ad_4)

R2 <- function(real,pred){
  
  R2=1-sum((real-pred)^2)/sum((real-mean(real))^2)
  return(round(R2,3))
}

##### The data we use

#df_default <- ISLR::Default #defual credit

df_ad <- read_csv("Advertising.csv") %>%  #sales with TV,Newspapers,Radio
  select(-X1)


##### Linear regression one predictor - Specific functions

## Plot and tables


Sales_media <- function(x,y,w,z){
  
  df_ad %>% 
    ggplot(aes(x={{x}})) +
    geom_point(aes(y = {{y}})) +
   labs(x = paste0(w),
        y = paste0(z)) +
   ggtitle(paste0(w," vs ",z)) +
    theme(panel.background = element_rect(fill = 'white',color="#add8e6"),
          panel.grid.major = element_line(size = 0.2,
                                          linetype = 'solid',
                                          colour = "#add8e6"),
          panel.grid.minor = element_line(size = 0.2,
                                          linetype = 'solid',
                                          colour = "#add8e6"),
          legend.position="right",
          legend.title = element_text(size = 10, family = "AppleGothic",color="black"),
          legend.text=element_text(size=13, color="black",family = "AppleGothic"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.margin=margin(t = 1, r = 0, b = 0, l = 0, unit = "cm"),
          axis.text = element_text(size = 14,color="black",family = "AppleGothic"),
          axis.title = element_text(size = 20,family = "AppleGothic"),
       #   axis.text.x = element_text(angle = 0,family = "AppleGothic"),
          plot.title = element_text(size = 25, face = "bold",family = "AppleGothic",
                                    color="black",hjust = 0.5,vjust = 2) )
  
  
}


Sales_media(Radio,Sales,"Radio","Sales")

########


ad_data <- function(){
df_ad %>% 
  datatable(rownames = TRUE,
            #filter = list(position = 'top', clear = TRUE),
            caption = "Sales vs TV, Radio, Newspaper",
            options = list(searching = FALSE, pageLength = 5, lengthMenu = c(5,15,100,200),scrollX = T,
                             columnDefs = list(list(className = 'dt-center', targets = 1:4)))
            )
}

ad_data() 

#######

lm_by_hand <- function(beta0,beta1,data_x){
  
y=round((beta0+beta1*data_x),1)*1000
return(y)
  
}

#######

ad_real_vs_pred_TV <- function(data){
  
  data %>% 
    ggplot(aes(x = TV)) +
    geom_point(aes(y = Sales),color="black") +
    geom_line(aes(y = Predicted_Sales),color="blue") +
    labs(x = "TV",
         y = "Sales") +
    ggtitle(paste0("Real and predicted Sales vs TV")) +
    theme(panel.background = element_rect(fill = 'white',color="#add8e6"),
          panel.grid.major = element_line(size = 0.2,
                                          linetype = 'solid',
                                          colour = "#add8e6"),
          panel.grid.minor = element_line(size = 0.2,
                                          linetype = 'solid',
                                          colour = "#add8e6"),
          legend.position="right",
          legend.title = element_text(size = 10, family = "AppleGothic",color="black"),
          legend.text=element_text(size=13, color="black",family = "AppleGothic"),
          legend.key = element_rect(fill = "transparent", colour = "transparent"),
          legend.margin=margin(t = 1, r = 0, b = 0, l = 0, unit = "cm"),
          axis.text = element_text(size = 14,color="black",family = "AppleGothic"),
          axis.title = element_text(size = 20,family = "AppleGothic"),
          #   axis.text.x = element_text(angle = 0,family = "AppleGothic"),
          plot.title = element_text(size = 25, face = "bold",family = "AppleGothic",
                                    color="black",hjust = 0.5,vjust = 2) )
  
  
}

# ad_real_vs_pred_TV(df_ad_4)

#######

##### Linear regression two predictors - Specific functions

# a <- df_ad %>%
#   lm(Sales ~ TV+Radio,.)
# # 
# # a$coefficients[1]
# 
# lm(as.formula(a), df_ad)[[1]]
# 
# 
# xnam <- paste( a$coefficients[[1]])
# ynam <- paste( a$coefficients[[2]])
# 
# paste( a$coefficients[[3]])
# (fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))
# 
# paste0("y = ", xnam,"X")
