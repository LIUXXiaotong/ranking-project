###### prepare R and load the data ############# 
library(tidyverse)
library(latex2exp)
library(afex)
library(emmeans)
library(ggplot2)

theme_set(theme_bw())

df <- read.csv("study_1_df.csv")


################################################
### look at the distribution of participants' probabilities for providing logically impossible rankings 
### bar chat 

df_prob_error <- df %>% group_by(ID) %>%
  summarise(prob = mean(if_there_are_errors)) 

df %>% group_by(ID) %>%
  summarise(prob = mean(if_there_are_errors)) %>% 
  group_by(prob) %>%
  count() 

mean(df_prob_error$prob) # 0.3893597
mean(df_prob_error$prob) * 12 #4.672316 
median(df_prob_error$prob) #0.4166667
# one person made logical errors in 10 out of 12 rankings, 
# and 10 person did not make any logical errors in all 12 rankings.

ggplot(data = df_prob_error, aes(x= prob)) + 
  geom_bar() + 
  scale_x_continuous(breaks = c(0/12, 2/12, 4/12, 6/12, 8/12, 10/12), 
                   label = c(expression(frac(0,12)), expression(frac(2,12)), expression(frac(4,12)), 
                             expression(frac(6,12)),expression(frac(8,12)),expression(frac(10, 12)))) + 
  xlab(expression(" \n Probability of providing a logically incorrect ranking")) + 
  ylab("Count") + 
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        axis.title.y  = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.position = "none")


ggsave("distribution_of_participants_probabilities_of_making_errors.jpg")                   
              
                   
################################################
### Analysis DV1: the rate of providing logically correct responses
df <- df %>%
  mutate(if_correct = ifelse(if_there_are_errors==0, 1, 0))


a1 <- aov_ez("ID", "if_correct", df,  between = "between_subject_condition", within  = "within_subject_condition")
a1


# afex plot
afex_plot(a1, "within_subject_condition", "between_subject_condition",
          data_geom = geom_violin,
          data_arg = list(width = 0.5,
                          color = "Grey28"),
          
          point_arg = list(size = 2), 
          line_arg = list(size = 1),
          error_arg = list(size = 0.8, width = 0),
          factor_levels = list(between_subject_condition = c("allowed", "not allowed")), 
          legend_title = "Ties") + 
  ylab(expression(paste("Pr(logical)"))) + 
  xlab("Type of events")  +
  scale_x_discrete(labels=c("indiff" = "middle events", "extreme" = "edge events")) + 
  ggplot2::theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        axis.title.y  = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        legend.text=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.position = "bottom") 


#ggsave("probability_of_being_right.jpg") 




### Analyse DV2: the rate of providing the Type 3 rankings conditional on that participants do not give logically correct responses
### for my information: only analyse with DV with participants in ties_allwed  condition


DV2_df <- df %>%
  filter(between_subject_condition == "ties_allowed") %>%
  select(ID, within_subject_condition, error_type) %>%
  drop_na() 


### for my information, "0" represents the type 2 ranking, "1" represents the type 1 ranking, and "2" represents the type 3 ranking
DV2_df <- DV2_df %>%
  mutate(res_DV2 = ifelse(error_type == 2, 1, 0)) %>%
  select(ID, within_subject_condition, res_DV2)

a2 <- aov_ez("ID", "res_DV2", DV2_df, within = "within_subject_condition")
a2


# afex plot
afex_plot(a2, "within_subject_condition",dodge = 0.5,
          data_geom = ggbeeswarm::geom_quasirandom,
          data_arg = list(
            dodge.width = 0.5,  ## needs to be same as dodge
            color = "Grey27"),
          point_arg = list(size = 2), 
          line_arg = list(size = 2),
          error_arg = list(size = 0.8, width = 0)) + 
  ylab(expression(paste("Pr(Type 3 | illogical)"))) + 
  xlab("Type of events")  +
  theme(plot.margin = margin(l = 20, t=35, b=35, r=20)) +
  scale_x_discrete(labels=c("indiff" = "middle events", "extreme" = "edge events")) + 
  ggplot2::theme(axis.text=element_text(size=14),
                 axis.title=element_text(size=16),
                 axis.title.y  = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.title.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 legend.text=element_text(size=14),
                 legend.title = element_text(size = 14),
                 legend.position = "bottom") 

#ggsave("conditional_probability_of_giving_type_3.jpg") 

######## Analyse DV3: the rate of providing type 1 rankings conditional on that participants give logically incorrect rankings and the ranking does not belong to type 3 ranking 



## not sure if we can integrate two between-subject conditions.
DV3_df <- df %>%
  select(ID, between_subject_condition, within_subject_condition, error_type) %>%
  drop_na() %>% 
  filter(error_type != 2)


a3 <- aov_ez("ID", "error_type", DV3_df, between = "between_subject_condition", within  = "within_subject_condition")
a3

emmeans(a2, c("within_subject_condition", "between_subject_condition"))


# afex plot
afex_plot(a3, "within_subject_condition", "between_subject_condition", dodge = 0.8,
          data_geom = ggbeeswarm::geom_quasirandom,
          data_arg = list(
            dodge.width = 0.8,  ## needs to be same as dodge
            color = "Grey27"),
          point_arg = list(size = 2), line_arg = list(size = 1),
          error_arg = list(size = 0.8, width = 0),
          factor_levels = list(between_subject_condition = c("allowed", "not allowed")), 
          legend_title = "Ties") + 
  ylab(expression(paste("Pr(Type 1 | illogical, not Type 3)"))) + 
  xlab("Type of events") +
  theme(plot.margin = margin(l = 20, t=35, b=35, r=20)) +
  scale_x_discrete(labels=c("indiff" = "middle events", "extreme" = "edge events"))+ 
  ggplot2::theme(axis.text=element_text(size=14),
                 axis.title=element_text(size=16),
                 axis.title.y  = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                 axis.title.x =  element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 legend.text=element_text(size=14),
                 legend.title = element_text(size = 14),
                 legend.position = "bottom") 

#ggsave("conditional_probability_of_giving_type_1.jpg") 


