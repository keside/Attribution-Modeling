# Title: Attribution Modelling in R
# Name : Akano, Keside, K
#  Data Analyst

# Date: 2020.01.04



#Libraries

if (!require("pacman")) install.packages("pacman")

pacman::p_load(ChannelAttribution,ggplot2,dplyr, 
               lubridate, markovchain, reshape, visNetwork)

###############

# Project Notes

###############

# Summarize project: To build an Attribution Model and calculate the revenue attribution among the marketing channel,

# Identify which channel are responsible for revenue.

###############

# Setting working directory and Import data

##############
# Working directory
#setwd("C:/Users/akano/OneDrive/Desktop/Dstv.data/Raw Data")

## Load dataset
dstv <- read.csv('Customerattributiondata (1).csv')


################

# Evaluate data

################

#--- EDA -- dstv dataSet ---#
summary(dstv)
str(dstv)
head(dstv)
tail(dstv)
table(dstv$REVENUE)


# check for missing values 
is.na(dstv) 
any(is.na(dstv))

length(which(is.na(dstv$REVENUE))) ## 12017

length(which(is.na(dstv$TIMESTAMP_TOUCHPOINT))) # 1

length(unique(dstv$CUSTOMERID)) # 10001

length(which(duplicated(dstv$CUSTOMERID))) # 3159


# verify number of obs and variables

nrow(dstv)
ncol(dstv)

hist(dstv1$REVENUE)


#############

# Preprocess

#############

# Removing SEA-NON-BRAND FROM CUSTOMERID AND ALSO CORRECTING MISPELLED NAMES LIKE ADWORDS/

dstv <- subset(dstv, CUSTOMERID!= "SEA_NON-BRAND\"")

dstv$MARKETINGCHANNEL <- sub("Adwords/", "Adwords", dstv$MARKETINGCHANNEL)

dstv$REVENUE[which(is.na(dstv$REVENUE))] <- 0
                 
dstv$MARKETINGCHANNEL <- gsub("SEA_NON-BRAND\"", "SEA_NON-BRAND", dstv$MARKETINGCHANNEL)

# Using the lubridate to arrange my DateTime

dstv$TIMESTAMP_TOUCHPOINT <- mdy_hm(dstv$TIMESTAMP_TOUCHPOINT)


### Prepare the files - Split path ---- using dplyr Functions
df_data = dstv %>%
  group_by(CUSTOMERID) %>%
  arrange(TIMESTAMP_TOUCHPOINT) %>%
  mutate(path_no = ifelse(is.na(lag(cumsum(REVENUE))), 0, lag(cumsum(REVENUE))) + 1) %>%
  ungroup() %>%
  mutate(path_id = paste0(CUSTOMERID, path_no))



### Prepare the file - Create the paths ----
data_paths = df_data %>%
  group_by(path_id) %>%
  arrange(TIMESTAMP_TOUCHPOINT) %>%
  summarise(path = paste(MARKETINGCHANNEL, collapse = ">"),
            total_revenue = sum(REVENUE)) %>%
  ungroup() %>% 
  mutate(null_revenue = ifelse(total_revenue == 1, 0, 1))



################

# Markov Chain and Heuristic Models ----

################


markov_attribution <- markov_model(data_paths,
                                   var_path = "path",
                                   var_conv = "total_revenue",
                                   var_value = NULL,
                                   order = 3, 
                                   var_null = "null_revenue",
                                   out_more = TRUE)



# 
heuristic_attribution <- heuristic_models(data_paths,
                                          var_path = "path",
                                          var_conv = "total_revenue")



#### Joining models of my Markov results

final_result <- merge(markov_attribution$result, heuristic_attribution)


## Save the output
write.csv(final_result, "revenue_attribution.csv")


################

# Visualization of Revenue Attribution ----

################

## Loading file -- Revenue Attribution
revenue_att <- read.csv("revenue_attribution.csv")


# Re-arranging the factors for channel names for proper visualization

revenue_att$channel_name <- factor(revenue_att$channel_name, 
levels = c("Adwords", "Direct_NON-BRAND", "Referral", "Referral_NON-BRAND", "SEA_NON-BRAND",
    "SEO_BRAND","SEO_NON-BRAND","Social Media organic_NON-BRAND"))



ggplot(data=revenue_att, aes(x=channel_name, y=total_conversions, fill=channel_name)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label= round(total_conversions)),  vjust=-1, color="black",
  position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+ labs(x = "", y = "conversion") + 
  ggtitle("Channel Performance") + theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 30, hjust = 0.6, face = "bold")) +
  theme(panel.grid.major.x = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)
                 
                 

#### Calculating Transition Probability from Markov Chain



transition_prob = markov_attribution$transition_matrix

transition_prob[, c(1,2)] = lapply(transition_prob[, c(1,2)], as.character)



### Visualize the calculated transition matrix inside the model ----

proab_edges <-
  
  data.frame(
    
    from = transition_prob$channel_from,
    
    to = transition_prob$channel_to,
    
    label = round(transition_prob$transition_probability, 2),
    
    font.size = transition_prob$transition_probability * 100,
    
    width = transition_prob$transition_probability * 15,
    
    shadow = TRUE,
    
    arrows = "to",
    
    color = list(color = "#95cbee", highlight = "red")
    
  )



proba_nodes <- data_frame(id = c( c(transition_prob$channel_from), c(transition_prob$channel_to) )) %>%
  
  distinct(id) %>%
  
  arrange(id) %>%
  
  mutate(label = id,color = ifelse(label %in% c('(start)', '(conversion)'),'#4ab04a',
      
      ifelse(label == '(null)', '#ce472e', '#ffd73e')), shadow = TRUE, shape = "box")



visNetwork(proba_nodes,
           
           proab_edges,
           
           height = "2000px",
           
           width = "100%",
           
           main = "Markov Chain Probability Visualized") %>%
  
  visIgraphLayout(randomSeed = 123) %>%
  
  visNodes(size = 5) %>%
  
  visOptions(highlightNearest = TRUE)




final_result2 <- merge(markov_attribution$removal_effects, heuristic_attribution)

#### Reordering the variable levels
final_result2$channel_name <- factor(final_result2$channel_name, 
                                     levels = c("Adwords", "Direct_NON-BRAND", "Referral", "Referral_NON-BRAND", "SEA_NON-BRAND",
                                                "SEO_BRAND","SEO_NON-BRAND","Social Media organic_NON-BRAND"))


#### Visualization of Transition Matrix
ggplot(data=final_result2, aes(x=channel_name, y=removal_effects, fill=channel_name)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  geom_text(aes(label= round(removal_effects, 1)),  vjust=0.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  scale_fill_brewer(palette="Paired")+ labs(x = "", y = "Effect") + 
  ggtitle("Removal Effect Performance") + theme_minimal() +
  theme(axis.text.x = element_text(size = 9, angle = 30, hjust = 0.6, face = "bold")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + guides(fill=FALSE)



################

# Comparing Markov results and Heutristic  ----

################


compare_df = revenue_att[, c("channel_name", "first_touch", "last_touch", "linear_touch")]

compare_df = melt(compare_df, id = "channel_name")





model_comparison <- ggplot(compare_df, aes(x = channel_name, y = value, fill = variable)) + 
  
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  
  scale_fill_manual(labels = c( "First Touch", "Last Touch", "Linear"), 
                    
                    values = c("#e65368",
                               
                               "#4e74ff",
                               
                               "#87BFFF",
                               
                               "#3BCEAC")) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(size = 9, angle = 30, hjust = 0.6, face = "bold")) +
  
  theme(panel.grid.major.x = element_blank()) +
  
  labs(x = "", y = "conversion") +
  
  ggtitle("Markov vs Heuristics") +
  
  theme(plot.title = element_text(hjust = 0.5))



model_comparison




## MULTI CHANNEL ATTRIBUTION


df_multi_paths_tl <- dstv %>%
  
  group_by(CUSTOMERID) %>%
  
  mutate(date = mdy_hm(TIMESTAMP_TOUCHPOINT)) %>%
  
  summarise(path = paste(MARKETINGCHANNEL, collapse = ' > '),
            
            first_touch_date = min(date),
            
            last_touch_date = max(date),
            
            tot_time_lapse = round(as.numeric(last_touch_date - first_touch_date)),
            
            revenue = sum(REVENUE)) %>%
  
  ungroup()

                 
                 
                 