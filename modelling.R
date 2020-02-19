## --- AKANO, KESIDE KENNARD ---
## --- Attribution Modelling --- Cost sample dataset was provided

# A Cost budget allocation was provided based on the cost of each channels with data


# Loading my budget sample dataset
campaign_budget_sample <- read.csv('Budgetsample.csv')


# Our pervious models was merged to get our model results 
model_results <- merge(markov_attribution$result,heuristic_attribution)


# Aggregate compaign budget
campaign_budget_total = as.data.frame(
  campaign_budget_sample %>%
    group_by(Channel) %>%
    summarise(total_cost = round(sum(Cost)))
  )



# Join into final results
budget_attribution = merge(model_results, campaign_budget_total, 
                             by.x = "channel_name", by.y = "Channel")




# Calculation of our performance metric/KPI
budget_attribution %>%
  mutate(channel_weight = (total_conversions / sum(total_conversions)),
         cost_weight = (total_cost / sum(total_cost)),
         roas = (channel_weight / cost_weight)*100,
         optimal_budget = total_cost * roas,
         CPA = total_cost / total_conversions) -> budget_attribution



# Saving file in working directory for visualization in Tableau for more business insight
write.csv(budget_attribution, 'KPI.csv')



