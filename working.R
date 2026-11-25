summary(tumor)


library(dplyr)

tumor_summary <- tumor %>%
  arrange(ID, Day) %>%                 
  group_by(ID) %>%
  summarise(
    Days= list(Day),               
    Sizes= list(Size),        
    mean_size= mean(Size, na.rm = TRUE),
    max_size= max(Size, na.rm = TRUE),
    min_size= min(Size, na.rm = TRUE),
    total_change = last(Size) - first(Size),
    n_Days = n(),
    avg_daily_change =((last(Size) - first(Size)) / first(Size)) /n_Days,
    .groups = "drop"
  )%>%mutate(Group= as.factor(floor(ID/100)))


model<- lm(avg_daily_change~ Group, data=tumor_summary)
summary(model)
