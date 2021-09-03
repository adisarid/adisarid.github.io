# randomization for illustration purposes
library(tidyverse)
library(glue)

set.seed(0)
responses <- tibble(rating = runif(500)*100, 
                      type = runif(500, 0, 2)) %>% 
  mutate(rating = case_when(rating <= 40 ~ 5,
                            rating <= 60 ~ 4,
                            rating <= 75 ~ 3,
                            rating <= 90 ~ 2,
                            TRUE ~ 1),
         type = if_else(type <= 1, "Complete", "Partial")) %>% 
  mutate(response_time = "Before 2021-06-01") %>% 
  bind_rows(
    tibble(rating = round(runif(100, 1, 5)),
           type = "Partial",
           response_time = "After 2021-06-01"),
    tibble(rating = runif(100)*100) %>% 
      mutate(rating = case_when(rating <= 40 ~ 5,
                                rating <= 60 ~ 4,
                                rating <= 75 ~ 3,
                                rating <= 90 ~ 2,
                                TRUE ~ 1),
             type = "Complete",
             response_time = "After 2021-06-01")
  ) %>% 
  mutate(response_time = factor(response_time, c("Before 2021-06-01", "After 2021-06-01"))) %>% 
  write_csv("content/post/2021-07-30-data-science-forensics/example_data.csv")

ggplot(responses, aes(x = type, fill = factor(rating))) + 
  geom_bar(position = "fill") + 
  facet_wrap(~response_time) + 
  scale_fill_brewer(palette = "RdYlGn") + 
  guides(fill = guide_legend("Rating")) + 
  theme_bw() + 
  scale_y_continuous(labels = scales::percent) + 
  ylab("Respondents [%]") + 
  xlab("") + 
  ggtitle("Distribution of complete vs. partial responses,\nbefore/after June 21")
