# eda ----

# trait variation: mean_radius_mm 
root_traits_clean %>%
  filter(!is.na(mean_radius_mm)) %>%
  ggplot(aes(x = species, y = mean_radius_mm)) +
  geom_boxplot(aes(color = species)) + 
  geom_point(alpha = 0.2) + 
  coord_flip() + 
  facet_wrap( ~ trt) 

# trait variation: root length density
root_traits_clean %>%
  filter(!is.na((rld))) %>%
  ggplot(aes(x = species, y = rld)) +
  geom_boxplot() + 
  geom_point(alpha = 0.2) + 
  coord_flip() + 
  facet_wrap( ~ trt) 
