
Let's visualize this data!
```{r}
library(ggplot2)
library(dplyr)
# Create Data
EduM <- data.frame(
  group= c("Some HS", "HS Grad", "Some College", "College Grad", "Grad Degree"),
  value=c(82,92,306,120,135))

# Compute the position of labels
EduM <- EduM %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(EduM$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.47*prop )
  
  # Basic piechart
ggplot(EduM, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
theme(legend.position="none") +
  geom_text(aes(y = ypos, label = group), color = "black", size=3.5) +
  scale_fill_brewer(palette="Set1")

#Create visual for females
EduF <- data.frame(
  group= c("Some HS", "HS Grad", "Some College", "College Grad", "Grad Degree"),
  value=c(116, 158,772,226,229))

EduF <- EduF %>% 
  arrange(desc(group)) %>%
  mutate(prop = value / sum(EduF$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.47*prop )
  
ggplot(EduF, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start=0) +
  theme_void() + 
theme(legend.position="none") +
  geom_text(aes(y = ypos, label = group), color = "black", size=3.5) +
  scale_fill_brewer(palette="Set2")
```