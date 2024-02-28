library(readxl)
recommendations_HW3 <- read_excel("C:/Users/charl/OneDrive/Desktop/R/data/recommendations_HW3.xlsx")
View(recommendations_HW3)

library(tidyverse)
library(ggplot2)

# 2 - Fit an appropriate model to this data to analyse how recommendation mode affects whether a recommendation 
#     is followed. Remember to describe your analysis choices and why you made them. 

recommendations_HW3 <- recommendations_HW3%>%
  mutate(mode = as.factor(Mode))


recom_new <- recommendations_HW3%>%
  group_by(Mode)%>%
  summarise(
    followed = mean(RecommendationFollowed))

recom_new

ggplot(recommendations_HW3, aes(x = RecommendationFollowed, y = Mode)) +
  stat_summary(fun = mean, geom = "bar", fill = "Orange", position = "dodge") +
  labs(x = "Probability of following the recommendation", y = "Sensory Modality") +
  ggtitle("Probability of following the recommendation by sensory modailty")+
  theme_minimal()+
  coord_flip()

# the data shows that participants who had an auditory recommendation followed the recommendation 51.4% of the time
# whereas those who had an auditory recommendation followed it 39.4% of the time. 

recom_glm <- glm(data = recommendations_HW3, 
                 RecommendationFollowed ~ Mode, 
                 family = "binomial")

summary(recom_glm)


# seeing as whether the mode of recommendation could come in the form of auditory or visual, i decided to make a 
# generalised linear model as both mode and recommendation followed is a binary variable. Regressing whether 
# participants followed the recommendation or not depending on mode, the model shows a negative relationship with 
# the visual mode and following the recommendation (B = -0.49). There was a positive association between the auditory 
# mode and following the recommendation (B = 0.06), although this relationship is very small and not particularly 
# meaningful. 



# i wanted to see if there was an effect for whether the recommendation was followed for the sensory modality
# and stimulus type. This makes the model more complicated but certainly more informative by looking at the different
# levels at which the recommendation was followed, depending on sensory modality and stimulus type. 

recom_int_glm <- glm(data = recommendations_HW3, 
                 RecommendationFollowed ~ Mode * Stimulus, 
                 family = "binomial")

summary(recom_int_glm)

plogis(1.158)

# the model shows that only the extension cord stimulus type was predictive of following the recommendation 
# (B = 1.16), which converts to 76% followed the recommendation,  whereas the other stimulus types were not 
# predictive of following the recommendation. This was the case for both sensory modalities also. This 
# model is certainly more informative than the previous more simple one as we can see that the visual condition 
# for the extension cord stimulus was actually positively associated with following the recommendation. 
# Complicated models truly are our saving grace! Praise be!!


# plotting the new  model
    
  ggplot(recom_int_glm, aes(x = Stimulus, y = Mode, fill = RecommendationFollowed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sensory Mode and Recommendation Followed by Stimulus",
       x = "Stimulus Type", y = "Proportion",
       fill = "Recommendation Followed")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# what this horrible  graph tells us is that in the visual condition, only the recommendation on the extension cord
# stimulus prompted people to follow the recommendation (as shown in the light blue column). All the other stimulus
# types were negatively associated with following the recommendation. 


# The authors of the study wondered whether one explanation for the results could be that participants perceive 
# recommendations in one modality as being more competent, intelligent and/or thoughtful than in
# the other. Can youfind any evidence for this? Does it affect your conclusions
# about the effect of modality?

  
# so is there a difference between perceived competence, intelligence and thoughtfulness for the different 
# modalities? a first thought might be to see the composite intellect - the aggregation of the three variables, 
# and see if this shares a relationship with modality. 


Composite_int <- recommendations_HW3%>%
  group_by(Mode)%>%
  summarise(
    Intellect = mean(CompositeIntellect))


Composite_int

# so we can see that the general intellect ratings in the visual condition are higher than that of the auditory 
# condition. Now let's go and test the modality against each separate rating variable. 


# making a generalised linear model accounting for the difference rating variables with sensory modality. So, in 
# this model we are regressing mode against competence, intelligence and thoughtfulness. I chose to do this instead 
# of the composite intellect variable as i believe that looking at the three variables seperately reveals more than 
# looking at an aggregate value. Mode is binary, hence why this model is using family = binomial. 


funky_glm <- glm(data = recommendations_HW3, mode ~ Competent + Intelligent + Thoughtful,
                 family = "binomial")

summary(funky_glm)

# so in the summary we can see that there are significant effects for the intercept and the three variables. Now, 
# making a plot will make this data easier to visualise. I will make three separate plots for simplicity. 

# for competence

ggplot(recommendations_HW3, aes(x = mode, y = Competent, fill = mode)) +
  geom_boxplot() +
  labs(x = "Sensory Mode", y = "Competence") +
  scale_fill_manual(values = c("beige", "pink"))

plogis(-0.35)

# so we can see that 41% of  participants rating the recommendation as less competent in the auditory mode.


# for intelligence

ggplot(recommendations_HW3, aes(x = mode, y = Intelligent, fill = mode)) +
  geom_boxplot() +
  labs(x = "Sensory Mode", y = "Intelligence") +
  scale_fill_manual(values = c("brown", "purple"))  

plogis(0.30)

# we can see that 57% of participants rated the visual mode as more intelligent as those in the auditory mode. 


# for thoughtfulness

ggplot(recommendations_HW3, aes(x = mode, y = Thoughtful, fill = mode)) +
  geom_boxplot() +
  labs(x = "Sensory Mode", y = "Thoughtfulness") +
  scale_fill_manual(values = c("yellow", "orange"))  

plogis(0.27)

# finally, we can see that 56% of participants ratede the visual mode as more thoughtful than those in the auditory
# mode. 


# overall, we can see that there were significant differences for the auditory mode, recommendations were rated as 
# more competent, however, for the visual mode participants rated the visual recommendations are more intelligent 
# and thoughtful. 
