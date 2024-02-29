library(readxl)
recommendations_HW3 <- read_excel("C:/Users/charl/OneDrive/Desktop/R/data/recommendations_HW3.xlsx")
View(recommendations_HW3)

library(tidyverse)
library(ggplot2)
library(lme4)

# 2 - Fit an appropriate model to this data to analyse how recommendation mode affects whether a recommendation is followed. Remember to describe your analysis choices and why you made them. 

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

# the data shows that participants who had an auditory recommendation followed the recommendation 51.4% of the time whereas those who had an auditory recommendation followed it 39.4% of the time. 

recom_glm <- glm(data = recommendations_HW3, 
                 RecommendationFollowed ~ Mode, 
                 family = "binomial")

summary(recom_glm)

plogis(-0.49)
plogis(-0.06)

# seeing as whether the mode of recommendation could come in the form of auditory or visual, i decided to make a generalised linear model as both mode and recommendation followed is a binary variable. Regressing whether participants followed the recommendation or not depending on mode, the model shows 38% who where in the visual mode followed the recommendation. There was a positive association between the auditory mode and following the recommendation, although this relationship is very small and not particularly meaningful. 



# i wanted to see if there was an effect for whether the recommendation was followed for the sensory modality and stimulus type. This makes the model more complicated but certainly more informative by looking at the different levels at which the recommendation was followed, depending on sensory modality and stimulus type. I have decided to put mode as a variable slope and response id as the variable intercept. Mode and response id represent random effects as Mode is variable with auditory and visual modes and response id is random too as this study had a repeated measures design whereby the same participant experienced multiple stimulus types for the same mode. 

recommendations_HW3$ResponseID <- factor(recommendations_HW3$ResponseID)



recom_int_glm <- glmer(data = recommendations_HW3, 
                 RecommendationFollowed ~ Mode + (Mode|ResponseID) * Stimulus, 
                 family = "binomial")

summary(recom_int_glm)

plogis(-0.62)

# the model shows that only the extension cord stimulus type was predictive of following the recommendation (B = 1.16), which converts to 76% followed the recommendation,  whereas the other stimulus types were not predictive of following the recommendation. This was the case for both sensory modalities also. This model is certainly more informative than the previous more simple one as we can see that the visual condition for the extension cord stimulus was actually positively associated with following the recommendation. complicated models truly are our saving grace! Praise be!!


# plotting the new  model
    
  ggplot(recom_int_glm, aes(x = Stimulus, y = Mode, fill = RecommendationFollowed)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sensory Mode and Recommendation Followed by Stimulus",
       x = "Stimulus Type", y = "Proportion",
       fill = "Recommendation Followed")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# what this horrible  graph tells us is that in the visual condition, only the recommendation on the extension cord stimulus prompted people to follow the recommendation (as shown in the light blue column). All the other stimulus types were negatively associated with following the recommendation. 


# The authors of the study wondered whether one explanation for the results could be that participants perceive recommendations in one modality as being more competent, intelligent and/or thoughtful than in the other. Can youfind any evidence for this? Does it affect your conclusions about the effect of modality?

  
# so is there a difference between perceived competence, intelligence and thoughtfulness for the different modalities? a first thought might be to see the composite intellect - the aggregation of the three variables, and see if this shares a relationship with modality. 


Composite_int <- recommendations_HW3%>%
  group_by(Mode)%>%
  summarise(
    Intellect = mean(CompositeIntellect))


Composite_int

ggplot(recommendations_HW3, aes(x = Mode, y = CompositeIntellect, fill = Mode)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(x = "Sensory Modality", y = "Composite Intellect") +
  ggtitle("Composite Intellect by sensory modailty")+
  theme_minimal()

# so we can see that the general intellect ratings in the visual condition are higher than that of the auditory condition. Now let's go and test the modality against each separate rating variable. 


# making a generalised linear model accounting for the difference rating variables with sensory modality. So, in this model we are regressing mode against competence, intelligence and thoughtfulness. I chose to do this instead of the composite intellect variable as i believe that looking at the three variables seperately reveals more than looking at an aggregate value. Mode is binary, hence why this model is using family = binomial. 


funky_glm <- glm(data = recommendations_HW3, mode ~ Competent + Intelligent + Thoughtful,
                 family = "binomial")

summary(funky_glm)

# so in the summary we can see that there are significant effects for the intercept and the three variables. Now, making a plot will make this data easier to visualise. I will make three separate plots for simplicity. 

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

# finally, we can see that 56% of participants ratede the visual mode as more thoughtful than those in the auditory mode. 


# overall, we can see that there were significant differences for the auditory mode, recommendations were rated as more competent, however, for the visual mode participants rated the visual recommendations are more intelligent and thoughtful. 



# 3 


# why would we use a beta distribution? Beta distributions do not assume normality in the data and for this example, student scores may well not be normally distributed, especially since this is such a hard module!! Also, since there are clear bounds to the data set i.e., being between 0-100, we can convert the marks down to 0-1 instead. Beta distributions are more appropriate for percentage data as well, thus it seems appropriate to use it for student scores as the parameters of the B-D are set to reflect the minimum and maximum bounds of the data set. 

alpha = 20
beta = 14

tibble(p = seq(0, 1, 0.01), y = dbeta(p, 20, 10)) %>%
  ggplot(aes(x = p, y = y)) + geom_path()+
  xlab("Student Module Marks")+
  ylab("Number of students")+
  theme_minimal()

# as we can see with this plot, students scores are more likely to range around 67, therefore with an alpha of 3 and a beta of 2 i based my prior around this to reflect it in the beta distribution plot. 


# weakly informative

alpha = 2
beta = 2

tibble(p = seq(0, 1, 0.01), y = dbeta(p, 2, 2)) %>%
  ggplot(aes(x = p, y = y)) + geom_path()+
  xlab("Student Module Marks")+
  ylab("Number of students")+
  theme_minimal()

# this is weakly informative because the density of scores is very broad but slightly positively skewed so it is still informative towards where the scores may lie. 


# informative 

alpha = 3.5
beta = 2

tibble(p = seq(0, 1, 0.01), y = dbeta(p, 3.5, 2)) %>%
  ggplot(aes(x = p, y = y)) + geom_path()+
  xlab("Student Module Marks")+
  ylab("Number of students")+
  theme_minimal()

# this is certainly more informative than the previous plot. this shows that more students will be achieving a grade around the 60-70% range but the proportion of students achieving grades higher than this significantly drops off when grades are higher than 75 

# strongly informative 

alpha = 45
beta = 17

tibble(p = seq(0, 1, 0.01), y = dbeta(p, 45, 17)) %>%
  ggplot(aes(x = p, y = y)) + geom_path()+
  xlab("Student Module Marks")+
  ylab("Number of students")+
  theme_minimal()


# this is strongly informative because the distribution is very tight around the 67 mark. it shows that very a little amount of students will fail the module, and most will do relatively well around the 60-70 mark. students, however, will be unlikely to achieve higher than a 75 mark, with the right tail of the distribution cutting off around the 85-90 mark. 



# Setting  parameters for the beta distribution (it is the same as the first beta distribution created)
alpha <- 20
beta <- 14

# Generating 100 simulated marks from a beta distribution
sim_marks <- rbeta(100, alpha, beta)

# Creating a tibble with simulated marks for the module
sim_marks_tibble <- tibble(Student_Marks = sim_marks)


ggplot(sim_marks_tibble, aes(x = Student_Marks)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black")+
  labs(x = "Student Marks", y = "Frequency", 
       title = "Histogram of Simulated Student Marks from Beta Distribution")+
  theme_minimal()





priors <- tibble(n = 1:50) %>% group_by(n) %>%
  mutate(alpha = rnorm(1, 67, 5), beta = rexp(1, 25))

gen_prior_pred <- function(n, alpha, beta) {
 
 x <- seq(0, 100, 0.01)
 d <- tibble(n = n, alpha = alpha, beta = beta,
                 x = x,
                 y = dnorm(x, alpha, beta))
  
   return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)

 
ggplot(prior_student_marks, aes(x, y, group = alpha)) + 
  geom_path (alpha = 0.5)+
  theme_minimal()+
  labs(x = "student marks", y = "Frequency")+
  ggtitle("Distribution of student marks")





set.seed(100)

priors <- tibble(n = 1:50) %>% 
  group_by(n) %>%
  mutate(alpha = runif(1, 67, 75), beta = runif(1, 10, 30))

gen_prior_pred <- function(n, alpha, beta) {
  x <- seq(0, 100, 0.05)
  y <- dunif(x, min = alpha - beta, max = alpha + beta)
  
  d <- tibble(n = n, alpha = alpha, beta = beta,
              x = x,
              y = y)
  
  return(d)
}

prior_student_marks <- pmap_df(priors, gen_prior_pred)

ggplot(prior_student_marks, aes(x, y, group = n)) + 
  geom_path(alpha = 0.5) +
  theme_minimal() +
  labs(x = "Student Marks", y = "Density") +
  ggtitle("Distribution of Student Marks (Uniform)")
























