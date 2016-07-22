install.packages("ggplot2") 
install.packages("ggthemes")
install.packages("dplyr")
install.packages("lubridate")

library(lubridate) 
library(ggplot2)
library(ggthemes) # visualization
library(dplyr)

AnimalShelter.train <- read.csv("C:/Web/animalshelter/data/train.csv",stringsAsFactors = F)
AnimalShelter.test <- read.csv("C:/Web/animalshelter/data/test.csv",stringsAsFactors = F)

AnimalShelter.train$timeDelivert <- ceiling(hour(AnimalShelter.train$DateTime) / 6)
#AnimalShelter.train$Intact <- 

summary(AnimalShelter.train)

outcomes <- AnimalShelter.train[1:26729, ] %>%
  group_by(AnimalType, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(outcomes, aes(x = AnimalType, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Cats & Dogs') +
  theme_few()

Dog.train <- filter(AnimalShelter.train, AnimalType == "Dog")

Cat.train <- filter(AnimalShelter.train, AnimalType == "Cat")

#dogs based on sex
outcomes_dog <- Dog.train %>%
  group_by(SexuponOutcome, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(outcomes_dog, aes(x = SexuponOutcome, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Dogs') +
  theme_few()

#cats based on sex
outcomes_cat <- Cat.train %>%
  group_by(SexuponOutcome, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(outcomes_cat, aes(x = SexuponOutcome, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Dogs') +
  theme_few()

AnimalShelter.train$timeDelivert <- ceiling(hour(AnimalShelter.train$DateTime) / 6)

outcomes_dog.delivered <- Dog.train %>%
  group_by(timeDelivert, OutcomeType) %>%
  summarise(num_animals = n())

ggplot(outcomes_dog, aes(x = SexuponOutcome, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Animal',
       title = 'Outcomes: Dogs') +
  theme_few()

outcomes<- AnimalShelter %>%
  group_by(year(DateTime), Color) 

