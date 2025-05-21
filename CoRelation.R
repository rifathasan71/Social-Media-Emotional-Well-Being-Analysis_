
library(readr)
library(dplyr)


data <- read.csv("C:/Users/Hp/Desktop/train.csv")


str(data)
sum(is.na(data))
colSums(is.na(data))
data[data == ""] <- NA
colSums(is.na(data))

wrong_gender <- data$Gender[251:289]
wrong_age <- data$Age[251:289]
wrong_gender2 <- data$Gender[751:788]
wrong_age2 <- data$Age[751:788]
data$Gender[251:288] <- wrong_age
data$Age[251:288] <- wrong_gender
data$Gender[751:788] <- wrong_age2
data$Age[751:788] <- wrong_gender2


data$Gender[791] <- "Non-binary"
data[data$Age == "Non-binary", ]
data$Age[791] <- 27


data[data$Platform == "Female", ]
data[data$Platform == "Non-binary", ]
data[data$Platform == "Male", ]

head(data)     
nrow(data)      

unique(data$Gender)
unique(data$Age)
unique(data$Platform)
unique(data$Dominant_Emotion)





data <- data[-751, ]
data <- data[data$Platform != "Male", ]
data <- data[-c(760, 769, 780), ]


duplicated(data)
data[duplicated(data), ]

write.csv(data, "C:/Users/Hp/Desktop/train.csv", row.names = FALSE)

anova_result <- aov(Likes_Received_Per_Day~ Gender, data = data)
summary(anova_result)
TukeyHSD(anova_result)


table_data <- table(data$Platform, data$Dominant_Emotion)
chi_result <- chisq.test(table_data)
print(chi_result)

str(data$Age)
str(data$Daily_Usage_Time..minutes.)

data$Age <- as.numeric(as.character(data$Age))
data$Daily_Usage_Time..minutes. <- as.numeric(as.character(data$Daily_Usage_Time..minutes.))

cor.test(data$Age, data$Daily_Usage_Time..minutes., method = "pearson")

install.packages("ggplot2") 
library(ggplot2)
ggplot(data, aes(x = Age, y = Daily_Usage_Time..minutes.)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +  
  labs(
    title = "Scatterplot of Age vs Daily Usage Time",
    x = "Age",
    y = "Daily Usage Time (minutes)"
  ) +
  theme_minimal()

data$Dominant_Emotion <- as.numeric(factor(data$Dominant_Emotion, ordered = TRUE))
data$Comments_Received_Per_Day <- as.numeric(factor(data$Comments_Received_Per_Day, ordered = TRUE))

str(data$Dominant_Emotion)

cor.test(data$Comments_Received_Per_Day, data$Dominant_Emotion, method = "kendall")

