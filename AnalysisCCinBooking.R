load("/home/juan/BookingData.RData")
fares <- data.frame(table(Bookingdata$Fare))
names(fares)[1] <- "Fare"
library(ggplot2)
# The following syntax generate Figure 1A
ggplot(fares, aes(x=Fare, y=Freq)) + geom_bar(stat = "identity", fill = "lightgreen") + coord_flip() + ylab("Frequency") + theme(axis.text.x = element_text(color = "black", 
                                                                                                                                                          size = 10, angle = 0),
                                                                                                                                 axis.text.y = element_text(color = "black", 
                                                                                                                                                            size = 10, angle = 0))
# As Lodging records are originally provided in Spanish, we
# translated into Engish as follows
table(Bookingdata$Lodging)
# We considered as synonyms the following 
# lodging types "Albergue", "Hostal", "Pensión", and "Posada"
# Thus, we recoded them as follows
Bookingdata$Lodging[Bookingdata$Lodging=="Albergue"] <- "Hostel"
Bookingdata$Lodging[Bookingdata$Lodging=="Hostal"] <- "Hostel"
Bookingdata$Lodging[Bookingdata$Lodging=="Pensión"] <- "Hostel"
Bookingdata$Lodging[Bookingdata$Lodging=="Posada"] <- "Hostel"
# We also considered as synonims
# "Casa de vacaciones", "Casas o chalets", "Casas rurales"
# "Casas y chalets". So we recoded as follows:
library(tidyverse)
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Casa de vacaciones'= 'House')
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Casas o chalets'= 'House')
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Casas rurales'= 'House')
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Casas y chalets'= 'House')
# "Apartamento" was translated as "Apartment" as follows:
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Apartamento'= 'Apartment')
# "Habitaciones en casas particulares" was translated as
# "Rooms"
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Habitaciones en casas particulares'= 'Rooms')
# "Barcos" (boats) and "Camping" were translated as "other"
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Barcos'= 'Other')
Bookingdata$Lodging <- recode(Bookingdata$Lodging, 'Camping'= 'Other')
lodging <- data.frame(table(Bookingdata$Lodging))
names(lodging)[1] <- "Accommodation"

ggplot(lodging, aes(x=reorder(Accommodation, Freq), y=Freq)) + geom_bar(stat = "identity", fill = "lightgreen") + coord_flip() + ylab("Frequency") + xlab("Accommodation") + theme(axis.text.x = element_text(color = "black", size = 10, angle = 0),
axis.text.y = element_text(color = "black", size = 10, angle = 0))

library(ggridges)
ggplot(Bookingdata, aes(x=Bookingdata$NumberofComments, y=as.factor(Bookingdata$Lodging))) + geom_density_ridges(fill="green", alpha = 0.4) + ylab("Lodging Type") + xlab("Number of Comments") + theme(axis.text.y = element_text(family="Arial", face="bold", colour="black", size=rel(4))) + theme(axis.text.x = element_text(family="Arial", face="bold", colour="black", size=rel(2)))
Bookingdata <- na.omit(Bookingdata)

ggplot(Bookingdata, aes(x=Bookingdata$NumberofComments, y=as.factor(Bookingdata$Lodging))) + 
geom_density_ridges(fill="green", alpha = 0.4) + 
ylab("Lodging Type") + xlab("Number of Comments") + 
theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20), axis.text.y = element_text(family="Arial", colour="black", size=rel(2))) + theme(axis.text.x = element_text(family="Arial", face="bold", colour="black", size=rel(2)))

library(psych)
lodgingcomments <- describe.by(Bookingdata$NumberofComments, group = Bookingdata$Lodging, mat = TRUE)
# Now let's explore the relationship between
# all of our quantitative variables. 
# To fulfil this purpose, we firstly need
# to create an ordinal variable that represents
# the fares, as follows
library(dplyr)
Bookingdata <- mutate(Bookingdata, fares = ifelse(grepl("COP 0 - COP 170.000", Fare), 1,
                                      ifelse(grepl("COP 170.000 - COP 340.000", Fare), 2,
                                      ifelse(grepl("COP 340.000 - COP 520.000", Fare), 3,
                                      ifelse(grepl("COP 520.000 - COP 690.000", Fare), 4,5)))))
BookingFactors <- Bookingdata[c(5:6,8:9)]
pairs.panels(BookingFactors, smooth = TRUE, density = TRUE, digits = 2, method = "spearman", hist.col = "lightgreen", rug = TRUE, stars = TRUE, scale = FALSE, cex.axis = 40)

# Now, we turn to a multivariate analysis 
# by specifying the number of comments as our 
# dependent variable, and all other variables as 
# independent ones. As the statistical distribution
# of number of comments looks like a Poisson, 
# this leads us to model the multivariate
# relationship as a negative binomial regression model
library(MASS)
reg <- glm.nb(NumberofComments ~ ., data = BookingFactors)
summary(reg)
