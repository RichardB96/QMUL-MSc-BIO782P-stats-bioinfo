attach(Marine_diversity)
#Initial viewing of the data to observe obvious trends and data distribution
boxplot(UniFracInd~season,xlab="season",ylab="UniFracInd",main="Microbial diversity by Season",col="steel blue")
boxplot(UniFracInd~latitude,xlab="latitude",ylab="UniFracInd",main="Microbial diversity by latitude",col="steel blue")
#Test for normal distribution of the UnifracID over the samples
shapiro.test(UniFracInd)
#We know that the data is normally distributed,so can perform a T-test on the data
#First the data is separated by season
UnifracJan<- subset(UniFracInd, season=="Jan")
UnifracAug<-subset(UniFracInd,season=="Aug")
#Performed a T-test on the two subsets to see if there is a significant difference between diversity by season
t.test(UnifracJan,UnifracAug,paired=TRUE)
#p=0.01604, reject null hypothesis, season has significant effect on diversity
#Repeated for latitude
UnifracTrop<-subset(UniFracInd,latitude=="tropical")
UnifracTemp<-subset(UniFracInd,latitude=="temperate")
t.test(UnifracTemp,UnifracTrop,paired=TRUE)
#p=0.3113, accept null hypothesis, latitude has no significant effect on diversity
#Performed Anova on the dataset to test for interactions between season and latitude on diversity
MarineAnova1<-lm(UniFracInd~season*latitude)
anova(MarineAnova1)
#Anova agrees with t-test results that there is no significant interaction between diversity and latitude
#But that there is an interaction between season and diversity.
#Anova also suggests there is no joint interaction between season and latitude on diversity (p=0.846)