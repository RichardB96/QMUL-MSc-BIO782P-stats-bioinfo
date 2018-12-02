full.model<-lm(VLoad~.,data = HIV_load)
step.model<-stepAIC(full.model,direction = "both",trace = FALSE)
summary(step.model)
#gives VLoad ~ CD4 + score_shannon + score_distance, tissue dropped
backwards_final=step(lm(VLoad ~ CD4 * score_shannon * score_distance),direction = "backward")
#formula VLoad ~ CD4 + score_shannon + score_distance + CD4:score_shannon + CD4:score_distance
#AIC=20.02
#Tried reintroducing tissue variable
final_final=step(lm(VLoad ~ score_distance + CD4 + score_shannon + CD4:score_shannon + score_distance:CD4),scope=c(lower=~score_shannon,upper=~ CD4*score_shannon*score_distance*tissue),direction="both")
#formula VLoad ~ score_distance + CD4 + score_shannon + tissue + CD4:score_shannon + score_distance:CD4 + score_shannon:tissue
#AIC=16.43
final_HIV<-lm(VLoad ~ score_distance + CD4 + score_shannon + tissue + CD4:score_shannon + score_distance:CD4 + score_shannon:tissue)
#anova of final_HIV shows high variance in tissue variable, however removing it increases AIC
#VLoad ~ score_distance + CD4 + score_shannon + CD4:score_shannon + score_distance:CD4
#AIC=20.02
#Test viral load data for normality
hist(VLoad)
shapiro.test(VLoad)
#Paired t test prep
VLbrain<-subset(VLoad,tissue=="brain")
VLcord<-subset(VLoad,tissue=="spinalCord")
t.test(VLbrain,VLcord,paired = TRUE)
VLHi<-subset(VLoad,CD4=="hi")
VLLo<-subset(VLoad,CD4=="lo")
t.test(VLHi,VLLo,paired = TRUE)
summary(final_HIV)
anova(final_HIV)
