names(RiceData)
RiceData$Country


### We now create the subset Bangladesh and China ###

Bangladesh<-droplevels(subset(RiceData,Country=="Bangladesh"))
China<-droplevels(subset(RiceData,Country=="China"))

StudyCase<-droplevels(rbind(Bangladesh,China))


# We now investigate As in Bangladesh & China #

plot(StudyCase$As~StudyCase$Country,xlab="Country",ylab="As (mg/kg)")
plot(StudyCase$As~StudyCase$Country,xlab="Country",ylab="As (mg/kg)",ylime=c(0,0.8))
kruskal.test(StudyCase$As~StudyCase$Country)

# We now investigate Pb in Bangladesh & China #

plot(StudyCase$Pb~StudyCase$Country,xlab="Country",ylab="Pb (mg/kg)",ylime=c(0,0.6))
kruskal.test(StudyCase$Pb~StudyCase$Country)

# We now investigate Cd in Bangladesh & China #

plot(StudyCase$Cd~StudyCase$Country,xlab="Country",ylab="Cd (mg/kg)",ylime=c(0,1))
kruskal.test(StudyCase$Cd~StudyCase$Country)


####We now compare all the regions: more and more specifications are added to the plot function so to improve the the figure.

plot(Bangladesh$As~Bangladesh$Region,main="Regions within Bangladesh",ylab="As (mg/kg)",ylime=c(0,0.8),las=2,xlab="",cex.axis=0.8)
kruskal.test(Bangladesh$As~Bangladesh$Region)###

plot(China$As~China$Region,main="Regions within China",ylab="As (mg/kg)",ylime=c(0,0.8),las=2,xlab="",cex.axis=0.8)
kruskal.test(China$As~China$Region)###

plot(Bangladesh$Pb~Bangladesh$Region,main="Regions within Bangladesh",ylab="Pb (mg/kg)",ylime=c(0,0.2),las=2,xlab="",cex.axis=0.8)
kruskal.test(Bangladesh$Pb~Bangladesh$Region)###

plot(China$Pb~China$Region,main="Regions within China",ylab="Pb (mg/kg)",ylime=c(0,1),las=2,xlab="",cex.axis=0.8)
kruskal.test(China$Pb~China$Region)###

plot(Bangladesh$Cd~Bangladesh$Region,main="Regions within Bangladesh",ylab="Cd (mg/kg)",ylime=c(0,0.2),las=2,xlab="",cex.axis=0.8)
kruskal.test(Bangladesh$Cd~Bangladesh$Region)###

plot(China$Cd~China$Region,main="Regions within China",ylab="Cd (mg/kg)",ylime=c(0,0.8),las=2,xlab="",cex.axis=0.8)
kruskal.test(China$Cd~China$Region)###




####Testing for statistically significant differences
# This is a non-parametric test called Wilcoxon rank sum, making no assumptions on the shape (for example Normal form) of data distribution
## The p-value is very small (2.2e-16 means something like 0.000000... there are 16 zero figures after the decimal point!)
###This means that the PROBABILITY that Bangladesh and China are EQUAL is small. Conclusion: they are different###
### FOR STATISTICAL TESTING YOU WANT TO HAVE A P-Value < 0.05 TO CLAIM THAT THERE ARE SIGNIFICANT DIFFERENCES###


### We now study correlation between elements##

### As, Pb, Cd

plot(Bangladesh$As,Bangladesh$Cd,xlab="As (mg/kg)",ylab="Cd mg/kg",cex=0.7,type="n")
text(Bangladesh$As,Bangladesh$Cd,"Bangladesh",cex=0.7)
cor.test(Bangladesh$As,Bangladesh$Cd)### The p-value is very small. The correlation is highly significant. Data are not random

plot(China$As,China$Cd,xlab="As (mg/kg)",ylab="Cd mg/kg",cex=0.7,type="n")
text(China$As,China$Cd,"China",cex=0.7)
cor.test(China$As,China$Cd)### The p-value is very small. The correlation is highly significant. Data are not random

plot(Bangladesh$As,Bangladesh$Pb,xlab="As (mg/kg)",ylab="Pb mg/kg",cex=0.7,type="n")
text(Bangladesh$As,Bangladesh$Pb,"Bangladesh",cex=0.7)
cor.test(Bangladesh$As,Bangladesh$Pb)### The p-value is very small. The correlation is highly significant. Data are not random

plot(China$As,China$Pb,xlab="As (mg/kg)",ylab="Pb mg/kg",cex=0.7,type="n")
text(China$As,China$Pb,"China",cex=0.7)
cor.test(China$As,China$Pb)### The p-value is very small. The correlation is highly significant. Data are not random

plot(Bangladesh$Cd,Bangladesh$Pb,xlab="Cd (mg/kg)",ylab="Pb mg/kg",cex=0.7,type="n")
text(Bangladesh$Cd,Bangladesh$Pb,"Bangladesh",cex=0.7)
cor.test(Bangladesh$Cd,Bangladesh$Pb)### The p-value is very small. The correlation is highly significant. Data are not random

plot(China$Cd,China$Pb,xlab="Cd (mg/kg)",ylab="Pb mg/kg",cex=0.7,type="n")
text(China$Cd,China$Pb,"China",cex=0.7)
cor.test(China$Cd,China$Pb)### The p-value is very small. The correlation is highly significant. Data are not random

