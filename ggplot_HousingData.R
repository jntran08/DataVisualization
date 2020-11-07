install.packages("ggplot2")
install.packages("tidyverse")
install.packages("lme4")
library(tidyverse)
ggplot2::mpg
str(mpg)
library(reshape2)
install.packages("Amelia")
library(Amelia)
install.packages("mice")
install.packages("VIM")
par(mfrow = c(1, 1))
#Problem1a
#3.2.4 exercise 4 and 5 
ggplot(mpg,aes(x=hwy,y=cyl))+geom_point() # scatterplot between hwy and cyl
ggplot(mpg,aes(x=class,y=drv))+geom_point() #scatterplot betweem class and drw
#the plot is not usefull because the scatterpoint doesnt show any trend
#not many datapoints
#3.3.1 exercise 3

View(mpg)
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,col=cty)) #map cty to color
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,size=cty)) #map cty to size
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,shape=cty)) #map cty to shape

ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,col=drv)) #map drv to color
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,size=drv)) #map drv to size
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,shape=drv)) #map drv to shape

#aesthetic color and size works fine for both continuous and categorical variables
#aesthetic shape only works for categorical variables

#3.3.1 exercise 4
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,size=cty,col=cty))
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,shape=drv,size=drv,col=drv))

#Mapping same variables to multiple-aesthetics, then the data points would be more recognized in 3rd aesthetics with continuous. But categorical variables, the data points would be distinguished by shape and color.

#3.3.1. exercise 5
ggplot(data=mpg)+geom_point(mapping = aes(x=displ,y=hwy,col=displ<5))
# identify variable displ into TRUE and FALSE 

#3.5.1 exercise 4

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
#the bins split to different numerical integer

#problem1b
library(lme4)
mixed<-lmer(hwy~displ+(1+displ|drv),data=mpg)
mpg$fit_mix<-predict(mixed)
ggplot(data=mpg,aes(x=displ,y=hwy))+geom_point(alpha=0.2)+facet_wrap(~drv)+
  geom_smooth(method="loess")+
  geom_line(aes(y=fit_mix),size=0.75)+
  labs(y="Highway MPG",x="Displacement")+
  theme_light()

#problem2a
x<-seq(-1,2,length=500) #generate random vector x
binom<-rbinom(x,20,0.2) # use random binominal distribution generate vector binom
rcauchy(500,scale=2) #generate random samples with cauchy distribution 

#create dataframe df with 500 rows and each variables generate randomly from different types of distribution
df<-data.frame("a"=rnorm(100,mean=1,sd=1.5),"b"=rpois(500,lambda=4),"c"=binom,"d"=rcauchy(10,scale=1)) 
#norman distrubition, poisson distribution, binary distribution, cauchy distribution
nrow(df) #check datafram have 500 rows
df2<-gather(data=df,key="groupVar",value="value")
nrow(df2) #check if 2000 rows
#Problem2b
#draw density overlaid plot
ggplot(data=df2,aes(x=value,fill=groupVar))+geom_density(alpha=0.25)+scale_alpha(range=c(0,4,0.8))

#Problem3
housing<-read.csv(file="housingData.csv", header=TRUE,sep=",") #upload and read data
view(housing)
str(housing)
#create 5 explotary analysis with ggplot
housing1<-housing[!(housing$WoodDeckSF==0),]
ggplot(data=housing1,aes(y=GrLivArea,x=WoodDeckSF))+geom_point()+geom_smooth(method="lm",se=FALSE,span=1.5)+facet_wrap(~OverallQual)
#visualiation 1

housing$SalePrice
ggplot(data=housing,aes(x=GrLivArea,y=SalePrice))+geom_point()+geom_smooth(method = "glm")+
  geom_point(aes(fill=MSZoning), 
             alpha=I(.65),                               
             position = "jitter",                        
             colour="black",pch=21, size=5) +
  theme_bw() +
  labs(y = "Sale Price",
       x = "Above grade(ground) living aera squarefeet") +
  theme(legend.key=element_blank(),
        axis.title = element_text(size = 13))
#visualization 2
housing2<-housing[!(housing$BsmtFinSF1==0),] #remove zero square foot basement type 1
ggplot(data=housing2,aes(x=BsmtFinSF1,y=TotalBsmtSF))+geom_point()+facet_wrap(~BsmtFinType1)+geom_smooth(method="lm",se=FALSE,span=1.5)
    labs(x="Type 1 finished square feet basement",y="Total Square feet of basement area")+scale_fill_discrete( labels = c( "Average Living Quarters ", "Below Average Living Quarters","Good Living Quarters ","Low Quality","Average Rec Room","Unf")) 
#visualization 3
ggplot(data=housing,aes(x=GarageCars,y=GarageArea))+geom_boxplot(aes(group=GarageCars,fill=GarageCars))
#visualization 4
housing4<-housing[!(is.na(housing$LotFrontage)),]
linModel<-lm(data=housing4,LotArea~LotFrontage)
housing4$res <- residuals(linModel) 
housing4$fit <- predict(linModel)
ggplot(housing4, aes(x= fit, y=res)) + geom_point() + geom_smooth()
ggplot(data=housing4,aes(x=LotFrontage,y=LotArea))+geom_point()+geom_line(aes(y=fit),size=1,color="red")+facet_wrap(~LotShape)
#visualization 5
ggplot(housing, aes(x=OverallQual, fill=Alley)) +
  geom_density(alpha=0.35) + labs(x = "Overall Quality",title = "Densities for Overall Quality")

#problem4
#problem4a: explore missingness 
#Use Amelia packages
library(Amelia)
??Amelia
packageDescription("Amelia")
help(package="Amelia")
require(Amelia)
data(freetrade) #loaddata
summary(freetrade) #explore data 
summary(lm(tariff ~ polity + pop + gdp.pc + year + country,data = freetrade)) #explore data
#remove the country because the non-numerical value could affect the validity of Amelia resulst
f_boottrap<-freetrade[,-2] 
missmap(f_boottrap) #from Amelia packages
library(mice) #mice packages
md.pairs(freetrade)
md.pattern(freetrade)
?md.pattern
library(VIM) #VIM packages
# Use "aggr" function to also get overall information on missing

f<-aggr(freetrade)
summary(f)
# use VIM function "marginplot" to get a scatter plot that includes information on missing values
marginplot(freetrade[c("tariff","intresmi")], col = c("blue", "red", "orange"))
# all plots have Missing Information
scattmatrixMiss(freetrade)

#problem4b
View(freetrade)
f_4b<-freetrade[,c(2,3)] #extract data to tariff and country
f_4b[is.na(f_4b$tariff),]$tariff<-0 # set NA values to zero
f_4b[!(f_4b$tariff==0),]$tariff<-1 # set non-missing values to 1
#create table and Chisq-test
table_f4b<-table(f_4b$tariff,f_4b$country)
chisq.test(table_f4b)

#remove Phillipines 
f_rm_phil<-freetrade[!(freetrade$country=="Phillipines"),]
#create table and Chisq-test
table_f_rmphil<-table(f_rm_phil$tariff,f_rm_phil$country)
chisq.test(table_f_rmphil)
