# This is the code for computing IAA in R and text processing

# Here's where I got it from
# https://corpuslinguisticmethods.wordpress.com/2014/01/15/what-is-inter-annotator-agreement/


# Kappa
# to compute the kappa you need to have rater1 and rater2 options in only one variable, then compute kappa.
# First I need to create the will test it

#remove variables different from the categories
df.all=subset(mydf, select=-c(Timestamp, learning, follow, learning.1, follow.1, learning.2, follow.2, learning.3, follow.3, learning.4, follow.4, learning.5, follow.5, learning.6, follow.6, learning.7, follow.7, learning.8, follow.8, learning.9, follow.9, learning.10, follow.10, learning.11, follow.11, learning.12, follow.12, learning.13, follow.13, learning.14, follow.14, learning.15, follow.15, learning.16, follow.16, learning.17, follow.17, learning.18, follow.18, learning.19, follow.19, learning.20, follow.20, learning.21, follow.21, learning.22, follow.22, learning.23, follow.23, learning.24, follow.24, learning.25, follow.25, learning.26, follow.26, learning.27, follow.27, learning.28, follow.28, learning.29, follow.29, learning.30, follow.30, learning.31, follow.31, learning.32, follow.32, learning.33, follow.33, learning.34, follow.34))

anotadores=c("anotador1","anotador2")


#merge all categories variables in one column
df_kappa=melt(df.all, id.var = c('Terapeuta','number'), variable.name = 'annotated',na.rm = "TRUE")
rm(df.all)

#insert all the replacement of the annotators

#Begin the normalization
#make them all lower case
df_kappa$value=tolower(df_kappa$value)

#remove quotations
df_kappa$value=gsub("\"", "",df_kappa$value)

#remove accents
df_kappa$value=iconv(df_kappa$value, from="UTF-8", to='ASCII//TRANSLIT')

#remove gramatical errors
pattern=c("\"|\\:\\s|\\s$")
df_kappa$value=gsub(pattern, "",df_kappa$value)

#Ejemplo
# load the required library
install.packages("irr")
library(irr)

# read in the dataset
ds.full <- read.delim("https://corpuslinguisticmethods.files.wordpress.com/2014/01/coha_real-ly-good-bad_period_attributive_genre.key", 
                      header=T, sep="\t")
# combine the two columns of the annotators in a single data frame
ds.iaa <- data.frame(ds.full$attributive, ds.full$attributive.anno2)

df_kappa$anotador1=""
df_kappa$anotador1=ifelse(df_kappa$Terapeuta=="anotador1","anotado","")
df_kappa$anotador2=""
df_kappa$anotador2=ifelse(df_kappa$Terapeuta=="anotador2","anotado","")

df_iaa=data.frame(df_kappa$anotador1,df_kappa$anotador2)
df_kappa_exp=as.array(unique(df_kappa$value))
write.csv(df_kappa_exp, "C:/Users/SergioMC/Documents/Equilibrio/Research/data4.csv")
df_kappa_exp[2]



# https://github.com/keighrim/mae-annotation this is our last choice
