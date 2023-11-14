
#### install necessary packages

install.packages(c("meta","metadat"))

#library necessary functions so you can use their specific functions
library(meta)
library(metadat)
library(metafor)

#read in the data of interest.
# ".csv" implies that you are reading in a CSV file 
# if the there is an error check the file type

MetaAnalysisData<-read.csv("Data/CO2metaanalysis_curtis1998.csv",header = T)
#header = T means that we are considering the first row as the names for each column

#view the Dataset

View(MetaAnalysisData)


#lets make a basic plot of the data


plot(MetaAnalysisData$MeanTreatment, MetaAnalysisData$MeanControl)

#find outlier point and delete (* it is study 64*)

#remove the row for study = 63


MetaAnalysisData<-MetaAnalysisData[which(MetaAnalysisData$Study != 63),]
#over wirte variable MetaAnalysisData with MetaAnalysisData with rows that do not have 63 in the Study column



#check plot without outlier
plot(MetaAnalysisData$MeanTreatment, MetaAnalysisData$MeanControl)





# Lets do our meta analysis

res.MetaAnalysisData <-  metacont(
  #number of treatments
  ReplicatesTreatment, 
  #mean of treatmetns
                                 MeanTreatment, 
  #standard deviation of treatment
                                 StandardDevTreatment, 
  #number of controls
                                 ReplicatesControl, 
  #mean of controls
                                 MeanControl,  
  #standard deviation of controls
                                 StandardDevControl,
  #run a fixed effect model and randome effect model
                        comb.fixed = T, comb.random = T,
  #what are the labels for the studies
  studlab = Study,
  #hat is the data set
                        data = MetaAnalysisData, 
  #what to compare here SMD = standardized mean difference
  sm = "SMD",method.smd = "Cohen") 


#view summary statistics
summary(res.MetaAnalysisData)

#check test of heterogenetiy
# p-value < 0.01 indicates that the variance is heterogenous across studies 
#meaning we should use the random effect model, if not used the fixed effect


#lets make a forest plot **note if you have a large number of studies this will not look pretty
pdf("Plots/forest.pdf",paper = "USr")
forest(res.MetaAnalysisData, leftcols = c('Study'),sortvar = TE,prediction = TRUE,print.tau = F,print.tau2 = F)
dev.off()

#the number of studies are too large so we need a different type of graph



#try a drapery plot to make comparisons These tell you a bit more information
#change the thrshold to see how the graph changes
pdf("Plots/drapery.pdf")
drapery(res.MetaAnalysisData, 
        #the column for study labels
        labels = "studlab",
        #print study labels above a threshold
        subset.labels = pval < 0.001,
        #angle for study labels
        srt.labels = 45, 
        #color of labels
        col.labels = "lightpink",
        #color the distribiton of the study pink if it is below threshold or make it clear
        col.study = ifelse(pval < 0.001, "lightpink", adjustcolor( "red", alpha.f = 0)),
        #print all studies
        study.results =T,
        #measure for y axis of interest
        type = "pval", 
        #add legend
        legend = TRUE,pos.legend = "topright")
dev.off()


###if we really want a forest plot we can use other methods
NumberOfStudies<-nrow(MetaAnalysisData)
SequenceOfStudies<- seq(from=1,to=NumberOfStudies,by=20)


### calculate log relative risks and corresponding sampling variances
dat <- escalc(measure="SMD", 
              #number of treatments
             n1i = ReplicatesTreatment, 
              #mean of treatmetns
             m1i = MeanTreatment, 
              #standard deviation of treatment
              sd1i = StandardDevTreatment, 
              #number of controls
              n2i = ReplicatesControl, 
              #mean of controls
             m2i =  MeanControl,  
              #standard deviation of controls
              sd2i = StandardDevControl,
              data=MetaAnalysisData, 
             #what are the labels for the studies
             slab = Study)

res <- rma(yi, vi, data=dat)




#view summary statistics
summary(res)


#plot forest
pdf("Plots/Forest_pretty.pdf")





i<-1

for (i in 1:(length(SequenceOfStudies)-1)) {
  #find the begining and end of studies we want to use
  StartSequence<-SequenceOfStudies[i]
  EndSequence<-SequenceOfStudies[i+1]
 
  #re run our model
  res <- rma(yi, vi, data=dat)
  #break up data so that we can plot it
  res$vi.f <- res$vi.f[StartSequence:EndSequence]


#plot
forest(res,addpred = T)


  
}


#close PDF
dev.off()

