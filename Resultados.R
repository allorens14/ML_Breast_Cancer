###################################################-RESULTADOS-####################################################

write.csv(res,"resultados.csv")


ggplot(res,aes(y=res$Accuracy,x=reorder(res$algoritmo,-res$Accuracy))) +
geom_bar(stat="sum") 

#Ordeno el dataframe de resultados y extraigo el primero, el mejor en cuanto a Accuracy
res<- res[order(-res$Accuracy),]
res[1,]



#Genero
col <- c("#ed3b3b", "#0099ff")
par(mfrow=c(2,3))

fourfoldplot(confMRF$table, color = col, conf.level = 0, margin = 1, 
             main=paste("Random Forest (",round(confMRF$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMRF2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("Random Forest PCA (",round(confMRF2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMRF3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("Random Forest LDA (",round(confMRF3$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMRP$table, color = col, conf.level = 0, margin = 1, 
             main=paste("RPart (",round(confMRP$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMRP2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("RPart PCA (",round(confMRP2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMRP3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("RPart LDA (",round(confMRP3$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMKNN$table, color = col, conf.level = 0, margin = 1, 
             main=paste("KNN (",round(confMKNN$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMKNN2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("KNN PCA (",round(confMKNN2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMKNN3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("KNN LDA (",round(confMKNN3$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMC53$table, color = col, conf.level = 0, margin = 1, 
             main=paste("C5.0 (",round(confMC5$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMC52$table, color = col, conf.level = 0, margin = 1, 
             main=paste("C5.0 PCA (",round(confMC52$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMC53$table, color = col, conf.level = 0, margin = 1, 
             main=paste("C5.0 LDA (",round(confMC53$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMNB$table, color = col, conf.level = 0, margin = 1, 
             main=paste("NaiveBayes (",round(confMNB$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMNB2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("NaiveBayes PCA (",round(confMNB2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMNB3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("NaiveBayes LDA (",round(confMNB3$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMGBM$table, color = col, conf.level = 0, margin = 1, 
             main=paste("GBM (",round(confMGBM$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMGBM2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("GBM PCA (",round(confMGBM2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMGBM3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("GBM LDA (",round(confMGBM3$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMSVM$table, color = col, conf.level = 0, margin = 1, 
             main=paste("SVM Linear (",round(confMSVM$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMSVMRad$table, color = col, conf.level = 0, margin = 1, 
             main=paste("SVM Radial (",round(confMSVMRad$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMSVM2$table, color = col, conf.level = 0, margin = 1, 
             main=paste("SVM PCA (",round(confMSVM2$overall[1]*100,2),"%)",sep=""))

fourfoldplot(confMSVM3$table, color = col, conf.level = 0, margin = 1, 
             main=paste("SVM LDA (",round(confMSVM3$overall[1]*100,2),"%)",sep=""))












