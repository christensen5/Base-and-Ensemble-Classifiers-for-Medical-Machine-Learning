SetupIbcc("bc-wisc-diag")
SetupIbcc("bc-wisc-original")
SetupIbcc("haberman-surv")
SetupIbcc("heart-hungary")
SetupIbcc("ilpd")
SetupIbcc("mammographic")
SetupIbcc("spect")
SetupIbcc("spectF")
SetupIbcc("st-heart")
SetupIbcc("vertebral")



predictions.Simpson.Mtest = read.table("datasets/vertebral/Simpson/output-discrete.csv", sep=" ")
start = length(tv.Mtrain.vertebral) + 1
end = length(tv.Mtrain.vertebral) + length(tv.Mtest.vertebral)

AUC.ensemble = matrix(nrow=3, ncol=1)
rownames(AUC.ensemble) = c("AUC", "CI-lower", "CI-upper")
colnames(AUC.ensemble) = "IBCC"

rocCurve = roc(as.numeric(tv.Mtest.vertebral), predictions.Simpson.Mtest[seq(start,end,1),3], ci=TRUE, direction="auto")

AUC.ensemble[1,1] = as.numeric(rocCurve$auc)
AUC.ensemble[2,1] = as.numeric(rocCurve$ci)[1]
AUC.ensemble[3,1] = as.numeric(rocCurve$ci)[3]

write.table(AUC.ensemble, "datasets/vertebral/Simpson/AUCci(discrete).txt", quote=FALSE)



SetupIbcc("bc-wisc-diag", super=FALSE)
SetupIbcc("bc-wisc-original", super=FALSE)
SetupIbcc("haberman-surv", super=FALSE)
SetupIbcc("heart-hungary", super=FALSE)
SetupIbcc("ilpd", super=FALSE)
SetupIbcc("mammographic", super=FALSE)
SetupIbcc("spect", super=FALSE)
SetupIbcc("spectF", super=FALSE)
SetupIbcc("st-heart", super=FALSE)
SetupIbcc("vertebral", super=FALSE)


predictions.Simpson.Mtest = read.table("datasets/bc-wisc-diag/Simpson/output-discrete-unsup.csv", sep=" ")

AUC.ensemble = matrix(nrow=3, ncol=1)
rownames(AUC.ensemble) = c("AUC", "CI-lower", "CI-upper")
colnames(AUC.ensemble) = "IBCC"

rocCurve = roc(as.numeric(tv.Mtest.bcWiscDiag), predictions.Simpson.Mtest[seq(1,length(tv.Mtest.bcWiscDiag),1),3], ci=TRUE, direction="auto")

AUC.ensemble[1,1] = as.numeric(rocCurve$auc)
AUC.ensemble[2,1] = as.numeric(rocCurve$ci)[1]
AUC.ensemble[3,1] = as.numeric(rocCurve$ci)[3]

write.table(AUC.ensemble, "datasets/bc-wisc-diag/Simpson/AUCci(discrete)(unsup).txt", quote=FALSE)

