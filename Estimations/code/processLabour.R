processLabour <- function()
{
	labourData<-read.table("../data_17_07/oecd_hours.csv",header=T,sep=",",quote='"',na.strings="..")
	names(labourData)[2]<-"Year"
	labourData
}
