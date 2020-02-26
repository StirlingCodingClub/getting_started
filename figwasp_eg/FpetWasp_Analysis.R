# XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX #
# The subsequent code replicates the analyses used in Duthie and Nason (2016) Oikos. All data were collected in between 2007-2013 in the field from populations of Sonoran Desert rock fig (Ficus petiolaris) trees. For this analysis, the file `FpetWasp_Data.csv' is needed. This data file is a table where each row correponds to a single fig syconia (enclosed infloresence) collected in the field. Columns show: 

# Year: Year of collection
# Site: The site from which collection occurred
# Tree: The tree from which the syconia was collected
# 0pt1km: The number of neighbouring F. petiolaris trees within 100 meters
# 0pt5km: The number of neighbouring F. petiolaris trees within 500 meters
# 1km: The number of neighbouring F. petiolaris trees within 1 km
# 2km: The number of neighbouring F. petiolaris trees within 2 km
# Foundresses: The number of foundress corpses collected from the syconia
# Poll: The number of pollinators (offspring of foundresses) collected from the syconia
# Npoll: The total number of nonpollinators collected from the syconia
# SO1: The total number of short Idarnes species 1 collected from the syconia
# SO2: The total number of short Idarnes species 2 collected from the syconia
# LO1: The total number of long Idarnes (one species) collected from the syconia
# Het1: The total number of Heterandrium species 1 collected from the syconia
# Het2: The total number of Heterandrium species 2 collected from the syconia
# Aepoc: The total number of Aepocerus collected from the syconia
# Physo: The total number of Physothorax collected from the syconia
# Seeds: The total number of seeds collected from the syconia
# Crop: An arbitrary number assigned to each crop (unique bout of reproduction)
# Vol: Estimated syconia volume
# Lat: Latitude of the tree from which syconia were collected
# Nearest20: Distance (km) to the 20th nearest tree
# Nearest5: Distance (km) to the 5th nearest tree
# Nearest10: Distance (km) to the 10th nearest tree
# Nearest40: Distance (km) to the 40th nearest tree

# The code below will replicate the statistical analysis of Duthie and Nason (2016). This analysis includes linear models testing how mean foundress counts, pollinator counts, nonpollinator counts, and seeds per crop covary with key variables that test hypotheses stated in Duthie and Nason (2016). They also recreate analyses correlating non-pollinator species wing loading to how species are affected by tree connectivity. After the analyses are replicated, the remaining code re-builds the figures presented in Duthie and Nason (2016). Lastly, analyses for the appendix are also presented.

# Any enquiries about these data or the analysis and code that follows can be made to Brad Duthie (aduthie@abdn.ac.uk; brad.duthie@gmail.com).
# XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX #

NeighAn <- read.table(file="FpetWasp_Data.csv",header=TRUE);

attach(NeighAn);

N20 <- -1*NeighAn[,22]; # This makes N20 increase, not decrease, with tree connectivity

Year <- as.factor(Year); # All of these variables should be factors
Site <- as.factor(Site);
Tree <- as.factor(Tree);

# =========================================================================================
# Below, crop-level means of variables are calculated from the raw data
# =========================================================================================
crop          <- paste(Year,Site,Tree);

c.N20         <- as.numeric(tapply(X=N20[!is.na(N20)],INDEX=crop[!is.na(N20)],FUN=mean));
c.Foundresses <- as.numeric(tapply(X=Foundresses[!is.na(Foundresses)],
                     INDEX=crop[!is.na(Foundresses)],FUN=mean));
c.Poll        <- as.numeric(tapply(X=Poll[!is.na(Poll)],INDEX=crop[!is.na(Poll)],FUN=mean)); 
c.Npoll       <- as.numeric(tapply(X=Npoll[!is.na(Npoll)],
                     INDEX=crop[!is.na(Npoll)],FUN=mean)); 
c.Vol         <- as.numeric(tapply(X=Vol[!is.na(Vol)],INDEX=crop[!is.na(Vol)],FUN=mean)); 
c.Year        <- as.numeric(tapply(X=as.numeric(NeighAn[,1]),INDEX=crop,FUN=mean));
c.Site        <- as.numeric(tapply(X=as.numeric(NeighAn[,2]),INDEX=crop,FUN=mean));
c.LO1         <- as.numeric(tapply(X=LO1[!is.na(LO1)],INDEX=crop[!is.na(LO1)],FUN=mean)); 
c.SO1         <- as.numeric(tapply(X=SO1[!is.na(SO1)],INDEX=crop[!is.na(SO1)],FUN=mean)); 
c.SO2         <- as.numeric(tapply(X=SO2[!is.na(SO2)],INDEX=crop[!is.na(SO2)],FUN=mean)); 
c.Het1        <- as.numeric(tapply(X=Het1[!is.na(Het1)],INDEX=crop[!is.na(Het1)],FUN=mean)); 
c.Het2        <- as.numeric(tapply(X=Het2[!is.na(Het2)],INDEX=crop[!is.na(Het2)],FUN=mean)); 
c.Seeds       <- as.numeric(tapply(X=Seeds[!is.na(Seeds)],
                    INDEX=crop[!is.na(Seeds)],FUN=mean)); 
c.Lat         <- as.numeric(tapply(X=Lat[!is.na(Lat)],INDEX=crop[!is.na(Lat)],FUN=mean)); 
c.Year        <- as.factor(c.Year);
c.Site        <- as.factor(c.Site); # Crop-level data now can be used for analysis below:

# =========================================================================================
# Foundresses: ----------------------------
use       <- which(!is.na(Foundresses)); # Don't use if NA foundress count
c.syco    <- as.numeric(tapply(X=Foundresses[use],INDEX=crop[use],FUN=length));
summary(lm(c.Foundresses~c.N20+c.Vol+c.Lat,weights=c.syco));
# ------------------------------------

# Pollinators: ----------------------------
use       <- which(!is.na(Poll)); # Don't use if NA Poll count
c.syco    <- as.numeric(tapply(X=Poll[use],INDEX=crop[use],FUN=length));
summary(lm(c.Poll~c.N20+c.Vol+c.Lat+c.Foundresses,weights=c.syco));
# ------------------------------------

# Non-pollinators: ----------------------------
use       <- which(!is.na(Npoll)); # Don't use if NA Npoll count
c.syco    <- as.numeric(tapply(X=Npoll[use],INDEX=crop[use],FUN=length));
summary(lm(c.Npoll~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco));
# ------------------------------------

# Seeds: ----------------------------
use       <- which(!is.na(Seeds)); # Don't use if NA Seeds count
c.syco    <- as.numeric(tapply(X=Seeds[use],INDEX=crop[use],FUN=length));
c.Lat.s   <- as.numeric(tapply(X=Lat[use],INDEX=crop[use],FUN=mean));
c.N20.s   <- as.numeric(tapply(X=N20[use],INDEX=crop[use],FUN=mean));
c.fw.s    <- as.numeric(tapply(X=Foundresses[use],INDEX=crop[use],FUN=mean));
c.vol.s   <- as.numeric(tapply(X=Vol[use],INDEX=crop[use],FUN=mean));
c.Poll.s  <- as.numeric(tapply(X=Poll[use],INDEX=crop[use],FUN=mean));
c.NPoll.s <- as.numeric(tapply(X=Npoll[use],INDEX=crop[use],FUN=mean));
summary(lm(c.Seeds~c.N20.s+c.vol.s+c.Lat.s+c.fw.s+c.Poll.s+c.NPoll.s,weights=c.syco));
# =========================================================================================

# Individual non-pollinator species used are analysed below
# =========================================================================================
# LO1: ----------------------------
use     <- which(!is.na(LO1)); # Don't use if NA LO1 count
c.syco  <- as.numeric(tapply(X=LO1[use],INDEX=crop[use],FUN=length));
LO1.s   <- summary(lm(c.LO1~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco)); LO1.s
LO1.r   <- as.numeric(LO1.s$coefficients[2,1]); # Regression coefficient on neighbours
LO1.se  <- as.numeric(LO1.s$coefficients[2,2]); # Regression coefficient standard error

# Het2: ----------------------------
use     <- which(!is.na(Het2)); # Don't use if NA Het2 count
c.syco  <- as.numeric(tapply(X=Het2[use],INDEX=crop[use],FUN=length));
Het2.s  <- summary(lm(c.Het2~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco)); Het2.s
Het2.r  <- as.numeric(Het2.s$coefficients[2,1]); # Regression coefficient on neighbours
Het2.se <- as.numeric(Het2.s$coefficients[2,2]); # Regression coefficient standard error

# SO1: ----------------------------
use     <- which(!is.na(SO1)); # Don't use if NA SO1 count
c.syco  <- as.numeric(tapply(X=SO1[use],INDEX=crop[use],FUN=length));
SO1.s   <- summary(lm(c.SO1~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco)); SO1.s
SO1.r   <- as.numeric(SO1.s$coefficients[2,1]); # Regression coefficient on neighbours
SO1.se  <- as.numeric(SO1.s$coefficients[2,2]); # Regression coefficient standard error

# SO2: ----------------------------
use     <- which(!is.na(SO2)); # Don't use if NA SO2 count
c.syco  <- as.numeric(tapply(X=SO2[use],INDEX=crop[use],FUN=length));
SO2.s   <- summary(lm(c.SO2~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco)); SO2.s
SO2.r   <- as.numeric(SO2.s$coefficients[2,1]); # Regression coefficient on neighbours
SO2.se  <- as.numeric(SO2.s$coefficients[2,2]); # Regression coefficient standard error

# Het1: ----------------------------
use     <- which(!is.na(Het1)); # Don't use if NA Het1 count
c.syco  <- as.numeric(tapply(X=Het1[use],INDEX=crop[use],FUN=length));
Het1.s  <- summary(lm(c.Het1~c.N20+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco)); Het1.s
Het1.r  <- as.numeric(Het1.s$coefficients[2,1]); # Regression coefficient on neighbours
Het1.se <- as.numeric(Het1.s$coefficients[2,2]); # Regression coefficient standard error

# Wing loadings and their standard errors are taken from Duthie et al. (2015) AmNat (on Dryad)
WL   <- c(0.1660479,0.1611573,0.1502240,0.1408515,0.1355617); # From previous analysis.
WLse <- 1.96*c(0.002069708,0.001207202,0.001818673,0.001913731,0.001883616);
# Use the above wing loadings to compare with species-specific regression slopes
RG   <- c(LO1.r,Het2.r,SO1.r,SO2.r,Het1.r);           # Regression slopes from above.
RGse <- 1.96*c(LO1.se,Het2.se,SO1.se,SO2.se,Het1.se); # Standard error from above.
# Plot of wing-loading versus slope is shown below. Correlation test immediately below:
cor.test(WL,RG); # Marginally significant (P == 0.051; R^2 == 0.768)

# =========================================================================================
# Plots of all figures are recreated as in Duthie and Nason (2016) below
# =========================================================================================
# Plot histograms: =====================================================================
# This will produce Figure 1 from Duthie and Nason (2016)
par(mfrow=c(2,2),mar=c(5,5,4,1));
hist(c.Foundresses,freq=TRUE,col="grey30",main="",xlab="Foundresses per syconium",cex.lab=1.5,cex.axis=1.5,breaks=seq(from=0,to=3.5,by=0.25),cex=2);
text(x=3.5,y=200,labels="a",cex=2.5);
par(mar=c(5,1.25,4,1));
hist(c.Poll,freq=TRUE,col="grey30",main="",xlab="Pollinators per syconium",cex.lab=1.5,cex.axis=1.5,breaks=seq(from=0,to=165,by=15));
text(x=250,y=250,labels="b",cex=2.5);
par(mar=c(5,5,4,1));
hist(c.Seeds,freq=TRUE,col="grey30",main="",xlab="Seeds per syconium",cex.lab=1.5,cex.axis=1.5,breaks=seq(from=0,to=300,by=20),ylim=c(0,2.25));
text(x=350,y=19,labels="c",cex=2.5);
par(mar=c(5,1.25,4,2));
hist(c.Npoll,freq=TRUE,col="grey30",main="",xlab="Non-pollinators per syconium",cex.lab=1.5,cex.axis=1.5,breaks=seq(from=0,to=60,by=5));
text(x=130,y=93,labels="d",cex=2.5);
#=======================================================================================

# Plot foundress versus connectivity: ==============================================
# This will produce Figure 2 from Duthie and Nason (2016)
par(mar=c(5,5,1,1));
plot(x=-1*c.N20,y=c.Foundresses,pch=20,cex=1.5,
          xlab=expression(paste("Tree connectivity (km to nearest 20" ^{th}," neighbour)")),
          ylab="Foundresses per syconium",cex.axis=1.25,cex.lab=1.5);
#=======================================================================================

# Plot foundress versus pollinators: ===========================================
# This will produce Figure 3 from Duthie and Nason (2016)
par(mar=c(5,5,1,1));
plot(x=c.Foundresses,y=c.Poll,pch=20,cex=1.5,xlab="Foundresses per syconium",
          ylab="Pollinators per syconium", cex.axis=1.25,cex.lab=1.5);
#=======================================================================================

# Plot pollinators versus non-pollinators: ===========================================
# This will produce Figure 4 from Duthie and Nason (2016)
par(mar=c(5,5,1,1));
plot(x=c.Poll,y=c.Npoll,pch=20,cex=1.5,xlab="Pollinators per syconium",
          ylab="Non-pollinators per syconium", cex.axis=1.25,cex.lab=1.5);
#=======================================================================================

# Plot foundress versus seeds: ===========================================
# This will produce Figure 5 from Duthie and Nason (2016)
par(mar=c(4.5,0.25,0.5,0.25),mfrow=c(2,2),oma=c(0.25,4.5,1,1));
plot(x=c.fw.s ,y=c.Seeds,pch=20,cex=1.5,ylim=c(65,285),
          xlab="Foundresses per syconium",
          ylab="",cex.axis=1.25,cex.lab=1.25);
text(x=2.125,y=279,labels="a",cex=2.5);
plot(x=c.Poll.s ,y=c.Seeds,pch=20,cex=1.5,ylim=c(65,285),
          xlab="Pollinators per syconium",yaxt="n",
          ylab="",cex.axis=1.25,cex.lab=1.25);
text(x=165,y=279,labels="b",cex=2.5);
plot(x=c.NPoll.s ,y=c.Seeds,pch=20,cex=1.5,ylim=c(65,285),
          xlab="Non-pollinators per syconium",
          ylab="",cex.axis=1.25,cex.lab=1.25);
text(x=47,y=279,labels="c",cex=2.5);
plot(x=c.vol.s ,y=c.Seeds,pch=20,cex=1.5,yaxt="n",ylim=c(65,285),
          xlab=expression(paste("Mean syconium volume (",mm^3,")")),
          ylab="",cex.axis=1.25,cex.lab=1.25);
text(x=4000,y=279,labels="d",cex=2.5);
mtext("Seeds per syconium",outer=TRUE,side=2,line=2.5,cex=1.5);
#=======================================================================================

# Plot regression: =====================================================================
# This will produce Figure 6 from Duthie and Nason (2016)
par(mar=c(5,5,1,1));
plot(x=WL,y=RG,pch=20,cex=1.5,xlim=c(0.125,0.175),ylim=c(-1.125,1.125),
          xlab=expression(paste("Wing loading (",mm^3/mm^2,")")),
          ylab=expression(paste("Effect of tree connectivity ",(r[n]), " on wasp density")),
          cex.axis=1.25,cex.lab=1.5);
for(i in 1:length(WL)){
	arrows(x0=WL[i],x1=WL[i],y0=RG[i]-RGse[i],y1=RG[i]+RGse[i],
	angle=90,code=3,length=0.05,lwd=2);
	arrows(x0=WL[i]-WLse[i],x1=WL[i]+WLse[i],y0=RG[i],y1=RG[i],
	angle=90,code=3,length=0.05,lwd=2);
}
B0 <- lm(RG~WL)$coefficients[1];
B1 <- lm(RG~WL)$coefficients[2];
x  <- seq(from=0.8,to=1.22,by=0.0001);
y  <- B0 + B1*x;
points(x=x,y=y,type="l",lwd=1.5,lty="dashed");
text(y=RG[1],x=WL[1]-WLse[1]-0.002,labels=expression(LO1),cex=1.125);
text(y=RG[2],x=WL[2]+WLse[2]+0.0025,labels=expression(Het2),cex=1.125);
text(y=RG[3],x=WL[3]-WLse[3]-0.002,labels=expression(SO1),cex=1.125);
text(y=RG[4],x=WL[4]+WLse[4]+0.0025,labels=expression(SO2),cex=1.125);
text(y=RG[5],x=WL[5]+WLse[5]+0.0025,labels=expression(Het1),cex=1.125);
abline(h=0,lwd=0.8,lty="dotted");
#=======================================================================================

# ==========================================================================================
# Appendix -- using km neighbours instead of connectivity to test hypotheses:
# ==========================================================================================

c.X0pt1km <- as.numeric(tapply(X=X0pt1km[!is.na(X0pt1km)],
                 INDEX=crop[!is.na(X0pt1km)],FUN=mean));
c.X0pt5km <- as.numeric(tapply(X=X0pt5km[!is.na(X0pt5km)],
                 INDEX=crop[!is.na(X0pt5km)],FUN=mean));
c.X1km    <- as.numeric(tapply(X=X1km[!is.na(X1km)],INDEX=crop[!is.na(X1km)],FUN=mean));
c.X2km    <- as.numeric(tapply(X=X2km[!is.na(X2km)],INDEX=crop[!is.na(X2km)],FUN=mean));

c.Xneigh  <- c.X1km; # Replace the c.X1km with one of the above for a different scale

# Foundresses: ----------------------------
use       <- which(!is.na(Foundresses)); # Don't use if NA foundress count
c.syco    <- as.numeric(tapply(X=Foundresses[use],INDEX=crop[use],FUN=length));
summary(lm(c.Foundresses~c.Xneigh+I(c.Xneigh^2)+c.Vol+c.Lat,weights=c.syco));
# -------- Neighbour distance and latitude significant.

# Pollinators: ----------------------------
use       <- which(!is.na(Poll)); # Don't use if NA foundress count
c.syco    <- as.numeric(tapply(X=Poll[use],INDEX=crop[use],FUN=length));
summary(lm(c.Poll~c.Xneigh+I(c.Xneigh^2)+c.Vol+c.Lat+c.Foundresses,weights=c.syco));
# -------- Only foundresses are significant

# Non-pollinators: ----------------------------
use       <- which(!is.na(Npoll)); # Don't use if NA foundress count
c.syco    <- as.numeric(tapply(X=Npoll[use],INDEX=crop[use],FUN=length));
summary(lm(c.Npoll~c.Xneigh+I(c.Xneigh^2)+c.Vol+c.Lat+c.Foundresses+c.Poll,weights=c.syco));
# -------- Only pollinators significant, neighbour is marginally significant

# Seeds: ----------------------------
use           <- which(!is.na(Seeds)); # Don't use if NA foundress count
c.syco        <- as.numeric(tapply(X=Seeds[use],INDEX=crop[use],FUN=length));
c.Lat.s       <- as.numeric(tapply(X=Lat[use],INDEX=crop[use],FUN=mean));
c.Xneighs.s   <- as.numeric(tapply(X=X2km[use],INDEX=crop[use],FUN=mean));
c.fw.s        <- as.numeric(tapply(X=Foundresses[use],INDEX=crop[use],FUN=mean));
c.vol.s       <- as.numeric(tapply(X=Vol[use],INDEX=crop[use],FUN=mean));
summary(lm(c.Seeds~c.Xneighs.s+I(c.Xneighs.s^2)+c.vol.s+c.Lat.s+c.fw.s+c.Poll.s+c.NPoll.s,weights=c.syco));
# ==========================================================================================


# Foundresses with pollinators - Figure A1 of appendix: ----------------------------
# A simple bootstrap function from Manly (2007 p. 46) ##############################
simpleboot <- function(freqs,repli=1000,alpha=0.05){
	vals  <- NULL;
	i     <- 0;
	while(i < repli){
		boot  <- sample(x=freqs,size=length(freqs),replace=TRUE);
		strap <- mean(boot);
		vals  <- c(vals,strap);
		i     <- i + 1;
	}
	vals   <- sort(x=vals,decreasing=FALSE);
	lowCI  <- vals[round((alpha*0.5)*repli)]
	highCI <- vals[round((1-(alpha*0.5))*repli)]
	CIs    <- c(lowCI,highCI);
	return(CIs);
}

# Below looks at syconia-level pollinator increase with foundresses 
vals.Na  <- which(is.na(Poll) | is.na(Foundresses)); # Remove NA values
Poll.Syc <- Poll[-vals.Na];
Foun.Syc <- Foundresses[-vals.Na];
vals.out <- which(Foun.Syc > 4) # Remove three outliers (7, 7, 10).
Poll.Syc <- Poll.Syc[-vals.out];
Foun.Syc <- Foun.Syc[-vals.out];
t.Polls  <- tapply(X=Poll.Syc,INDEX=Foun.Syc,FUN=mean);
t.Found  <- tapply(X=Foun.Syc,INDEX=Foun.Syc,FUN=mean);

mod1     <- lm(Poll.Syc~Foun.Syc+I(Foun.Syc^2));
coef     <- as.numeric(mod1$coefficients);
x        <- seq(from=0,to=4,by=0.01);
y        <- coef[1] + x*coef[2] + x*x*coef[3];

CIs      <- NULL;
for(i in 0:4){
    civ <- simpleboot(freqs=Poll.Syc[Foun.Syc==i]);
    CIs <- rbind(CIs,civ);
}

# Plot foundresses versus pollinators at syconia level ============================
par(mar=c(5,5,1,1));
plot(x=t.Found,y=t.Polls,pch=16,ylim=c(0,100),cex.lab=1.5,cex.axis=1.5,cex=1.5,
     xlab="Foundresses in syconium",ylab="Pollinator offspring in syconium");
points(x=x,y=y,type="l",lwd=2);
for(i in 0:4){
    arrows(x0=t.Found[i],x1=t.Found[i],y0=t.Polls[i],y1=CIs[i,1],angle=90,lwd=2,len=0.1);
    arrows(x0=t.Found[i],x1=t.Found[i],y0=t.Polls[i],y1=CIs[i,2],angle=90,lwd=2,len=0.1);
}
# This will reproduce Figure A1 from Duthie and Nason (2016)
#=======================================================================================


