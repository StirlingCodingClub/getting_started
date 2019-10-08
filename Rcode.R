# Here is your first R script. 
# Any line which begins with a hash (#) is a
# comment, any line without, is a line of code to run You can run code either by
# highlighting it and copy and pasting it into the R Console. Or else, you can
# highlight it and click run in the top right of this frame:

#First we install the package needed:
install.packages("unmarked");
library(unmarked);

# Make sure R knows which folder you use on your computer. Use getwd() to find
# out and setwd() to set the working directory. IN RStudio click on Session in
# the menu bar, then Set Working directory then Choose Directory and then select
# the folder where you data is stored. You won't see your file in the folder at
# this stage, so you just need to know which folder to look for and navigate
# there. If you are struggling here, let me know.

### INSERT DATA
# Make sure that the names of the two columns in your data file are exactly
# Transect and Distance (if not, go back and change them and resave the file)
# What is the name of your file? My data is called Transects.csv
dat <- read.csv("MyTransect.csv", header = TRUE);
# The '<-' symbols above assigns some piece of information (in this
# case your data) to an object (in this case called dat) If we want to look at
# what we have put into an object, we just type its name, like this:
dat;
#And R will print out the contents of the object.

#We can look at different elements of a data like this:
head(dat);
names(dat);
str(dat); 
attributes(dat); 
ncol(dat);
nrow(dat); 
summary(dat); 
plot(dat);
hist(dat$Distance); 
# use 'hist' to check whether you have mistyped any values, there shouldn't be
# any outliers

# We divide our data into bins based on distance classes. Assuming the furthest
# we can see is 10m, we can use 6 classes, starting at 0m and ending at 10m. If
# your data has observations further than 10m from the transect line, see if you
# can make the necessary adjustments to the code to look at these further
# distance classes. (But stick with 10m to start with)
yDat <- formatDistData(dat, distCol = "Distance", transectNameCol = "Transect",
                       dist.breaks = c(0, 2, 4, 6, 8, 10));

# Have a look at the new format of the data:
yDat;

# We record the length of each transect (remember to do this in the correct
# order) 
tr_length <- c(28, 28, 60);

# We need to further transform the data, making sure we have inputed the right
# transect length and number in the length section
umf <- unmarkedFrameDS(y=as.matrix(yDat),  
                       survey= "line", dist.breaks = c(0, 2, 4, 6, 8, 10), 
                       tlength=tr_length, unitsIn="m");

#Have a look again:
umf

# Or:
summary(umf)

# Or, make a histogram:
hist(x = umf, xlab = "Distance (m)", main = "", cex.lab = 0.8, cex.axis = 0.8);

# Now we are fitting a model to these data - you can read the tutorial document
# to think about alternative models we could have fitted. The model recognises
# that detection probability declines with distance from the transect line and
# can be used to 'correct' for the missed individuals and give you an estimate 
# of the true number of individuals at the site
hn_Null <- distsamp(formula = ~1~1, data = umf, keyfun = "halfnorm", 
                    output = "density", unitsOut = "ha");

# Here we plot the detection function
predicted_vals <- predict(object = hn_Null, type='det');
sigmaVAL       <- predicted_vals$Predicted[1]; # Just need the first prediction
gxhn_fun       <- function(x) gxhn(x, sigma = sigmaVAL) # Defines function
plot(gxhn_fun, 0, 10,xlab="Distance (m)",ylab="Detection probability",
     lwd = 2, cex.lab = 1.25, cex.axis = 1.25);

# Think about shape of curve - does it match histogram (plot the two graphs side
# by side)
par(mfrow=c(1,2))
plot(gxhn_fun, 0, 10,xlab="Distance (m)",ylab="Detection probability",
     lwd = 2, cex.lab = 1.25, cex.axis = 1.25);
hist(x = umf, xlab = "Distance (m)", main = "", cex.lab = 0.8, cex.axis = 0.8);

# We use our fitted model to predict the density of individuals in our site (in
# individuals per hectare)
site_level_predict <- predict(hn_Null, type = "state");
site_level_density <- site_level_predict$Predicted; # Gets just the predicted
site_level_density;

### CHECK SITE SIZE
# We think we know the size of the area sampled so we can multiply the density
# by the area to get the number of individuals in our site Our transect length
# varied among transects, but we decided we could see 2m either side of the
# transect line (so the area we observed per transect is (2+2)*transect length).
plotArea_m <- tr_length * 20;
# How large is the area in m divided by 10000 to obtain area in hectares 
plotArea_h <- plotArea_m / 10000;
# Have a look at these numbers
plotArea_h;
# Have a look at the transformed numbers
site_abundance <- site_level_density * plotArea_h;
site_abundance;
# We sum over our three transects to get the total number for the whole area.
N_hat <- sum(site_abundance);
N_hat;

#How does this number compare to the number observed?
dim(dat)[1]; # Gets the total number of rows in dat. Could also use nrow(dat).

# To save the two plots for use in your report, first 'open' a pdf and give it a
# file name, like this:
pdf('Myfigure.pdf');
# Then plot your data:
par(mfrow=c(1,2))
plot(gxhn_fun, 0, 10,xlab="Distance (m)",ylab="Detection probability",
     lwd = 2, cex.lab = 1.25, cex.axis = 1.25);
hist(x = umf, xlab = "Distance (m)", main = "", cex.lab = 0.8, cex.axis = 0.8);
# Then close your pdf:
dev.off();
# You should now see a saved file named 'Myfigure.pdf' in your working directory