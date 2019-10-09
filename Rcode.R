# First we need to read in the Bumpus data below
# Make sure that R is set to the same working directory as Bumpus_data.csv
# Also make sure that the filenames match exact (including capitalisation)
dat <- read.csv(file = "Bumpus_data.csv", header = TRUE);
# So now the whole data table is saved as the variable 'dat'
# You can save it as something different if you want, but avoid spaces

# Let's take a look at the first six rows of the data
head(x = dat); # Note that the 'x = ' is not necessary. 'head(dat)' works fine

# How many rows and columns are in the data?
dim(x = dat);

dat[4, 6]; # First row, second column?

# The below produces a histogram of sparrow mass
# Note that I break the line below mid-function after specifying 'ylab'. This
# isn't required, but it often makes code more readable to break to a new line
# when the line exceeds 80 characters.
hist(x = dat$wgt, main = "", xlab = "Sparrow weight (g)", ylab = "Frequency",
     cex.lab = 1.25, cex.axis = 1.25, col = "grey");

# Let's get a summary of just the sparrow mass
summary(object = dat$wgt);

# Now let's plot sparrow totel length against sparrow mass
plot(x = dat$wgt, y = dat$totlen, xlab = "Sparrow body mass (g)", 
     ylab = "Sparrow body length (mm)", cex.lab = 1.25, cex.axis = 1.25,
     pch = 20); # Note: cex.lab, cex.axis, and pch are purely cosmetic

# Test the correlatoin between total length and mass
cor.test(x = dat$wgt, y = dat$totlen);

# Make a linear model of total length regressed against body mass
our_model <- lm(formula = dat$totlen ~ dat$wgt);

# Now summarise 'our_model' that we saved above
summary(object = our_model);

# Now test if bird mass differs by sex using a t-test
t.test(formula = dat$wgt ~ dat$sex, var.equal = TRUE);

# We could also do a linear model get the same results
our_lm <- lm(formula = dat$wgt ~ dat$sex)
summary(object = our_lm);

# And we can do the same with an ANOVA
our_aov <- aov(dat$wgt ~ dat$sex)
summary(our_aov);