


# add color to hist
hist(chickwts$weight, xlab="weight", labels=T,
           col=c("seagreen2", "seashell2", "skyblue1"))



# jitter so that you can see overlapping points
plot(cars$speed, cars$dist)
plot(jitter(cars$speed), jitter(cars$dist))

mtext("TopLeft", side=3, adj=0)    # side=3 is top, adj=0 is left 
mtext("TopRight", side=3, adj=1)   # side=3 is top, adj=1 is right


data(chickwts)
head(chickwts)

boxplot(chickwts$weight ~ chickwts$feed)
dotchart(chickwts$weight, groups=chickwts$feed)
stripchart(chickwts$weight ~ chickwts$feed)




## Mosaic plot (categorical vs categorical)

HairEyeColor # this is already table.  Not data.frame

mosaicplot(HairEyeColor)

mosaicplot(HairEyeColor[,,"Male"])


# to make table from data.frame
library(ggplot2)                
data(ggplot2::mpg)         
mosaicplot( table(mpg$class, mpg$cyl) )






<!--------->
## 34. Stacked Barplot

library(ggplot2)
data(ggplot2::mpg)

T1 <- table(mpg$cyl, mpg$class)
barplot(T1, legend=rownames(T1),
        args.legend=list(x="topleft", title="Num of Cly"))


barplot(T1, legend=c("4 cyl","5 cyl","6 cyl","8 cyl"),
        args.legend=list(x="topleft", title="Num of Cly"))

   
mtcars
table(mtcars$vs, mtcars$cyl)













---------------------------------------
###
###
### ggplot2 with data:diamonds
###
###
#######################################


##  ggplot grammer: 
##
##    ggplot(data= , aes(  ))    +    (required)
##      geom_    and/or   stat_  +    (one is required)
##        Facests                     (optional) 
##        Coordinate system           (optional)
##        Themes                      (optional)
##
##  See CheatSheet-ggplot2 for list of available functions




library(ggplot2)
ggplot2::diamonds

names(diamonds)

str(diamonds)

?diamonds

##  A data frame with 53940 rows and 10 variables:
##
##  price : price in US dollars (\$326–\$18,823)
##  carat : weight of the diamond (0.2–5.01)
##  cut   : quality of the cut (Fair, Good, Very Good, Premium, Ideal)
##  color : diamond colour, from D (best) to J (worst)
##  clarity : a measurement of how clear the diamond is (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))
##  x : length in mm (0–10.74)
##  y : width  in mm (0–58.9)
##  z : depth  in mm (0–31.8)
##  depth : total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43–79)
##  table : width of top of diamond relative to widest point (43–95)





###-----------------------------------
###---  Scatter Plot (geom_point)
###

# plot two attributes 
ggplot(data=diamonds, aes(x=carat, y=price)) + geom_point()

# zoom in 
ggplot(data=diamonds, aes(x=carat, y=price)) +
                   geom_point() +
                   coord_cartesian(xlim=c(0,1), ylim=c(0,1500))



# add CUT using color 
ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) + geom_point()






# add CLARITY using shape
ggplot(data=diamonds, aes(x=carat, y=price, color=cut, shape=clarity)) + geom_point()



# you can also use alpha (opacity) and size
ggplot(data=diamonds,
        aes(x=carat, y=price, color=cut,
            shape=clarity, size=depth, alpha=.1)) + geom_point()










# stat layer
base <- ggplot(data=diamonds, aes(x=carat, y=price, color=cut)) 
base + geom_point() + geom_smooth()      #<- what method does does this use? 

base + stat_smooth()      
base + stat_smooth(se=FALSE)      

base + stat_smooth(method="lm", se=FALSE)
base + stat_smooth(method="glm", se=FALSE)
base + stat_smooth(method="gam", se=FALSE)
base + stat_smooth(method="loess", se=FALSE)
base + stat_smooth(method="rlm", se=FALSE)


bsse + stat_point(shape=24, color="red", fill="green") + geom_smooth()      







# facetting layer
p <- ggplot(data=diamonds, aes(x=carat, y=price)) + geom_point()

p + facet_grid(.~cut)
p + facet_grid(cut~.)
p + facet_grid(cut~clarity)
p + facet_wrap(~cut)
p + facet_wrap(~clarity)



P + coord_fixed(ratio=4)

p + xlim(c(3,5))    # cuts off data
 
p + coord_catesian(xlim=c(3,5))  # uses all data, just zooms in













###-----------------------------------
###  Box Plot (geom_boxplot)
###


ggplot(data=diamonds, aes(y=price)) + geom_boxplot()


# use cut() to categorize by CARAT 
ggplot(data=diamonds, aes(x=cut(carat, breaks=12), y=price)) + geom_boxplot()
ggplot(data=diamonds, aes(x=cut(carat, breaks=c(0:6)), y=price)) + geom_boxplot()


# separate and color by CUT
ggplot(data=diamonds, aes(x=cut(carat, breaks=c(0:6)), y=price, color=cut)) +
                      geom_boxplot()


ggplot(data=diamonds, aes(x=cut(carat, breaks=c(0:6)), y=price, fill=cut)) +
                      geom_boxplot()






d2931 <- diamonds[diamonds$carat>=.29 & diamonds$carat<.31, ]
ggplot(data=d2931, aes(x=cut, y=price)) + geom_boxplot()



###-----------------------------------
###  Bar Plot (geom_bar)
###

ggplot( data=d2931, aes(x=clarity, fill=cut)) + geom_bar()
ggplot( data=d2931, aes(x=clarity, fill=cut)) + geom_bar(position="fill")
ggplot( data=d2931, aes(x=clarity, fill=cut)) + geom_bar(position="dodge")





###-----------------------------------
###  Pie Chart (geom_bar + coord_polar)
###

ggplot(data=diamonds, aes(x=clarity, fill=clarity)) + geom_bar()
ggplot(data=diamonds, aes(x=clarity, fill=clarity)) + geom_bar() + coord_polar()

ggplot(data=diamonds, aes(x=factor(1), fill=clarity)) + 
  geom_bar(width=1) +
  coord_polar(theta="y")





###-----------------------------------
###  Histogram (geom_histogram)
###

ggplot( data=diamonds, aes(x=price)) + geom_histogram()
ggplot( data=diamonds, aes(x=price)) + geom_histogram(bins=10)
ggplot( data=diamonds, aes(x=price)) + geom_histogram(binwidth=100)


# color by CUT within each bar 
ggplot( data=diamonds, aes(x=price, fill=cut)) + geom_histogram()


# color by CUT side-by-side
ggplot( data=diamonds, aes(x=price, fill=cut)) + geom_histogram(position="dodge")


# color by CUT all bars 100%
ggplot( data=diamonds, aes(x=price, fill=cut)) + geom_histogram(position="fill")







str(mtcars)

library(ggplot2)
p <- ggplot(data=mtcars, aes(x=mpg))
p + geom_histogram()
p + geom_histogram(bins=6)

p + geom_density(col="blue", fill="red", alpha=.4, linetype="dotted", size=2)









<br><br> \hspace{10mm}

<!--------->
## 66.



p <- ggplot(data=diamonds, aes(x=carat, y=price)) + geom_point()

p + facet_grid(.~cut)
p + facet_grid(cut~.)
p + facet_grid(cut~clarity)
p + facet_wrap(~cut)
p + facet_wrap(~clarity)




ggplot(diamonds, aes(x=log10(carat), y=log10(price))) + geom_point() 

ggplot(diamonds, aes(x=log(carat), y=log(price))) + geom_point() 

ggplot(diamonds, aes(x=carat, y=price)) + geom_point() +
  coord_trans(x="log10", y="log10")

ggplot(diamonds, aes(x=carat, y=price)) + geom_point() +
  coord_trans(x="log", y="log")

ggplot(diamonds, aes(x=carat, y=price)) + geom_point() +
  coord_trans(x="sqrt", y="sqrt")


# Name       Trans         Inverse 
# "exp"      exp(x)        log(y)
# "log"      log(x)        exp(y)
# "log10"    log10(x)      10^y
# "log2"     log(x,2)      2^y
# "logit"    log(x/(1-x))  log(x/(1+e(y)))
# "pow10"    10^x          log10(y)
# "probit"   pnorm(x)      qnorm(y)
# "recip"    x^(-1)        y^(-1)
# "reverse"  -x            -y
# "sqrt"     x^(1/2)       y^2
# "asn"      atanh(x)      tanh(y)

































