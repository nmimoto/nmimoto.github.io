


datasets folder moved up, but not done so in dotCMS.






acci.csv
advertising.csv
AutoMPG.csv
barley.csv
blenders.csv
Boise.csv
BrainToBody.csv
buffalo.csv
canop.csv
car.csv
cbe.csv
cereals.csv
chicago.csv
clear.csv
companyx.csv
concentration.csv
copper.csv
djao2.csv
dowj.csv
earthquakes.csv
engines.csv
erie.csv
feather.csv
Frazier.csv
Gas.csv
gas2.csv
gtemp.csv
houses.csv
interest.csv
iron.csv
jokulsa.csv
lake.csv
light.csv
Lottery.csv
ls2.csv
lynx.csv
mad.csv
magne.csv
MercuryInBass.csv
methane.csv
michigan.csv
milk.csv
neches.csv
NFL1976.csv
NuclearPlants.csv
oldman.csv
oshorts.csv
ozone.csv
pi.csv
Pigs.csv
PitCorrosion.csv
polio.csv
quakes.csv
radios.csv
repair.csv
Rhine.csv
sales2.csv
scott.csv
shampoo.csv
sheep.csv
Snow.csv
SP100.csv
SP500.ad.csv
SP500.csv
SPY.csv
Steel.csv
stream.csv
sunspot.csv
sunspot2.csv
sunspots.csv
Temp.csv
Temp2.csv
Trade.csv
unemployment.csv
volcanic.csv
water.csv
wave.csv
Wind.csv
wine.csv
winnebago.csv
winter.csv
WolfcampAquifer.csv





deaths.TSM from Brockwell and Davis (2002)



---------------------------
R code to load the data directly:

D  <- read.csv("http://gozips.uakron.edu/~nmimoto/pages/datasets/acci.txt")
D1 <- ts(D, start=c(1,1), freq=12)
plot(D1, type='o')







