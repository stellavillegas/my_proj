####CALF GROWTH COSTS####
#Calculated based on the total energy value of increase in MJ/kg for the change in mass (kg) of newborn calves (Sumich
#1986, Table 5.3, p.75) and calves when they leave the lagoon at 3 months (Sumich 1986 p.74 & Table 5.2 & 5.3). And 
#applying that value of increase (MJ/kg) to calf mass gain during north migration (3-5.75 months old) and foraging 
#grounds (5.75-7 months old, when they are weaned). 
#Point estimate of length in meters for a newborn calf, from Sumich 1986 p.66 Table 5.1, Rice & Wolman 1971 & Rice 1983
#and a 3-7 month old calf from graph in Wahrenbrock 1974, fig.2, from Gigi II actual measurement (real data) & 
#regression line
length_newb <- 4.6
length_3mo <- 5.69
length_5.75mo <- 6.2
length_7mo <- 6.5
#Calculating newborn girth as G=0.5Length from 5 live neonates (Norris & Gentry 1974 and Kooyman, pers. comm in Sumich
#1986, pg.74. And for older calves as G=0.60Length from Gigi II real measurements at 2 months of age  and at 14 months
#old, 0.60L remains the same at least through their first summer, Wahrenbrock pers. comm. in Sumich 1986, pg.75 & 75.
girth_newb <- 0.5*length_newb
girth_3mo <- 0.60*length_3mo
girth_5.75mo <- 0.60*length_5.75mo
girth_7mo <- 0.60*length_7mo
#Calculating mass in kg from Sumich 1986, table 5.3, pag.75 based on equation on page 74, Mass = 18.7*girth*length^2 
#regression eq. obtained from data from 9 gray whales weighted (real data) from several authors (See Sumich 1986, Table
#5.2, pg.73), including neonates, immatures & adults, R^2=0.970
Mass_newb <- 18.7*girth_newb*(length_newb^2)
Mass_3mo <- 18.7*girth_3mo*(length_3mo^2)
Mass_5.75mo <- 18.7*girth_5.75mo*(length_5.75mo^2)
Mass_7mo <- 18.7*girth_7mo*(length_7mo^2)
Calf0_3moMassChange <- (Mass_3mo - Mass_newb)/1000
#Assuming a newborn lipid mass of 5% (fraction lipid content (Flipc) (Sumich 1986, p.136, Table 7.1 and p.135, fig.
#7.3, only 3 values given for uncertainty mean:4.66 +-2.08 SD), a muscle mass of 12% (fraction muscle content (Fmusc)
#(Sumich 1986, p.163, for uncertainty see Sumich 1986 p.155 Table 8.3 and Rice & Wolman 1971, Table 5, but there's not
#much) and a remaining % of "other protein" mass (1 -Flipc - Fmusc) of 83% (fraction "other protein" content(Fothc)).
#Assuming a 3 month old, departing lagoon calf lipid mass of 34% (Flipc_3mo), muscle mass of 18% (Fmusc_3mo) and
#"other protein" mass of 48% (Fothc_3mo) (Sumich 1986 p.163). 
#Lipid and protein masses calculated based on adult (not newborn lipid mass) late pregnant females on their South
#migration (n=54) and post-partum females on their north migration (n=2) both caught near San Fco.(Rice & Wolman 1971,
#p.31, fig.8) and reconstruction of temporal changes in lipid and muscle mass contents by Sumich 1986, p.163 Table 8.5
#based on extracted oil and meat from whales (Rice & Wolman 1971).Mass in kg*10^3
Flipc<-0.05
Fmusc<-0.12
Fothc<-0.83
Flipc_3mo<-0.34
Fmusc_3mo<-0.18
Fothc_3mo<-0.48
#Fraction lipid, muscle and other mass in newborn (e.g.Flipnewb) and 3 month old calves (e.g.Flip_3mo) and fraction 
#lipid, muscle and other mass change
Flipnewb<- Mass_newb/1000*Flipc
Fmusnewb<- Mass_newb/1000*Fmusc
Fothnewb<- Mass_newb/1000*Fothc
Flip_3mo<- Mass_3mo/1000*Flipc_3mo
Fmus_3mo<- Mass_3mo/1000*Fmusc_3mo
Foth_3mo<- Mass_3mo/1000*Fothc_3mo
FlipcalfChange <- Flip_3mo - Flipnewb
FmuscalfChange <- Fmus_3mo - Fmusnewb
FothcalfChange <- Foth_3mo - Fothnewb
#Fraction lipid, muscle and other in added mass
FlipcalfaddedMass <- FlipcalfChange/Calf0_3moMassChange
FmuscalfaddedMass <- FmuscalfChange/Calf0_3moMassChange
FothcalfaddedMass <- FothcalfChange/Calf0_3moMassChange
#Energy value of increase (Qi)in MJ/kg in lipid, muscle and other protein mass, assuming 100% of lipid mass is energy,
#22% of muscle mass is protein (Arai & Sakai 1952, In Sumich 1986, p.163) and 12% of "other mass" is protein (Sumich 
#1986 p.163, Table 8.5 also see "calf growth & newborn energetics" spreadsheet). Assuming a lipid caloric equivalent of
#39.7 MJ/kg (Schmidt-Nielsen 1990) and a protein caloric equivalent of 23.8 MJ/kg (for stored protein) (Kleiber 1975). 
Qicalflip <- FlipcalfaddedMass*39.7
Qicalfmus <- FmuscalfaddedMass*0.22*23.8
Qicalfoth <- FothcalfaddedMass*0.12*23.8
#Total energy value of increase in MJ/kg from newborn calves to 3 month old (Sumich 1986 p.163 estimated this for 
#calves but using adult data)
Qicalftotal <- Qicalflip + Qicalfmus + Qicalfoth
#Cal growth costs in Mcal during north migration and at foraging grounds by means of weight gain (kg). Mcal=4.2 MJ
Calf3_5.75moMassChange <- Mass_5.75mo - Mass_3mo
Calf5.75_7moMassChange <- Mass_7mo - Mass_5.75mo
CalfGrowthCost_North <- (Calf3_5.75moMassChange*Qicalftotal)/4.2
CalfGrowthCost_Forag <- (Calf5.75_7moMassChange*Qicalftotal)/4.2

#Adding code to test