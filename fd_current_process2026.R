##CFI process to produce firedata_current.csv
#Incomplete - doesn't include validation data yet


##############################Process 1
## Initialize and packages
rm(list=ls())

.libPaths("C:/Dan/RPackages")

library(tidyverse)

setwd("C:/Dan/_Remote_projects/LadderFuels2")

#moisture functions
load(file="./files/wbmc5.rda")
load(file="./files/mcF.rda")
load(file="./files/ISI.rda")

#Get fire database
#fd_old <-read.csv('c:/Dan/_Remote_projects/ccp_git/ccp-cfi/fire_data_may2023b.csv')
fd <-read.csv('./files/fire_data_2026.csv')

#rename for stand-adjusted mc
fd$CFI <- as.factor(fd$CFI1)

## Define fire sites and assign mcSA Density classes and FSG; other minor data edits as needed
fd2 <- mutate(fd, MC.SA_dens=2,
              CFI=CFI1,
              FSG=FSG1) %>%   #all mod density to start
  filter(!num > 113)

#index numbers for different sites
porters <- filter(fd2, str_detect(fire, "PORTER")) %>% pull(num)
sc <- filter(fd2, str_detect(fire, "SC")) %>% pull(num) #south corner fire is low dens.
pg.low <- filter(fd2, str_detect(fire, "PRINCE")) %>%
  filter(str_detect(fire, "# 4")) %>% pull(num)   #select shorter stands at PG site only
pg.high <- filter(fd2, str_detect(fire, "PRINCE")) %>% 
  filter(str_detect(fire, "# 1")) %>% pull(num)
kenshoe <- filter(fd2, str_detect(fire, "KENSHOE")) %>% pull(num)
sharp <- filter(fd2, str_detect(ExpProject, "Sharpsand IM")) %>% pull(num)  #original Sharpsand Immature stands only
sharp.th <- filter(fd2, str_detect(ExpProject, "Sharpsand TH")) %>% pull(num)
sharp.sm <- filter(fd2, str_detect(ExpProject, "Sharpsand SM")) %>% pull(num)
rp <- filter(fd2, str_detect(fire, "PNFI RP")) %>% pull(num)
darwin <- filter(fd2, str_detect(fire, "DARWIN")) %>% pull(num)  #1, 2, 6, 7 are  low density plots
darwin.low <- c(66, 67, 90, 91)
bigfish <- filter(fd2, str_detect(fire, "BIG FISH")) %>% pull(num)
icfme <- 77:87 #
icfme.hd <- c(77:79, 82:87)  #all but p3, p4
pelican <- 109:110  
################################Process 2

#Low density sites:
fd2$MC.SA_dens[c(porters, sc, pg.low, darwin.low, bigfish)] <- 1  #sc, pg.low, darwin?

#High density sites:
fd2$MC.SA_dens[c(rp, kenshoe, sharp, icfme.hd, sharp.sm)] <- 3  #sharp? icfme.hd?

fd2$MC.SA_dens[50] <- 2   #PNFI RP stand that was thinned to expose fuels (?)
fd2$MC.SA_dens[93:94] <- 1  #RWP B1 stands heavily cut over


#Kenshoe Lake

#Main dataset in database-crowning of mid-story spruce: FSG=2m same as VW and Cruz
ken.sp.LCBH <- 2
fd2$FSG[kenshoe] <- ken.sp.LCBH
#make these crown fires: fires described by Stocks as 'various degrees of torching or ICF'
ken.cfList <- c(2, 3, 5, 9, 11, 12) 
new.CFI <- kenshoe %in% ken.cfList %>% as.integer %>% as.factor()
fd2$CFI[kenshoe] <- new.CFI  #chg CFI to reflect subcanopy spruce crowning

#LCBH analysis from raw data mixed-effects modelling
ken.ME0 <- read.csv("./files/kenshoe_crown_sharma4.csv")
ken.ME <- ken.ME0 %>%   #full stand vars from ME modelling
  mutate(ba.per.bs=1-ba.perc,  #percent bs by BA
         st.per.bs=s.ha.bs/(s.ha.jp+s.ha.bs), #percent by density?
         sCD.sp=SH.sp-ken.sp.LCBH, #spruce crown depth
         CL=ken.sp.LCBH+sCD.sp/2, #spruce centroid
         psp.FSG=sLCBH.jp-CL) %>%  #pine-spruce FSG final
  select(-X)

#write.csv(ken.ME %>% select(plot, psp.FSG), './files/kenPSPFSG.csv')
#write.csv(ken.ME, './files/ken_ME.csv')

#################################Process 3

#New Sharpsand - based on mixed-effects modelling, snags as 'elevated' surface fuels
##increase LCBH due to dead standing; 1981 fires have half reduction from 1988 (McRae 2017) fires
# half of 5.3-4.29 = ~1

sharpsand.increase = 0 #0.5?  #change for 1981 plots; IM only (can't assume crown recession for thinned ones)

#
sh.new <- read.csv('./files/sharp_full.csv') 
sh.new$TRT[16] <- 'ctrl'   #change P16 to ctrl (IM), based on high density dead & live stems

sh.new2 <- sh.new %>%
  mutate(FSG=LCBH) %>%
  mutate(FSG=case_when(
    Plot %in% c(15:18) ~ FSG+sharpsand.increase,  #consider doing this only for IM: 17 and 18
    Plot > 0 ~ FSG)) #,
    # s.ha.d=case_when(
    #   Plot %in% c(15:18) ~ s.ha.d*(1-3830/(2*10229)) %>% round(0) %>% as.integer(),  #half of 1974-1988 density chg for 1981 plots
    #   Plot > 0 ~ s.ha.d))

sh.new2$s.ha.d[8:9] <- 1175  #mean of those that look properly thinned (excl. p16)
sh.new2$s.ha.l[8:9] <- 4725.27  #mean of those that look prop. thinned 

#Stocks 1987: CFL dead trees < 1 cm (ctrl/imm only):  mean: 0.161
dead.cfc <- c(0.142, 0.073, 0.313, 0.183, 0.133, 0.146, 0.131, 
              0.176, 0.182, 0.151, 0.218, 0.084) #* 0.815#; use full CFL here I think; 7 & 18 are zero
#Although P7, 18 (ctrl) and P8, 9, 16 (th) are surface fires, assume dead snags burn
dead.cfc.mean <- mean(dead.cfc)

#How much of the dead cfl was consumed? 
#Surface fl < 1 cm: sfl <- c(.072, .1, .07, .101, .078, .098, .04, .081, .062, .099, .111, .121)
#surface fc < 1 cm: sfc <- c(.048, .054, .053, .084, .052, .098, .032, .057, .062, .081, .111, .121)
#mean sfc = 0.8153 * sfl
s.ha.ctrl <- filter(sh.new2, TRT=='ctrl') %>% summarize(mean(s.ha.d))
#mean stems/ha in ctrl stands
dead.cfl.snag <- mean(dead.cfc)/s.ha.ctrl  #mean snag weight cfl (<1 cm) per stem, from ctrl stands

baseline.snags <- 250

#treat dead cfl as elevated surface fuels; height of 1 cm dbh ~ 2.18 m

#Ladder fuel scaling
#load(file='c:/Dan/_Fire_tools/r_scripts_functions/lf-sfc.rda') #check if it works

#
FCse <- function(z, Cl, fcl) {
  (z/(z-Cl))^1.5 * fcl
  #was lf.cfg, with z as zg, Cl as zl
}

scale.exponent <- 3/2   #3/2 per Van Wagner, or 5/3 as per McCaffrey

sh.new.ctrl <- filter(sh.new, TRT=='ctrl') %>% 
  mutate(cfc.d=dead.cfc,
         cfc.d.stem=cfc.d * 10000/s.ha.d,   #kg/m2  * 10000 m2/ha  / s/ha   = kg/stem
         excess.snag.prop= (s.ha.d-baseline.snags)/s.ha.d,
         snag.centroid=HT.D/2,
         sfc.lf=FCse(z=FSG, Cl=snag.centroid, fcl=cfc.d*excess.snag.prop),   #using lf.sfc function
         sfc.lf.stem=sfc.lf/s.ha.d) 

sh.new.th <- filter(sh.new, TRT=='th') %>%
  mutate(cfc.d=s.ha.d*dead.cfl.snag,   #assume dead cfc is actual s.ha.dead * dead cfl/stem
         cfc.d.stem=cfc.d * 10000/s.ha.d,
         excess.snag.prop=case_when(
           s.ha.d > baseline.snags ~ (s.ha.d-baseline.snags)/s.ha.d,
           s.ha.d <= baseline.snags ~ 0),  #avoid negative values
         snag.centroid=HT.D/2,
         sfc.lf=FCse(z=FSG, Cl=snag.centroid, fcl=cfc.d*excess.snag.prop),   #using lf.sfc func
         sfc.lf.stem=sfc.lf/s.ha.d)

#mean additional SFC per dead snag (not used)
sfc.lf2<- mean(sh.new.ctrl$sfc.lf.stem)


sh.new3 <- full_join(sh.new.ctrl, sh.new.th) %>%
  select(-c(sfc.lf.stem, excess.snag.prop)) %>%
  arrange(Plot)

##To here Aug 2026 - analysis moved to ladder_fuels7.qmd
##copied sfc.lf values to firedataMay2023b May 28, 2023
#neex to update; switched to sh.new3 July 14, 2023 (review phase of CFO paper), and didn't update LF

#Snag density, dbh, and height by treatment
dead.sum <- sh.new3 %>% filter(!Plot %in% c(8,9, 16)) %>% group_by(TRT) %>% 
  summarize(snags=mean(s.ha.d),
            dbh.d=mean(DBH.d),
            ht.d=mean(HT.D))


old.sfc.ctrl <- fd2$SFC[sharp]
old.sfc.th <- fd2$SFC[sharp.th]

fd3 <- fd2 %>%
  mutate(SFC.LF=0)

#Assign FSG from the AllSites script, Sharma & Parton modelling
fd3$FSG[sharp.th] <- sh.new3 %>% filter(TRT=='th') %>% pull(FSG) %>% round(2)
#fd3$LCBH[sharp.th] <- sh.new2 %>% filter(TRT=='th') %>% pull(LCBH) %>% round(2)
#fd3$LCBH[sharp] <- sh.new2 %>% filter(TRT=='ctrl') %>% slice(c(1:7, 7, 8:12)) %>%
#  pull(LCBH) %>% round(2)  #add Plot 11 twice for 11A, 11B
fd3$FSG[sharp] <- sh.new3 %>% filter(TRT=='ctrl') %>% slice(c(1:7, 7, 8:12)) %>% 
  pull(FSG) %>% round(2)  #add Plot 11 twice for 11A, 11B
fd3$SFC.LF[sharp] <- sh.new3 %>%  filter(TRT=='ctrl') %>% #add orig. sfc + snag sfc, ctrls and thinned
  slice(c(1:7, 7, 8:12)) %>% pull(sfc.lf)
fd3$SFC.LF[sharp.th] <- sh.new3 %>%  filter(TRT=='th') %>% #add orig. sfc + snag sfc, ctrls and thinned
  pull(sfc.lf)

#No understory conifer or surface fuelbed height?

#Sharp-SM; use dead.cfc.mean to estimate snag contribution
#First, fix row numbers
sm.nums <- fd3$num[sharp.sm]  #extract num values 
sm.arrange <- fd3[sharp.sm,] %>% arrange(fire) %>%   #arrange by fire number (like McRae, except first one)
  mutate(num=sm.nums)

fd3[sharp.sm,] <- sm.arrange

#model SFC by DOB, based on data from McRae et al. 2017; SF from Table 5 first then Table 2
#Not sure this is the way to go for SFC
# sm.DOB <- c(3.2,4.9,4.4,2.2,2.7,2.8,2.7,3,3.5,3.7,4.1,4.6,5,4.9,6,5.5)
# sm.SFC.dat <- c(2.2,2.62,1.41,0.65,1.15,0.71,1.15,1.01,1.51,1.2,1.5,1.79,1.82,1.78,2.27,2.04) #surf. fires only
# sm.DOB.cf <- c(3.7, 4.6, 5.7, 2.5, 4.3)  #03/91 first, then #16-19
# sm.num.cf <- filter(fd3, num %in% sharp.sm,
#                     CFI==1) %>% pull(num)
# 
# sm.sfc.df <- data.frame(sm.DOB, sm.SFC.dat)
# 
# #fit nls to DOB data
# sm.nls <- nls(sm.SFC.dat ~ a*exp(sm.DOB)^b, start=list(a=1, b=1), data=sm.sfc.df)
# 
# #Function for predicting Sharpsand semi-mature SFC from depth of burn
# sm.pred <- function(dob) {   
#   predict(sm.nls, newdata=list(sm.DOB=dob))
# }

#use corrected SFC for SM, based on TFC- estimated CFC from other Sharpsand fires (organized by fire type)
#Plots 14:21
#SF: 0.016 (mean of S fires among SH-IM and SH-TH)
#PC: 0.39
#AC: 1.135
#sm.sfc.corr <- c(2.2, 2.6, 3.11, 3.58, 2.87, 2.02, 1.39, 1.95)  #eh, not quite
sm.sfc.corr <- c(2.18, 2.6, 3.11, 3.58, 2.87, 2.02, 1.39, 1.92)  #corrected July 13
#               (0.02,0.02, 0.20, 0.39, 0.20, 0.39, 0.02, 1.14)  #subtracted based on FT
#               (S  , S  ,ST(S),  T  ,ST(C),  T  , S   , AC)

#ggplot(sm.sfc.df, aes(x=sm.DOB, y=sm.SFC))+geom_point() + stat_function(fun=sm.pred)
sm.SFC.cf <- sm.pred(dob=sm.DOB.cf)
#ok for 'reconstructed' SFC for these fires
#No need for surface fires, since TFC=SFC for those

#model snag SFC contribution; dead s.ha = 3830 for all; height =5.7; 
#Use mean sharp-IM snag contribution to SFC: 0.161

#use new lf.fcg function

sm.new <- data.frame(cfc.d.stem=0.156, s.ha.d=3830, HT.D=5.7, LCBH=5.3, sm.sfc.corr) %>%  #sm.SFC maybe?
  mutate(excess.snag.prop = (s.ha.d-baseline.snags)/s.ha.d,
         fcl=cfc.d.stem * 3830 * excess.snag.prop /10000,
         snag.centroid=HT.D/2,
         sfc.lf=lf.fcg(zg=LCBH, zl=snag.centroid, fcl=fcl))   

#new values copied to firedataMay2023b on May 28

#keep old sfc values
sm.sfc.old <- fd3$SFC[sm.nums]
#sm.sfc.old <- fd3$SFC[sm.num.cf]

#fd3$SFC[sm.num.cf] <- sm.SFC.cf   #keep or not?? new SFC vs original TFC
#fd3$SFC[sm.num.cf] <- sm.sfc.old   #I think original is best actually
fd3$SFC.LF[sm.nums] <- sm.new$sfc.lf[1]   #

#Interpret 'torching' as PC, 'some torching' gets split 50/50 into S and PC
#Do this after previous analysis due to excess SFC from snags in TFC?
some.torch.sf <- fd3[sm.nums,] %>% filter(CFI==1 & ISI < 9) %>% pull(num)
fd3$CFI[some.torch.sf] <- 0
fd3$Fire.type[some.torch.sf] <- 'S'


#end Sharpsand

#ICFME
#updated Aug 2026

#order has been corrected to match Alexander et al 2004 paper; 

#Constants
icfme.lcbh.jp <- c(8.9, 8.1, 8.6, 8.2, 7, 5.2, 7.9, 8.5, 7.4, 7.4, 8.1) #LCBH
#icfme.lcbh.combined <- c(6.5, 7.1, 6.1, 8.2, 7, 6.2, 6.9, 5.8, 3.6, 3.6, 7.2)  #plot 5 est. from remaining 7: 6.2
icfme.cbh.bs <- c(0.9, 2.4, 1.8, 8.2, 7, 0.7, 0.7, 1.3, 1.7, 1.7, 2.4)  #correct order, #Plot 5 corrected from 10 m to 0.7 based on Marty; P3,4 from JP
icfme.lcd.bs <- c(4.4, 3.8, 4, 0, 0, 4.1, 4, 4.3, 4.3, 4.3, 3.2) #correct order
icfme.usH <- c(1.4, 1.8, 1.7, 1, 1, 1.4, 1.8, 1.9, 1.8, 1.8, 1.6)  #correct order; all plots
icfme.us.lcbh <- c(0.4, 0.6, 0.6, 0.2, 0.3, 0.4, 0.7, 0.7, 0.8, 0.8, 0.5)
#Alexander et al 2004, Table 5 data
icfme.snags <- c(1089, 1372, 679, 1912, 1992, 2844, 2253, 1689, 1494, 1494, 2735) #A, 1-9, 8 twice
#Table 4 for heights
icfme.snagHT.jp <- c(7.1, 9.4, 10.2, 7.9, 7.8, 8.3, 7.8, 8.4, 8.1, 8.1, 8.8) #same order
icfme.snagHT.bs <- c(0, 7.5, 6.8, 0, 0, 5.2, 5.7, 4.1, 3.9, 3.9, 6.1) #same

#Table 4 and Table 5 data for snags
icfme.jp.snagDBH <- c(6.7, 7.4, 8.4, 7.3, 4.8, 5.3, 5.4, 5.4, 5.6, 5.6, 5.4) #same order
icfme.bs.snagDBH <- c(0, 6.8, 5, 0, 0, 6.6, 5.8, 3.3, 5.1, 5.1, 5.2) #same order
icfme.snagPercentBS <-c(0, 18.6, 17.5, 0, 0, 6.7, 34.5, 8.5, 17.1, 17.1, 8.3)
icfme.snagDBH.total <-icfme.jp.snagDBH * (1-icfme.snagPercentBS*0.01) + icfme.bs.snagDBH * (icfme.snagPercentBS*0.01)
icfme.snagHT.mean <- icfme.snagHT.jp * (1-0.01*icfme.snagPercentBS) + icfme.snagHT.bs * (0.01 * icfme.snagPercentBS)

#For snags, used only JP snag equations, since they represented vast majority of snags (89%) and there are no dead BS 
#equations
#get 0-5 and .5-1 functions to estimate fuel load; then do SFC equivalent
#functions give kg of dry fuel; need to convert with density to kg/m^2
#kg * density/10000 m^2
icfme.snag.05 <- function(dbh) {
  a=0.02188
  b=1.63319
  Y=a * dbh^b
  return(Y)
}

icfme.snag.51 <- function(dbh) {
  a=0.0006
  b=3.10118
  Y=a * dbh^b
  return(Y)
}

#compute per tree CFC and per plot snag FC
icfme.snag.wt <- icfme.snag.05(icfme.snagDBH.total) + icfme.snag.51(icfme.snagDBH.total)  #kg of fine fuel per tree
icfme.snag.cfc <- icfme.snag.wt * icfme.snags / 10000   #kg/m^2 cfl per plot

#Need to take only proportion below LCBH; do 'centroid' of that proportion (HT/2); for all plots except 3, 4
#icfme.snag.cfc.subCrown

#Process icfme snag SFC.LF 
icfme.lf <- data.frame(      #icfme.snags.add <-filter(fd3, num %in% icfme) %>%    #create new records for ICFME;
  #select(num, fire, MC.SA, FSG, SH, ws, FFMC, DMC, SFC, CFI, MC.FFMC) %>%
  num=icfme,
  fire=fd %>% filter(num %in% icfme) %>% select(fire),
  s.ha.d=icfme.snags, #s/ha
  SH.d=icfme.snagHT.mean,
  cfc.d=icfme.snag.cfc,
  excess.snag.prop = (icfme.snags-baseline.snags)/icfme.snags) %>%
  #
  mutate(LCBH = icfme.cbh.bs,    #just distance to BS LCBH or jp LCBH for p3 & p4
  cfc.subCrown = if_else(  #need special rules when snags are below fsg and above
           LCBH < SH.d, LCBH/SH.d * cfc.d,
           cfc.d),
  Cl = if_else(  #
           SH.d > LCBH, LCBH/2,  #for most icfme plots, snags are taller than bs LCBH, so use the prop
           SH.d * 0.75), #assume CR of 0.5
  fc.se = FCse(z=LCBH, Cl=Cl, fcl=cfc.subCrown * excess.snag.prop))   


#To here Aug 31 2026

#To here May 28 2023; copied SFC.LF to fire_data_may2023b

#icfme.data for discussions and upper canopy crowning work
icfme.data.partial <- tibble(Plot=c('A', 1:8, 8:9), 
                             jp.lcbh=icfme.lcbh.jp, 
                             bs.lcbh=icfme.cbh.bs,
                             bs.H=icfme.lcd.bs+bs.lcbh,
                             U.bs.lcbh=icfme.us.lcbh,
                             U.bs.H=icfme.usH) %>%
  slice(-10)   #get rid of duplicate P8 here 

icfme.data.most <- icfme.data.partial %>%  #get rid of P3, 4
  filter(!Plot %in% c(3, 4))
icfme.data.34 <- icfme.data.partial %>%
  filter(Plot %in% c(3,4)) %>%
  mutate(bs.lcbh=NA,
         bs.H=NA)
icfme.data.A <- icfme.data.partial %>% filter(Plot=='A')
icfme.order <- full_join(icfme.data.most, icfme.data.34) %>%
  arrange(Plot) %>%
  filter(Plot %in% 1:9)
icfme.data <- rbind(icfme.data.A, icfme.order)  

fd3$LCBH[icfme] <- icfme.lcbh.jp
fd3$FSG[icfme] <-icfme.cbh.bs  #use BS mid-story LCBH here, uncorrected

fd3$SFC.LF[icfme] <-icfme.snags.add$sfc.lf

#End main database CFI

##################################################Process 4A

#Final DB prep before modelling: MC.SA calculations and Upper canopy calcs (Kenshoe and ICFME)

#recode any high FFMC and high density to Mod density
fd4 <- fd3 %>% mutate(Exp=factor(ExpProject) %>% as.integer()) %>%
  mutate(MC.SA_dens=case_when(
    FFMC >= 92.9 & MC.SA_dens==3 ~ 2,
    TRUE ~ MC.SA_dens))

#new seasons -simple
sp.fires <- filter(fd4, yday(Date) < 153) %>% pull(num)
tr.fires <- filter(fd4, yday(Date) >= 153 & yday(Date) < (153+15)) %>% pull(num)
sum.fires <- filter(fd4, yday(Date) >= (153+15)) %>% pull(num) 
fd4$MC.SA_season[sp.fires] <- 1
fd4$MC.SA_season[tr.fires] <- 1.5
fd4$MC.SA_season[sum.fires] <- 2

#Recalculate MC.SA and SFC class - mean of pine and spruce for Kenshoe, ICFME
fd4 <- mutate(fd4, MC.SA_stand2=as.integer(case_when(
  Exp %in% c(3, 4) ~ 5,
  TRUE ~ 1)  #just a placeholder for fires with just one litter type
))

fd4 <- mutate(rowwise(fd4), MC1=wbmc(ffmc=FFMC, dmc=DMC, stand=MC.SA_stand, 
                                     density=MC.SA_dens, season=MC.SA_season)) %>% ungroup()

fd4 <- mutate(rowwise(fd4), MC2=wbmc(ffmc=FFMC, dmc=DMC, stand=MC.SA_stand2, 
                                     density=MC.SA_dens, season=MC.SA_season)) %>% ungroup()

fd4 <- mutate(fd4, 
              MC2=case_when(
                Exp %in% c(3, 4) ~ MC2,
                TRUE ~ MC1),
              MC.SA=(MC1 + MC2)/2)

fd4 <- mutate(fd4, SFC.CLS=case_when(
  SFC < 1 ~ 1, 
  SFC >= 1 & SFC <= 2 ~ 2,
  SFC > 2 ~ 3) %>% as.factor())

#write.csv(fd4, 'c:/Dan/_Remote_projects/ccp_git/ccp-cfi/fd4_out.csv')

#Upper canopy crowning analysis - Kenshoe L
#Add fires that had understory crowning again, including 'successes' (overstory crown fires) - 5, 9 (maybe), 12 and 'failures' (non-OS crown fires)
ken.sfc <-fd4$SFC[kenshoe]
#Kenshoe using FSG using LCBH-bs.centroid

FSG.adj.ken <- 0.5  #crown centroid, one half of crown height
FSG.subtract <- 1  #subtract top 1 m of trees to match understory 'nugget effect'

# FSG.ken.upper <- mutate(ken.ME,   #pine LCBH minus spruce height plus half spruce crown depth
#                         FSG.calc=sLCBH.jp-SH.sp+FSG.adj.ken*(SH.sp-ken.LCBH)) %>% 
#   pull(FSG.calc)

FSG.ken.upper <- mutate(ken.ME,  #in this case, using pine LCBH minus spruce height plus top metre or so (FSG.subtract)
                        FSG.calc=sLCBH.jp - (SH.sp-FSG.subtract)) %>%
   pull(FSG.calc)


ken.CFC <- c(0, 0.07, 0.21, 0, 0.95, 0, 0, 0, 0.99, 0.21, 0.45, 0.60)  #from Stocks 89
ken.sp.dbh <- ken.ME %>% pull(dbh.sp)

#CFL, needles alone, Stocks 89:
#Note - at ICFME, overstory CFC < 1 cm (needles plus roundwood) was ~ 2.08 x needle CFC 
ken.bs.CFL <- c(0.307, 0.249, 0.327, 0.438, 0.371, 0.150, 0.537, 0.850, 0.486, 0.703, 0.614, 0.844) 
ken.fuel <- tibble(cfc=ken.CFC, cfl.needles=ken.bs.CFL, sfc=ken.sfc, dbh=ken.sp.dbh)

ken.CFC.adjust <- ken.fuel[ken.cfList,] %>%  pull(cfc) #select only US crown fires 

#FYI, icfme consumed 100% of needles, 86% of 0-0.5 cm branches; 70 % of 0.5-1
#see also Walker and Stocks (1975): overall BS crown fuel weights (<0.6 cm ): 0.541 kg/m^2

#Babrauskas 2006 regression to normalize live & dead fuel heat flux:
#DeltaH = 16.52-0.057*mc
DeltaH <- function(fmc) {
  16.52-0.057 * fmc   #
}

#add only those that had understory crowning 
#Upper crown calcs - spruce layer centroid and scaled SFC (for upper crown ignition)
bs.mc <- 82  #Van Wagner 1993
ken.add<-fd4[ken.cfList,] %>%
  mutate(CFI=as.factor(as.integer(num %in% c(5, 9, 12))),   #if num %in% is TRUE, then CFI=1
         num=seq(max(fd2$num)+1, max(fd2$num)+length(ken.cfList)), #add new fire numbers at end of list
         CFC=ken.CFC.adjust,  #CFC from US crown fires
         FSG=FSG.ken.upper[ken.cfList],  #distance from spruce crown tops (or centroid) to jp LCBH
         SH.sp=ken.ME$SH.sp[ken.cfList],
         sp.centroid=SH.sp-(SH.sp-ken.LCBH)/2,  #ken.LCBH here is 'deemed' to be 1 m (or whatever), the BS LCBH
         #SFC.2=SFC/(sp.centroid-0.5)^1.5,   #scale 'up' (vertically) surface FC to centroid height, which reduces its influence; is this wrong?
         SFC.scale=SFC * (FSG/LCBH.ken[ken.cfList])^1.5, #I think this is the correct scaling for surface fuels
         DeltaH.CFC=DeltaH(bs.mc)/16 * CFC,  #scale using Babrauskas Eff. H of combustion eq.; 
         M = 2.59,  #multiplier
         SFC.upper=SFC.scale + DeltaH.CFC * M,  #uses actual CFC from Stocks, scaled using Bab. and * 2.59 for the 'fitTo2m' model
         estCFC = ken.bs.CFL[ken.cfList] * 2,   #based on CFL for this one
         DeltaH.CFL=DeltaH(bs.mc)/16 * ken.bs.CFL[ken.cfList] * 2,  #uses needle CFL * 2; 
         SFC.upper.alt=SFC.scale + DeltaH.CFL * M   #used as well for testing final models
  )
#SFC.upper or SFC.upper.alt  to be renamed to 'SFC2' for fitTo2m model

# predict.glm(CFI.fitTo2m, newdata=ken.add %>% mutate(SFC2=SFC.upper), type='response')   #actual CFC
# predict.glm(CFI.fitTo2m, newdata=ken.add %>% mutate(SFC2=SFC.upper.alt), type='response') 
#ok, both versions work; with CFC and CFL (needles * 2)


#icfme.add - upper canopy analysis:
icfme.sh.sp <- icfme.cbh.bs + icfme.lcd.bs  #spruce height

icfme.centroid0 <- icfme.cbh.bs + 0.5*icfme.lcd.bs   #ground to bs centroid distance, for icfme.add
icfme.centroid <- icfme.centroid0[-(4:5)]  #cut plots with no bs midstory

# icfme.fsg0 <- icfme.lcbh.jp-icfme.centroid0  #centroid to jp LCBH
# icfme.fsg <-icfme.fsg0[-(4:5)]   #cut plots with no bs midstory

icfme.fsg0 <- icfme.lcbh.jp - (icfme.cbh.bs + icfme.lcd.bs - FSG.subtract)  #in this case, subtract tops of spruce trees
icfme.fsg <- icfme.fsg0[-(4:5)] #cut p3, p4 with no bs midstory

#nums for icfme.add, starting with end of ken.add
icfme.add.num <- (max(ken.add$num)+1):(length(icfme)-2 + max(ken.add$num)) #remove 2 plots with no midstory BS

#exclude two plots with no midstory spruce:
icfme.exc <- filter(fd4[icfme,], str_detect(fire, 'NWT-3') | str_detect(fire, 'NWT-4'))

#CFC from spruce
#equations are weird....ignore for now. Just stick with CFC.needles
#Data (CFL) from Alexander et al. 2004, Table 13 (or Stocks et al. for consumption)
icfme.cfc.N <- c(0.754, 0.796, 0.515, 0.426, 0.459, 0.424,   #these are for both BS and JP; need to multiply by proportions to get BS prop only
                 0.762, 0.792, 0.822, 0.822, 0.354)  #A-9, Plot 8 twice
#icfme.cfl.05 <- c(0.686, 0.709, 0.476, 0.514, 0.524, 0.459, 
#                  0.635, 0.735, 0.673, 0.673, 0.443)  #same
icfme.cfc.05 <- c(0.560, 0.655, 0.329, 0.451, 0.451, 0.394, 
                  0.518, 0.674, 0.623, 0.623, 0.406)
#icfme.cfl.51 <- c(0.402, 0.380, 0.290, 0.294, 0.257, 0.235,
#                  0.320, 0.381, 0.336, 0.336, 0.209)  #same
icfme.cfc.51 <- c(0.336, 0.278, 0.136, 0.210, 0.114, 0.184, 
                  0.268, 0.251, 0.266, 0.266, 0.138)

icfme.cfc.all0 <- 0.213*icfme.cfc.N + 0.14625 * icfme.cfc.05 + 0.0713 * icfme.cfc.51   #multiply by prop BS in each plot
icfme.cfc.all <- icfme.cfc.all0[-(4:5)]  #remove P3, 4

bs.mc.icfme <- c(73.9, 69.1, 76.3, 71.1, 88.6, 88.3, 93.5, 93.5, 72) #not P3, P4; P8 twice; Stocks 2004, Table 3

icfme.add <- filter(fd4, num %in% icfme) %>%    #create new records for ICFME; all plots
  filter(!fire %in% icfme.exc$fire) %>%     # exclude P3, P4
  mutate(num = icfme.add.num,   #new index nums
         FSG = icfme.fsg,
         mc.bs = bs.mc.icfme,
         LCBH.jp = icfme.lcbh.jp[-c(4,5)],
         sp.centroid = icfme.centroid,
         SFC.scale = SFC * (FSG/LCBH.jp)^1.5,  #scale surface fuels using scale equation
         estCFC = icfme.cfc.all,
         DeltaH.CFC = DeltaH(mc.bs)/16 * icfme.cfc.all,  #use sum of all fine CFC
         M = 2.59,
         snag.cfc=icfme.snag.cfc[-c(4, 5)],
         snagHT=icfme.snagHT.mean[-c(4,5)],
         snag.centroid.full=snagHT/2,
         sp.centroid=icfme.centroid,
         FSG=icfme.fsg,
         scaling=(snag.centroid.full/sp.centroid)^1.5,
         SFC.sn.scaled=snag.cfc * scaling,
         SFC.upper = SFC.scale + (SFC.sn.scaled + DeltaH.CFC) * M #scale using Babrauskas Eff. H of combustion eq., fitTo2m multiplier
  ) %>% select(-c(LCBH, FWI, ROS, Fire.intensity, Fire.type, MC1, MC2))

# #Add snag contribution to upper crown
# #Unlikely to make a difference to results, but should complete for completeness
# icfme.snags.add0 <- data.frame(num=icfme.add$num, fire=icfme.add$fire, 
#                                snag.cfc=icfme.snag.cfc[-c(4, 5)],
#                            snagHT=icfme.snagHT.mean[-c(4,5)]) %>%
#   mutate(snag.centroid.full=snagHT/2,
#          sp.centroid=icfme.centroid,
#          FSG=icfme.fsg,
#          scaling=(snag.centroid.full/sp.centroid)^1.5,
#          SFC.sn.scaled=snag.cfc * scaling)
# 


