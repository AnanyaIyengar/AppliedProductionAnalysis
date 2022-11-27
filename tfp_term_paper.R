#############################################
###Applied Production Analysis: Term Paper###
#############################################

###CLEANING###

#Packages

library(readxl) #to import data
library(dplyr) #data manipulation
library(ggplot2) #plotting
library(stargazer) #LaTeX output
library(modelsummary) #LaTeX output
library(DJL) #as per Dong-Joon Lim (2015)
library(deaR) #for data-envelopment analysis
library(AER)
library(car)
library(lmtest)
library(viridis)
library(panelr) #for panel data
library(skimr) #for tables
library(strucchange) #for structural change 
library(tseries) #for time series 
library(frontier) #for stochastic frontier analysis


#Importing Data 

x <- paste("https://raw.github.com/AnanyaIyengar/AppliedProductionAnalysis/main/klems.csv")
klems <- read.csv(x)
klems <- as.data.frame(klems)

#Managing Data Type: Converting to Numeric 

klems$gross_output_cp <- as.numeric(gsub(",","",klems$gross_output_cp))
klems$va_cp <- as.numeric(gsub(",","",klems$va_cp))
klems$number_employed <- as.numeric(gsub(",","",klems$number_employed))
klems$capital_stock_cp <- as.numeric(gsub(",","",klems$capital_stock_cp))
klems$industry <- as.numeric(klems$industry)
klems$t <- as.numeric(klems$t)

#Creating Subset of Organised Manufacturing Data: 
# - Industry 3: Food, Beverages, Tobacco
# - Industry 4: Textiles, Leather, Footwear
# - Industry 5: Wood and Wood Products
# - Industry 6: Pulp, Paper and Printing
# - Industry 7: Coke, Petrol, Nuclear Fuel
# - Industry 8: Chemical and Chemical Products
# - Industry 9: Rubber and Plastic
# - Industry 10: Non-Metallic Minerals
# - Industry 11: Metals
# - Industry 12: Machinery
# - Industry 13: Electrical Equipment
# - Industry 14: Transport Equipment
# - Industry 15: Recycling
# - Industry 16: Electricity, Gas, Water Supply

manufacturing <- klems%>%dplyr::filter(industry < 17, industry>2)
manufacturing <- panel_data(manufacturing, wave = t, id = industry)
manufacturing
#Describing Variables
# - gross_output_cp : Gross Output 2011-12 Constant Prices in Crores of Rupees
# - va_cp : Value Added 2011-12 Constant Prices in Crores of Rupees
# - energy_cp : Energy Input 2011-12 Constant Prices in Crores of Rupees
# - material_cp : Material Input 2011-12 Constant Prices in Crores of Rupees
# - services_cp : Services Input 2011-12 Constant Prices in Crores of Rupees
# - number_employed : Number of Persons Employed in 1000s
# - lab_quality : Labour Quality Indices (1980 = 100)
# - capital_stock_cp : Capital Stock 2011-12 Constant Prices in Crores of Rupees

#Treating sub-industries within Manufacturing as DMUs, we divide industries into groups on the basis of growth of K/L ratio.

manufacturing$logklratio <- log(manufacturing$capital_stock_cp/manufacturing$number_employed)

klreg <- lm(data = manufacturing, logklratio ~ t)
summary(klreg)

#Across sub-categories of manufacturing, average log K/L is 3.2926. Then, for industries that had a K/L higher that the mean in 1981 are classified as capital intensive and industries with lower K/L in 1981 are classified as labour intensive.

data_1981 <- manufacturing%>%dplyr::filter(t == 1)

less_k_intensive <- manufacturing%>%dplyr::filter(industry == 3 | industry == 4 | industry == 5 | industry == 9 | industry == 10 | industry == 15)
more_k_intensive <- manufacturing%>%dplyr::filter(industry == 6 | industry == 7 | industry == 8 | industry == 11 | industry == 12 | industry == 13 | industry == 14 | industry == 16 )

#Summary Statistics:

datasummary_skim(manufacturing, output = "latex") #overall

#Industry-wise Summary Statistics

food <- manufacturing%>%dplyr::filter(industry == 3)
datasummary_skim(food, output = "latex")

textile <- manufacturing%>%dplyr::filter(industry == 4)
datasummary_skim(textile, output = "latex")

wood <- manufacturing%>%dplyr::filter(industry == 5)
datasummary_skim(wood, output = "latex")

paper <- manufacturing%>%dplyr::filter(industry == 6)
datasummary_skim(paper, output = "latex")

nuclear <- manufacturing%>%dplyr::filter(industry == 7)
datasummary_skim(nuclear, output = "latex")

chem <- manufacturing%>%dplyr::filter(industry == 8)
datasummary_skim(chem, output = "latex")

rubber <- manufacturing%>%dplyr::filter(industry == 9)
datasummary_skim(rubber, output = "latex")

nonmetalminerals <- manufacturing%>%dplyr::filter(industry == 10)
datasummary_skim(nonmetalminerals, output = "latex")

metals <- manufacturing%>%dplyr::filter(industry == 11)
datasummary_skim(metals, output = "latex")

machinery <- manufacturing%>%dplyr::filter(industry == 12)
datasummary_skim(machinery, output = "latex")

elec <- manufacturing%>%dplyr::filter(industry == 13)
datasummary_skim(elec, output = "latex")

transport <- manufacturing%>%dplyr::filter(industry == 14)
datasummary_skim(transport, output = "latex")

recycling <- manufacturing%>%dplyr::filter(industry == 15)
datasummary_skim(recycling, output = "latex")

elecgaswater <- manufacturing%>%dplyr::filter(industry == 16)
datasummary_skim(elecgaswater, output = "latex")

# Exploratory Data Visualisation

ktitles <- as_labeller(c("11" = "Metals", "12" = "Machinery", "13" = "Electrical Equipment", "14" = "Transport Equipment", "16" = "Electricity, Gas, Water", "6" = "Paper", "7" = "Coke, Petrol, Nuclear", "8" = "Chemicals"))
ltitles <- as_labeller(c("10" = "Non-Metallic Minerals", "15" = "Recycling", "3" = "Food", "4" = "Textiles & Leather", "5" = "Wood", "9" = "Rubber/Plastic"))

ggplot(data = more_k_intensive, aes(x = year)) + geom_line(aes(y = gross_output_cp), color = "black") +  facet_wrap(~ industry, labeller = ktitles) + xlab("Year") + ylab("Gross Output") + labs(caption = "Data: KLEMS")

ggplot(data = less_k_intensive, aes(x = year)) + geom_line(aes(y = gross_output_cp), color = "black") +  facet_wrap(~ industry, labeller = ltitles) + xlab("Year") + ylab("Gross Output") + labs(caption = "Data: KLEMS")

ggplot(data = more_k_intensive, aes(x = year)) + geom_line(aes(y = logklratio), color = "black") +  facet_wrap(~ industry, labeller = ktitles) + xlab("Year") + ylab("Log K/L") + labs(caption = "Data: KLEMS")

ggplot(data = less_k_intensive, aes(x = year)) + geom_line(aes(y = logklratio), color = "black") +  facet_wrap(~ industry, labeller = ltitles) + xlab("Year") + ylab("Log K/L") + labs(caption = "Data: KLEMS")



#We use each sub-industry as a DMU while calculating the Malmquist Indices

more_k_intensive$industry <- as.character(more_k_intensive$industry)
less_k_intensive$industry <- as.character(less_k_intensive$industry)

###MALMQUIST INDEX COMPUTATION AND OLS-MOSUM STRUCTURAL BREAK TEST###

#Consider gross_output_cp as output, and energy, materials, services, labour employed and capital as inputs. Preparing Data:

selected_more_k_intensive <- more_k_intensive%>%dplyr::select(industry, year, gross_output_cp, energy_cp, material_cp, number_employed, capital_stock_cp, services_cp)

k_int_dea <- read_malmquist(selected_more_k_intensive, percol = 2, arrangement = "vertical", outputs = 3, inputs = 4:8)

selected_less_k_intensive <- less_k_intensive%>%dplyr::select(industry, year, gross_output_cp, energy_cp, material_cp, number_employed, capital_stock_cp, services_cp)

l_int_dea <- read_malmquist(selected_less_k_intensive, percol = 2, arrangement = "vertical", outputs = 3, inputs = 4:8)



#Evaluating the Malmquist Index for Capital Intensive Manufacturing according to Fare et al. (1994)

#Input Orientation

r1 <- malmquist_index(k_int_dea, orientation = c("io"), rts = c("vrs"), type2 = "fgnz", tc_vrs = TRUE)
k_io_mi <- as.data.frame(r1$mi) 
k_io_scale <- as.data.frame(r1$sech)
k_io_tech <- as.data.frame(r1$tc) 
k_io_p <- as.data.frame(r1$pech)



#Output Orientation

r2 <- malmquist_index(k_int_dea, orientation = c("oo"), rts = c("vrs"), type2 = "fgnz", tc_vrs = TRUE)
k_oo_mi <- as.data.frame(r2$mi)
k_oo_tech <- as.data.frame(r2$tc)
k_oo_p <- as.data.frame(r2$pech)
k_oo_scale <- as.data.frame(r2$sech)


#Evaluating the Malmquist Index for Labour Intensive Manufacturing according to Fare er al. (1994)

#Input Orientation

r3 <- malmquist_index(l_int_dea, orientation = "io", rts = "vrs", type2 = "fgnz", tc_vrs = TRUE)
l_io_mi <- as.data.frame(r3$mi)
l_io_tech <- as.data.frame(r3$tc)
l_io_p <- as.data.frame(r3$pech)
l_io_scale <- as.data.frame(r3$sech)


#Output Orientation

r4 <- malmquist_index(l_int_dea, orientation = "oo", rts = "vrs", type2 = "fgnz", tc_vrs = TRUE)
l_oo_mi <- as.data.frame(r4$mi)
l_oo_tech <- as.data.frame(r4$tc)
l_oo_p <- as.data.frame(r4$pech)
l_oo_scale <- as.data.frame(r4$sech)

#Labelling Columns

colnames(k_oo_mi) <- c("output_malm_6", "output_malm_7", "output_malm_8", "output_malm_11", "output_malm_12", "output_malm_13", "output_malm_14", "output_malm_16")
colnames(k_io_mi) <- c("input_malm_6", "input_malm_7", "input_malm_8", "input_malm_11", "input_malm_12", "input_malm_13", "input_malm_14", "input_malm_16")
colnames(l_io_mi) <- c("input_malm_3", "input_malm_4", "input_malm_5", "input_malm_9", "input_malm_10", "input_malm_15")
colnames(l_oo_mi) <- c("output_malm_3", "output_malm_4", "output_malm_5", "output_malm_9", "output_malm_10", "output_malm_15")

#Merging Data

capital_intensive_malmquist <- data.frame(k_io_mi, k_oo_mi)
labour_intensive_malmquist <- data.frame(l_io_mi, l_oo_mi)


#Is there a difference between output and input indices over time? No! So we stick with the output-orientation. 


#Tables for TFP according to Malmquist Indices

capital_intensive_malmquist$t <- seq(1981, 2019)
labour_intensive_malmquist$t <- seq(1981, 2019)

tfp_nonpar_lintensive <- labour_intensive_malmquist%>%dplyr::select(output_malm_3, output_malm_4, output_malm_5, output_malm_9, output_malm_10, output_malm_15, t)
colnames(tfp_nonpar_lintensive) <- c("food", "textile", "wood", "rubber_plastic", "non_metallic_minerals", "recycling", "time")

tfp_nonpar_kintensive <- capital_intensive_malmquist%>%dplyr::select(output_malm_6, output_malm_7, output_malm_8, output_malm_11, output_malm_12, output_malm_13, output_malm_14, output_malm_16, t)
colnames(tfp_nonpar_kintensive) <- c("paper", "coke_petrol_nuclear", "chemicals", "metals", "machinery", "electrical", "transport", "electricity_gas_water", "time")

stargazer(tfp_nonpar_kintensive)
stargazer(tfp_nonpar_lintensive)


#Structural Break Analysis: OLS-MOSUM Test

plot(efp(ts(capital_intensive_malmquist$output_malm_12) ~ capital_intensive_malmquist$t, type = "OLS-MOSUM"))
sctest(ts(capital_intensive_malmquist$output_malm_12) ~ capital_intensive_malmquist$t, type = "OLS-MOSUM")


###STOCHASTIC FRONTIER ANALYSIS FOR PANEL DATA: ERROR COMPONENTS FRONTIER (BATTESE & COELLI, 1992)###

#Creating squared time variables

more_k_intensive$tsq <- (more_k_intensive$t)^2
less_k_intensive$tsq <- (less_k_intensive$t)^2

#Capital Intensive Industry (Without Time Variant Efficiency)

#Cobb Douglas Specification

capital_cobb_douglas <- sfa(log(va_cp) ~ log(capital_stock_cp) + log(number_employed) + factor(industry) + t, data = more_k_intensive, truncNorm = FALSE, ineffDecrease = TRUE)
summary(capital_cobb_douglas)
sigmaU_cd_capital <- coefficients(capital_cobb_douglas)[12]*coefficients(capital_cobb_douglas)[13]
sigmaV_cd_capital <- coefficients(capital_cobb_douglas)[12] - sigmaU_cd_capital
sigmaU_cd_capital
sigmaV_cd_capital

#Translog Specification

capital_translog <- sfa(log(va_cp) ~ log(capital_stock_cp) + log(number_employed) + 0.5*I(log(capital_stock_cp))*I(log(number_employed)) + factor(industry) + t + 0.5*tsq + I(log(capital_stock_cp))*t + I(log(number_employed))*t, data = more_k_intensive, truncNorm = FALSE, ineffDecrease = TRUE)
summary(capital_translog)
sigmaU_translog_capital <- coefficients(capital_translog)[16]*coefficients(capital_translog)[17]
sigmaV_translog_capital <- coefficients(capital_translog)[16] - sigmaU_translog_capital
sigmaU_translog_capital
sigmaV_translog_capital

cor(more_k_intensive$number_employed, more_k_intensive$capital_stock_cp) #0.32

#Labour Intensive Industry (Without Time Variant Efficiency)

#Cobb Douglas Specification

labour_cobb_douglas <- sfa(log(va_cp) ~ log(capital_stock_cp) + log(number_employed) + factor(industry) + t, data = less_k_intensive, truncNorm = FALSE, ineffDecrease = TRUE)
summary(labour_cobb_douglas)
sigmaU_cd_labour <- coefficients(labour_cobb_douglas)[10]*coefficients(labour_cobb_douglas)[11]
sigmaV_cd_labour <- coefficients(labour_cobb_douglas)[10] - sigmaU_cd_labour
sigmaU_cd_labour
sigmaV_cd_labour

#Translog Specification

labour_translog <- sfa(log(va_cp) ~ I(log(capital_stock_cp)) + I(log(number_employed)) + 0.5*I(log(capital_stock_cp))*I(log(number_employed)) + factor(industry) + t + 0.5*tsq + t*I(log(capital_stock_cp)) + t*I(log(number_employed)), data = less_k_intensive, truncNorm = FALSE, ineffDecrease = TRUE)
summary(labour_translog)
sigmaU_translog_labour <- coefficients(labour_translog)[14]*coefficients(labour_translog)[15]
sigmaV_translog_labour <- coefficients(labour_translog)[14] - sigmaU_translog_labour
sigmaU_translog_labour
sigmaV_translog_labour

#Likelihood Ratio Test for Inefficiency

lrtest(capital_cobb_douglas)
lrtest(capital_translog)
lrtest(labour_cobb_douglas)
lrtest(labour_translog)


#Comparing Efficiency Across Sub-Industries

eff_cap_cd <- efficiencies(capital_cobb_douglas)
eff_lab_cd <- efficiencies(labour_cobb_douglas)
eff_cap_translog <- efficiencies(capital_translog)
eff_lab_translog <- efficiencies(labour_translog)

more_k_intensive$efficiencycobbdouglas <- eff_cap_cd
more_k_intensive$efficiencytranslog <- eff_cap_translog
less_k_intensive$efficiencycobbdouglas <- eff_lab_cd
less_k_intensive$efficiencytranslog <- eff_lab_translog

#Calculating Scale Elasticity as Sum of Output Elasticities

#Cobb Douglas: Just the Sum of Coefficients!

rts_capital_cobbdouglas <- 0.1345852 + 0.5991470
rts_labour_cobbdouglas <- 0.38136 + 0.50528

print(rts_capital_cobbdouglas)
print(rts_labour_cobbdouglas)

#Translog: 

#For capital intensive industry: 0.12488 + 0.5*0.0157*ln(number_employed) + (-0.00192)*t + 0.31 + 0.5*0.0157*ln(capital_stock_cp) + 0.0017*t

average_aggregate_scale_capital <- 0

for (t in 1:40) {
  average_aggregate_scale_capital[t] = 0.12488 + 0.5*0.0157*(mean(log(more_k_intensive$capital_stock_cp)) + mean(log(more_k_intensive$number_employed))) + 0.31 + (0.0017-0.00192)*t
}

print(average_aggregate_scale_capital)
exp(mean(log(average_aggregate_scale_capital)))

#For labour intensive industry: 0.05086 + 0.5*(-0.00823)(ln(capital_stock_cp) + ln(number_employed)) + 0.02366*t + (-0.0139)*t

average_aggregate_scale_labour <- 0

for (t in 1:40) {
  average_aggregate_scale_labour[t] = 0.05086 + 0.5*(-0.00823)*(mean(log(less_k_intensive$capital_stock_cp)) + mean(log(less_k_intensive$number_employed))) + (0.02366-0.0139)*t
}

print(average_aggregate_scale_labour)
mean(average_aggregate_scale_labour)

#Visualising Technical Efficiency with Line and Area Graphs 

ggplot(more_k_intensive, aes(x = year, y = efficiencytranslog, group = industry, fill = industry)) + geom_area(colour = "white", size = 0.5, alpha = 0.6) + ylab("Efficiency") + xlab("Years") + ggtitle("SFA Efficiency Measures for Capital Intensive Industry: Translog") + labs(caption = "Source: Author's Calculations and KLEMS India") + scale_fill_viridis(labels = c("Metals", "Machinery", "Electrical Equipment", "Transport Equipment", "Electricity, Gas, Water", "Paper", "Coke, Petrol, Fuel", "Chemicals"), discrete = T)                                                      

ggplot(more_k_intensive, aes(x = year, y = efficiencycobbdouglas, group = industry, fill = industry)) + geom_area(colour = "white", size = 0.5, alpha = 0.6) + ylab("Efficiency") + xlab("Years") + ggtitle("SFA Efficiency Measures for Capital Intensive Industry: Cobb Douglas") + labs(caption = "Source: Author's Calculations and KLEMS India") + scale_fill_viridis(labels = c("Metals", "Machinery", "Electrical Equipment", "Transport Equipment", "Electricity, Gas, Water", "Paper", "Coke, Petrol, Fuel", "Chemicals"), discrete = T)                                                       

ggplot(less_k_intensive, aes(x = year, y = efficiencycobbdouglas, group = industry, color = industry)) + geom_line(size = 2, alpha = 0.6) + ylab("Efficiency") + xlab("Years") + ggtitle("SFA Efficiency Measures for Labour Intensive Industry: Cobb Douglas") + labs(caption = "Source: Author's Calculations and KLEMS India") + scale_color_viridis(labels = c("Non-Metallic Minerals", "Recycling", "Food, Beverages, Tobacco", "Textiles, Leather, Footwear", "Wood & Wood Products", "Rubber and Plastic"), discrete = T)                                                      


ggplot(less_k_intensive, aes(x = year, y = efficiencytranslog, group = industry, color = industry)) + geom_line(size = 2, alpha = 0.6) + ylab("Efficiency") + xlab("Years") + ggtitle("SFA Efficiency Measures for Labour Intensive Industry: Translog") + labs(caption = "Source: Author's Calculations and KLEMS India") + scale_color_viridis(labels = c("Non-Metallic Minerals", "Recycling", "Food, Beverages, Tobacco", "Textiles, Leather, Footwear", "Wood & Wood Products", "Rubber and Plastic"), discrete = T)                                                      



#Structural Break Test for Efficiencies in Stochastic Frontier Analysis

#Labour Intensive
food_eff <- less_k_intensive%>%dplyr::filter(industry==3)
textile_eff <- less_k_intensive%>%dplyr::filter(industry==4)
wood_eff <- less_k_intensive%>%dplyr::filter(industry==5)
rubber_eff <- less_k_intensive%>%dplyr::filter(industry==9)
nonmetal_eff <- less_k_intensive%>%dplyr::filter(industry==10)
recycling_eff <- less_k_intensive%>%dplyr::filter(industry==15)

#Capital Intensive
paper_eff <- more_k_intensive%>%dplyr::filter(industry==6)
fuel_eff <- more_k_intensive%>%dplyr::filter(industry==7)
chem_eff <- more_k_intensive%>%dplyr::filter(industry==8)
metals_eff <- more_k_intensive%>%dplyr::filter(industry==11)
mach_eff <- more_k_intensive%>%dplyr::filter(industry==12)
eleceq_eff <- more_k_intensive%>%dplyr::filter(industry==13)
transp_eff <- more_k_intensive%>%dplyr::filter(industry==14)
power_eff <- more_k_intensive%>%dplyr::filter(industry==16)


#OLS-MOSUM Test (One Case Shown Here)

plot(efp(ts(power_eff$efficiencycobbdouglas) ~ power_eff$t, type = "OLS-MOSUM"))
sctest(ts(power_eff$efficiencycobbdouglas) ~ power_eff$t, type = "OLS-MOSUM")







