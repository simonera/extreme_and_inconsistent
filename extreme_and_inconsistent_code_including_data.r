# In any use of this script for academic purposes, PLEASE CITE the following paper:
# 
#   Rambotti, Simone, and Ronald L. Breiger. 2020 “Extreme and Inconsistent:  
#       A Case-Oriented Regression Analysis of Health, Inequality, and Poverty.” 
#       Socius 6:1–13. DOI: 10.1177/2378023120906064
# 
# Thank you.


##############################
## construct the data frame ##
##############################

# Construct the data matrix:
zinequality <- c(1.1244433,-0.50872159,-0.67766958,0.091982834,0.16707116,-0.2271411,-0.90293348,0.035666954,-1.33469,0.073210754,1.2558472,0.52373892,0.41110712,0.95549536,0.86163527,-1.5787259,-0.15205313,-1.2408297,-1.1657417,2.2883074)
zpoverty <- c(-0.037866674,-0.76168096,-0.67732799,0.81118757,-0.44784385,-0.60439408,-1.1635544,0.82629001,-1.1013026,-0.52556658,-0.10601201,0.48003793,0.1916173,2.4566219,0.51871502,0.40452552,-0.66222554,-0.6600154,-1.0563636,2.1151583)
zinteraction <- c(-0.56928205,-0.21050778,-0.15084223,-0.47151417,-0.5961802,-0.41923466,0.34269899,-0.50917506,0.69248253,-0.56586009,-0.64482725,-0.32402161,-0.46804377,1.4244366,-0.16090429,-1.0665343,-0.4497588,0.1494517,0.49355826,3.5040584)
zlife <- c(0.47719216,-0.15906498,0.11362062,0.56809199,0.38629928,-0.34085077,-1.8860576,0.56809199,-0.61353636,0.2954064,-0.4317506,-0.34085077,-1.5224791,0.47719216,0.022720795,2.6586561,-0.34085077,0.20451351,1.2952421,-1.4315863)
life <- c(79.2,78.5,78.8,79.3,79.1,78.3,76.6,79.3,78,79,78.2,78.3,77,79.2,78.7,81.6,78.3,78.9,80.1,77.1)
mydata <- cbind(zinequality, zpoverty, zinteraction, zlife, life)
nations <- c("AUS", "AUT", "BEL", "CAN", "CHE", "DEU", "DNK" ,"ESP" ,"FIN", "FRA", "GBR" ,"GRC" ,"IRL", "ISR", "ITA", "JPN" ,"NLD" ,"NOR", "SWE", "USA")
rownames(mydata) <- nations
mydata <- as.data.frame(mydata)


#################
###### ols ######
#################

## mmodel 1 ##
ols1 <- lm(zlife ~ zinequality - 1, data = mydata)
summary(ols1)

## mmodel 2 ##
ols2 <- lm(zlife ~ zpoverty - 1, data = mydata)
summary(ols2)

## mmodel 3 ##
ols3 <- lm(zlife ~ zinequality + zpoverty - 1, data = mydata)
summary(ols3)

## mmodel 4 ##
ols4 <- lm(zlife ~ zinequality + zpoverty + zinteraction - 1, data = mydata)
summary(ols4)

#################
## RIO w/o SVD ##
#################

## mmodel 1 ##
X1 <- model.matrix(ols1)
Y <- diag(mydata$zlife)
b_case1 <- solve(crossprod(X1)) %*% t(X1) %*% Y
round(rowSums(b_case1),2)
round(ols1$coefficients,2) # double checking: correct 

## mmodel 2 ##
X2 <- model.matrix(ols2)
Y <- diag(mydata$zlife)
b_case2 <- solve(crossprod(X2)) %*% t(X2) %*% Y
round(rowSums(b_case2),2)
round(ols2$coefficients,2) # double checking: correct 

## mmodel 3 ##
X3 <- model.matrix(ols3)
Y <- diag(mydata$zlife)
b_case3 <- solve(crossprod(X3)) %*% t(X3) %*% Y
round(rowSums(b_case3),2)
round(ols3$coefficients,2) # double checking: correct 

## mmodel 4 ##
X4 <- model.matrix(ols4)
Y <- diag(mydata$zlife)
b_case4 <- solve(crossprod(X4)) %*% t(X4) %*% Y
round(rowSums(b_case4),2)
round(ols4$coefficients,2) # double checking: correct 

#################
## add country ##
##### names #####
#################

colnames(b_case1) <- rownames(mydata)
colnames(b_case2) <- rownames(mydata)
colnames(b_case3) <- rownames(mydata)
colnames(b_case4) <- rownames(mydata)

intensities1 <- t(b_case1)
intensities2 <- t(b_case2)
intensities3 <- t(b_case3)
intensities4 <- t(b_case4)

round(colSums(intensities1),2)
round(ols1$coefficients,2) # double checking: correct 

round(colSums(intensities2),2)
round(ols2$coefficients,2) # double checking: correct 

round(colSums(intensities3),2)
round(ols3$coefficients,2) # double checking: correct 

round(colSums(intensities4),2)
round(ols4$coefficients,2) # double checking: correct 


#################
## rename cols ##
## merge data ###
#################

colnames(intensities1)[colnames(intensities1)=="zinequality"] <- "i_ineq_m1"

colnames(intensities2)[colnames(intensities2)=="zpoverty"] <- "i_pov_m2"

colnames(intensities3)[colnames(intensities3)=="zinequality"] <- "i_ineq_m3"
colnames(intensities3)[colnames(intensities3)=="zpoverty"] <- "i_pov_m3"

colnames(intensities4)[colnames(intensities4)=="zinequality"] <- "i_ineq_m4"
colnames(intensities4)[colnames(intensities4)=="zpoverty"] <- "i_pov_m4"
colnames(intensities4)[colnames(intensities4)=="zinteraction"] <- "i_int_m4"

mydataInOut <- cbind(mydata, intensities1, intensities2, intensities3, intensities4)

write.csv(mydataInOut, file = "mydataInsideOut.csv") # export 

#################
#### create #####
#### figures ####
#################

### figure 2 ###
pdf("figure2inR.pdf",width=8,height=6) # to export the figure as PDF, size: 6x8
plot(mydata$zinequality, mydata$zlife, cex=(40 * abs(mydataInOut$i_ineq_m1)), col="blue", 
     xlab="Inequality", ylab="Life Expectancy",
     sub = "Notes: Size of circles is proportional to contributions to the regression coefficient",
     main="Regression Coefficient for Inequality")
text(mydata$zinequality, mydata$zlife, labels=row.names(mydata))
abline(h=0, col="darkgrey"); abline(v=0, col="darkgrey")
abline(lm(zlife ~ zinequality - 1, data = mydata), lwd = 2, col="blue") # regression line (y~x)
text(2, -0.4, "b = -.36", font=2, cex = 1.5) # I added this text ("b = -.36") in the plot, 
                                             # the note, and I changed the title (SR)
dev.off() # ends the command pdf()
round(lm(zlife ~ zinequality - 1, data = mydata)$coefficients,2) # double check: correct 

### figure 3 ###
pdf("figure3inR.pdf",width=8,height=6) # to export the figure as PDF, size: 6x8
plot(mydata$zpoverty, mydata$zlife, cex=(40 * abs(mydataInOut$i_pov_m2)), col="blue", 
     xlab="Poverty", ylab="Life Expectancy",
     sub = "Notes: Size of circles is proportional to contributions to the regression coefficient",
     main="Regression Coefficient for Poverty")
text(mydata$zpoverty, mydata$zlife, labels=row.names(mydata))
abline(h=0, col="darkgrey"); abline(v=0, col="darkgrey")
abline(lm(zlife ~ zpoverty - 1, data = mydata), lwd = 2, col="blue") # regression line (y~x)
text(2, 0.3, "b = +.07", font=2, cex = 1.5) # I added this text ("b = +.07") in the plot, 
                                            # the note, and I changed the title (SR)
dev.off() # ends the command pdf()
round(lm(zlife ~ zpoverty - 1, data = mydata)$coefficients,2) # double check: correct 

### figure 4a ###
res_m3a <- lm(zinequality ~ zpoverty, data = mydata)
res_inequality <- resid(res_m3a)
res_inequality <- as.matrix(res_inequality)

pdf("figure4AinR.pdf",width=8,height=6) # to export the figure as PDF, size: 6x8
plot(res_inequality, mydata$zlife, cex=(40 * abs(mydataInOut$i_ineq_m3)), col="blue", 
     xlab="Residualized Inequality", ylab="Life Expectancy",
     main="Figure 4-A: Multiple Reg. Coef. for Inequality")
text(res_inequality, mydata$zlife, labels=row.names(mydata))
abline(h=0, col="darkgrey"); abline(v=0, col="darkgrey")
abline(lm(mydata$zlife ~ res_inequality - 1), lwd = 2, col="blue") # regression line (y~x)
text(-1.5, .75, "b = -.73", font=2, cex = 1.5) # I added this text ("b = -.73") in the plot (SR)
dev.off() # ends the command pdf()
round(lm(mydata$zlife ~ res_inequality - 1)$coefficients,2) # double check: correct 

### figure 4b ###
res_m3b <- lm(zpoverty ~ zinequality, data = mydata)
res_poverty <- resid(res_m3b)
res_poverty <- as.matrix(res_poverty)

pdf("figure4BinR.pdf",width=8,height=6) # to export the figure as PDF, size: 6x8
plot(res_poverty, mydata$zlife, cex=(40 * abs(mydataInOut$i_pov_m3)), col="blue", 
     xlab="Residualized Poverty", ylab="Life Expectancy",
     sub = "Notes: Size of circles is proportional to contributions to the (respective) multiple regression coefficient",
     main="Figure 4-B: Multiple Reg. Coef. for Poverty")
text(res_poverty, mydata$zlife, labels=row.names(mydata))
abline(h=0, col="darkgrey"); abline(v=0, col="darkgrey")
abline(lm(mydata$zlife ~ res_poverty - 1), lwd = 2, col="blue") # regression line (y~x)
text(1.2, 1, "b = +.56", font=2, cex = 1.5) # I added this text ("b = +.56") in the plot 
                                            # and the note(SR)
dev.off() # ends the command pdf()
round(lm(mydata$zlife ~ res_poverty - 1)$coefficients,2) # double check: correct 

### figure 5 ###
res_m4 <- lm(zinteraction ~ zinequality + zpoverty, data = mydata)
res_interaction <- resid(res_m4)
res_interaction <- as.matrix(res_interaction)

pdf("figure5inR.pdf",width=8,height=6) # to export the figure as PDF, size: 6x8
plot(res_interaction, mydata$zlife, cex=(40 * abs(mydataInOut$i_int_m4)), col="blue", 
     xlab="Residualized Interaction", ylab="Life Expectancy",
     sub = "Size of circles is proportional to contributions to the multiple regression coef. for the interaction term",
     main="Multiple Reg. Coef. for the Interaction Term")
text(res_interaction, mydata$zlife, labels=row.names(mydata))
abline(h=0, col="darkgrey"); abline(v=0, col="darkgrey")
abline(lm(mydata$zlife ~ res_interaction - 1), lwd = 3, col="blue") # regression line (y~x)
text(2, -.5, "b = -.49", font=2, cex = 1.5) # I added this text ("b = -.49") in the plot 
                                            # and the note (SR)
dev.off() # ends the command pdf()
round(lm(mydata$zlife ~ res_interaction - 1)$coefficients,2) # double check: correct 
