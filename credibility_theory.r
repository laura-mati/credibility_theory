#EBCT Model 1

#For data stored in a matrix data:
x = c(221.795, 694.652,  432.049, 985.887,  353.927, 224.916,
      273.636, 811.209,  405.070, 1124.063, 288.626, 263.547,
      337.000, 560.436,  253.807, 1143.607, 529.996, 301.706,
      289.864, 566.269,  316.224, 1238.056, 668.871, 366.679,
      483.315, 631.540,  432.374,  771.202, 852.009, 469.159,
      523.792, 500.636,  661.618, 1194.596, 430.640, 448.796,
      309.981, 725.376,  340.822,  969.900, 653.554, 431.756,
      477.500, 669.398,  597.548, 1304.641, 740.981, 411.244,
      414.559, 669.464,  399.152,  887.812, 781.086, 597.785,
      385.069, 597.154,  464.619,  727.749, 729.921, 705.840
      )
x

data = matrix(x, ncol = 10, nrow = 6)
data

#n
n<-ncol(data)
n

#N
N<-nrow(data)
N


#m(theta-i)
rowMeans(data)
sum(rowMeans(data))

#E[m(theta)]
m <-mean(rowMeans(data))
m

#s2(theta-i)
apply(data,1,var)
sum(apply(data,1,var))

#E[s2(theta)]
s <-mean(apply(data,1,var))
s

#Var[m(theta)]
v<-var(rowMeans(data))-mean(apply(data,1,var))/n
v

#Z
Z<-n/(n+s/v)
Z

#Credibility premiums 2021
Premiums = Z* rowMeans(data)+(1-Z)*m
Premiums
Premiums1 = Premiums/10
Premiums1
Observed=c(592.670,
           421.323,
           636.608,
           1043.536,
           647.599,
           860.395)
Observed1= Observed/10
Observed1

table<- matrix(c(Premiums1, Observed1),ncol = 6, byrow = TRUE)
colnames<-c('R1','R2','R3','R4','R5','R6')
rownames(table)<- c('Estimates','Actual')
table<-as.table(table)
table

chisq.test(table, correct=FALSE)

#############################################
x1 = c( 554.027,  763.953,  1997.763,   7343.727,   6343.600,   5248.086,
        699.653,  747.349,  1873.414,   8687.282,   7749.039,   6538.485,
        803.448,  884.041,  2024.589,   9271.908,   8784.204,   9260.252,
        801.192, 1296.968,  2649.305,  10746.378,  10957.919,  12774.064,
        1179.620,  672.750,  2671.450, 129924.772,  12887.219,  15063.107,
        988.870, 1237.378,  2987.439,  12750.248,  14968.463,  18095.201,
        1100.373, 1081.310,  3126.587,  12646.816,  14181.895,  20632.162,
        1141.362,  747.349,  2374.487,  13618.583,  14301.162,  20515.402,
        825.219,  464.566,  1548.271,  15304.543,  16635.527,  20424.313,
        1145.668,  496.935,  1961.545,  14926.529,  15594.119,  20696.239
)
x1

data1 = matrix(x1, ncol = 10, nrow = 6)
data1

#n1
n1<-ncol(data1)
n1

#N
N1<-nrow(data1)
N1


#m(theta-i)
rowMeans(data1)
sum(rowMeans(data1))

#E[m(theta)]
m1 <-mean(rowMeans(data1))
m1

#s2(theta-i)
apply(data1,1,var)
sum(apply(data1,1,var))

#E[s2(theta)]
s1 <-mean(apply(data1,1,var))
s1

#Var[m(theta)]
v1<-var(rowMeans(data1))-mean(apply(data1,1,var))/n1
v1

#Z
Z1<-n1/(n1+s1/v1)
Z1

#Credibility premiums 2021
Prem = Z1* rowMeans(data1)+(1-Z1)*m1
Prem
Prem1 = Prem/10
Prem1
Observ=c(979.632,
         1063.712,
         1752.873,
         17884.591,
         18015.197,
         25941.119
           )
Observ1= Observ/10
Observ1

table1<- matrix(c(Prem1, Observ1),ncol = 6, byrow = TRUE)
colnames<-c('R1','R2','R3','R4','R5','R6')
rownames(table1)<- c('Estimates','Actual')
table1<-as.table(table1)
table1

chisq.test(table1, correct=FALSE)

#EBCT Model 2

#For claims data stored in matrix data and risk volume stored in matrix volume:
x = c(221.795, 694.652,  432.049, 985.887,  353.927, 224.916,
      273.636, 811.209,  405.070, 1124.063, 288.626, 263.547,
      337.000, 560.436,  253.807, 1143.607, 529.996, 301.706,
      289.864, 566.269,  316.224, 1238.056, 668.871, 366.679,
      483.315, 631.540,  432.374,  771.202, 852.009, 469.159,
      523.792, 500.636,  661.618, 1194.596, 430.640, 448.796,
      309.981, 725.376,  340.822,  969.900, 653.554, 431.756,
      477.500, 669.398,  597.548, 1304.641, 740.981, 411.244,
      414.559, 669.464,  399.152,  887.812, 781.086, 597.785,
      385.069, 597.154,  464.619,  727.749, 729.921, 705.840
)
x

data = matrix(x, ncol = 10, nrow = 6)
data

y = c( 758.265, 1162.775,  716.079, 1616.643, 1169.425, 540.835,
       850.376, 1318.533,  645.319, 1750.758, 1127.315, 534.865,
       928.306, 1559.600,  764.752, 2014.637, 1398.108, 561.960,
       994.373, 2048.620, 1309.202, 2426.142, 1373.231, 592.821,
       1026.079, 1517.808, 1123.831, 2327.907, 2152.857, 846.016,
       1207.648, 1504.123, 1492.144, 2530.417, 2039.060, 869.340,
       1139.214, 1884.960, 1455.183, 2646.572, 1721.208, 934.448,
       772.553, 2022.669, 1289.380, 2789.596, 1673.029, 932.975,
       1177.804, 2041.809, 1399.732, 2604.371, 1635.314, 863.841,
       1225.976, 1757.003, 1574.922, 2446.735, 1708.727, 789.258
)
y

volume = matrix(y, ncol = 10, nrow = 6)
volume

#n
n <-ncol(data)
n

#N
N<-nrow(data)
N

#XIJ
Xij <- data/volume
Xij

#Xi bar
Xibar<-rowSums(data)/rowSums(volume)
Xibar

#X bar
Xbar <-mean(rowMeans(data))
Xbar

#Pi bar
Pi <-rowSums(volume)
Pi

#P bar
P <-sum(Pi)
P

a <-((Xij-Xibar)^2)
b <- (row(volume)*a)
c <- rowSums(b)
c
sum(c)

d <-((Xij-Xbar)^2)
e <- (row(volume)*d)
f <- rowSums(e)
f
sum(f)

Ps <-(Pi*(1-Pi/P))/(N*n-1)
Ps

#P*
Pstar <-sum(Pi*(1-Pi/P))/(N*n-1)
Pstar

#E[m(theta)]
m <- sum(data)/P
m

#E[s2(theta)]
s<-mean(rowSums(volume*(Xij-Xibar)^2)/(n-1))
s

#Var[m(theta)]
V <-(sum(rowSums(volume*(Xij-m)^2))/(n*N-1)-s)/Pstar
V

#Z
Zi<-Pi/(Pi+s/V)
Zi

#Credibility premiums per unit volume
CP = Zi* Xibar +(1-Zi)*m
CP

#Volume for 2021
z = c(1214.369,
      2108.588,
      1527.621,
      2225.475,
      1967.576,
      966.979
)
z
volume2021 = matrix(z, ncol = 1, nrow = 6)
volume2021

#Credibility premiums 2021
Premiums = CP*volume2021
Premiums

Premiums1 = Premiums/10
Premiums1

Observed = c(421.323,
             1043.536,
             636.608,
             860.395,
             647.599,
             592.670
)
Observed1 = Observed/10
Observed1

table <- matrix(c(Premiums1, Observed1), ncol=6, byrow=TRUE)
colnames(table) <- c('R1','R2','R3','R4','R5','R6' )
rownames(table) <- c('Estimates','Actual')
table <- as.table(table)
table

chisq.test(table, correct=FALSE)

########################################################
#EBCT Model 2

#For claims data stored in matrix data and risk volume stored in matrix volume:
x1 = c( 554.027,  763.953,  1997.763,   7343.727,   6343.600,   5248.086,
        699.653,  747.349,  1873.414,   8687.282,   7749.039,   6538.485,
        803.448,  884.041,  2024.589,   9271.908,   8784.204,   9260.252,
        801.192, 1296.968,  2649.305,  10746.378,  10957.919,  12774.064,
        1179.620,  672.750,  2671.450, 129924.772,  12887.219,  15063.107,
        988.870, 1237.378,  2987.439,  12750.248,  14968.463,  18095.201,
        1100.373, 1081.310,  3126.587,  12646.816,  14181.895,  20632.162,
        1141.362,  747.349,  2374.487,  13618.583,  14301.162,  20515.402,
        825.219,  464.566,  1548.271,  15304.543,  16635.527,  20424.313,
        1145.668,  496.935,  1961.545,  14926.529,  15594.119,  20696.239
)
x

data1 = matrix(x, ncol = 10, nrow = 6)
data1

y1 = c( 1435.497, 2011.145, 3708.056, 14809.532, 11040.389, 6685.368,
        1543.956, 1849.835, 3885.033, 16208.245, 11917.431, 8440.074,
        1947.678, 2125.788, 4284.463, 17659.666, 12829.358, 12433.137,
        1901.633, 2465.210, 4834.911, 20589.366, 15067.546, 16474.600,
        2446.535, 2707.125, 5093.484, 23193.478, 17157.988, 19753.345,
        2543.132, 2910.887, 5338.001, 23302.639, 19753.345, 23920.288,
        2499.023, 2337.795, 5395.336, 23198.916, 19660.738, 28399.766,
        3017.916, 2294.610, 5727.339, 22661.465, 20752.621, 27133.652,
        2694.476, 2380.044, 6036.624, 21692.602, 21633.475, 27577.190,
        2455.779, 1581.660, 5873.835, 20597.478, 21655.255, 29863.123)
y1

volume1 = matrix(y, ncol = 10, nrow = 6)
volume1

#n
n1 <-ncol(data1)
n1

#N
N1<-nrow(data1)
N1

#XIJ
X1ij <- data1/volume1
X1ij

#Xi bar
X1ibar<-rowSums(data1)/rowSums(volume1)
X1ibar

#X bar
X1bar <-mean(rowMeans(data1))
X1bar

#Pi bar
P1i <-rowSums(volume1)
P1i

#P bar
P1 <-sum(P1i)
P1

a1 <-((X1ij-X1ibar)^2)
b1 <- (row(volume1)*a1)
c1 <- rowSums(b1)
c1
sum(c1)

d1 <-((X1ij-X1bar)^2)
e1 <- (row(volume1)*d1)
f1 <- rowSums(e1)
f1
sum(f1)

P1s <-(P1i*(1-P1i/P1))/(N1*n1-1)
P1s

#P*
P1star <-sum(P1i*(1-P1i/P1))/(N1*n1-1)
P1star

#E[m(theta)]
m1 <- sum(data1)/P1
m1

#E[s2(theta)]
s1<-mean(rowSums(volume1*(X1ij-X1ibar)^2)/(n1-1))
s1

#Var[m(theta)]
V1 <-(sum(rowSums(volume1*(X1ij-m1)^2))/(n1*N1-1)-s1)/P1star
V1

#Z
Z1i<-P1i/(P1i+s1/V1)
Z1i

#Credibility premiums per unit volume
CP1 = Z1i* X1ibar +(1-Z1i)*m1
CP1

#Volume for 2021
z1 = c(1214.369,
       2108.588,
       1527.621,
       2225.475,
       1967.576,
       966.979
)
z1
vol2021 = matrix(z1, ncol = 1, nrow = 6)
vol2021

#Credibility premiums 2021
Prem = CP1*vol2021
Prem

Prem1 = Prem/10
Prem1

Observed = c(421.323,
             1043.536,
             636.608,
             860.395,
             647.599,
             592.670
)
Observ1 = Observ/10
Observ1

tabl <- matrix(c(Prem1, Observ1), ncol=6, byrow=TRUE)
colnames(tabl) <- c('R1','R2','R3','R4','R5','R6' )
rownames(tabl) <- c('Estimates','Actual')
table1 <- as.table(tabl)
table1

chisq.test(table1, correct=FALSE)
