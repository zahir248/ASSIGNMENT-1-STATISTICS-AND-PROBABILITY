#Question 1
#Question 1(a)
library(readxl)
BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_ <- read_excel("C:/Users/Muhd Zahir/Downloads/BITI2233 Statistics And Probability -Time Spending Among College Students (Responses).xlsx")
print(BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_)

#Question 1(b)
Table_semweek <- BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_[,c(5:17)]
print(Table_semweek)

#Question 1(c)
Table_sembreak <- BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_[,c(5, 18:29)]
print(Table_sembreak)

#Question 2(a) for A1
freq <- table (BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$Course)
relfreq <- freq/nrow(BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_)
cumfreq <- cumsum(freq)
cumreffreq <- cumsum(relfreq)
cbind (freq,relfreq,cumfreq,cumreffreq)

#Question 2(a) for A2
freq <- table(BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$`Section/Group`)
relfreq <- freq/nrow(BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_)
cumfreq <- cumsum(freq)
cumreffreq <- cumsum(relfreq)
cbind (freq,relfreq,cumfreq,cumreffreq)

#Question 2(a) for A3
freq <- table (BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$Gender)
relfreq <- freq/nrow(BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_)
cumfreq <- cumsum(freq)
cumreffreq <- cumsum(relfreq)
cbind (freq,relfreq,cumfreq,cumreffreq)

#Question 2(b)
colMeans(Table_semweek)

#Question 2(c)
colMeans(Table_sembreak)

#Question 3(a) for A1
freq <- table (BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$Course)
pie (freq,xlab="pie chart for course")

#Question 3(a) for A2
freq <- table (BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$`Section/Group`)
pie (freq,xlab="pie chart for section/group")

#Question 3(a) for A3
freq <- table (BITI2233_Statistics_And_Probability_Time_Spending_Among_College_Students_Responses_$Gender)
pie (freq,xlab="pie chart for gender")

#Question 3(b)
chart_semweek <- colMeans(Table_semweek)
barplot(chart_semweek, main="chart_semweek" ,xlab="Average")

#Question 3(c)
chart_sembreak <- colMeans(Table_sembreak)
barplot(chart_sembreak, main="chart_sembreak" ,xlab="Average")

#Question 5a(i)
sapply(Table_semweek, sd)

#Question 5a(ii)
chart_semweek <- sapply(Table_semweek, sd) 
barplot(chart_semweek, main="chart_semweek" ,xlab="Standard deviation")

#Question 5b(i)
sapply(Table_sembreak, sd) 

#Question 5b(ii)
chart_sembreak <- sapply(Table_sembreak, sd) 
barplot(chart_sembreak, main="chart_sembreak" ,xlab="Standard deviation") 

###############################################################################

#Question 2
set.seed(4) #prevent different values each time generate random values

#Let Vector a = Arrival based on modes of transportation
#Let Vector b = Satisfication based on modes of transportation

A = runif(4) #randomly generate values for A1 - A4 as a single vector a
A

B = runif(4) #randomly generate values for B1 - B4 as a single vector b
B

#Save the vector A and B as data frame
DataA = data.frame(A)
DataB = data.frame(B)

#get A1 - A4 values by obtain first element in vector A
A1 = A[1]
A2 = A[2]
A3 = A[3]
A4 = A[4]

#calculate value for A'1 - A'4 as they are complementary values for A1 - A4
A1X = 1 - A1
A1X

A2X = 1 - A2
A2X

A3X = 1 - A3
A3X

A4X = 1 - A4
A4X

#get B1 - B4 values by obtain first element in vector B
B1 = B[1]
B2 = B[2]
B3 = B[3]
B4 = B[4]

#calculate value for A'1 - A'4 as they are complementary values for A1 - A4
B1X = 1 - B1
B1X

B2X = 1 - B2
B2X

B3X = 1 - B3
B3X

B4X = 1 - B4
B4X

#Question (i)
P_late = (0.25 * A1X) + (0.25 * A2X) + (0.25 * A3X) + (0.25 * A4X)
P_late

#mode 1: Transportation = Rapid Train
P_rapid_and_late = (0.25 * A1X)
P_rapid_and_late
P_late_rapid = (P_rapid_and_late) / (P_late) 
P_late_rapid

#mode 2: Transportation = E-hailing Service
P_eHailing_and_late = (0.25 * A2X)
P_eHailing_and_late
P_late_eHailing = (P_eHailing_and_late) / (P_late) 
P_late_eHailing

#mode 3: Transportation = Public bus
P_pubBus_and_late = (0.25 * A3X)
P_pubBus_and_late
P_late_pubBus = (P_pubBus_and_late) / (P_late) 
P_late_pubBus

#mode 4: Transportation = University bus
P_uniBus_and_late = (0.25 * A4X)
P_uniBus_and_late
P_late_uniBus = (P_uniBus_and_late) / (P_late) 
P_late_uniBus

#Question (ii)
#P_notSatisfy_transport = (transportation | not satisified)
#P_notSatisfy_transport = P(not_satisfy_and_transportation) / P(not_satisfy)
P_not_satisfy = (0.25 * B1X) + (0.25 * B2X) + (0.25 * B3X) + (0.25 * B4X)
P_not_satisfy

#mode 1: Transportation = Rapid Train
P_rapid_and_not_satisfy = (0.25 * B1X)
P_rapid_and_not_satisfy 
P_not_satisfy_rapid = (P_rapid_and_not_satisfy ) / (P_not_satisfy) 
P_not_satisfy_rapid

#mode 2: Transportation = E-hailing Service
P_eHailing_and_not_satisfy = (0.25 * B2X)
P_eHailing_and_not_satisfy
P_not_satisfy_eHailing = (P_eHailing_and_not_satisfy) / (P_not_satisfy) 
P_not_satisfy_eHailing

#mode 3: Transportation = Public bus
P_pubBus_and_not_satisfy = (0.25 * B3X)
P_pubBus_and_not_satisfy
P_not_satisfy_pubBus = (P_pubBus_and_not_satisfy) / (P_not_satisfy) 
P_not_satisfy_pubBus

#mode 4: Transportation = University bus
P_uniBus_and_not_satisfy = (0.25 * A4X)
P_uniBus_and_not_satisfy
P_not_satisfy_uniBus = (P_uniBus_and_not_satisfy) / (P_not_satisfy) 
P_not_satisfy_uniBus

###############################################################################

#Question 3
#Question (a)
dotchart(table(Number_of_students_borrow_books_from_UteM_library$`Number of borrowing book`),xlab="frequency",ylab="Number of book", bg="black",xlim=c(1,5))

barplot(table(Number_of_students_borrow_books_from_UteM_library$`Number of borrowing book`),ylab="frequency",xlab="Number of book")

hist(table(Number_of_students_borrow_books_from_UteM_library$`Number of borrowing book`),ylab = "frequency",xlab = "Number of book")

#Question (b)
mean(Number_of_students_borrow_books_from_UteM_library$`Number of borrowing book ( week )`)

sd(Number_of_students_borrow_books_from_UteM_library$`Number of borrowing book ( week )`)

#Question (c) 

#Question (i)
pnorm(15, mean = 3.933, sd= 2.876, lower.tail = FALSE)

#Question (ii)
pnorm(30, mean = 3.933, sd= 2.876, lower.tail = FALSE)

#Question (iii)
pnorm(60, mean = 3.933, sd= 2.876, lower.tail = FALSE)

###############################################################################

#Question 4
#Question (a)
new_data <- Fruitweight[sample(nrow(Fruitweight), "1000", replace = FALSE),]
print(new_data , n=1000) 

#Question (b)
chart_weight <- new_data$Weight
hist(chart_weight, xlab= "weight")

#Question (c) for fruit A
fruita <- new_data[new_data$Weight & new_data$TypeofFruit=="FruitA" ,]
fruitA <- fruita$Weight
hist(fruitA, xlab = "weight")

#Question (c) for fruit B
fruitb <- new_data[new_data$Weight & new_data$TypeofFruit=="FruitB" ,]
fruitB <- fruitb$Weight
hist(fruitB, xlab = "weight") 

#Question (d) for mean for fruit A
meanA <- mean(fruitA)
meanA

#Question (d) for standard deviation for fruit A
sdA <- sd(fruitA)
sdA 

#Question (d) for mean for fruit B
meanB <- mean(fruitB)
meanB

#Question (d) for standard deviation for fruit B
sdB <- sd(fruitB)
sdB 

#Question (e)
pnorm(3.50, mean = meanA, sd=sdA , lower.tail = FALSE) 

#Question (f)
pnorm(3.10 ,mean= meanA , sd=sdA , lower.tail = TRUE) - pnorm (2.55 ,mean= meanA , sd=sdA , lower.tail = TRUE) 

#Question (g)
pnorm(4 ,mean= meanA , sd=sdA , lower.tail = TRUE) - pnorm (3.2 ,mean= meanA , sd=sdA , lower.tail = TRUE) 

#Question (i)
pnorm(3.3 , mean = meanB , sd=sdB , lower.tail = TRUE) 

#Question (j)
pnorm(4 ,mean= meanB , sd=sdB , lower.tail = TRUE) - pnorm (3.2 ,mean= meanB , sd=sdB , lower.tail = TRUE) 

#Question (l)
qnorm(0.25, mean=meanA , sd=sdA/sqrt(358) , lower.tail = TRUE) 

#Question (m)
-qnorm(0.75, mean=meanA , sd=sdA/sqrt(358) , lower.tail = TRUE) 
qnorm(0.25, mean=meanA , sd=sdA/sqrt(358) , lower.tail = TRUE) 

#Question (n)
qnorm(0.85, mean=meanB , sd=sdB/sqrt(642) , lower.tail = FALSE) 

#Question (o)
-qnorm(0.75, mean=meanB , sd=sdB/sqrt(642) , lower.tail = TRUE) 
qnorm(0.25, mean=meanB , sd=sdB/sqrt(642) , lower.tail = TRUE) 

################################################################################

#Question 5
#Question (i)
pnorm(18, mean = 17.2, sd = 2.5/sqrt(55), lower.tail = TRUE) - pnorm(17, mean = 17.2, sd = 2.5/sqrt(55), lower.tail = TRUE)

#Question (ii)
#Question (a)
pnorm(13, mean = 12, sd = 3.2/sqrt(36), lower.tail = TRUE) 
#Question (b)
pnorm(13, mean = 12, sd = 3.2/sqrt(36), lower.tail = FALSE)
#Question (c)
pnorm(12, mean = 12, sd = 3.2/sqrt(36), lower.tail = TRUE) - pnorm(11, mean = 12, sd = 3.2/sqrt(36), lower.tail = TRUE)

#Question (iii)
pnorm(37.5, mean = 36, sd = 3.6/sqrt(35), lower.tail = TRUE) - pnorm(34, mean = 36, sd = 3.6/sqrt(35), lower.tail = TRUE) 

#Question (iv)
pnorm(83500, mean = 82000, sd = 5000/sqrt(50), lower.tail = FALSE) 

#Question (v)
pnorm(50000, mean = 51803, sd = 4850/sqrt(34), lower.tail = FALSE)
pnorm(48000, mean = 51803, sd = 4850/sqrt(34), lower.tail = TRUE)










