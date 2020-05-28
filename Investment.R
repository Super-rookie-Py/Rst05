# Investment Problem

# (a1, a2) (3, 8) 30%, (2, 4) 40%, (1, 1) 30%
# max a1*x1 + a2*x2
# s.t. 2*x1 + 4*x2 <= 100, x1, x2 >=0


x1 = 0:50  # x2가 0일때 갖을 수 있는값   
x2 = (100 - 2*x1)/4 # x1이 갖을 수 있는 값
 
Good = rep(0, time=51)
Normal = rep(0, time = 51)
Bad = rep(0, time = 51)
Average =rep(0, time=51)
G_mean = rep(0, time=51)

result <- data.frame(x1, x2, Good, Normal, Bad, Average, G_mean)

result$Good = 3*x1+8*x2
result$Normal = 2*x1 + 4*x2
result$Bad = x1 + x2
result$Average = result$Good*0.3+result$Normal*0.4+result$Bad*0.3
result$G_mean = (result$Good^3*result$Normal^4*result$Bad^3)^(1/10)

result

write.csv(result, 'Investment.csv')
