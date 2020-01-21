# Lab one master copy compiled by yours truly \
 

# Exercise 1
p <- ggplot(mpg, aes(displ, hwy))
p + geom_point()



p <- ggplot(data = mpg, 
            mapping = aes(x = displ, y = hwy))
p + geom_point()



p <- ggplot(data = mpg, 
            mapping = aes(x = class, y = drv))
p + geom_point()


#Exercise 1b:
  
p <- ggplot(mpg, aes(displ, hwy, color = class)) 
p + geom_point()

  
  
#Exercise 2 
bankdata <-read.csv("bank.csv")

ggplot(data = bankdata, aes(x = marital, fill = housing)) + 
  geom_bar()

p <- ggplot(bankdata, aes(factor(y), duration))
p + geom_boxplot(aes(fill = factor(y)))


  

