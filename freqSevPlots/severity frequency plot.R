#R severity frequency plots
library(tidyverse)

#create dataset:
# disenroll yes no
# female
# plan type: a, b, c
set.seed(839)
female = rbinom(1000, 1, .6)
sum(female)
#590

plan_type0 = runif(1000)
plan_type1 = ifelse(plan_type0 < .6, "A", "Z")
plan_type1 = ifelse((plan_type0 >= .6 & plan_type0 < .9), "B", plan_type1)
plan_type1 = ifelse((plan_type0 >= .9), "C", plan_type1)
plan_type = plan_type1
planB = ifelse(plan_type == "B", 1, 0)
planC = ifelse(plan_type == "C", 1, 0)

tibble(plan_type1) %>% group_by(plan_type1) %>% summarize(n())

#likelihood of leaving: logistic equation
# -2*female + 3*planB + 5*planC
# p  = 1/(1 + exp(-(-2*female + 3*planB + 5*planC)))
?rlogis #gonna leave this out for now

p  = 1/(1 + exp(-(-4*female + 3*planB + 5*planC)))
hist(p)

disenroll = ifelse(p > .5, 1, 0)

disenrollData = data.frame(cbind(disenroll, female, plan_type, planB, planC, p))

#bar graph the frequency female
ggplot(disenrollData, aes(x=female)) + 
  geom_bar(stat="count", width=0.7, fill = "steelblue")

#plan type
ggplot(disenrollData, aes(x = plan_type)) + 
  geom_bar(stat = "count", width = 0.7, fill = "steelblue")

#line chart of rate of disenroll
str(disenrollData)

#calculate disenrollment rate by female
disenrollData = disenrollData %>% mutate(disenroll = 
                                           as.numeric(as.character(disenroll)))

femDisEn = disenrollData %>% group_by(female) %>% 
  summarise(disRate = sum(disenroll)/n())

ggplot(data = femDisEn, aes(x = female, y = disRate, group = 1)) +
  geom_line(linetype = "dashed") + geom_point() + ylim(0, 1)

# Overlay plots
ggplot(disenrollData, aes(x=female)) + 
  geom_bar(stat="count", width=0.7, fill = "steelblue") +
  geom_line(data = femDisEn, aes(x = female, y = disRate, group = 1),
            linetype = "dashed") +
  geom_point(data = femDisEn, aes(x = female, y = disRate, group = 1))

#from internet on how to set up the axes mathematically:
#https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
ylim.prim <- c(0, 600)   
ylim.sec <- c(0, 1) 

b = diff(ylim.prim)/diff(ylim.sec)
a = b*(ylim.prim[1] - ylim.sec[1])

ggplot() + 
  geom_bar(data = disenrollData, aes(x=female),
           stat="count", width=0.7, fill = "steelblue") +
  geom_line(data = femDisEn, aes(x = female, y = a + disRate*b, group = 1),
            linetype = "dashed") +
  geom_point(data = femDisEn, aes(x = female, y = a + disRate*b, group = 1)) +
  scale_y_continuous(name = "Count",
                     sec.axis = sec_axis(~ (. - a)/b, name = "% Disenroll",
                                         labels = function(x) {
                                           paste0(round(x * 100, 0), "%")
                                           }
                                         )
                     ) +
  theme(axis.title.y.left = element_text(color = "steelblue"),
        axis.text.y.left = element_text(color = "steelblue")
        )



# se = sqrt(p*q/N)
# p is mean for data where disenroll = 1
# q is mean for data where disenroll = 0
seDF = disenrollData %>% group_by(female) %>%
  summarize(p = sum(disenroll)/n(),
            q = 1 - sum(disenroll)/n(),
            var = p*q,
            sd = sqrt(p*q),
            se = sqrt(var/n()))

ggplot() + 
  geom_bar(data = disenrollData, aes(x=female),
           stat="count", width=0.7, fill = "steelblue") +
  geom_line(data = femDisEn, aes(x = female, y = a + disRate*b, group = 1),
            linetype = "dashed") +
  geom_point(data = femDisEn, aes(x = female, y = a + disRate*b, group = 1)) + 
  geom_errorbar(data = femDisEn,
                aes(x = female, y = a + disRate*b,
                    ymin = a + (disRate - 2*seDF$se)*b, 
                    ymax = a + (disRate + 2*seDF$se)*b,
                width = .1
                # position = position_dodge(.9)
                )
                ) +
  scale_y_continuous(name = "Count",
                     sec.axis = sec_axis(~ (. - a)/b, name = "% Disenroll",
                                         labels = function(x) {
                                           paste0(round(x * 100, 0), "%")
                                         }
                     )
  ) +
  theme(axis.title.y.left = element_text(color = "steelblue"),
        axis.text.y.left = element_text(color = "steelblue")
  ) 





ggplot() + 
  geom_bar(mapping = aes(x = dt$when, y = dt$numinter), stat = "identity", fill = "grey") +
  geom_line(mapping = aes(x = dt$when, y = dt$prod*5), size = 2, color = "blue") + 
  scale_x_date(name = "Day", labels = NULL) +
  scale_y_continuous(name = "Interruptions/day", 
                     sec.axis = sec_axis(~./5, name = "Productivity % of best", 
                                         labels = function(b) { paste0(round(b * 100, 0), "%")})) + 
  theme(
    axis.title.y = element_text(color = "grey"),
    axis.title.y.right = element_text(color = "blue"))

#Idea: make it so you can change the right y-axis based on the rates
#do the above for the plan type








