module Content where

data A = A Int String 
data B = B1 | B2 Int | B3 String Float

data Student = Student
 { age :: Int,
   name :: String,
   className :: String,
   score :: Float
 } 

student1 = Student { age = 1, name = "s", className = "一年二班", score = 20.4 }

student2 = Student 15 "韩梅梅" "二年三班" 180.0

class1 = className student1
name1 = name student1
name2 = name student2