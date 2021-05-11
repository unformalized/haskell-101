module Primary.DataType where

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq, Ord, Enum, Read)

-- 利用 Enum succ 函数实现 tomorrow

tomorrow :: Day -> Day
tomorrow Sun = Mon
tomorrow d = succ d

yesterday :: Day -> Day
yesterday Mon = Sun
yesterday d = pred d

type Name = String
type Auther = String
type ISBN = String
type Price = Float


-- = 左边 Book 为类型构造器，右边为数据构造器
data Book = Book Name Auther ISBN Price deriving (Show, Eq)

data Book' = Book'
  {
    name :: Name,
    author :: Auther,
    isbn :: ISBN,
    price :: Price
  }

increasePrice :: ([Book'], [Book']) -> Book' -> Price -> ([Book'], [Book'])
increasePrice (b1, b2) b pri = (b : b1, (b { price = pri + price b }) : b2)

-- 参数化类型 Maybe 为参数化类型
-- data Maybe a = Nothing | Just a

-- Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

disjoint :: [a] -> [b] -> [Either a b]
disjoint as bs = map Left as ++ map Right bs

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right y) = g y

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either' left right) ([], [])
  where
    left a (l, r) = (a:l, r)
    right b (l, r) = (l, b : r)


data Pair a b = Pair a b

pf (Pair a b) = a
ps (Pair a b) = b


-- 递归类型

data Nat = Zero | Succ Nat deriving (Show, Eq, Ord)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n - 1)

add :: Nat -> Nat -> Nat
add m Zero = m
add m (Succ n) = add (Succ m) n

-- 杂合定义类型


-- 枚举构造的类型
data Shape = Circle Float | Rect Float Float deriving (Show, Eq)

data Shape2 = Circle2 { radius :: Float } | Rect2 { len :: Float , width :: Float  } deriving (Show, Eq)

data Person = Person
  {
    p_name :: String,
    age  :: Int,
    sex  :: Bool
  }

showPerson :: Person -> String
showPerson Person { p_name = str, sex = s } = str ++ show s


-- 枚举递归类型

data BoolExp = TURE | FALSE | IF BoolExp BoolExp BoolExp deriving (Show, Eq)

eval :: BoolExp -> Bool
eval TURE = True
eval FALSE = False
eval (IF con b1 b2) | eval con  = eval b1
                    | otherwise = eval b2

newtype State s a = State
  {
    runState :: s -> (a, s)
  }

-- 参数化递归类型

infix 5 :.
data List a = Nil | a :. List a deriving (Eq, Show)

lhead :: List a -> a
lhead Nil = error "List is empty"
lhead (a:._)  = a

-- 类型的同构
-- 同构的类型：对于类型 A，B ，若可与定义 f :: A -> B ，将 A 映射到 B，并且可以定义 f 的反函数 g :: B -> A
-- 将 B 映射到 A 且满足 f . g = id(b) g . f = id(a)，则类型 A 与类型 B 为同构

data Unit = Unit

list2Nat :: List a -> Nat
list2Nat Nil = Zero
list2Nat (x:.xs) = Succ (list2Nat xs)

nat2List :: Nat -> List Unit
nat2List Zero = Nil
nat2List (Succ n) = Unit :. nat2List n


-- 柯里化性质：对于任意函数 f :: (A,B) -> C 与 g :: A' -> A，都有 /\(f).g = /\(f.(g x id_b))

f :: (a,b) -> c
f = undefined

g :: a' -> a
g = undefined

(><) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(><) f g (a,b) = (f a,g b)


l = curry f . g
r = curry (f . (g >< id))

e = g >< id
-- (a, c) ->





