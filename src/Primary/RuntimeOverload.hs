{-# LANGUAGE GADTs, ExistentialQuantification #-}

module Primary.RuntimeOverload where
-- 运行时重载

data Rect = Rect Double Double
newtype Circle = Circle Double

class HasArea t where
  area :: t -> Double

instance HasArea Rect where
  area (Rect x y) = x * y

instance HasArea Circle where
  area (Circle r) = r ^ 2 * pi

-- Rect 和 Circle 都属于 Shape 类，现在需要 map area [Shape]
-- 需要考虑 Shape 类型的实现，使用 GADT 实现运行时重载

data Shape where
  Shape :: HasArea t => t -> Shape

-- 这里使用 GADTs 实现 Shape 类型，相当于是将实现了 HasArea 类型类的类型用 Shape 包装起来
-- 所以可以把 Shape 充当一个函数，接受一个实现了 HasArea 类型类的类型

shapes :: [Shape]
shapes = [Shape (Rect 2 2), Shape (Circle 2)]

-- 对 Shape 类型使用 area 必须先实现 HasArea

instance HasArea Shape where
  area (Shape shape) = area shape

areas :: [Double]
areas = map area shapes

-- impl by using Existential
data Shape2 = forall a. (HasArea a) => Shape2 a

instance HasArea Shape2 where
  area (Shape2 a) = area a


