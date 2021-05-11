{-# LANGUAGE DerivingStrategies, DeriveAnyClass, GeneralizedNewtypeDeriving, DeriveFunctor, StandaloneDeriving, DeriveFoldable #-}

module Primary.Deriving where

-- deriving 导出策略
-- 前面介绍了 DerivingAnyClass, GeneralizedNewtypeDeriving, 已经类型类默认的实现，三者之间可能会有冲突或者优先级区分
-- 需要语言扩展 DerivingStrategies 实现对于类型类实例的实现精细的控制

newtype T a = T a
  deriving Show
  deriving stock (Eq, Foldable)   -- stock 表示 GHC 中可以导出的 Haskell 2010 语言标准中规定可以导出的类型
  deriving newtype Ord            -- newtype 表示应用 GeneralizedNewtypeDeriving 语言扩展导出类型类实例
  deriving anyclass Read          -- anyclass 表示应用 DeriveAnyClass 语言扩展来导出类型类实例

deriving stock instance Functor T

