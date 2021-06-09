module Intermediate.DataStream where

-- IO Monad 存在以下问题
-- 1. Haskell 是惰性求值的，可能会造成文件并没有读取出来时，关闭句柄
-- 2. readFile 等基于 IO Monad 的基本函数没有提供内存控制和句柄等资源的接口
-- 3. 需要手动处理句柄的资源与处理可能出现的输入输出
-- 4. IO Monad 缺乏有效的组合函数





