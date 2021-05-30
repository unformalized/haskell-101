module Intermediate.IO where

-- IO 引用，可以在 IO Monad 内部更改内存状态
import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
import Control.Monad
import System.Environment
import System.IO

bool :: IO ()
bool = do
  bRef <- newIORef True
  modifyIORef bRef not
  b <- readIORef bRef
  print b
  writeIORef bRef True
  b <- readIORef bRef
  print b

forever' :: Applicative f => f a -> f b
forever' a =
  let a' = a *> a'
  in a'

foreverTellName :: IO b
foreverTellName = forever $ do
  print "Can you tell me your name?"
  name <- getLine
  print ("Hello " ++ name)

debug :: IO ()
debug = do
  when True (print "Debugging")

collect3Line :: IO ()
collect3Line = do
  strs <- sequence $ replicate 3 getLine
  print strs


-- sequence  :: Monad m => [m a] -> m [a]
-- sequence_ :: Monad m => [m a] -> m ()

print3Line :: IO [()]
print3Line = sequence $ replicate 3 $ print "Haskell is fun!"

print3Line' :: IO ()
print3Line' = sequence_ $ replicate 3 $ print "Haskell is fun!"

-- mapM  :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()

-- 环境变量与命令行参数
-- getArgs :: IO [String]
-- getProgName :: IO String
-- getEnv :: String -> IO String

-- 数据读写
-- 对于文件或数据流的操作需要句柄（handle），句柄为一个值，当用户访问文件时，就会得到一个句柄
-- 句柄就像文本文件的光标，在光标的位置插入字符，选取文本等操作

-- stdin :: Handle, stdout :: Handle 就是从操作系统命令行输入与输出的句柄
-- 通过句柄来得到文件的属性 hFileSize :: Handle -> IO Integer 获取文件大小
-- hClose :: Handle -> IO () 关闭句柄

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
-- ReadMode 只能读文件，文件必须存在，否则返回错误 readFile :: FilePath -> IO String
-- WriteMode 只能写文件，若文件存在，内容被清空，若不存在则创建 writeFile :: FilePath -> String -> IO ()
-- AppendMode 只能写文件，文件存在则内容条件到末尾，若不存在则创建，appendFile :: FilePath -> String -> IO ()
-- ReadWriteMode 可读可写，文件存在，内容不变，不存在则创建
-- openFile :: FilePath -> IOMode -> IO Handle


-- 通过 openFile 获取到句柄后，Haskell 会进行加锁保护
-- 如获取到文件的 ReadMode 句柄后，只能再获取 ReadMode 的句柄，不能获取其他三个模式的句柄
-- 若得到文件写模式的句柄，则也只能再获取 ReadMode 的句柄，获取其他模式句柄会得到异常（isAlreadyInUseError）

-- 获取到句柄后可以用 hSeek :: Handle -> SeekMode -> Integer -> IO ()
-- 在文件中进行移动，data SeekMode = AbsoluteSeek | RelativeSeek | SeekFromEnd
-- AbsoluteSeek 绝对移动，从文件头 0位置开始，移动给定的字节数
-- RelativeSeek 相对移动，从当前位置移动给定的字节数
-- SeekFromEnd 从文件末尾向前移动给定字节数

-- hTell :: Handle -> IO Integer 获取句柄当前访问的绝对位置
-- hIsEOF :: Handle -> IO Integer 判断是否在文件末尾处

-- hGetChar :: Handle -> IO Char 返回一个字符，并向后移动一个字节
-- hGetLine :: Handle -> IO String 返回一行字符串，并移动对应长度
-- hLookAhead :: Handle -> IO Char 保持当前位置不变，并返回下一个字符，若当前位置在末尾则抛出异常（isEOFError）
-- hGetContent :: Handle -> IO Char 返回当前位置后所有数据，并关闭句柄，若位置已在末尾则抛出异常

getCharSelf :: IO Char
getCharSelf = hGetChar stdin


-- 从数据流，如文件，网络端口，读取数据时不会一次性将所有数据读取完毕，而且按字符进行读取效率低
-- Haskell 提供缓冲模式进行读取数据，数据会读或写到内存中一段一段处理
-- data BufferMode = NoBuffer | LineBuffering | BlockBuffering (Mabye Int)
-- NoBuffer 无缓冲模式，字符逐个被操作
-- LineBuffering 行缓冲模式，字符串以行为单位被操作
-- BlockBuffering 块缓冲模式，字符串被存储在指定字节长度的缓冲区中被操作，若Nothing则采用默认大小

-- hSetBuffering :: Handle -> BufferMode -> IO () 改变缓冲模式
-- hFlush :: Handle -> IO () 使用行或块缓冲模式时，若缓冲区未达到要求则可能字符无法在命令行上输出，通过
-- hFlush 强制将数据写给操作系统并清空缓冲区




