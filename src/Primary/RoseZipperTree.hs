module Primary.RoseZipperTree where

import Data.Tree (Forest, Tree(Node), drawTree)
import Data.Tree.Zipper (label, fromTree, TreePos, childAt, tree, delete, parent, toTree)
import Data.Function ((&))
import Data.Maybe

data TreePos' t a = Loc
  {
    _content  :: t a
  , _before   :: Forest a
  , _after    :: Forest a
  , _parents  :: [(Forest a, a, Forest a)]
  }

directory :: Tree String
directory = Node "Home"
  [ Node "Picture" [ Node "travel" [], Node "family" []]
  , Node "Video" [Node "Fast and Furious" [], Node "True Lies" []]
  , Node "Music" [Node "My Love" [], Node "Destiny" []]
  ]

firstDicInHome = fromTree directory & childAt 0 & fromJust & label

onlyVideoTree = fromTree directory & childAt 1 & fromJust & tree

deletePictureAndPutStr = fromTree directory & childAt 0 & fromJust & delete & parent & fromJust & toTree & drawTree & putStrLn




