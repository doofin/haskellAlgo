{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Main where
import Text.ParserCombinators.Parsec
import Control.Monad
import Debug.Trace
import Data.List
import Test.QuickCheck
import System.Random
import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Ord
import qualified Data.Text as T
import Text.Blaze
import Text.Blaze.Html.Renderer.Text


equ21=do
  ops<-[(/),(*),(+),(-)]
  ops2<-[(/),(*),(+),(-)]
  nums<-[0..9]
  nums2<-[0..9]
  nums3<-[0..9]
  let expr=ops nums $ ops2 nums2 nums3
  guard $ expr==21
  return expr
--type Aj f g =Adjoint f g 
class (Functor f, Functor g) => Adjoint f g where
     counit :: f (g a) -> a
     unit   :: a -> g (f a)
     
instance Adjoint ((,) a) ((->) a) where
    -- counit :: (a,a -> b) -> b
    counit (x, f) = f x
    -- unit :: b -> (a -> (a,b))
    unit x = \y -> (y, x)     

t="dsfds\n"::T.Text
main = do
  --putStrLn $ show $ pacDup "adfdsdfffffaaaaa"
  s<-getLine
  putStrLn s
  when (s/="exit") $ main
  
 
tls=[1,2,3,4,4,5,6]  
xy 0 xs=head xs
xy i xs= xy (i-1) $ tail xs

len::[a]->Int
len [] = 0
len xs =(len $ tail xs) + 1

rev::[a]->[a]
rev []=[]
rev (x:xs)=(rev xs)++[x]

pldli=[1,2,2,1]

isPld xs = (\(a,b)->a==reverse b) $ if isOdd $ len xs
                                    then splitAt ((len xsodd) `quot` 2) $ xsodd
                                    else splitAt ((len xs)`quot`2 ) xs
           where
             xsodd=delAt (quot ((len xs) -1) 2) xs
             isOdd i=(mod i 2 )==1
-- Problem 20 (*) Remove the K'th element from a list.            
delAt::Int->[a]->[a]
delAt i xs=uncurry (++)  ((\(x,y)->(init x , y))  (splitAt (i+1)  xs))


data NL a=E a|L [NL a]             
nls=L [E 1,L [E 1,E 2 ,E 3,L [E 789,E 45] ]]

nll::NL a->[a]
nll (L [])=[]
nll (E a)=[a]
nll (L (x:xs))=(nll x)++(nll $ L xs)

flatten (E x) = return x
flatten (L x) = x>>= flatten

tails []=[]
tails xs=tail xs


deldup::(Eq a)=>[a]->[a]
deldup []=[]
deldup (x:y:xs)=if x == y  then deldup $ x:deldup xs else x:y:deldup xs
deldup [x]=[x]

mtrace=trace "dbg:"

pacDup::(Eq a,Show a)=>[a]->[[a]]
pacDup []=[]
pacDup (x:xs)=(\(a,na)->(x:a):(pacDup na)) $ span (==x) xs 

encDup::(Eq a)=>[a]->[(Int,a)]
encDup =fmap (\xs->(length xs,head xs)).group 

data VarLen a =Plu Int a|Sig a
  deriving Show
encDupv::(Eq a)=>[a]->[VarLen a]
encDupv =fmap (\xs->case len xs of
                         1->Sig $ head xs
                         _->Plu (length xs) $ head xs ).group 
decDupv::(Eq a)=>[VarLen a]->[a]
decDupv =join . fmap (\xs-> case xs of
                       Sig a->[a]
                       Plu i a->replicate i a)
dupli::Int->[a]->[a]
dupli i =join . fmap ( replicate i )

delEv _ []=[]
delEv i x=(take (i-1) x)++(delEv i $ drop i x)

slice::Int->Int->[a]->[a]
slice i j xs=take (j-i+1) $ drop (i-1) xs

slice1 i j xs=do
  (k,x)<-zip [1..(j+1)] xs
  guard $ k>=i&&k<=j
  return x

range m n
  | m<=n =m:(range (m+1) n)
  | otherwise = []

takes is xs=fmap snd $ join $ fmap (\i->filter (\(a,b)->a==i) $ zip [1..] xs) is
rand::Int->[a]->[a]
rand 0 _=[]
--rand i xs=takes (fst $ gen (mkStdGen 13) i) xs
         
--gen g 0=([],g)
--gen g r =(\(a,g3)->(if a/=head as then a:as else a:fst $ gen g3 r,) ) $ randomR (1,r) g


comb::Int->[a]->[[a]]
comb 0 _=[]
comb _ []=[]
comb i xs=(fmap (\q->(take (i-1) xs)++[q]) ( drop (i-1) xs) )++(comb i $ Main.tails xs)


combinations :: Int -> [a] -> [[a]]
combinations 0 _  = []
combinations n xs = do y:xs' <- Data.List.tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

gp2::Int->[a]->[[[a]]]
gp2 1 xs=[ [[head xs],tail xs] ]
gp2 n xs=(gp2 (n-1) xs)++[(\(x,y)->[x,y]) $ splitAt n xs]

gpn::Int->[a]->[[[a]]]
gpn 2 xs=gp2 (len xs) xs
gpn 3 xs=fmap (\xss->(head xss) :(join $ gpn 2 $ tail xs) ) $ gpn 2 xs

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination _ []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

lsort::[[a]]->[[a]]
lsort xs=llsort ((len xs)-1) xs
         where
           llsort 0 lxs=lxs
           llsort i (x:y:zs)=if length x>length y
                             then llsort (i-1) $ y:(llsort (i-1) (x:zs))
                             else llsort (i-1) $  x:(llsort (i-1) (y:zs))     
           llsort _ _=[]

prime n=pmeh (n-1) n
        where
          pmeh 1 _=True
          pmeh i pn=if (mod pn i)==0 then False else pmeh (i-1) pn

--Problem 31 (**) Determine whether a given integer number is prime. 
isPrime :: Int -> Bool
isPrime n | n < 4 = n > 1
isPrime n = all ((/=0).mod n) $ 2:3:[x + i | x <- [6,12..s], i <- [-1,1]]
            where s = floor $ sqrt $ fromIntegral n

isFac fac n=fac*(quot n fac)==n


factors 1=[1]
factors x =filter (/=1) $  (head facs):factors (quot x $ head facs)
  where
    facs =filter (\l->isPrime l && (isFac l x )) [2..x]

factorsc  =fmap (\li->(head li,len li)) .  group . factors 

--Problem 39 (*) A list of prime numbers. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. 
primesR l h =filter (\x->x>l&&x<h&&(prime x)) [1..h]

adder x =addera (quot x 2) x
         where
           addera::Int->Int->[(Int,Int)]
           addera 1 _=[]
           addera n x=((x-n),n):(addera (n-1) x)

--Problem 40 (**) Goldbach's conjecture.            
goldbach =filter (\(x,y)->(prime x)&&(prime y) ).adder

and1 True x=x
and1 False _=False
or1 True _=True
or1 False x=x

table1 li=do
  a<-li
  b<-li
  return $ show a ++" " ++ show b
  
--Problem 46 (**) A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
tablebn size expf=mapM_ putStrLn $ do
  a<-blns size
  return $ tostr a ++"-->" ++ expf a
    where
      blns n=replicateM n [True,False]
      tostr a=show a



-- Questions 54A to 60: Binary trees,Questions 61 to 69: Binary trees, continued
data Tr a =Nd a (Tr a) (Tr a)|Lf a|Nil
  deriving (Eq)

--depth of a tree
dptTr Nil=0
dptTr (Lf _)=1
dptTr (Nd _ t1 t2)=1+(max (dptTr t1) (dptTr t2) )


--Problem 56 (**) Symmetric binary trees 
symTr (Nd _ l r)=mirrTr l==r
  where
    mirrTr::Tr a->Tr a
    mirrTr (Lf a)=Lf a
    mirrTr Nil=Nil
    mirrTr (Nd l a b)=Nd l (mirrTr b) (mirrTr a)

-- Problem 55 (**) Construct completely balanced binary trees 
cbTr 0=[Nil]
cbTr n=do
  i<-[q..q+r]
  l<-cbTr i
  r<-cbTr (n1-i)
  return $ Nd 1 l r
    where
      n1=n-1
      (q,r)=quotRem n1 2
symcbTr n=(cbTr n1)>>=(\x->[Nd 1 x (mirrTr x)])
          where
            n1=quot (n-1) 2

-- Problem 59 (**) Construct height-balanced binary trees             
hbTr::Int->[Tr Int]
hbTr 0=[Nil]
hbTr 1=[Nd 1 Nil Nil]
hbTr n=do
  (ln,rn)<-[(n-1,n-2),(n-2,n-1),(n-1,n-1)]
  l<-hbTr ln
  r<-hbTr rn
  return $ Nd 1 l r


cardTr::Tr a->Int
cardTr (Nd _ l r)=1 + cardTr l +cardTr r
cardTr _=0

minLi (x:y:xs)=if x<y then minLi (x:xs) else minLi $ y:xs
minLi [x]=x



minHbTr::Int->(Tr Int,Int)
minHbTr =minimumBy (comparing snd) . fmap (\xs->(xs,cardTr xs)) . hbTr

iHbTr::Int->(Tr Int,Int)
iHbTr x=head.filter (\(t,c)->c==x) . fmap (\xs->(xs,cardTr xs)) . hbTr $ x

-- Problem 61 Count the leaves of a binary tree 
cardTrLf (Nd _ Nil Nil)=1
cardTrLf Nil=0
cardTrLf (Nd _ a b)=cardTrLf a + cardTrLf b

--Problem 61A Collect the leaves of a binary tree in a list 
tr2li (Nd x Nil Nil)=[x]
tr2li Nil=[]
tr2li (Nd _ a b)=tr2li a ++ tr2li b

-- Problem 62 Collect the internal nodes of a binary tree in a list 
intTr (Nd _ Nil Nil)=[]
intTr Nil=[]
intTr (Nd x a b)=[x]++intTr a ++ intTr b

levTr 0 (Nd x _ _)=[x]
levTr n (Nd x a b)=levTr (n-1) a ++ levTr (n-1) b
levTr _ _=[]

coorTr::Int->Tr String->Tr String
coorTr _ Nil=Nil
coorTr 0 (Nd x l r)=Nd (show (x,0)) l r
coorTr n (Nd x l r)=Nd (show (x,n)) (coorTr (n-1) l) $ coorTr (n-1) r
coorTr1 t=coorTr (dptTr t) t


instance Show a=>Show (Tr a) where
  show (Nd x Nil Nil)="Lf "++show x 
  show t=sho 0 t

    where
      nl="\n"
      ql="("
      qr=")"
      spac="   "

      dptC=dptTr t::Int
      
      sho1 Nil="_"
      sho1 (Lf x)="Lf "++ show x
      sho1 (Nd x a b)="Nd "++show x ++ nl ++ spac++show a ++ nl ++ spac++show b
      
      sho n (Nd x Nil Nil)=nl++(unwords $ replicate n spac )++ "Nd "++show x ++ " _ _"
      sho n (Nd x l r)=nl++(unwords $ replicate n spac )++ "Nd "++show x ++ (sho (n+1) l )++ (sho (n+1) r)
      sho dptC tr=nl++(unwords $ replicate dptC spac )++ sho1 tr
      
  

tr1=(Nd "a"
     (Nd "b" Nil Nil)
     (Nd "c"
        (Nd "d" Nil Nil)
        Nil))

-- Problem 64 :  As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid
layout :: Tr a -> Tr (a, (Int,Int))
layout  = fst . layoutAux 1 1
  where layoutAux x y Nil = (Nil, x)
        layoutAux x y (Nd a l r) = (Nd (a, (x',y))  l' r', x'')
          where (l', x')  = layoutAux x (y+1) l
                (r', x'') = layoutAux (x'+1) (y+1) r

layout1 :: Tr a -> Tr (a, (Int,Int))
layout1 t = layoutAux 1 1 sep1 t
  where d = dptTr t ::Int
        sep1 = 2^(d-2)
        layoutAux x y sep Nil = Nil
        layoutAux x y sep (Nd a l r) =
                Nd (a, (x,y))
                        (layoutAux (x-sep) (y+1) (sep `div` 2) l)
                        (layoutAux (x+sep) (y+1) (sep `div` 2) r)

readTr t=case parse tr "" t of
  Left e->show e
  Right v->show v
  where
    tr:: Parser (Tr Char) 
    tr=try bch<|>try lf <|>(return Nil)

    lf=do
      a<-letter
      --char ','
      --tr
      return $ Nd a Nil Nil
      
    bch:: Parser (Tr Char) 
    bch=do
      a<-letter
      char '('; l<-tr; char ',';r<-tr;char ')'
      return $ Nd a l r
      
    bch'= do 
      a<-letter
      (do char '(';
          l<-tr; char ','; r<-tr;char ')'
          return $ Nd a l r)
         <|> (return ( Nd a Nil Nil))

tr2ds Nil="."
tr2ds (Nd x l r) =x++ tr2ds l ++ tr2ds r

parse1 s parser=case parse parser "" s of
  Left e->show e
  Right v->show v

ds2tr s=parse1 s tr
  where
    tr=nd<|>(char '.'>>return Nil)
    nd=do
      a<-letter
      l<-tr
      r<-tr
      return $ Nd a l r

--99 questions/70B to 73, Multiway Trees
data MTr a =MNd a [MTr a]
     deriving Show
mMTr1::MTr Char
mMTr1=MNd 'a' [MNd 'b' [MNd 'd' []] ,MNd 'c' []]

--Problem 70C (*) Count the nodes of a multiway tree. 
cardMTr (MNd a [])=1
cardMTr (MNd a li)=1+(foldr1 (+) $ fmap (cardMTr) li)

--Problem 71 (*) Determine the internal path length of a tree. 
iplMTr (MNd a (x:xs))=1+(foldr1 (+) $ fmap (iplMTr) $ xs)

--Problem 72 (*) Construct the bottom-up order sequence of the tree nodes. 
btrMTr (MNd a [])=[a]
btrMTr (MNd a xs)=(xs>>=btrMTr)++ [a]

--brace s="("++s++")"


-- graph algo
data Graph a=Gcan [a] [(a,a)]|Gadj [(a,[a])]|Ghum [(a,a)]

gr2a::Eq a=>[(a,a)]->[(a,[a])]
gr2a li=fmap (maxtp) $ groupBy (\x y->fst x==fst y) $ fmap (head) $ group $ g2a li
  where
    maxtp::[(a,[a])]->(a,[a])
    maxtp (x:y:[])=if (len $ snd x )>(len $ snd y) then x else y
    maxtp [x]=x
    maxtp (x:y:xs)=if (len $ snd  x )>(len $ snd y) then maxtp (x:xs) else maxtp $ y:xs
    g2a::Eq a=>[(a,a)]->[(a,[a])]
    g2a li=do
      a<-li
      b<-li
      guard $ a/=b
      return (fst a,if (fst a==fst b )then [snd a]++[snd b] else [snd a])
a2gr::[(a,[a])]->[(a,a)]
a2gr li=do
  (a,as)<-li
  b<-as
  return (a,b)

--Problem 81 (**) Path from one node to another one          
paths :: Eq a =>a -> a -> [(a,a)] -> [[a]] 
paths source sink edges 
    | source == sink = [[sink]]
    | otherwise = do
        edge<-edges
        guard $ (fst edge) == source
        path<-paths (snd edge) sink (edges\\[edge])
        return $ source:path

--Problem 82 (*) Cycle from a given node 
--gcycle::Eq a=>a->[(a,a)]->[[a]]
gcycle _ _ []=[]
gcycle x (l,r) eg
  |x==r=[[r]]
  |otherwise=do
     (a,b)<-eg
     guard $ x==a
     y<-eg\\[(a,b)]
     cyc<-gcycle b y (eg\\[(a,b)])
--     guard $ x==snd y
     return $ b:cyc

cyclebi :: (Eq a) => a -> [(a, a)] -> [[a]]
cyclebi a xs = [a : path | e <- xs, fst e == a, path <- paths (snd e) a (xs\\[e])]
               ++[a : path | e <- xs, snd e == a, path <- paths (fst e) a (xs\\[e])]

isog1=[(1,[2,3]),(2,[3,1])]
isog2=[(2,[1,3]),(1,[2,3])]

--Problem 85 (**) Graph isomorphism 
isogrh::Eq a=>[(a,[a])]->[(a,[a])]->Bool
isogrh l r =(deleteFirstsBy (\x y-> (fst x==fst y)&&(snd x\\snd y==[])) l r)==[]

ls [] []=[]
ls [] a=[a]
ls a []=[a]
ls xs ys =do
  x<-xs
  y<-ys
  guard $ x==y
  ls (xs\\[x]) (ys\\[y])
--  where

--class SimpleGraph where
  --nodes::Graph a->[a]
gadj=[('b',['c','f']), ('c',['b','f']), ('d',['a','k']), ('f',['b','c']),('a',"dk"),('k',"ad")]
clrgph::Eq a=>[(a,[a])]->Int->[(a,[a],Int)]
clrgph [] _=[]
clrgph ghs n=do
  gs<-gphs
  gs2<-gphs
  guard $ not $ elem (fst gs2) (snd gs)
  tl<-clrgph (gphs\\[gs,gs2]) (n+2)
      {-}(do
        gss<-gphs
        guard $ not $ elem (fst gs2) (snd gss)
        return gss)-}

  (fst gs,snd gs,n):(fst gs2,snd gs2,n+1):tl:[]
    where
      --gphs::[(a,[a])]
      gphs=sortBy (comparing (length.snd)) ghs

reachable::(Eq a)=>[(a,[a])]->[a]->[a]
reachable [] _=[]
reachable grh as=do
  g<-grh
  a<-as
  guard $ (fst g)==a
  (snd g)++(reachable [g1|g1<-grh,(fst g1)/=a] (snd g))

delete1::(Eq a)=>a->[a]->[a]
delete1 _ []=[]
delete1 x (z:zs)=if x==z then delete1 z zs else z:(delete1 x zs)
--group1::(Eq a)=>[a]->[[a]]

sameset xs ys=(nub xs)\\(nub ys)==[]
partitiongrh g=nubBy sameset $ partitiong g --h88
  where
    partitiong grh=do
      (x,xs)<-grh
      return $ nub $ reachable grh [x]
  
--queens::Int->[[Int]]
--queens n=
qbool::[(Int,Int)]->[(Int,Int)]
qbool is=do
  (x1,y1)<-is
  (x2,y2)<-is
  guard $ x1/=x2 && y1/=y2 && (x1-x2)/=(y1-y2)
  return (x1,y1)
--queens n=qbool pos
--  where
pos n=do
  p<-permutations [1..n]
  return $ zip [1..n] p
q2 xxs=do
  xs<-xxs
  xy@(x1,y1)<-xs
  (x2,y2)<-xs
  guard $ (abs $ x1-x2)/=(abs $ y1-y2)
  return xy
--delim::(Eq a)=>a->[a]->[[a]]
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
