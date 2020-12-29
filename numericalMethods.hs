{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

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
import Control.Lens

-- data definitions
type Matrix a=[[a]]
type Vector a=[a]

data Inflis=Nil|Inf [Inflis]  

--basic operations
coprod::(Ord a)=>[a]->a
coprod=foldl1 max

elemM::Matrix Double->Int->Int->Double
elemM matrix i j=(matrix !! (i-1))!!(j-1)

elemVec::Vector Double->Int->Double
elemVec vector i=vector !! (i-1)

diff x y=abs $ x-y
concats = foldl1 (++)

sgm f x0 x1= foldl1 (+) $ fmap (\i->f i) [x0..x1]
sumlist::(Num a)=>[a]->a
sumlist=foldl1 (+)

seqdiff i xs=zipWith (-) (drop i xs) $ xs

meanSquareError xs1 xs2=sqrt $ realToFrac $ foldl1 (+) $ zipWith (\x1 x2->(x1-x2)^2) xs1 xs2 

--printMatrix::(Show a)=>Matrix a->IO ()
printMatrix mx name=do
  let delim=foldl1 (++) $ replicate 10 "-"
  putStrLn $ delim++"Matrix start! Matrix name: "++name++delim
  mapM_ (putStrLn) $ fmap show mx
  putStrLn $ delim++"Matrix end! Matrix name: "++name++delim
powerset = filterM (\_ -> [True, False])
--baseFunc::(Rational a)=>Int->(a->a)
baseFunc i=case i of
  -1->(\x->1/x)
  0->const 1
  j->(\x->x^j)




             
fs2v::Num a=>[a->a]->[a]->a->a
fs2v fs coeff x=foldl1 (+) $ zipWith (*) coeff $ fmap (\f->f x) fs

min2mult_poly::(Num a,Show a)=>Vector a->Vector a->[a->a]->(Matrix a,Matrix a)
min2mult_poly xi yi fs=(ata,aty)
  where
    matrixA=fmap (\x->fmap (\f->f x) $ fs) xi
    ata = (flip multMatrix) matrixA $ transposeMatrix matrixA
    aty = multMatrix  (transposeMatrix matrixA) $ transposeVector yi
transposeMatrix::Num a=>Matrix a->Matrix a
transposeMatrix m=fmap (\i->fmap (\row->row!!i) m) [0..(length $ head m)-1]

transposeVector::Num a=>Vector a->Matrix a
transposeVector =fmap (\x->[x])

multline::Num a=>Vector a->Matrix a->Vector a
multline m1row m2T=do
  m2trow<-m2T
  return $ multLinewise m1row m2trow
  
multLinewise l1 l2=foldl1 (+) $ zipWith (*) l1 l2
multMatrix::Num a=>Matrix a->Matrix a->Matrix a
multMatrix (m1:m1s) m2'=(multline m1 m2):(multMatrix m1s m2')
  where m2= transposeMatrix m2'
multMatrix [] m2'=[]
        --(m2:m2s)=m2''

compTrape h x0 x1 f xs=(h/2)*(sumlist [f x0,2*(sumlist $ fmap f $ tail $ init xs),f x1])

--chapter 6: numerical integration---
--simpson method for numerical integration
compSimpson::(Fractional a)=>a->a->a->(a->a)->[a]->a
compSimpson h x0 x1 f xs=(h/3)*(sumlist [f x0,f x1,4*(sumlist $ fmap f xsodd),2*(sumlist $ fmap f xseven)])
  where
    xsi=zip [0..] xs
    indElem bool=fmap snd $ filter (bool.fst) xsi
    xsodd=indElem (\i->i `mod` 2 ==1)
    xseven=indElem (\i->i `mod` 2 ==0)

-- Romberg method for numerical integration
rberg :: (Fractional a, Num a, Ord a,Show a,Enum a) => a -> a -> (a -> a) -> a -> a
rberg x0 x1 f theta=rberg'  [t 0 0,t 0 1] 2
  where
    rberg' (p1:p2:[]) k
      |(abs $ p1 - p2)<theta=p2
      |otherwise=rberg' [p2,uncurry t $ rbgi'!!k] $ k+1
    rberg' _ _=undefined
    t 0 0=((x1-x0)/2)*((f x0) + (f x1))
    t 0 k=(t 0 $ k-1)/2 + (x1-x0)/2^k*(sumlist $ fmap (\i->f $ x0+(2*i-1)*(x1-x0)/2^k) [1..2^(k-1)])
    t m l=(4^m*(t (m-1) (l+1)) - t (m-1) l)/(4^m - 1)
    rbgi'=[(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0)]
    rbgi x=undefined
      where
        numsApproIndex y ys=fmap fst $ minimumBy (comparing fst) $ fmap (abs.(-y)) ys 
        n=((sqrt $ 1+8*x) - 1)/2
        n'=floor n
        

greducet=[[2,3,4,6],
          [3,5,2,5],
          [4,3,30,32.0]
         ]
--substitute :: Matrix -> Row

--   solving linear equations
-- solve Triangle Matrix  directly
solveTriangleMatrix matrix = foldr next [last (last matrix)] (init matrix)
  where
    next row found = let
      subpart = init $ drop (length matrix - length found) row
      solution = last row - sum (zipWith (*) found subpart)
      in solution : found         
-- gauss elimination method     
gsReduce :: (Num a,Fractional a,Eq a)=>Matrix a-> Matrix a
gsReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1]
  where
    swap xs a b
      | a > b = swap xs b a
      | a == b = xs
      | a < b = let
          (p1,p2) = splitAt a xs
          (p3,p4) = splitAt (b-a-1) (tail p2)
          in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)
 
    reduceRow matrix1 r = let
      firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
      matrix2 = swap matrix1 r firstnonzero
      row = matrix2 !! r
      row1 = map (\x -> x / (row !! r)) row
      subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
      nextrows = map subrow $ drop (r+1) matrix2
      in take r matrix2 ++ [row1] ++ nextrows
    fixlastrow matrix' = let
      a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
      in a ++ [init (init row) ++ [1, z / nz]]

--Newton interpolation 
--newton::Vector Double->Vector Double->Double->Double
-- xs and ys : list of known values for a function.
newton xs ys x = ["differential quotient:"++(show dqw)++"\n",
                  "result:"++(show $ foldr1 (+) $ zipWith (*) (fmap head $ ys:dqw) polys)++"\n",
                  "remains:"++show remains]
  where
    len=length xs
    remains=(last $ last dqw)*(foldl1 (*) $ fmap ((x-).(xs!!)) [0..len-1])
    dqw::[[Double]]
    dqw=fmap dq [1..((length xs)-1)]
      where
        dq 1 =zipWith (/) (seqdiff 1 ys) (seqdiff 1 xs)
        dq n =zipWith (/) (seqdiff 1 (dq $ n-1)) (seqdiff n xs)
    polygen 0 =x-(xs!!0)
    polygen i =(x-(xs!!i))*(polygen (i-1))
    polys=1:(fmap polygen [0..(length xs)])

-- chapter 3 : numerical linear algebra,for solving a linear system of equations,like Ax=b 

--Jacobi method is an iterative algorithm,for solving a linear system of equations,like Ax=b 
-- params: xsinit matrixA:the matrix for the set of linear equations . vectorB:the b in the right hand side. precision omega
jacob::Vector Double->Matrix Double->Vector Double->Double->Vector Double
jacob xsinit matrixA vectorB precision=jcb [] xsinit
  where
    jcb::Vector Double->Vector Double->Vector Double
    jcb [] p2=jcb p2 $ expr p2
    jcb p1 p2
        |(coprod $ fmap (uncurry diff) $ zip p1 p2)<precision=p1
        |otherwise=jcb p2 $ expr p2

    expr::Vector Double->Vector Double
    expr xm=fmap
             (\(i,js)->
                  (1/(a i i))*((-) (b i) $ foldl1 (+) $ fmap (\j->(a i j)*(elemVec xm j)) js)
             )
            (index $ length xsinit)
        where
          a i j=elemM matrixA i j
          b i =elemVec vectorB i
    index::Int->[(Int,[Int])]
    index n=fmap (\x->(x,filter (/=x) [1..n])) [1..n]
--        where n=length xsinit;gstest=gs t_xi t_A t_B 0.00001

-- gauss elimination method,for solving a linear system of equations,like Ax=b 
-- params: 
-- xsinit matrixA:the matrix for the set of linear equations . vectorB:the b in the right hand side. precision omega
gs::Vector Double->Matrix Double->Vector Double->Double->Vector Double            
gs xsinit matrixA vectorB precision =gauss 0 [] xsinit
    where
      len=length xsinit
      gauss::Int->Vector Double->Vector Double->Vector Double
      gauss ind p1 p2
          |p1==[]=f
          |(coprod $ fmap (uncurry diff) $ zip p1 p2)<precision=p2
          |otherwise=f
          where
            f=gauss (ind+1) p2 $ ga ind p2--fmap (\i->x i $ ind+1) [1..len]
      ga ind pre=fmap (\i->tc i (ind+1) pre) [1..len]
      tc i m p=trace ("Gauss-Seidel:[ith,recursion count,value]="++ show [show i,show m,show $ x i m p])  $ x i m p
      x i 0 _=elemVec xsinit i
      x i m prexs=(1/(a i i))*
                     ((-) (b i) $ foldl1 (+) $ 
                              (fmap (\j-> (a i j)*(x j  m prexs)) f) ++ (fmap (\j-> (a i j)*(xmpre j)) s))
          where
            xmpre=elemVec prexs
            (f',s')=span (<i) [1..len]
            (f,s)= (f',tail s')
            a i j=elemM matrixA i j
            b i =elemVec vectorB i
-- the method of successive over-relaxation (SOR),for solving a linear system of equations,like Ax=b 
-- params: 
-- xsinit matrixA:the matrix for the set of linear equations . vectorB:the b in the right hand side. precision omega
sor::Vector Double->Matrix Double->Vector Double->Double->Double->Vector Double            
sor xsinit matrixA vectorB precision omega=gauss 0 [] xsinit
    where
      len=length xsinit
      gauss::Int->Vector Double->Vector Double->Vector Double
      gauss ind p1 p2
          |p1==[]=f
          |(coprod $ fmap (uncurry diff) $ zip p1 p2)<precision=p2
          |otherwise=f
          where
            f=gauss (ind+1) p2 $ ga ind p2--fmap (\i->x i $ ind+1) [1..len]

      ga ind p2=fmap (\i->tc i (ind+1) p2) [1..len]
      x::Int->Int->Vector Double->Double
--      tc i m=trace ("index,recur,val="++ show [show i,show m,show $ x i m])  $ x i m
      tc i m p=trace ("SOR:[ith,recursion count,value]="++ show [show i,show m,show $ x i m p])  $ x i m p
      x i 0 _=elemVec xsinit i
      x i m prexs=(xmpre i)+
            (omega/(a i i))*
                     ((-) (b i) $ foldl1 (+) $ 
                              (fmap (\j-> (a i j)*(x j  m prexs)) f) ++ (fmap (\j-> (a i j)*(xmpre j)) s))
          where
            xmpre=elemVec prexs
            (f',s')=span (<i) [1..len]
            (f,s)= (f', s')
            a i j=elemM matrixA i j
            b i =elemVec vectorB i




-- chapter 2 : Dichotomy methods
chapter2_1=do
  putStrLn $ show $ dichotomy f1 1 2 0.005 
  -- f1: an example function已定义的函数 ,1 2, - the range containing roots含根区间 ,0.005 precision精度

-- an example function  
f1 x=x^6-x-1
--dichotomy::(Int->Int)->Int->Int->Int->Int
dichotomy f x0 x1 e --function name:dichotomy,the followings are parameters
          |(x1-x0)<e = x1
          |(f x0)*(f m)<0 = dichotomy f x0 m e
          |otherwise = dichotomy f m x1 e
                     where m=((x0+x1)/2)

-- solutions for quiz
main=do
  finalProj
  return ()

t_A::Matrix Double
t_A=[[9,-1,-1],[-1,8,0],[-1,0,9]]
t_B::Vector Double
t_B=[7,7,8]
t_xi::Vector Double
t_xi=replicate 3 0
--final project
chapter4=do
  mapM_ (putStrLn.show) [jacob chapter4_xinit chapter4_1_2_A chapter4_1_2_B 0.0005,
                         gs chapter4_xinit chapter4_1_2_A chapter4_1_2_B 0.0005,
                         sor chapter4_xinit chapter4_2_A chapter4_2_B 0.00005 1.24]
chapter4_1_2_A,chapter4_2_A::Matrix Double
chapter4_1_2_B,chapter4_2_B::Vector Double

chapter4_xinit=[0,0,0]
chapter4_1_2_A=[[5,-2,1],
                [1,5,-3],
                [2,1,-5]]
               
chapter4_1_2_B=[4,2,-11]
--chapter4_1_2_prec=0.0005

chapter4_2_A=[[4,3,0],
              [3,4,-1],
              [0,-1,4]]
             
chapter4_2_B=[16,20,-12]
chapter4_2_prec=0.00005
chapter4_1_2_omega=1.24

chapter5=do
  putStrLn $ concats [--"textbook page 105 test:", show $ newton [10..13] [20,22,28,26] 12.5 ,
    "chapter 5 question 8 result:\n",concats $ newton [0.1,0.15,0.25,0.30] [0.904837,0.860708,0.778800,0.708180] 0.2,
    "\n\n",
    "chapter 5 question 17 delta value:",
    show $ delta c517f (fst c517) (snd c517)]
sigma=foldl1 (+)    
delta f xs ys=sqrt $ sigma $ fmap (\i->((f $ xs!!i)-(ys!!i))^2) [0..4]
c517f x=((3*(sqrt 2))/2+3)*(sin x)
c517=([0,pi/4,pi/2,3*pi/4,pi],[0,1.5,3,1.5,0])

chapter6=do
  mapM_ putStrLn [chapter6_6,"Question 13",show $ rberg 0 1 sqrt 0.02]
chapter6_6_function::Double->Double
chapter6_6_function (x::Double) = head $ fmap snd $ filter ((==x).fst) $ zip [1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8] [3,5,2,1,-3,-2,1,-1,2]
chapter6_6=concats ["Question 6\n",
                    "compTrape:",show $ compTrape 0.1 1.0 1.8 chapter6_6_function [1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8],"\n",
                   "compSimpson:",show $ compTrape 0.1 1.0 1.8 chapter6_6_function [1.0,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8],"\n"]
chapter6_13=rberg 0 1 (\x->4/(1+x^2)) 0.001      

--chapter 6 quiz---
finalProj=do 
  let
    fs= [[const 1,id,\x->x^2],
         [const 1,id,\x->x^2,\x->x^3],         
         [const 1,id,\x->x^2,\x->x^3,\x->x^4],
         [\x->1/x,const 1,id],
         [\x->1/x,const 1,id,\x->x^2],
         [\x->1/x,const 1,id,\x->x^2,\x->x^3],
         [\x->1/x,const 1,id,\x->x^2,\x->x^3],
         [\x->1/x,const 1,id,\x->x^2,\x->x^3,\x->x^4]]
    fexp=[const 1,id]
    

    comparef=fmap (\f->let
                          (ata,aty)=min2mult_poly years temps f
                          expanded=zipWith (++) ata $ aty
                          coeffs=solveTriangleMatrix $ gsReduce expanded
                          func x=foldl1 (+) $ zipWith (*) coeffs $ fmap (\f->f x) f
                          mse=meanSquareError temps $ fmap (\x->func x) years
                      in (mse,fmap (fromRational::Rational->Double) coeffs,func))
             fs
--  mapM_ (putStrLn.show) $  comparef
  let
    f year=(fromRational $ (view _3 (comparef!!2)) year)::Double
    temp1860=(fromRational $ (view _3 (comparef!!2)) 1880)::Double
    tempsprev=(f 1860)+(f 1870)+(fromRational $ sumlist temps)
    yearsfuture=[1990,2000..2100]
    tempsfut=scanl1 (+) $ fmap f yearsfuture
    tempsans=fmap (+tempsprev) tempsfut
  mapM_ (putStrLn.show) $ fmap (\(x1,x2,x3)->(x1,x2)) comparef
  putStrLn "------------"
  mapM_ (putStrLn.show) $  zipWith (,) tempsans $ yearsfuture

--final project
temps=[0.01,0.02,0.03,0.04,0.06,0.08,0.10,0.13,0.18,0.24,0.32]::[Rational]
years=[1880,1890..1980]::[Rational]


tst_p126xiyi=([1,2,5,10],[8,7,10,21])::([Rational],[Rational])

funcCombG=[\x->1/x,const 1]++ (fmap (\i->(\x->x^i)) [1..3])
funcCombG2=[const 1]++ (fmap (\i->(\x->x^i)) [1..3])