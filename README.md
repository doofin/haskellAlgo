# haskellAlgo
my repo for some algorithm implementations ,written while in college(2014) so that it might be outdated and it is for reference only.

# Numerical methods (numericalMethods.hs)
numerical Methods for function integration,solving linear equations,interpolation(approximate a function from its data)

simpson method for numerical integration

Romberg method for numerical integration

gauss elimination method for solving linear equations

Newton interpolation 

Jacobi method : an iterative algorithm,for solving a linear system of equations,like Ax=b 

The method of successive over-relaxation (SOR),for solving a linear system of equations,like Ax=b 

# some solution for haskell-99 problems(algo.hs)
(https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)
## lists:

Problem 20 (*) Remove the K'th element from a list.     

##set algo:
combination :: Int -> [a] -> [([a],[a])] : 

number theory

Problem 31 (**) Determine whether a given integer number is prime. 

Problem 39 (*) A list of prime numbers. Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range. 

Problem 40 (**) Goldbach's conjecture.  


## trees
Problem 56 (**) Symmetric binary trees 

Problem 55 (**) Construct completely balanced binary trees 

Problem 59 (**) Construct height-balanced binary trees     

Problem 61A Collect the leaves of a binary tree in a list 

Problem 64 :  As a preparation for drawing the tree, a layout algorithm is required to determine the position of each node in a rectangular grid

## graphs:
Problem 81 (**) Path from one node to another one(paths :: Eq a =>a -> a -> [(a,a)] -> [[a]] : find paths between two node in graph)

Problem 82 (*) Cycle from a given node (gcycle::Eq a=>a->[(a,a)]->[[a]] : find circles in graph)

Problem 85 (**) Graph isomorphism (isogrh::Eq a=>[(a,[a])]->[(a,[a])]->Bool : check whether two graphs are isomorphic)

reachable::(Eq a)=>[(a,[a])]->[a]->[a] : reachability from a list of nodes
