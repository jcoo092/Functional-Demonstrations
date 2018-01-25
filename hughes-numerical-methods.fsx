(*
Prepared by James Cooper at the University of Auckland during October 2017, as a supplement for UoA's Computer Science 335 paper
Email: jcoo092@aucklanduni.ac.nz

Copyright [2017] [James Cooper]

Transcribed from John Hughes' 1989 paper "Why Functional Programming Matters" (see https://doi.org/10.1093/comjnl/32.2.98).  
This code is intended as a supplement to demonstrate how to work with laziness in F#, 
and should be examined in conjunction with reading Hughes' paper, which explains in much more detail what is happening.
See also https://www.youtube.com/watch?v=vGVJYoKIzjU where Hughes re-summarises the paper in a modern retrospective.

The below functions demonstrate the use of lazy evaluation and higher-order functions to efficiently perform numerical computations.
Specifically, the Newton-Raphson method for calculating estimates of square roots, and a tradiitional method for estimation of numerical differentiation.

In the past I had to implement a form of differentiation estimation with numerical optimisation in C++.
It did more-or-less the same thing as below, but took many more lines of code, and wasn't as effective, efficient or readable.
*)

// Function to calculate the next estimate of the square root, based on the current estimate
// This is the basis of the Newton-Raphson method
let next n x = (x + n/x)/2.0

// Function to create a stream of successive evaluations of an input function
// Using the Seq type here means that the stream is lazily evaluated
let rec repeat f a = seq {yield a; yield! repeat f (f a)}

// Alternative way to implement the repeat function in F#
let rec repeat2 f a = Seq.unfold (fun state -> Some(state, f state)) a

// Function to find the value in a sequence where the difference between that value and the previous value is smaller than a given tolerance
let rec within eps values = 
    let a = Seq.head values
    let b = Seq.head (Seq.tail values)
    if abs(a - b) <= eps then
        b
    else
        Seq.tail values |> within eps
        // Note that the above line is tail-recursive

// Function to calculate an estimate of the square root of a number, built out of other functions
let sqrt a0 eps n = repeat (next n) a0 |> within eps

// Alternative implementation of the sqrt function, this version does not use the pipe operator
let sqrt2 a0 eps n = within eps (repeat (next n) a0)

printfn "\nTesting square root"
printfn "%A" <| sqrt 1.0 0.0001 2.0 // Should print ~1.4
printfn "%A" <| sqrt2 1.0 0.0001 9.0 // Should print 3.0
printfn "%A" <| sqrt 1.0 0.0001 10.0 // Should print ~3.16

// An alternative to the 'within' function, using the size of relative change between two values in the sequence to determine when to stop
let rec relative eps values =
    let a = Seq.head values
    let b = Seq.head <| Seq.tail values
    if abs((a / b) - 1.0) <= eps then b
    else Seq.tail values |> relative eps

let relativesqrt a0 eps n = relative eps <| repeat (next n) a0

printfn "\nTesting relative square root"
printfn "%A" <| relativesqrt 1.0 0.0001 2.0 // Should print ~1.4
printfn "%A" <| relativesqrt 1.0 0.0001 9.0 // Should print 3.0
printfn "%A" <| relativesqrt 1.0 0.0001 10.0 // Should print ~3.16

// Function to estimate the first derivative of a given single-variable numerical function, at any given point for which the function is defined.
// Note that h should very small here (but be careful of precision issues when working with floating point numbers)
let easydiff f x h = ((f (x+h)) - f x) / h

// Helper function used in the succession of approximations
let halve x = x / 2.0

// Helper function that returns a sequence of better approximations of the differential
let differentiate h0 f x = repeat2 halve h0 |> Seq.map (easydiff f x) 

// Final function that determines the output differential.  It takes the initial estimate, the function to differentiate, and 
// the input to that function at which point the differential is to be determined, and returns a final estimate.abs
// Note the re-use of the within function from before, without any modifications.
let deriveF eps h0 f x = differentiate h0 f x |> within eps

// Simple function with which to test our differentiator
let square x = x * x

// A different test function
let testFunc x = (square x) + (3.0 * x) + 1.0

printfn "\nTesting deriveF"
printfn "%A" <| deriveF 0.0001 5.0 square 3.0 // Should print 6.0
printfn "%A" <| deriveF 0.0001 10.0 square 2.0 // Should print 4.0
printfn "%A" <| deriveF 0.0001 10.0 testFunc 3.0 // Should print 9.0
printfn "%A" <| deriveF 0.0001 10.0 testFunc 2.0 // Should print 7.0

(*
If your evaluations are anything like mine, there will be some imprecision on the printed values.  Turns out Hughes knew about that,
and, with the aid of functional programming techniques, could do something about it
*)

// A function used to help eliminate the error term that is a part of the differentiation estimation used above
// This function is used in between the differentiate function which generates the normal estimate sequence, and the
// within or relative functions, which check if there is a sufficiently small change between estimates
// Note that this function is specific to the differentiation technique used
let rec elimerror n values = 
    let a = Seq.head values
    let b = Seq.head (Seq.tail values)
    seq {yield ((b * System.Math.Pow(2.0, n) - a) / (System.Math.Pow(2.0, n) - 1.0)); yield! elimerror n (Seq.tail values)}

// Alternative implementation of elimerror
let rec elimerror' n values = 
    Seq.unfold (fun state -> 
        let a = Seq.item 0 state // Equivalent to 'Seq.head state'
        let b = Seq.item 1 state
        Some((b * (System.Math.Pow(2.0, n)) - a) / ((System.Math.Pow(2.0, n)) - 1.0), Seq.tail state)
    ) values

// Reimplementation of deriveF with the elimerror function interposed
let deriveF' eps h0 f x = differentiate h0 f x |> elimerror' 1.0 |> within eps

printfn "\nTesting deriveF'"
printfn "%A" <| deriveF' 0.0001 5.0 square 3.0 // Should print 6.0
printfn "%A" <| deriveF' 0.0001 10.0 square 2.0 // Should print 4.0
printfn "%A" <| deriveF' 0.0001 10.0 testFunc 3.0 // Should print 9.0
printfn "%A" <| deriveF' 0.0001 10.0 testFunc 2.0 // Should print 7.0

//We can take this further.  The below should be quite good for estimations of differentiation in most situations
let deriveF3 eps h0 f x = within eps (elimerror' 3.0 (elimerror' 2.0 (elimerror' 1.0 (differentiate h0 f x))))

// Gives us a sequence of sequences with more and more elimerror functions applied
let rec elimall n s = seq {yield s; yield! elimall (n + 1.0) (elimerror' n s)}

// Takes the first term from the above sequence of sequences, as required
// In other words, it gets more and more precise values as we go on
let super s = Seq.map Seq.head (elimall 1.0 s)

// Our best-yet derivation function, which should be able to handle just about every challenge we throw at it
// (so long as a derivative is mathematically defined for the given input to the derived function)
let ultimateDeriveF eps h0 f x = differentiate h0 f x |> super |> within eps

printfn "\nTesting ultimate deriveF'"
printfn "%A" <| ultimateDeriveF 0.0001 5.0 square 3.0 // Should print 6.0
printfn "%A" <| ultimateDeriveF 0.0001 10.0 square 2.0 // Should print 4.0
printfn "%A" <| ultimateDeriveF 0.0001 10.0 testFunc 3.0 // Should print 9.0
printfn "%A" <| ultimateDeriveF 0.0001 10.0 testFunc 2.0 // Should print 7.0

(*
Lazy evaluation has been used here to enable us to work with potentially infinite sequences, where the next value in the sequence is
more useful to us than the last.  We compute the sequence only as far as we need to for our purposes, and then stop there.
The advantages is that we compute exactly as many elements in the sequence as we require, without having to guess at any point how many that might be.

Note that we never once had to make a variable mutable, and nor did we even once use a for or while loop.

Why not try the ultimateDeriveF function out for yourself?
There are other numerical techniques in the paper to look at too.
*)
