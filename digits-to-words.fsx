(*
Prepared by James Cooper at the University of Auckland, February 2018
Email: jcoo092@aucklanduni.ac.nz

Copyright [2018] [James Cooper]

This is based on an exercise from Chapter 8 of the book "Haskell From First Principles" by Allen and Moronuki.  It demonstrates a way to create a functional program that 
can turn a positive integer into a string of the words of its digits.  Note in particular that by using function composition and partial application, in numToWords2
we can create a neat data pipeline that doesn't even need to have an argument specified for it, but which will do exactly what we want.

It's probably best not to get stuck on figuring out the intersperse function immediately - it was written to emaulate functionality in Haskell, rather than be obvious.
Essentially, it takes a list of a given type, and inserts an element of that type between each pre-existing element in the list.
A couple of demonstrations of it are included in running this example.

There's at least one big problem with this little program though:  If you pass a negative number to numToWord, you'll get an exception.  See if you can work out how
to prevent a fatal exception from occurring in this program (there are at least three relatively simple ways, two of which involve changing the types of functions).  
Exactly what should happen when an invalid number is given to the function is up to you, but you are encouraged to take a functional approach to the challenge and
avoid using try blocks or exception handlers.  In other words, don't handle an exception after it happens;  instead, make sure it doesn't happen!
*)

let digitToWord x =
    match x with
    | 0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | _ -> failwith "Digit outside the range of 0-9 passed in"

let divMod x y = (x / y, x % y)

let dm10 x = divMod x 10

let numToDigits x = 
    let rec go y z = 
        if y = 0 then
            z
        else
            let c, d = dm10 y
            go c (d :: z)

    go x List.empty

let intersperse x y = 
    let rec go a b c = 
        match a with
        | (d :: ds) -> 
            match c with
            | true -> go a (x :: b) (not c)
            | false -> go ds (d :: b) (not c)
        | [] -> b

    go y [] false |> List.rev

let numToWords x = numToDigits x |> List.map digitToWord |> intersperse "-" |> String.concat ""

let numToWords2 = numToDigits >> List.map digitToWord >> intersperse "-" >> String.concat ""

printfn "%A" (intersperse 0 [1 .. 10])
printfn "%A" (intersperse 0 [10 .. -1 .. 1])

printfn "%s" (numToWords 1234256)
printfn "%s" (numToWords2 1234256)