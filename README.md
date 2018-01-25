# Functional-Demonstrations
Small demonstrative code files for functional programming education purposes

Currently this repository just hosts an F# script file that I wrote for education purposes.  If/when I write further ones, I will post them here too.

The script is an implementation of some of the material from Hughes' 1987 paper 'Why functional programming matters' ([available online](https://doi.org/10.1093/comjnl/32.2.98)), specifically material relating to lazy evaluation and its uses.  Numerical methods relying on laziness for finding a square root, and estimating the derivative of a function at a given point, are shown and explained.  This is targeted primarily at undergraduate level students who have just been introduced to functional programming, but should be suitable for anyone who has learnt the basics of programming.

To run the script, simply open a terminal window, navigate to the folder you have the script contained in, and run the following command (assuming that you have F# installed and the Interactive executable on your system's path):

```
fsi hughes-numerical-methods.fsx
```

Alternatively, if you are using Visual Studio 2015 or later, or Visual Studio Code with the Ionide plugin, you should be able to use the facilities in the editor to send the contents of the file to F# Interactive.
