
# Table of Contents

1.  [Bday](#orgd972bc9)
    1.  [Usage](#org8c88082)
    2.  [Installation](#org6996a12)
        1.  [Testing](#orgb480f83)
    3.  [Author](#org11de96e)
    4.  [Copyright](#orge42fbd1)
    5.  [License](#org344c9fb)


<a id="orgd972bc9"></a>

# Bday

** ADVISORY

-   This is for MY personal education only in my attempt to learn Property-Based Testing
    from a highly recommended book.
-   While I am using code and techniques from the book, It is NOT my intention
    to turn this into a direct port, I am also learning Common Lisp and how to do PBT in CL.

Birthday greetings kata used for Property-Based Testing.
Guided by and based on "Property-Based Testing with PropEr, Erlang and Elixir by Fred Hebert"
Part II - Stateless Propertise in Practice
Chapter 5. Responsible Testing


<a id="org8c88082"></a>

## Usage


<a id="org6996a12"></a>

## Installation

```(ql:quickload '("cl-csv" "postmaster" "bday"))```


<a id="orgb480f83"></a>

### Testing

Load the required packages first: 
```(ql:quickload '(:mockingbird :postmaster-mock cl-quickcheck))```
Then:
```(asdf:test-system "bday")```


<a id="org11de96e"></a>

## Author

-   Jason S. Robinson (json.robinson@gmail.com)


<a id="orge42fbd1"></a>

## Copyright

The original Erlang / Elixer code and concepts are from Frank Hebert, this is just my
attempt at applying them in Common Lisp to have a better grasp on the concept.


<a id="org344c9fb"></a>

## License

