
# Table of Contents

1.  [Bday](#org17407bd)
    1.  [Usage](#orgef92017)
    2.  [Installation](#org551c70a)
        1.  [Testing](#org1f4ab79)
    3.  [Author](#org8a661fc)
    4.  [Copyright](#org03d3f41)
    5.  [License](#org5a1c91c)


<a id="org17407bd"></a>

# Bday

Birthday greetings kata used for Property-Based Testing.
Guided by and based on "Property-Based Testing with PropEr, Erlang and Elixir by Fred Hebert"
Part II - Stateless Propertise in Practice
Chapter 5. Responsible Testing


<a id="orgef92017"></a>

## Usage


<a id="org551c70a"></a>

## Installation

```(ql:quickload '("cl-csv" "postmaster" "bday"))```


<a id="org1f4ab79"></a>

### Testing

Load the required packages first: 
```(ql:quickload '(:mockingbird :postmaster-mock cl-quickcheck))```
Then:
```(asdf:test-system "bday")```


<a id="org8a661fc"></a>

## Author

-   Jason S. Robinson (json.robinson@gmail.com)


<a id="org03d3f41"></a>

## Copyright

Copyright (c) 2021 Jason S. Robinson (json.robinson@gmail.com)


<a id="org5a1c91c"></a>

## License

Licensed under the BSD-3 License.

