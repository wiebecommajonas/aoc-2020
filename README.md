# Janky-as-hell solutions for the [Advent of Code](https://adventofcode.com/2020) event in Haskell

                      .!,            .!,
                     ~ 6 ~          ~ 6 ~
                .    ' i `  .-^-.   ' i `
              _.|,_   | |  / .-. \   | |
               '|`   .|_|.| (-` ) | .|_|.
               / \ ___)_(_|__`-'__|__)_(______
              /`,o\)_______________________o_(
             /_* ~_\[___]___[___]___[___[_[\`-.
             / o .'\[_]___[___]___[___]_[___)`-)
            /_,~' *_\_]                 [_[(  (
            /`. *  *\_]                 [___\ _\
           /   `~. o \]      ;( ( ;     [_[_]`-'
          /_ *    `~,_\    (( )( ;(;    [___]
          /   o  *  ~'\   /\ /\ /\ /\   [_[_]
         / *    .~~'  o\  ||_||_||_||   [___]
        /_,.~~'`    *  _\_||_||_||_||___[_[_]_
        /`~..  o        \:::::::::::::::::::::\
       / *   `'~..   *   \:::::::::::::::::::::\
      /_     o    ``~~.,,_\=========\_/========='
      /  *      *     ..~'\         _|_ .-_--.
     /*    o   _..~~`'*   o\           ( (_)  )
     `-.__.~'`'   *   ___.-'            `----'
           ":-------:"
             \_____/

To run the code type

```sh
λ> git clone https://github.com/wiebecommajonas/aoc-2020.git
λ> cd aoc-2020
λ> cd Day\ X/
λ> runhaskell dayX.hs data.txt
```
into your commandline. ```X```needs to be replaced by the number of the day.

## Main loop

The main loop is responsible for handling the argument ```data.txt``` given through the commandline and outputting the results. To use the function ```getArgs``` ("get commandline arguments") you will have to ```import System.Environment```.

```haskell
main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day X Part 1: " ++ show (somefunction stringData)
    putStrLn $ "Solution Day X Part 2: " ++ show (somefunction stringData)
```

## Problems solved

| Day | Problem | Part 1 | Part 2 |
| :-: |	:------- | :----: | :----: |
| [1](01/) | Report Repair | ☆ | ☆ |
| [2](02/) | Password Philosophy | ☆ | ☆ |
| [3](03/) | Toboggan Trajectory | ☆ | ☆ |
| [4](04/) | Passport Processing | ☆ | ☆ |
| [5](05/) | Binary Boarding | ☆ | ☆ |
| [6](06/) | Custom Customs | ☆ | ☆ |
| [7](07/) | Handy Haversacks | ☆ | ☆ |
| [8](08/) | Handheld Halting | ☆ | ☆ |
| [9](09/) | Encoding Error | ☆ | ☆ |
| [10](10/) | Adapter Array | ☆ | ☆ | <- would not try to run that again
| [11](11/) | Seating System | ☆ | ☆ |
| [12](12/) | Rain Risk | ☆ | ☆ |
| [13](13/) | Shuttle Search | ☆ | ☆ |
| [14](14/) | Docking Data | ☆ | ☆ |
| [15](15/) | Rambunctious Recitation | ☆ | ☆ |
<!--| [16](16/) |  |  |  |
| [17](17/) |  |  |  |
| [18](18/) |  |  |  |
| [19](19/) |  |  |  |
| [20](20/) |  |  |  |
| [21](21/) |  |  |  |
| [22](22/) |  |  |  |
| [23](23/) |  |  |  |
| [24](24/) |  |  |  |
| [25](25/) |  |  |  |
-->
