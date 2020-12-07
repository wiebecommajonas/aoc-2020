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

The main loop is responsible for handling the argument ```data.txt``` given through the commandline. To use the function ```getArgs``` ("get commandline arguments") you will have to ```import System.Environment```

```haskell
main :: IO()
main = do
    args <- getArgs
    stringData <- readFile $ head args
    putStrLn $ "Solution Day X Part 1: " ++ show (somefunction stringData)
    putStrLn $ "Solution Day X Part 2: " ++ show (somefunction stringData)
```

## Problems solved

| Day | Problem 	| Part 1 | Part 2 |
| :-: |	:-------	| :----: | :----: |
| 1 | Report Repair	| X	 | X	  |
| 2 | Password Philosophy | X | X |
| 3 | Toboggan Trajectory | X | X |
| 4 | Passport Processing | X | X |
| 5 | Binary Boarding | X | X |
| 6 | Custom Customs | X | X |
| 7 | Handy Haversacks | X | X |
