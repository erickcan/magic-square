# magic-square
Create a magic square of order three in your terminal.

## Usage
```
$ magic-square A B C [--style STYLE]
```
where
- `A` is a positive integer: `A > 0`;
- `B` is an integer greater than `A` and not equal to the double of `A`: `B > A && B /= 2 * A`;
- `C` is an integer greater than `B` plus `A`: `C > B + A`;
- `STYLE` prints the magic square with the chosen style (`--styles-list` shows available styles).

### Examples
```
$ magic-square 1 5 9
 4 | 15 |  8
------------
13 |  9 |  5
------------
10 |  3 | 14
```
---
```
$ magic-square 1 3 5 -sMinimal
2 9 4
7 5 3
6 1 8
```
---
```
$ magic-square 1 4 7 -sPlus
 3 + 12 +  6
++++++++++++
10 +  7 +  4
++++++++++++
 8 +  2 + 11
```
---
```
$ magic-square 2 3 8 -sBox
 5 || 13 ||  6
##############
 9 ||  8 ||  7
##############
10 ||  3 || 11
```
---
```
$ magic-square 1 6 9 -sArray
[[ 3, 16,  8],
 [14,  9,  4],
 [10,  2, 15]]
```
