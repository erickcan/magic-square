# magic-square
Create a magic square of order three in your terminal.

## Usage
```
$ magic-square A B C
               [--default-style | --minimal-style | --plus-style | --box-style]
```
where
- `A` is a non-negative integer;
- `B` > `A` and `B` ≠ 2 * `A`;
- `C` > `B` + `A`;
- `--default-style` splits lines with `-` and columns with `|`;
- `--minimal-style` doesn't use separators;
- `--plus-style` uses `+` as column and row separators;
- `--box-style` separates lines using `#` and columns `||`.

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
$ magic-square 1 3 5 --minimal-style
2 9 4
7 5 3
6 1 8
```
---
```
$ magic-square 1 4 7 --plus-style
 3 + 12 +  6
++++++++++++
10 +  7 +  4
++++++++++++
 8 +  2 + 11
```
---
```
$ magic-square 2 3 8 --box-style
 5 || 13 ||  6
##############
 9 ||  8 ||  7
##############
10 ||  3 || 11
```
