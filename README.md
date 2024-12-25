# Advent of Code 2024

My solutions to the 2024 [Advent of Code](https://adventofcode.com/) challenges. To run the solution for (e.g.) day 5 part 2:

```haskell
import AoC2024.Solutions.Day5 (parse, part2)
part2 . parse <$> readFile "input/day5.txt"
# 4077
```

The total run time of all 49 solutions is 25 to 30 seconds on my computer (and most of this is taken up by day 18 part 2).