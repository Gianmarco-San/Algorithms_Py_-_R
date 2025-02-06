## Programming and Algorithm in Python & R

### Python Project

Guinea Baboons dataset analysis without using any library to help iterate. Program has been optimised to have the best big O notation algorithmic complexity, e.g. creating dictionaries, and still answer all the questions about the dataset, e.g. what primates couples has 28 interaction days.

<br>Big O notation: in the context of algorithmic complexity analysis, the notation O(x) represents the worst-case
time complexity of an algorithm, which is connected to efficiency and time, where x represents the size of the
input. So, in this script all calls to the main function have an efficiency of about:

<br>𝑂(𝑛 · 𝑚), where 𝑚 ≪ 𝑛 => 𝑂(𝑛 · 𝑚) ≈ 𝑂(𝑛 )

<br>Since n are the number of lines in the txt file (for cycle => O(n)) and m is number of elements in very short lists
(some calls have nested “for cycle” so O(n · m)). There are some other operations which could be summed to the
count of big-O, but they have very lower magnitude, so can be not considered, for example: append() is usually
O(1).

<br>Global time to run the whole Python script: 1.568643 seconds on a MacBook Pro Retina (13-inch, Mid 2014), 2.6GHz dual-core Intel Core i5 processor (Turbo Boost up to 3.1GHz) with 3MB shared L3 cache, 8GB of 1600MHz DDR3L onboard memory.

### R Project

Math marks EDA and visualisation on dataset composed by different schools types and divided by sex
