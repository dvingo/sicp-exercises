This interchange makes the program run slowly due to 
a build up an exponential number of recursive calls of queen-cols.
This happens because every past board position is considered <board-size> times
Each member in <rest-of-queens> results in <board-size> calls to queen-cols.
This describes a tree-recursive process, whereas the first version in the text
described a linear-recursive process. In the slow version, each row from
(enumberate-interal 1 board-size) generates <board-size> calls to queen-cols.
In the first example, the enumerate-interval call happens for each element produced by
queen-cols, which grows linearly in size, not exponentially.

For every row in the kth column the procedure evaluates every possible solution for
the previous k-1 columns.

Because a tree-recursive process grows exponentially, we can estimate the running time
of the new process to be T^board-size

Instead of generating a pair of the current column, k, with the current row, new-row, 
the inefficient procedure generates all previous board positions (using queen-cols) for
every row, and does this for every column, needlessly computing the same data over and over.
The running time in terms of T for the inefficient procedure will be T^n where n is the board size.
To reason about this, it is as if the inefficient procedure is performing the work of T to the power of n times.
