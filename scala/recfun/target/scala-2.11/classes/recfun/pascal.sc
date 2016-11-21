def pascal(c: Int, r: Int): Int =
{
  if (0 == c && r == 0) 1 else
  if (r < 0) 0 else
  if (c < 0 || c > r+1) 0 else
  pascal(c-1, r-1) + pascal(c, r-1);
}

pascal(0,2) == 1
pascal(1,2) == 2
pascal(1,3) == 3

println("Pascal's Triangle")
for (row <- 0 to 10) {
  for (col <- 0 to row)
    print(pascal(col, row) + " ")
  println()
}

pascal(1,3)

