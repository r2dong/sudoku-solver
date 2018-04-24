type Cell = int * int

type SudokuBoard = {
  fills: Cell list
  clues: Cell list
  size: int
}
with
  member b.getInd i =
    if i >= b.size * b.size || i < 0
      then failwith "invalid index"
    else
      let row = i / b.size in
      let col = i % b.size in
      let origin = row * b.size in
      let rows = [origin .. (origin + b.size - 1)] in
      let cols = [for j in 0 .. 8 -> col + j * b.size]
      List.concat [rows; cols]

// test getInd
let tb = {
  fills = [];
  size = 9;
  clues = [];
}
let r = tb.getInd 20