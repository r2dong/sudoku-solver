type Cell = int * int

type SudokuBoard = {
  fills: Cell list
  clues: Cell list
  size: int
  size': int
}
with
  (* 
    construct a new sudoku board 
    s': size ^ 0.5 of the board, must be integer
    c: clues
    f: cells already filled
  *)
  static member construct s' c f =
    let s = s' * s' in
    let m = s * s in
    if s' < 2 then
      failwith "board size must be at least 2 ^ 2"
    elif List.fold (fun n (_, v) -> if v > s then n + 1 else n) 0 c > 0 then
      failwith "invalid cell value"
    elif List.fold (fun n (i, _) -> if i >= m then n + 1 else n) 0 c > 0 then
      failwith "invalid cell index"
    else
      let ac = List.concat [c; f] in
      let ne = List.fold (fun l (ind, _) -> ind :: l) [] ac in
      let foldFun l i' = if List.contains i' ne then l else (i', 0) :: l in
      let e = List.fold foldFun [] [for i in 0 .. m - 1 -> i]
      {
        clues = c
        fills = List.concat [e; f]
        size = s
        size' = s'
      }
  (* all cells in sorted order *)
  member b.allCells = 
    let ac = List.concat [b.clues; b.fills] in 
    List.sortBy (fun (ind, _) -> ind) ac
  (* indicies of all cells that may attack that with index i *)
  member b.getInd i =
    if i >= b.size * b.size || i < 0
      then failwith "invalid index"
    else
      let row = i / b.size in
      let col = i % b.size in
      let origin = row * b.size in
      let rows = [origin .. (origin + b.size - 1)] in
      let cols = [for j in 0 .. b.size - 1 -> col + j * b.size]
      let row' = row / b.size' in
      let col' = col / b.size' in
      let upperLeft = row' * b.size' * b.size + col' * b.size' in
      let squares = [for i in upperLeft .. upperLeft + b.size' - 1 -> 
                      [for j in 0 .. b.size' - 1 -> i + j * b.size]
                    ]
      List.distinct (List.concat [rows; cols; List.concat squares])
  (* check if cell with index i is empty *)
  member b.isFilled i = match b.getCell i with (_, v) -> v <> 0
  (* retrieve a particular cell with index i *)
  member b.getCell i = 
    let l = List.filter (fun (i', _) -> i = i') b.allCells in
    if l.Length <> 1 then
      failwith "cells have duplicate index!"
    else
      List.item 0 l
  (* calculate attacks on cell with index i *)
  member b.getCellAttacks i =
    match b.isFilled i with
      false -> 0
    | _ ->
      let inds' = b.getInd i in
      let inds = List.filter (fun num -> num <> i) inds' in
      let isAttack i j =
        let (_, v1) = b.getCell i in
        let (_, v2) = b.getCell j in
        if v1 = 0 || v2 = 0 then 0 else (if v1 = v2 then 1 else 0)
      List.fold (fun s j -> s + isAttack i j) 0 inds
  (* total number of attacks on this board *)
  member b.getAttacks = 
    let allInds = [for i in 0 .. b.size * b.size - 1 -> i] in
    List.fold (fun n i -> n + b.getCellAttacks i) 0 allInds
  (* set value of cell with index i to v *)
  member b.set i v =
    if i >= b.size * b.size then
      failwith "invalid index"
    elif v > b.size then
      failwith "invalid value"
    elif List.contains i b.getClueInds then
      failwith "cannot modify a clue cell"
    else
      let fFun l (i', v') = if i' = i then (i', v) :: l else (i', v') :: l in
      let nf = List.fold fFun [] b.fills in
      SudokuBoard.construct b.size' b.clues nf
  (* list of indices of all clues *)
  member b.getClueInds = List.fold (fun l (i, _) -> i :: l) [] b.clues
  (* fitness function *)
  (*
  print board to console
  // TODO print borders dividing squares
  *)
  member b.print = 
    let rec printHelp ac =
      match ac with
        (i, v) :: res -> 
          if i % b.size = 0 then
            let s = String.concat "" ["\n"; (string v); " "] in
            let _ = printf "%s" s in printHelp res
          else
            let s = String.concat "" [string v; " "] in
            let _ = printf "%s" s in printHelp res
      | [] -> printf "\n"
    in
    printHelp b.allCells

let mutable Rand = System.Random ()

(*
generate a random borad with s' and clues c, other cells are filled randomly
*)
let randBoard s' (c: Cell list) =
  let b = SudokuBoard.construct s' c [] in
  let s = s' * s' in
  let maxIter = s * s - c.Length in
  let maxInd = s * s - 1 in
  let maxVal = s in
  let rec findInd n l l' =
    match n with
      0 -> l
    | _ -> 
      let ni = Rand.Next (0, maxInd + 1) in
        if List.contains ni l' then
          findInd (n - 1) l l'
        else
          findInd (n - 1) ((ni, Rand.Next(0, maxVal + 1)) :: l) (ni :: l')
  in
  SudokuBoard.construct s' c (findInd maxIter [] b.getClueInds)
    
// tests
(*

// test all Cells
let tb2 = {
  fills = [(0, 1); (2, 2); (3, 1)]
  clues = [(1, 2)]
  size = 2
}
printf "%d\n" tb2.allCells.Length
*)

// test construct and print
let ws301Clues = [
  (1, 3);
  (2, 9);
  (3, 5);
  (12, 8);
  (16, 7);
  (22, 1);
  (24, 9);
  (26, 4);
  (27, 1);
  (30, 4);
  (35, 3);
  (47, 7);
  (51, 8);
  (52, 6);
  (56, 6);
  (57, 7);
  (59, 8);
  (60, 2);
  (64, 1);
  (67, 9);
  (71, 5);
  (77, 1);
  (80, 8);
]
let ws301 = SudokuBoard.construct 3 ws301Clues []
ws301.print

let ws301' = SudokuBoard.construct 3 ws301Clues [(18, 1); (54, 8); (79, 4); (53, 9)]
ws301'.print

// test getInd
let r = ws301.getInd 54
let r3 = ws301.getInd 70

// test allCells
let r1 = ws301.allCells

// test setCell
let w1 = ws301.set 0 1
w1.print
// let w2 = w1.set 80 1 // should fail
// let w3 = w1.set 79 10 // should fail
let w4 = w1.set 79 5
w4.print

// test isFilled
let b1 = w4.isFilled 59 // true
let b2 = w4.isFilled 80 // true
let b3 = w4.isFilled 79 // true
let b4 = w4.isFilled 78 // false

// test getCell
let g1 = w4.getCell 64 // 1
let g2 = w4.getCell 27 // 1
let g3 = w4.getCell 0 // 1
let g4 = w4.getCell 79 // 5

// test getCellAttacks
let a1 = w4.getCellAttacks 0 // 1
let w5 = w4.set 76 1
let a2 = w5.getCellAttacks 76 // 2

// test getAttacks
let a3 = w5.getAttacks

// test randBoard
let rand1 = randBoard 3 ws301Clues
rand1.print
rand1.allCells