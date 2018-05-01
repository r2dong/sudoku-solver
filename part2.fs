let mutable Rand = System.Random ()

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
      let c' = List.sortBy (fun (i, _) -> i) c
      let f' = List.sortBy (fun (i, _) -> i) (e @ f)
      {
        clues = c'
        fills = f'
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
  (* #cells filled *)
  member b.numFilled = 
    let foldFun n (i, _) = if b.isFilled i then n + 1 else n in
    List.fold foldFun 0 b.fills
  (* test if the board is completely filled *)
  member b.isAllFilled =
    let rec f l =
      match l with
      | (_, v) :: xs -> if v = 0 then false else f xs
      | [] -> true
    in
    f b.fills
  (* test if this board is correctly solved *)
  member b.isSolved = b.isAllFilled && (b.allAttacks = 0)
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
      in
      List.fold (fun s j -> s + isAttack i j) 0 inds
  (* total number of attacks on this board *)
  member b.allAttacks = 
    let allInds = [for i in 0 .. b.size * b.size - 1 -> i] in
    List.fold (fun n i -> n + b.getCellAttacks i) 0 allInds
  (* attacks on only non-clue cells *)
  member b.allFillAttacks =
    let fillFun n i = n + b.getCellAttacks i in
    List.fold fillFun 0 b.getFillInds
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
  (* list of indices of all non-clue cells *)
  member b.getFillInds = List.fold (fun l (i, _) -> i :: l) [] b.fills
  (* fitness function *)
  member b.fitness = 
    (*
    let maxCellAttack = (b.size * 2 + (b.size' - 1) * (b.size' - 1) - 2) in
    let fr = float b.numFilled / float (b.size * b.size - b.clues.Length) in
    *)
    b.numFilled - b.allAttacks - (b.fills.Length - b.numFilled) * ((b.size' - 1) * (b.size' - 1) + 2 * (b.size - 1))
  (* 
  mutate: change a non-clue cell to any valid number
  *)
  member b.mutate =
    if b.fills.Length < 1 then
      b
    else
      let rec mutate' n b' =
        let i' = Rand.Next(0, b.fills.Length) in
        let (i, _) = List.item i' b.fills in
        let v = Rand.Next(0, b.size + 1) in
        match n with
        | 0 -> b'
        | _ -> mutate' (n - 1) (b.set i v)
      in mutate' 1 b
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

(* generate a random borad with s' and clues c *)
let randBoard s' (c: Cell list) =
  let b = SudokuBoard.construct s' c [] in
  let s = s' * s' in
  let maxIter = s * s - c.Length in
  let maxInd = s * s - 1 in
  let maxVal = s in
  (*
  let rec findInd n l l' =
    match n with
      0 -> l
    | _ -> 
      let ni = Rand.Next (0, maxInd + 1) in
        if List.contains ni l' then
          findInd (n - 1) l l'
        else
          findInd (n - 1) ((ni, Rand.Next(1, maxVal + 1)) :: l) (ni :: l')
  in
  *)
  let rec randFill inds =
    match inds with
    | [] -> []
    | x :: xs -> (x, Rand.Next (1, maxVal + 1)) :: randFill xs
  in
  SudokuBoard.construct s' c (randFill b.getFillInds)

(* genenrate n random boards *)
let rec randBoards n s' c =
  match n with
  | 0 -> []
  | _ -> randBoard s' c :: randBoards (n - 1) s' c

(* select upto size ^ 2 individuals from a population *)
let select (pop: SudokuBoard list) =
  if pop.Length < 1 then
    []
  else
    let s = List.sortByDescending (fun (b: SudokuBoard) -> b.fitness) pop in
    let rec slice l n =
      match l with
        x :: xs ->
          match n with
            0 -> []
          | _ -> x :: slice xs (n - 1)
      | [] -> []
    in
    slice s (pop.Length / 2)

(* 
mate the selected population into #pairs equal to its size
the fittest will appear in each pair
*)
let pair (l: SudokuBoard list) =
  let rec genPair n =
    let i1 = Rand.Next (0, l.Length) in
    let i2 = Rand.Next (0, l.Length) in
    match n with
    | 0 -> []
    | _ -> (List.item i1 l, List.item i2 l) :: genPair (n - 1)
  in
  genPair l.Length
  (*
  let fittest = List.maxBy (fun (b: SudokuBoard) -> b.fitness) l in
  let pair' _ =
    let ni = Rand.Next(0, l.Length) in
    let spouse = List.item ni l in
    (fittest, spouse)
  in 
  List.map pair' l
  *)


(*
takes a list of paired boards and do crossover
each cell has equal probability to inherit from both parents
each pair produces only 1 parent
*)
let rec crossOver p =
  let rec genFill (b1, b2) =
    // let f1 = List.sortBy (fun (i, _) -> i) b1.fills in
    // let f2 = List.sortBy (fun (i, _) -> i) b2.fills in
    let randChoose l a b = if Rand.Next(0, 2) = 0 then a :: l else b :: l in
    List.fold2 randChoose [] b1.fills b2.fills
  in
  let rec crossOver' p' =
    match p' with
      (b1', b2') :: xs -> 
        let nextChild = SudokuBoard.construct b1'.size' b1'.clues (genFill (b1', b2')) in 
          nextChild :: crossOver xs
    | [] -> []
  crossOver' p

(* 
reproduce once with the given population 
*)
let reproduce p =
  let pairs = pair (select p) in
  // each parent produce two children, so our specie does not go extinct
  List.concat [crossOver pairs; crossOver pairs]


(*
run genetic algorithm
l: maximum iteration limit
n: size of initial population
s': size ^ 0.5 of boards
c: clues
*)
let genetic l n s' c =
  if l < 1 then
    failwith "iteration limit must be larger than 1"
  elif n < 0 then
    failwith "initial population size must not be empty"
  else
    let p = randBoards n s' c in 
    let rec findSolution (p: SudokuBoard list) =
      match p with
      | x :: xs ->
        if x.isSolved then
          Some x
        else
          findSolution xs
      | [] -> None
    in
    let rec mutate' (p: SudokuBoard list) =
      match p with
      | x :: xs -> x.mutate :: mutate' xs
      | [] -> []
    in
    let rec repeat n p =
      let _ = printfn "%d rounds remaining" n
      let rec printBoards (boards: SudokuBoard list) =
        match boards with
        | x :: xs -> let _ = x.print in printBoards xs
        | [] -> ()
      in
      let _ = printBoards p in
      let s = findSolution p in
      match n with
        | 0 -> s
        | _ ->
          match s with
          | None -> repeat (n - 1) (reproduce (mutate' p))
          | q -> q
    in
    repeat l p


// tests

// test construct and print
let ws301 = [
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

let easy1 = [
  (0, 2);
  (3, 8);
  (4, 1);
  (5, 6);
  (6, 4);
  (10, 5);
  (14, 3);
  (18, 7);
  (20, 6);
  (21, 5);
  (22, 9);
  (24, 3);
  (29, 9);
  (30, 1);
  (31, 7);
  (33, 2);
  (37, 3);
  (38, 5);
  (42, 1);
  (43, 4);
  (47, 7);
  (49, 4);
  (50, 5);
  (51, 9);
  (56, 8);
  (58, 3);
  (59, 7);
  (60, 6);
  (62, 4);
  (66, 4);
  (70, 8);
  (74, 4);
  (75, 6);
  (76, 8);
  (77, 2);
  (80, 1);
]

let solvedRaw = [
  3; 8; 6; 7; 4; 1; 2; 5; 9;
  2; 5; 4; 3; 9; 8; 7; 1; 6;
  7; 1; 9; 6; 5; 2; 3; 8; 4;
  6; 2; 8; 5; 7; 3; 9; 4; 1;
  5; 3; 7; 4; 1; 9; 6; 2; 8;
  4; 9; 1; 8; 2; 6; 5; 7; 3;
  1; 4; 5; 9; 6; 7; 8; 3; 2;
  9; 7; 3; 2; 8; 4; 1; 6; 5;
  8; 6; 2; 1; 3; 5; 4; 9; 7;
]
let solved = [for i in 0 .. 80 -> (i, List.item i solvedRaw)]
let almostSolved = [for i in 0 .. 74 -> (i, List.item i solvedRaw)]

(*
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
let a3 = w5.allAttacks

// test randBoard
let rand1 = randBoard 3 ws301Clues
rand1.print
rand1.allCells

// test fitness function
rand1.fitness
*)

// let ans = genetic 10 10 3 ws301
let b = SudokuBoard.construct 3 solved []
b.print

//let ans = genetic 1000 10 3 easy1

// TODO: check solution before reproduce
// TODO: increase rate of mutation
// TODO: 100 roudns of iteration may still be small