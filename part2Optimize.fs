
let mutable Rand = System.Random ()

type id = int

type SudokuBoard = {
  clues: id list
  cells: int list
  fills: int list
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
  static member construct s' clue (cell: int list) =
    let s = s' * s' in
    let m = s * s in
    if cell.Length <> m then
      failwith "#cells incorrect"
    if s' < 2 then
      failwith "board size must be at least 2 ^ 2"
    elif List.fold (fun n c -> if c > m then n + 1 else n) 0 cell > 0 then
      failwith "invalid cell value"
    elif List.fold (fun n i -> if i >= m then n + 1 else n) 0 clue > 0 then
      failwith "invalid cell index"
    else
      let clues = List.sortDescending clue in
      let rec findFills i clues fIds =
        match i with
        | -1 -> fIds
        | n -> 
          match clues with
          | x :: xs ->
            if x = n then 
              findFills (i - 1) xs fIds 
            else
              findFills (i - 1) (x :: xs) (i :: fIds)
          | [] -> findFills (i - 1) [] (i :: fIds)
      in
      let fills = findFills (m - 1) clues [] in
      {
        clues = clue
        cells = cell
        fills = fills
        size = s
        size' = s'
      }
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
      List.distinct (rows @ cols @ List.concat squares)
  (* test if the board is completely filled *)
  member b.isAllFilled =
    let rec f l =
      match l with
      | x :: xs -> if x = 0 then false else f xs
      | [] -> true
    in
    f b.cells
  (* test if this board is correctly solved *)
  member b.isSolved = b.isAllFilled && (b.allAttacks = 0)
  (* total #attacks on this board *)
  member b.allAttacks =
    let rec cellAtk v kIds aIds cells atks =
      match (kIds, aIds) with
      | ([], _) -> atks
      | (k :: k', a) -> 
        if k = a then
          if v = List.head cells then
            cellAtk v k' (a - 1) (List.tail cells) (atks + 1)
          else
            cellAtk v k' (a - 1) (List.tail cells) atks
        else
          cellAtk v (k :: k') (a - 1) (List.tail cells) atks
    in
    let rec allAtk curId cells atks =
      let sortedIds = List.sortDescending (b.getInd curId)
      let curAtk = cellAtk (List.head cells) sortedIds (b.size * b.size - 1) b.cells 0 
      match curId with
      | 0 -> atks
      | n -> allAtk (n - 1) (List.tail cells) (atks + curAtk)
    in
    allAtk (b.size * b.size - 1) b.cells 0
  (* fitness function *)
  member b.fitness = 
    let numFilled = List.fold (fun n v -> if v <> 0 then n + 1 else n) 0 b.cells in
    numFilled - b.allAttacks - (b.fills.Length - numFilled) * ((b.size' - 1) * (b.size' - 1) + 2 * (b.size - 1))
  (* 
  mutate: change n non-clue cells to any valid number
  *)
  member b.mutate n =
    let rec findInds n inds =
      match n with
      | 0 -> List.sortDescending inds
      | _ -> findInds (n - 1) (Rand.Next(0, b.fills.Length) :: inds)
    in
    let rec grabInds inds aIds curInd ids =
      match inds with
      | x :: xs ->
        if x = curInd then
          grabInds xs (List.tail aIds) (curInd - 1) ((List.head aIds) :: ids)
        else
          grabInds (x :: xs) (List.tail aIds) (curInd - 1) ids
      | [] -> List.sortDescending ids
    let rec genBoard old clues ids curId newB =
      match ids with
      | x :: xs ->
        if x = curId then
          genBoard (List.tail old) clues xs (curId - 1) (Rand.Next(0, b.size + 1) :: newB)
        else
          genBoard (List. tail old) clues (x :: xs) (curId - 1) ((List.head old) :: newB)
      | [] -> newB
    in
    let inds = findInds n [] in
    let ids = grabInds inds b.fills (b.fills.Length - 1) [] in
    let newCells = genBoard b.cells b.clues ids (b.size * b.size - 1) [] in
    SudokuBoard.construct b.size' b.clues newCells
  (*
  print board to console
  // TODO print borders dividing squares
  *)
  member b.print = 
    let rec printHelp ac i =
      match ac with
        v :: vs -> 
          if i % b.size = 0 then
            let s = String.concat "" ["\n"; (string v); " "] in
            let _ = printf "%s" s in printHelp vs (i - 1)
          else
            let s = String.concat "" [string v; " "] in
            let _ = printf "%s" s in printHelp vs (i - 1)
      | [] -> printf "\n"
    in
    printHelp b.cells (b.size * b.size - 1)

(* generate a random borad with s' and clues c *)
let randBoard s' clue =
  let s = s' * s' in
  let maxId = s * s - 1 in
  let maxVal = s in
  let clue = List.sortByDescending (fun (i, _) -> i) clue in
  let rec randFill inds clue cells =
    match inds with
    | 0 -> cells
    | n -> 
      match List.head clue with (i, v) ->
        if i = inds then 
          randFill (inds - 1) (List.tail clue) (v :: cells)
        else
          let v = Rand.Next(0, maxVal + 1) in
          randFill (inds - 1) (List.tail clue) (v :: cells)
  in
  SudokuBoard.construct s' clue (randFill b.getFillInds)

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