let mutable Rand = System.Random ()

let rec printList l =
  match l with
  | x :: xs -> let _ = printf "%d, " x in printList xs
  | [] -> let _ = printf "\n" in ()

let rec printArray (arr: int array) len = 
  match len with
  | -1 -> let _ = printf "\n" in ()
  | _ -> let _ = printf "%d, " arr.[len] in printArray arr (len - 1)
let duplicates l = List.length l - List.length (List.distinct l)
let getSqrInds sqrInd =
    let upperLeft = sqrInd / 3 * 27 + sqrInd % 3 * 3 in
    List.concat [
      for i in upperLeft .. upperLeft + 2 -> 
        [for k in 0 .. 2 -> i + k * 9]
    ]

(* take a pair of linear indices and check if corresponding elements are in
   the same sub squares *)
let isSameSquare i j = 
  let row = i / 9 in
  let col = i % 9 in
  let sqrInd = row / 3 * 3 + col / 3 in
  List.contains j (getSqrInds sqrInd)

(* return fitness of a row, column, or small square *)
let rec listFit l = 
  let all = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let c = Set.difference (Set.ofList all) (Set.ofList l) in
  Set.count c

type SudokuBoard = {
    clues: int list
    cells: int array
    mutable age: int
}
with
  (* methods and helpers to calculate the fitness *)
  member private b.getRow rowNum =
    let finish = rowNum * 9 + 9 in
    let rec getRowHelper cur =
      match finish - cur with
      | 0 -> []
      | _ -> b.cells.[cur] :: getRowHelper (cur + 1)
    in
    getRowHelper (rowNum * 9)
  member private b.getCol colNum =
    let finish = colNum + 9 * 8 in
    let rec getColHelper cur =
      if cur = finish then
        [b.cells.[cur]]
      else
        b.cells.[cur] :: getColHelper (cur + 9)
    in
    getColHelper colNum
  member b.getSqr squareNum =
    let start = (squareNum / 3) * 27 + (squareNum % 3) * 3 in
    let rec getSquareHelp r c = 
      match (r, c) with
      | (3, _) -> []
      | (_, 3) ->  getSquareHelp (r + 1) 0
      | _ -> b.cells.[start + r * 9 + c] :: getSquareHelp r (c + 1)
    in
    getSquareHelp 0 0 
  member b.fit =
    let rec fitHelp count =
      match count with
      | -1 -> 0
      | _ -> 
        let row = b.getRow count in
        let col = b.getCol count in
        let sqr = b.getSqr count in
        listFit row + listFit col + listFit sqr + fitHelp (count - 1)
    in
    b.age + fitHelp 8
  (* mutation modifies the original board, does not return a new one *)
  member b.mutate =
    let rec mutateHelp count =
      //let _ = printfn "%d mutations remaining" count
      match count with
      | 0 -> ()
      | _ ->
        let p1 = Rand.Next (0, 81) in
        let p2 = Rand.Next (0, 81) in
        let b1 = List.contains p1 b.clues in
        let b2 = List.contains p2 b.clues in
        let b3 = isSameSquare p1 p2 in
        if b1 || b2 || not b3 then
          //let _ = printfn "p1: %d, p2: %d" p1 p2 in
          //let _ = printfn "is p1 clue: %b, is p2 clues: %b" b1 b2 in
          //let _ = printfn "in same sub square? %b" b3 in
          mutateHelp (count - 1)
        else
          let v1 = b.cells.[p1] in
          let v2 = b.cells.[p2] in
          let _ = b.cells.[p1] <- v2 in
          let _ = b.cells.[p2] <- v1 in
          //let _ = printf "mutating cells at index %d and %d, with values %d and %d\n" p1 p2 v1 v2 in 
          (* enforce rows and columns involved after swap has < 3 duplicates *)
          let d1 = duplicates (b.getRow (p1 / 9)) in
          let d2 = duplicates (b.getCol (p1 % 9)) in
          let d3 = duplicates (b.getRow (p2 / 9)) in
          let d4 = duplicates (b.getCol (p2 % 9)) in
          if d1 > 2 || d2 > 2 || d3 > 2 || d4 > 2 then
            //let _ = printfn "aborting due to slack"
            let _ = b.cells.[p1] <- v1 in
            let _ = b.cells.[p2] <- v2 in
            mutateHelp (count - 1)
          else
            //let _ = printf "successful mutate" in
            mutateHelp (count - 1)
    in mutateHelp 5
  member b.print = 
    let rec printHelp n =
      match n with
      | 81 -> ()
      | _ -> 
        if n % 9 = 0 && n <> 0 then
          let _ = printf "\n" in
          let _ = printf "%d " b.cells.[n] in
          printHelp (n + 1)
        else
          let _ = printf "%d " b.cells.[n] in
          printHelp (n + 1)
    in
    let _ = printHelp 0 in
    printfn ""

(* select individuals for mating *)
let select p =
  //let p = Array.sortBy (fun (b: SudokuBoard) -> b.fit) p
  let start = Array.length p - 1 in
  let rec selectHelp n =
    match n with
    | 0 -> []
    | _ -> 
      let i1 = Rand.Next (0, n) in
      let i2 = Rand.Next (0, n) in
      (p.[i1], p.[i2]) :: selectHelp (n - 1)
  in
  selectHelp start

(* crossOver done uniformly *)
(* helper method that crosses over a single pair of parents *)
let  crossOverAndMutate p1 p2 =
  let childCells = Array.create 81 0 in
  let rec copyCell inds parent =
    match inds with
    | x :: xs -> let _ = childCells.[x] <- parent.cells.[x] in copyCell xs parent
    | [] -> ()
  in
  let rec help sqrInd =
    match sqrInd with
    | -1 -> ()
    | _ -> 
      let inds = getSqrInds sqrInd in
      let n = Rand.Next (0, 2) in
      //let _ = printList inds in 
      match n with
      | 0 -> let _ = copyCell inds p1 in help (sqrInd - 1) 
      | _ -> let _ = copyCell inds p2 in help (sqrInd - 1)
  in
  let _ = help 8 in
  let b = {clues = p1.clues; cells = childCells; age = 0} in
  let _ = b.mutate in
  //let _ = b.print in
  //let _ = printfn "" in
  b

(* crossOver main method *)
let crossOver p = 
  let rec help p =
    match p with
    | (p1, p2) :: xs -> crossOverAndMutate p1 p2 :: help xs
    | [] -> []
  in
  Array.ofList (help p)

(* extract list of indicies from input clues *)
let rec getClueInds clues =
  match clues with
  | (i, _) :: xs -> i :: getClueInds xs
  | [] -> [] 

(* get numbers that are already in a subsquare 
   list containing values of the small square -> list of values not there yet*)
let rec getNotContained l =
  let allNums = [1; 2; 3; 4; 5; 6; 7; 8; 9] in
  List.filter (fun a -> not (Set.contains a l)) allNums

(* generate a single random full board, ensure on duplicaetes in sub squares *)
let genBoard clues =
  let b = { clues = getClueInds clues; cells = Array.create 81 0; age = 0 } in
  let rec writeClues clues = (* write all clues *)
    match clues with
    | (ind, value) :: xs -> let _ = b.cells.[ind] <- value in writeClues xs
    | [] -> ()
  in
  let _ = writeClues clues in
  let rec fillSqrHelp inds (pool: int list) = (* fill one small square *) // TODO use arrays for pool, little bit faster
    match inds with
    | x :: xs ->
      if b.cells.[x] = 0 then
        let randVal = Rand.Next (0, List.length pool) in
        let value = pool.[randVal] in
        let rest = List.filter (fun a -> a <> value) pool in
          let _ = b.cells.[x] <- value in
          fillSqrHelp xs rest
      else
        fillSqrHelp xs pool
    | [] -> ()
  in
  let rec fillSquare sqrInd = (* fill all small squares *)
    match sqrInd with
    | -1 -> ()
    | _ ->
      let contained = Set.ofList (b.getSqr sqrInd) in
      let _ = fillSqrHelp (getSqrInds sqrInd) (getNotContained contained) in
      fillSquare (sqrInd - 1)
  in
  let _ = fillSquare 8 in
  b

(* generate n random boards *)
let genBoards n clues =
  let rec help n =
    match n with
    | 0 -> []
    | _ -> (genBoard clues) :: help (n - 1)
  in
  Array.ofList (help n)

let restart = 2000

let checkSolution (pop: SudokuBoard []) =
  let rec help n =
    match n with
    | 0 -> if pop.[n].fit = 0 then Some pop.[n] else None
    | _ -> if pop.[n].fit = 0 then Some pop.[n] else help (n - 1)
  in
  help ((Array.length pop) - 1)

let mutable totalRestart = 0

(* run genetic algortihm
   pSize: initial population size
   reStart: #rounds to trigger start over once reached *)
let rec runGenetic pSize clues =
  let rec help pop genNum =
    let _ = printfn "%dth iteration" genNum in
    let _ = Array.sortInPlaceBy (fun (b: SudokuBoard) -> b.fit) pop in
    if genNum = restart then (* catalytic restart *)
      let _ = totalRestart <- totalRestart + 1 in
      runGenetic pSize clues
    else
      match checkSolution pop with (* check if a solution is found *)
      | Some x -> Some x
      | _ ->
        let fittest = pop.[0] in
        let _ = fittest.age <- fittest.age + 1 in
        let newPop = Array.concat [[|fittest|]; crossOver (select pop)] in
        help newPop (genNum + 1)
  in 
  help (genBoards pSize clues) 0

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

let b1 = genBoard easy1
let b2 = genBoard easy1
let c = crossOverAndMutate b1 b2

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

let b3 = {clues = []; cells = Array.ofList solvedRaw; age = 0}
checkSolution [|b3|]