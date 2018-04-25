type Cell = int * int

type SudokuBoard = {
  fills: Cell list
  clues: Cell list
  size: int
}
with
  static member construct s c =
    let f n (_, value) = if value > s then n + 1 else n in
    if List.fold f 0 c > 0
      then failwith "invalid cell value"
    else
      let f n (ind, _) = if ind >= s * s then n + 1 else n in
      if List.fold f 0 c > 0
        then failwith "invalid cell index"
      else
        let f l (ind, _) = ind :: l
        let nonEmpty = List.fold f [] c in
        let rec genEmpty s nEmp =
          match s with
            -1 -> []
          | s' -> 
            let res = genEmpty (s - 1) nEmp in
              if List.contains s' nEmp then
                res
              else
                (s', 0) :: res
        in
        let empties = genEmpty ((s * s) - 1) nonEmpty in
        {
          clues = c
          size = s
          fills = empties
        }
  member b.allCells = 
    let allCells = List.concat [b.clues; b.fills] in
    List.sortBy (fun (ind, _) -> ind) allCells
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
  member b.isFilled i =
    let rec isFilledHelp i cells =
      match cells with
        (ind, value) :: res -> 
          if ind = i then
            value = 0
          else
            isFilledHelp i res
      | [] -> failwith "index not found, please double check"
    in
    isFilledHelp i b.allCells
  member b.getCell i =
    let rec gcHelp i ac =
      match ac with
        (ind, value) :: res ->
          if ind = i then
            (ind, value)
          else
            gcHelp i res
        | [] -> failwith "cannot get cell, please double check"
    in
    gcHelp i b.allCells
  member b.getCellAttacks i =
    match b.isFilled i with
      false -> 0
    | _ ->
      let inds = b.getInd i in
      let inds = List.filter (fun num -> num <> i) inds in
      let isAttack i j =
        let (_, v1) = b.getCell i in
        let (_, v2) = b.getCell j in
        v1 = v2
      let accFun s j =
        if isAttack i j then
          s + 1
        else
          s
      in
      List.fold accFun 0 inds
  member b.print = 
    let rec printHelp (ac: Cell list) =
      match ac with
        (ind, value) :: res -> 
          if ind % b.size = 0 then
            let s = String.concat "" ["\n"; (string value); " "] in
            let _ = printf "%s" s in printHelp res
          else
            let s = String.concat "" [string value; " "] in
            let _ = printf "%s" s in printHelp res
      | [] -> printf "\n"
    in
    printHelp b.allCells

// tests

// test getInd
let tb = {
  fills = []
  size = 9
  clues = []
}
let r = tb.getInd 20

// test all Cells
let tb2 = {
  fills = [(0, 1); (2, 2); (3, 1)]
  clues = [(1, 2)]
  size = 2
}
printf "%d\n" tb2.allCells.Length

// test print
let tb1 = {
  fills = [(0, 1); (2, 2); (3, 1)]
  clues = [(1, 2)]
  size = 2
}
tb1.print

// test construct
let tb3 = SudokuBoard.construct 5 [(0, 1); (24, 5); (10, 3); (19, 4)]
tb3.print

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
let ws301 = SudokuBoard.construct 9 ws301Clues
ws301.print

// next: check get #attacks