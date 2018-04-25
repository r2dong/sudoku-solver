type Cell = int * int

type SudokuBoard = {
  fills: Cell list
  clues: Cell list
  size: int
}
with
  static member construct s c =
    let f n (_, value) = if value >= s then n + 1 else n in
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
            0 -> []
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
    List.concat [b.clues; b.fills]
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

// test getInd
let tb = {
  fills = []
  size = 9
  clues = []
}
let r = tb.getInd 20