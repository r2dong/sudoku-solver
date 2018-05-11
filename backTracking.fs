//Sudoku Types
type SudokuBoard = Block array  
and Block = {Value: Value; X: int; Y: int; Z: int}
and Value = 
    | Answer of int               
    | AllowedValues of Set<int>

(* A Sudoku board is made up of an array of N blocks(usually 81) 
 * Each block has values attribute for the numerical information as well as positional attributes
 * The positional attributes x,y,z tell us the horizontal, vertical, and regions where the attribues can reside
 * The values attribute contains the numberical representation of the desginated value for a cell and another to list all the allowed values for a cell 
 *)

//Helpers
let sqrti i = i |> float |> sqrt |> int
let boardSize (board:SudokuBoard) = board.Length |> sqrti

let valT = [|1..9|]

//takes a grid and calculates what possible numbers each cell can take up
let CalculateAllowedValues board = 
    //Take a board and block parameter and maps the values 
    let findValues (grid:SudokuBoard) filter = 
        grid 
        |> Array.filter filter
        |> Array.map (fun s-> 
            match s.Value with
            | Answer v -> Some v
            | _ -> None)
        |> Array.choose id

    board 
    |> Array.map (fun block -> 
        let newValue = 
            match block.Value with
                | AllowedValues set -> 
                    let newAllowedValues = 
                        [
                        (fun s -> s.X = block.X)
                        (fun s -> s.Y = block.Y)
                        (fun s -> s.Z = block.Z)
                        ] 
                        |> List.map (findValues board)
                        |> Array.concat
                        |> Set.ofArray
                        |> Set.difference set

                    if newAllowedValues.Count = 1 then
                        // if a cell has only one possible value, then mark that cell as 'solved'
                        Answer (Set.minElement newAllowedValues)
                    else
                        //otherwise return the remaining possible values
                        AllowedValues newAllowedValues
                | value -> value
        {block with Value=newValue}
    )


//enumerates all the "groups" of a grid. A 'group' is a general term for a group of nine cells in either a row, a column or a region. 
let getGroups board = 
    let N = boardSize(board)
    let indexes = [ 1 .. N ]
    let groups = [
        (fun index cell-> cell.X = index)  //same column
        (fun index cell-> cell.Y = index)  //same row
        (fun index cell-> cell.Z = index) //same region
    ]

    seq { 
        for blocks in groups do 
        for i in indexes do 
        yield (board |> Array.filter (blocks i)) 
        }
    
//tries to solve the given group of the sudoku grid by checking if there is any number that appears only in ONE cell in that group
let populateGroup group board = 
    let N = board |> boardSize

    let toFind, toSolve = 
        group  
        |> Array.fold (fun acc cell ->
            let pVals, pBlocks = acc 
            match cell.Value with
            | Answer a -> (pVals |> Set.remove a, pBlocks)
            | AllowedValues av -> (pVals, (cell.X, cell.Y, av) :: pBlocks)
        ) (Set.ofArray [|1..N|], [])

    let computedBlocks = 
        toFind 
        |> Set.map (fun number -> 
            let blockNumber = toSolve |> List.filter (fun (_, _, pv) -> pv.Contains(number))
            match blockNumber with
            | [(x, y, _)] -> Some (x, y, number) //list has one one value -> we found the value of the block
            | _ -> None)
        |> Set.toArray
        |> Array.choose id 

    board 
    |> Array.map (fun cell ->
        match computedBlocks |> Array.tryFind (fun (x, y, _) -> cell.X = x && cell.Y = y) with
        | Some (_, _, num) -> {cell with Value=Answer num}
        | None -> cell
    )

let populateAllGroups grid =
    grid 
    |> getGroups
    |> Seq.fold (fun acc group ->  acc |> populateGroup group ) grid



//Check if board is invalid
let isInvalid board = 
    board 
    |> getGroups
    |> Seq.exists (fun group -> 
        let foundCount, values = 
            group 
            |> Array.fold (fun acc cell ->
                let foundCount, values = acc
                match cell.Value with
                | Answer v -> foundCount+1, values |> Set.add v
                | _ -> acc 
            ) (0, Set.empty)

        foundCount <> values.Count
    )

//Check if board is solved
let isSolved board = 
    let allValuesFound = 
        board |> Array.forall (
            fun cell -> 
                match cell.Value with 
                |Answer _ -> true 
                | _ -> false)
    allValuesFound && not (isInvalid board)

//Check if board is unsolvable
let isUnsolvable board = 
    let noMorePossibleValues = 
        board |> Array.exists (
            fun cell -> 
                match cell.Value with 
                |AllowedValues pv when pv.Count = 0 -> true 
                | _ -> false)
    noMorePossibleValues || isInvalid board


type Result = Unsolvable | Hard of SudokuBoard | Solved of SudokuBoard

let rec solve grid =

    //a solver which solves "easy" sudokus, ie, the ones not requiring backtracking
    let rec solveEasy grid = 
        let newGrid = 
            grid 
            |> CalculateAllowedValues 
            |> populateAllGroups 
            |> CalculateAllowedValues

        if newGrid |> isSolved then
            Solved newGrid
        elif newGrid <> grid then
            //if we are making progress, keep going
            solveEasy newGrid
        else
            //otherwise we are stuck and either unsolvable or a hard problem which needs backtracking
            if newGrid |> isUnsolvable then Unsolvable
            else Hard newGrid
    
    match solveEasy grid with
    | Solved grid -> 
        Some grid
    | Unsolvable ->
        None
    | Hard grid ->
        //Hard means we cannot easily deduce the solution, so we need to do backtracing

        //we need to guess here, let's pick a number from the cell that has the least number of available options
        let cellWithLeastPossibleNumbers = grid |> Array.minBy (fun cell -> match cell.Value with AllowedValues pv -> pv.Count | _ -> System.Int32.MaxValue)
        let possibleNumbers = match cellWithLeastPossibleNumbers.Value with
                                | AllowedValues pv -> pv
                                | _ -> failwith "shouldn't get here"

        possibleNumbers 
        |> Set.fold (fun acc number->
            match acc with
            | None -> 
                //make a guess here, fix a value from the available numbers..
                let grid' = grid |> Array.map (fun cell -> 
                    if cell = cellWithLeastPossibleNumbers then
                        {cell with Value=Answer number}
                        
                    else
                        cell
                    )
                //...and try to solve this grid
                solve grid'
            | Some solution -> 
                //if we've already found the solution, "flow" it through the fold
                Some solution
            ) None //init fold with None, ie, no solution found yet

//parses a sudoku grid represented as string lines (see below examples)
let parseGrid N lines =
    let RegionSize = N |> float |> sqrt |> int
    let parseLine (line:string) = 
        let line = line.Replace("|", "").Trim()
        if line.StartsWith("--") then
            None
        else
            let nums = line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
                        |> Array.map (fun c-> match c with
                                                | "-" -> AllowedValues (Set.ofArray [| 1..N |])
                                                | x -> Answer (System.Int32.Parse(x)))
            Some nums
    
    lines 
    |> List.map parseLine 
    |> List.choose id 
    |> Array.concat
    |> Array.mapi (fun ix v -> 
        let x, y = ix % N, ix /N
        let rx, ry = x / RegionSize, y / RegionSize
        {
            Value=v
            X=ix % N
            Y=ix / N
            Z=ry * RegionSize + rx
        }
       )

//pretty-prints a sudoku grid
let print (grid:SudokuBoard) =
    let N = grid |> boardSize
    let RegionSize = N |> sqrti
    let needsSeparator i = 
        (i+1) % RegionSize = 0 && i+1 <> N

    //sort by coordinates - as we don't want to rely on the input grid being sorted
    let gridArr = grid |> Array.sortBy (fun c->c.Y * N + c.X)

    for y in 0..N-1 do
        for x in 0..N-1 do
            match gridArr.[y * N + x].Value with
            | Answer v -> v |> printf "%i "
            | _ -> printf "- "
            if needsSeparator x then printf "| "
        printfn ""
        if needsSeparator y then 
            printfn "%s" (new System.String('-', 2 * N + RegionSize))

let sudoku = [
                "9 2 - | - - - | - 5 -"
                "- - - | - 9 - | - 6 -"
                "- - - | 1 - 5 | 4 - -"
                "---------------------"
                "- - 8 | - 5 - | - - 6"
                "- - 7 | 2 - 1 | 3 - -"
                "4 - - | - 6 - | 7 - -"
                "---------------------"
                "- - 5 | 6 - 9 | - - -"
                "- 1 - | - 7 - | - - -"
                "- 4 - | - - - | - 8 7"
]

let ws301 = [
                "- 3 9 | 5 - - | - - -"
                "- - - | 8 - - | - 7 -"
                "- - - | - 1 - | 9 - 4"
                "---------------------"
                "1 - - | 4 - - | - - 3"
                "- - - | - - - | - - -"
                "- - 7 | - - - | 8 6 -"
                "---------------------"
                "- - 6 | 7 - 8 | 2 - -"
                "- 1 - | - 9 - | - - 5"
                "- - - | - - 1 | - - 8"
]

let grid = parseGrid 9 ws301
print grid
match grid |> solve with
| Some solution -> 
    printfn "Solution:"
    print solution
| None -> printfn "Unsolvable"