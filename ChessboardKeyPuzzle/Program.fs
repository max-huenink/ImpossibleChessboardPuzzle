open System

let rnd = System.Random()

// Parse a string to an integer
let parseInt ( i: string ) = i |> int

// Format the board for output
let boardFormatter ( numRowsAndColumns: int) ( board: int[] ) =
    let chunkFolder ( a: string ) ( b: int ) = $"{a} {b}"
    let chunkMapper ( arr: int[] ) = Array.fold chunkFolder "" arr
    let boardFolder ( a: string ) ( b: string ) = $"{a}\n{b}"
    let chunks = Array.chunkBySize numRowsAndColumns board
    let mappedBoard = Array.map chunkMapper chunks
    Array.fold boardFolder "" mappedBoard

module impossibleBoard =
    module helpers =
        // Flip the bit at a given index of the array
        let flipBitOfIndx ( arr: int[] ) ( indx: int ) =
            let flipBit b =
                if (b = 1) then
                    0
                else
                    1
            let flipMatch ( matchIndx: int) ( indx: int ) ( cell: int ) =
                match indx with
                | x when x = matchIndx -> flipBit cell
                | _ -> cell
            Array.mapi (flipMatch indx) arr

        // Randomize each element to a 0 or 1
        let randomizeBoard ( x: int ) =
            let randomBit () = rnd.Next(0, 2)
            match x with
            | _ -> randomBit ()

        // Produce an xor of the index of every bit that is a 1
        let xorOnBits arr =
            let mapOnToIndx = 
                let matchToIndx ( i: int) ( x: int) =
                    match x with
                    | 1 -> i
                    | _ -> 0
                Array.mapi matchToIndx
            let foldXorBits = Array.fold (^^^) 0
            arr
            |> mapOnToIndx
            |> foldXorBits

    // Create and randomize the board
    let createBoard count =
        let initBoard = Array.create count 0
        Array.map helpers.randomizeBoard initBoard

    // Randomly determine the key location based on the size of the board
    let keyLocation ( boardSize: int) = rnd.Next(0, boardSize)

    // Flip the bit of a given board at a given index, returns a new array
    let flipIndxOnBoard ( board: int[] ) ( indx: int ) = helpers.flipBitOfIndx board indx

[<EntryPoint>]
let main argv =
    // Warden defines size of board
    let size =
        let mutable sizeAttempt = 0
        let mutable correct = false
        while (not correct) do
            Console.Clear()
            printfn "Enter Warden"
            let power =
                printfn "Size of the board must be a power of 2 that is a perfect square for display purposes"
                printf "2^x\nx: "
                parseInt (Console.ReadLine())
            sizeAttempt <- Math.Pow((float)2, (float)power) |> int
            let sqrt = Math.Sqrt((float)sizeAttempt)
            correct <- Math.Abs(Math.Ceiling(sqrt) - Math.Floor(sqrt)) < Double.Epsilon
            if not correct then
                printfn "Size %d is not a perfect square. Sqrt(size) = %f" sizeAttempt sqrt
                printf "Press enter to try again..."
                Console.ReadLine() |> ignore
        sizeAttempt
    printfn "Size: %d" size
    printfn "Press enter to continue..."
    Console.ReadLine() |> ignore
    Console.Clear()

    // Setup board and necessary functions
    let sqrt = Math.Sqrt((float)size) |> int
    let formatter = boardFormatter sqrt
    let board = impossibleBoard.createBoard size
    let keyLocation = impossibleBoard.keyLocation size
    let flipTheBoard = impossibleBoard.flipIndxOnBoard board

    // Prisoner 1 chooses the index of a bit to flip
    //TODO need out-of-bounds handling
    printfn "Enter Prisoner 1"
    printfn "board %s" (formatter board)
    printfn "key location %i" keyLocation

    printf "\nWhat bit should be flipped?\nbit index: "
    let bit = parseInt (Console.ReadLine())
    let flippedBoard = flipTheBoard bit

    printfn "Exit Prisoner 1"
    printfn "Press enter to continue..."
    Console.ReadLine() |> ignore
    Console.Clear();

    // Prisoner 2 guesses the location of the key (until correct)
    //TODO need out-of-bounds handling
    let mutable tryAgain = true
    while tryAgain do
        printfn "Enter Prisoner 2"
        printfn "flipped board %s" (formatter flippedBoard)
        printf "Where is the key?\nGuess: "
        let keyGuess = parseInt (Console.ReadLine())
        if keyGuess = keyLocation then
            printfn "Correct!"
            tryAgain <- false
        else
            printf "Incorrect. Guess again?\n[y]/n: "
            let attempt = (Console.ReadKey().KeyChar.ToString().ToLower())
            tryAgain <- not (attempt = "n")
            Console.Clear()
    0 // return an integer exit code

