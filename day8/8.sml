val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val rows = map explode lines
val mi = map (fn(x)=> map (valOf o Int.fromString) (map str x))
val intList = mi rows
fun toTuple (x : int) : (bool*int) = (false, x)
fun convert (lst) = map toTuple lst
val tList = map convert intList
val a = Array2.fromList tList

fun edge (i : int, j : int, n : int) : bool =
    let val right = (Array2.nCols a) - 1
        val bottom = (Array2.nRows a) - 1
    in
        i=0 orelse j=0 orelse i=bottom orelse j=right
    end

fun visibleFromLeft (i : int, j : int, n : int) : bool =
    let
        val row = Array2.row (a, i)
        val left = VectorSlice.slice (row, 0, SOME j)
    in
        not (VectorSlice.exists (fn(x)=>(#2 x)>=n) left)
    end


fun visibleFromRight (i : int, j : int, n : int) : bool =
    let
        val row = Array2.row (a, i)
        val right = VectorSlice.slice (row, j+1, NONE)
    in
        not (VectorSlice.exists (fn(x)=>(#2 x)>=n) right)
    end

fun visibleFromAbove (i : int, j : int, n : int) : bool =
    let
        val col = Array2.column (a, j)
        val above = VectorSlice.slice (col, 0, SOME i)
    in
        not (VectorSlice.exists (fn(x)=>(#2 x)>=n) above)
    end

fun visibleFromBelow (i : int, j : int, n : int) : bool =
    let
        val col = Array2.column (a, j)
        val below = VectorSlice.slice (col, i+1, NONE)
    in
        not (VectorSlice.exists (fn(x)=>(#2 x)>=n) below)
    end

fun runTests (i : int, j : int, x : (bool*int)) : (bool*int) =
    let val (b, n) = x in
        if b
        then x
        else if edge(i,j,n)
        then (true, n)
        else if visibleFromLeft(i,j,n)
        then (true, n)
        else if visibleFromRight(i,j,n)
        then (true, n)
        else if visibleFromAbove(i,j,n)
        then (true, n)
        else if visibleFromBelow(i,j,n)
        then (true, n)
        else x
    end

val u = Array2.modifyi Array2.RowMajor runTests {base=a,row=0,col=0,nrows=NONE,ncols=NONE}

fun isVisible(x : (bool*int), count) =
    if (#1 x)
    then count + 1
    else count

val part1 = Array2.fold Array2.RowMajor isVisible 0 a
