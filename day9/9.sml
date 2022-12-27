val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val directions' = map (fn(x)=>String.tokens Char.isSpace x) lines
fun toInt (lst) =
    let val [a, b] = lst
        val i = valOf (Int.fromString b)
    in
    (a, i)
    end

val directions = map toInt directions'

fun movement (s, i) =
    case s of
     "U" => (0,i)
      | "D" => (0,~i)
      | "L" => (~i,0)
      | "R" => (i,0) 

val steps = map movement directions

fun addT (a : (int*int), b : (int*int)) : (int*int) =
    let val (x, y) = a
        val (i, j) = b
    in
    (x+i, y+j)
    end

fun tupleCompare (a, b) =
    let val (a1, a2) = a
        val (b1, b2) = b
    in
    if a1 = b1
    then Int.compare (a2, b2)
    else Int.compare (a1, b1)
    end

structure tupleSet = ListSetFn (struct
                 type ord_key = (int*int)
                 val compare = tupleCompare
                 end)

fun moveHead (h : (int*int), steps : (int*int) list) =
    let val hPos = [h]
        fun loop (hPos, steps) =
            case steps of
             [] => hPos
              | x::steps' => loop ((addT(hd hPos, x))::hPos, steps')
    in
    loop (hPos, steps)
    end

val headMoves = rev(moveHead ((0,0), steps))

fun moveTail (t : (int*int), steps : (int*int) list) : (int*int) list =
    let val tPos = [t]
        fun loop (tPos, steps) =
            case steps of
             [] => tPos
              | hPos::steps' => let val t = hd tPos
                                    val (tx, ty) = t
                                    val (hx, hy) = hPos
                                    val tMove = if Int.abs (hx-tx) < 2 andalso Int.abs (hy-ty) < 2
                                                then (tx,ty)
                                                else if hx > tx andalso hy > ty
                                                then addT(t, (1,1))
                                                else if hx < tx andalso hy < ty
                                                then addT(t, (~1,~1))
                                                else if hx > tx andalso hy < ty
                                                then addT(t, (1,~1))
                                                else if hx < tx andalso hy > ty
                                                then addT(t, (~1,1))
                                                else if hx > tx
                                                then addT(t, (1,0))
                                                else if hx < tx
                                                then addT(t, (~1,0))
                                                else if hy > ty
                                                then addT(t, (0,1))
                                                else if hy < ty
                                                then addT(t, (0,~1))
                                                else raise Match
                                in
                                if tMove = t
                                then loop (tMove::tPos, steps')
                                else loop (tMove::tPos, steps)
                                end
    in
    loop (tPos, steps)
    end

val tailList = rev (moveTail((0,0), headMoves))
val s = tupleSet.fromList tailList
val part1 = tupleSet.numItems s

val t1 = tailList
val t2 = rev (moveTail((0,0), t1))
val t3 = rev (moveTail((0,0), t2))
val t4 = rev (moveTail((0,0), t3))
val t5 = rev (moveTail((0,0), t4))
val t6 = rev (moveTail((0,0), t5))
val t7 = rev (moveTail((0,0), t6))
val t8 = rev (moveTail((0,0), t7))
val t9 = rev (moveTail((0,0), t8))

val s2 = tupleSet.fromList t9
val part2 = tupleSet.numItems s2
