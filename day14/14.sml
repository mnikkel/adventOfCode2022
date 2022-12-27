val start = (500,0)
val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val onlyDigits = String.tokens (fn(x)=>not(Char.isDigit x))
val pointsList = map onlyDigits lines
val intList = map (valOf o Int.fromString)


fun pointCompare (a, b) =
    let val (a1, a2) = a
        val (b1, b2) = b
    in
    if a2 = b2
    then Int.compare (a1, b1)
    else Int.compare (a2, b2)
    end

structure pointSet = RedBlackSetFn (struct
                 type ord_key = (int*int)
                 val compare = pointCompare
                 end)

fun step (x1, x2) =
    if x1 > x2
    then x1 - 1
    else if x1 < x2
    then x1 + 1
    else x1

fun makePoints (list) =
    let
        val ints = intList list
        fun loop (ints, points) =
            case ints of
                [] => rev points
              | x::y::ints' => loop (ints', (x, y)::points)
              | _::[] => raise Match
    in
        loop (ints, [])
    end

val ends = map makePoints pointsList

fun fillPoints (p1, p2) =
    let
        val (x1, y1) = p1
        val (x2, y2) = p2
        fun loop (x1, y1, plist) =
            if x1 = x2 andalso y1 = y2
            then plist
            else let
                val x = step (x1, x2)
                val y = step (y1, y2)
            in
                loop (x, y, (x, y)::plist)
            end
    in
        loop (x1, y1, [p1])
    end

fun allPoints (points) =
    let fun loop (points, allPoints) =
            case points of
                [] => allPoints
              | a::b::points' => let val fill = fillPoints (a, b) in
                                     loop (b::points', fill @ allPoints)
                                 end
              | _::[] => allPoints
    in
        loop (points, [])
    end

val ap = List.concat (map allPoints ends)
val s = pointSet.fromList ap
val (_, floor) = pointSet.maxItem s

fun moveSand (point, set) =
    let
        val (x, y) = point
        val down = pointSet.member (set, (x, y+1))
        val downLeft = pointSet.member (set, (x-1, y+1))
        val downRight = pointSet.member (set, (x+1, y+1))
    in
        if down andalso downLeft andalso downRight
        then point
        else if down andalso downLeft
        then (x+1, y+1)
        else if down
        then (x-1, y+1)
        else (x, y+1)
    end

fun drop (start, set) =
    let
        val moveTo = moveSand (start, set)
        val (_, y) = moveTo
    in
        if y >= floor+2
        then set
        else if start = moveTo
        then pointSet.add (set, start)
        else drop (moveTo, set)
    end

fun countSand (start, set) =
    let
        fun loop (set, count) =
            let val s1 = drop (start, set) in
                if pointSet.equal (set, s1) orelse pointSet.member (set, start)
                then count
                else loop (s1, count+1)
            end
    in
        loop (set, 0)
    end

val part1 = countSand (start, s)

val floorPart2 = fillPoints ((~10000, floor+2), (10000, floor+2))
val s2 = pointSet.addList (s, floorPart2)
val part2 = countSand (start, s2)
