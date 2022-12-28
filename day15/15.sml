val ins = TextIO.openIn "input.txt"
val split1 = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split1 ins
val split2 = String.tokens (fn(x)=>(x = #"=" orelse x = #":" orelse x = #","))
val lines2 = map split2 lines
val ints = List.mapPartial Int.fromString
val intList = map ints lines2

fun distance (list) =
    let
        val x1::y1::x2::y2::[] = list
    in
        abs (x1-x2) + abs (y1-y2)
    end

fun sensors (list) =
    let val x::y::_ = list in
        (x, y)
    end

fun beacons (list) =
    let val _::_::x::y::[] = list in
        (x, y)
    end

val s = map sensors intList
val d = map distance intList
val b = map beacons intList

fun distance2 a b =
    let val (x1, y1) = a
        val (x2, y2) = b
    in
        abs (x1-x2) + abs (y1-y2)
    end

fun covered (point, sensors, distances) =
    let val dp = distance2 point
        val dps = map dp sensors
    in
        ListPair.exists (fn(a,b)=>(a<=b)) (dps, distances)
    end

fun coveredRow (y, sensors, beacons, distances) =
    let val min = ~10000000
        val max = 10000000
        fun loop (x, count) =
            if x > max
            then count
            else if covered ((x,y), sensors, distances)
                    andalso not (List.exists (fn(p)=>(p=(x,y))) beacons)
                    andalso not (List.exists (fn(p)=>(p=(x,y))) sensors)
            then loop (x+1, count+1)
            else loop (x+1, count)
    in
        loop (min, 0)
    end

fun searchAdjacent (sensors, distances, xBoundary, yBoundary) =
    let
        fun sa (sensor, x, y) =
            let
                val (sx, sy) = sensor
                val x0 = sx-x >= 0
                val xMax = sx+x <= 4000000
                val y0 = sy-y >= 0
                val yMax = sy+y <= 4000000
            in
                if x > ~1
                then
                    if xMax andalso yMax andalso not (covered ((sx+x, sy+y), sensors, distances))
                    then (sx+x, sy+y)
                    else if xMax andalso y0 andalso not (covered ((sx+x, sy-y), sensors, distances))
                    then (sx+x, sy-y)
                    else if x0 andalso yMax andalso not (covered ((sx-x, sy+y), sensors, distances))
                    then (sx-x, sy+y)
                    else if x0 andalso y0 andalso not (covered ((sx-x, sy-y), sensors, distances))
                    then (sx-x, sy-y)
                    else sa (sensor, x-1, y+1)
                else (~1, ~1)
            end
        fun loop (sensors, distances) =
            case (sensors, distances) of
                ([], []) => (~1, ~1)
              | (s::sensors', d::distances') => let val p = sa (s, d+1, 0) in
                                                    if p = (~1, ~1)
                                                    then loop (sensors', distances')
                                                    else p
                                                end

    in
        loop (sensors, distances)
    end

val part1 = coveredRow (2000000, s, b, d)
val part2 = let val (x, y) = searchAdjacent (s, d, 4000000, 4000000) in
                x * 4000000 + y
            end
