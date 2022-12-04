val ins = TextIO.openIn "input.txt"
val eachLine = String.tokens Char.isSpace o TextIO.inputAll

val lines = eachLine ins

fun stringToRanges (s) =
    let
	val split = String.tokens Char.isPunct s
	val stoi = valOf o Int.fromString
    in
	map stoi split
    end
	
fun rangeList (lines) =
    let
	fun loop (lines, ranges) =
	    case lines of
		[] => ranges
	      | a::rest => let
		  val [a, b, x, y] = stringToRanges a
	      in
		  loop (rest, [a, b, x, y]::ranges)
	      end
			       
    in
	loop (lines, [])
    end
		       
fun containTest (r) =
    let
	val [a, b, x, y] = r
	val o1 = x - a
	val o2 = y - b
    in
	(o1 > 0 andalso o2 > 0) orelse (o1 < 0 andalso o2 < 0)
    end

fun overlapTest (r) =
    let
	val [a, b, x, y] = r
	val o1 = y - a
	val o2 = x - b
    in
	(o1 > 0 andalso o2 > 0) orelse (o1 < 0 andalso o2 < 0)
    end
							      
fun findAll (ranges) =
    let
	fun loop (rs, count1, count2) =
	    case rs of
		[] => (count1, count2)
	      | r::rest => let
		  val nc1 = if containTest r
			    then count1
			    else count1 + 1
		  val nc2 = if overlapTest r
			    then count2
			    else count2 + 1
	      in
		  loop (rest, nc1, nc2)
	      end
    in
	loop (ranges, 0, 0)
    end
	
val ranges = rangeList lines
val (p1, p2) = findAll ranges
