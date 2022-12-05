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
    in
	(a <= x andalso y <= b) orelse (x <= a andalso b <= y)
    end

fun overlapTest (r) =
    let
	val [a, b, x, y] = r
    in
	(a <=y andalso b >= x) orelse (x <= a andalso y >= b)
    end
							      
val ranges = rangeList lines
val part1 = List.length (List.filter containTest ranges)
val part2 = List.length (List.filter overlapTest ranges)
