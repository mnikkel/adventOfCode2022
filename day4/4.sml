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

val ranges = rangeList lines

fun isContained (points) =
    let
	val [a, b, x, y] = points
	val p1 = [a, x, y, b]
	val p2 = [x, a, b, y]
	val sorted = ListMergeSort.sort op> points
    in
	sorted = p1 orelse sorted = p2
    end
	
fun find (ranges) =
    let
	fun loop (ranges, count) =
	    case ranges of
		[] => count
	      | ps::rest => if isContained ps
			    then loop (rest, count+1)
			    else loop (rest, count)
    in
	loop (ranges, 0)
    end
	
val part1 = find ranges;
