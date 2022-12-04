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

fun sortedRanges (ranges) =
    let
	val sort = ListMergeSort.sort op>
	fun loop (ranges, sorted) =
	    case ranges of
		[] => sorted
	      | x::rest => loop (rest, sorted @ [sort x])
    in
	loop (ranges, [])
    end
				     
fun isContained (points, sorted) =
    let
	val [a, b, x, y] = points
	val p1 = [a, x, y, b]
	val p2 = [x, a, b, y]
    in
	sorted = p1 orelse sorted = p2
    end
	
fun isOverlap (points, sorted) =
    let
	val [a, b, x, y] = points
	val p1 = [a, b, x, y]
	val p2 = [x, y, a, b]
	val single = b = x orelse y = a
    in
	sorted <> p1 andalso sorted <> p2 orelse single
    end

fun findAll (ranges, sorted) =
    let
	fun loop (ranges, sorted, count1, count2) =
	    case ranges of
		[] => (count1, count2)
	      | ps::rest => let
		  val c1 = if isContained (ps, (hd sorted)) then count1+1 else count1
		  val c2 = if isOverlap (ps, (hd sorted)) then count2+1 else count2
	      in
		  loop (rest, (tl sorted), c1, c2)
	      end
				
    in
	loop (ranges, sorted, 0, 0)
    end

val ranges = rangeList lines
val sorted = sortedRanges ranges
			  
val (p1, p2) = findAll (ranges, sorted)
