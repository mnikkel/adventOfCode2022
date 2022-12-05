val s1 = explode "RNPG"
val s2 = explode "TJBLCSVH"
val s3 = explode "TDBMNL"
val s4 = explode "RVPSB"
val s5 = explode "GCQSWMVH"
val s6 = explode "WQSCDBJ"
val s7 = explode "FQL"
val s8 = explode "WMHTDLFV"
val s9 = explode "LPBVMJF"

val cl = [s1, s2, s3, s4, s5, s6, s7]

fun listToQ (lst) =
    let
	val q = Queue.mkQueue ()
	fun loop (lst) =
	    case lst of
		[] => q
	      | x::rest => (Queue.enqueue (q, x);
			    loop (rest))
    in
	loop (rev lst)
    end
	
