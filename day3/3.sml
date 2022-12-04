val ins = TextIO.openIn "input.txt"
val words = String.tokens Char.isSpace o TextIO.inputAll

val sack = words ins

fun split s =
    let
      val half = (size s) div 2;
      val a = String.substring (s, 0, half)
      val b = String.substring (s, half, half)
    in
      (explode a, explode b)
    end

structure charSet = ListSetFn (struct
                                type ord_key = char
                                val compare = Char.compare
                                end)

fun overlapList (sack) =
    let
      fun loop (sack, lst) =
          case sack of
               [] => lst
             | x::rest => let
               val (a, b) = split x
               val csa = charSet.fromList a
               val csb = charSet.fromList b
               val intersectList = charSet.listItems (charSet.intersection (csa, csb))
             in
               loop (rest, lst @ intersectList)
             end
    in
      loop (sack, [])
    end

fun priorities (chars) =
    let
      fun loop (chars, lst) =
          case chars of
               [] => lst
             | c::rest => if Char.isUpper c
                          then loop (rest, ((ord c) - 38)::lst)
                          else loop (rest, ((ord c) - 96)::lst)
    in
      loop (chars, [])
    end

fun findBadge (a, b, c) =
    let
      val csa = charSet.fromList (explode a)
      val csb = charSet.fromList (explode b)
      val csc = charSet.fromList (explode c)
      val i1 = charSet.intersection (csa, csb)
      val i2 = charSet.intersection (i1, csc)
    in
      charSet.listItems i2
    end

fun elfBadges (sack) =
    let
      fun loop (sack, lst) =
          case sack of
               [] => lst
             | a::b::c::rest => loop(rest, (findBadge (a,b,c)) @ lst)
    in
      loop(sack, [])
    end

val priorityList = priorities (overlapList sack)
val badgeList = priorities (elfBadges sack)

val total1 = foldl op+ 0 priorityList
val total2 = foldl op+ 0 badgeList
