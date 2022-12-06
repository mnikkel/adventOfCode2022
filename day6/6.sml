val ins = TextIO.openIn "input.txt"
val ds = explode (TextIO.inputAll ins)

structure charSet = ListSetFn (struct
                                type ord_key = char
                                val compare = Char.compare
                                end)

fun allDiff (charList, take) =
    let
        val four = List.take (charList, take)
        val cset = charSet.fromList four
        val count = charSet.numItems cset
    in
        count = take
    end

fun checkStream (charList, take, count) =
    if allDiff (charList, take)
    then count
    else checkStream((List.drop (charList, 1)), take, count + 1)

val part1 = checkStream (ds, 4, 4)
val part2 = checkStream (ds, 14, 14)
