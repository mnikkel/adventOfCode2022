val datastream = explode "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
val datastream2 = explode "bvwbjplbgvbhsrlpgdmjqwftvncz"
val datastream3 = explode "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"

val ins = TextIO.openIn "input.txt"
val ds = explode (TextIO.inputAll ins)



structure charSet = ListSetFn (struct
                                type ord_key = char
                                val compare = Char.compare
                                end)

fun allDiff (charList) =
    let
        val four = List.take (charList, 4)
        val cset = charSet.fromList four
        val count = charSet.numItems cset
    in
        count = 4
    end

fun checkStream (charList, count) =
    if allDiff charList
    then count
    else checkStream((List.drop (charList, 1)), count + 1)

val part1 = checkStream (ds, 4)
