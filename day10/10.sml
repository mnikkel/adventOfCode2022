val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val instructions' = map (fn(x)=>String.tokens Char.isSpace x) lines
fun toInt (lst) =
    let
      val instruction::amount = lst
    in
      case instruction of
        "noop" => ("noop", 0, 1)
      | "addx" => let val i = valOf (Int.fromString (hd amount)) in
                    (instruction, i, 2)
                  end
    end

val instructions = map toInt instructions'

fun process (instructions : (string*int*int) list, x : int, signal : int list) : int list =
    case instructions of
        [] => signal
      | head::rest => let val (ins, amount, cycles) = head in
                        case ins of
                            "noop" => process (rest, x, x::signal)
                          | "addx" => if cycles = 2
                                      then process (("addx", amount, 1)::rest, x, x::signal)
                                      else process (rest, x + amount, x::signal)
                      end

val signals = rev(process (instructions, 1, [0]))

val check = [20,60,100,140,180,220]
val part1 = foldl (fn(x,acc)=>(List.nth(signals, x) * x) + acc) 0 check

fun draw (signals, crt, pattern) =
    case signals of
        [] => pattern
      | x::signals' => let val pos = crt mod 40 in
                         if Int.abs (x-pos) < 2
                         then draw (signals', crt+1, #"#"::pattern)
                         else draw (signals', crt+1, #"."::pattern)
                       end


val zeroIndex = List.drop (signals, 1)
val longline = draw (zeroIndex, 0, [])

fun splitLines (longline, count, split) =
    case longline of
        [] => split
      | x::longline' => if count mod 40 = 0
                      then splitLines (longline', count+1, x::(#"\n")::split)
                      else splitLines (longline', count+1, x::split)

val part2 = implode (splitLines (longline, 0, []));
print part2
