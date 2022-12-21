val ins = TextIO.openIn "test.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val tokens = map explode lines

datatype packet = Number of int
                | PacketList of packet list

fun makePacket (chars) =
    let fun loop (chars, stack) =
            case chars of
                [] => hd stack
              | #"["::chars' => loop (chars', PacketList([])::stack)
              | #"]"::chars' => (case stack of
                                    bottom::[] => bottom
                                  | top::bottom::stack' => let val PacketList b = bottom
                                                               val merged = PacketList (b @ [top])
                                                           in
                                                               loop (chars', merged::stack')
                                                           end)
              | #","::chars' => loop (chars', stack)
              | d::chars' => let val PacketList top = hd stack in
                                 if Char.isDigit d
                                 then let val n = valOf (Int.fromString (Char.toString d)) in
                                          loop (chars', (PacketList (top @ [Number(n)]))::(tl stack))
                                      end
                                 else raise Match
                             end
    in
        case chars of
            [] => PacketList []
          | _ => loop (chars, [])
    end

fun processPairs (input) =
    let fun loop (input, packets) =
            case input of
                [] => List.rev packets
             | l::r::input' => loop (input', (makePacket r)::(makePacket l)::packets)
    in
        loop (input, [])
    end

fun comparePair (left, right) =
    case (left, right) of
        (PacketList [], PacketList _) => true
      | (PacketList _, PacketList []) => false
      | (Number (l), Number (r)) => l <= r
      | (Number (l), PacketList (r)) => comparePair(PacketList([left]), right)
      | (PacketList (l), Number (r)) => comparePair(left, PacketList ([right]))
      | (PacketList (l), PacketList (r)) => let val a::l' = l
                                                val b::r' = r
                                            in
                                                comparePair(a, b) andalso comparePair(PacketList l', PacketList r')
                                            end

fun compare (input) =
    let fun loop (input, i, sum) =
            case input of
                [] => sum
              | left::right::input' => if comparePair (left, right)
                                       then loop (input', i+1, sum+i)
                                       else loop (input', i+1, sum)
    in
        loop (input, 1, 0)
    end

