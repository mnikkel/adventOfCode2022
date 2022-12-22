val ins = TextIO.openIn "input.txt"
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
              | d1::d2::chars' => let val PacketList top = hd stack
                                      val isTwoDigits = Char.isDigit d2
                                      val str = if isTwoDigits
                                                then implode [d1, d2]
                                                else Char.toString d1
                                      val n = valOf (Int.fromString str)
                                  in
                                      if isTwoDigits
                                      then loop (chars', (PacketList (top @ [Number(n)]))::(tl stack))
                                      else loop (d2::chars', (PacketList (top @ [Number(n)]))::(tl stack))
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
              | _::[] => raise Match
             | l::r::input' => loop (input', (makePacket r)::(makePacket l)::packets)
    in
        loop (input, [])
    end

fun comparePair (left : packet, right : packet) : (bool * bool) =
    case (left, right) of
        (PacketList [], PacketList (_::_)) => (true, true)
      | (PacketList [], PacketList []) => (false, true)
      | (PacketList (_::_), PacketList []) => (false, false)
      | (Number (l), Number (r)) => (l < r, l = r)
      | (Number _, PacketList _) => comparePair(PacketList([left]), right)
      | (PacketList _, Number _) => comparePair(left, PacketList ([right]))
      | (PacketList (l::l'), PacketList (r::r')) => let val (less, equal ) = comparePair(l, r)
                                                    in
                                                        if less
                                                        then (true, true)
                                                        else if equal
                                                        then comparePair(PacketList l', PacketList r')
                                                        else (false, false)
                                                    end

fun compare (input) =
    let fun loop (input, i, sum) =
            case input of
                [] => sum
              | _::[] => raise Match
              | left::right::input' => let val (less, _) = comparePair (left, right) in
                                           if less
                                           then loop (input', i+1, sum+i)
                                           else loop (input', i+1, sum)
                                       end
    in
        loop (input, 1, 0)
    end

val p1 = processPairs tokens
val part1 = compare p1
