val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val chars = map explode lines
val matrix = Array2.fromList chars

datatype edgenode =
         Empty
         | Node of int * char

datatype graph =
         Empty
       | G of edgenode list array

val graph = Array.array (length chars * length (hd chars), [] : int list)
val parents = Array.array (length chars * length (hd chars), ~1)

fun findChar (arr, ch, startAt) =
    let
        val i = startAt div Array2.nCols arr
        val j = startAt mod Array2.nCols arr
    in
        if Array2.sub (arr, i, j) = ch
        then startAt
        else findChar (arr, ch, startAt+1)
    end

val start = findChar (matrix, #"S", 0)
val finish = findChar (matrix, #"E", 0)

fun add (id, adjNode) =
    let val adjList = Array.sub (graph, id) in
        Array.update (graph, id, adjNode::adjList)
    end

fun adjacent (charX, charY) =
    let
        fun replace (ch1, ch2, ch3) =
            if ch1 = ch2
            then ch3
            else ch1
        val x = replace ((replace (charX, #"S", #"a")), #"E", #"z")
        val y = replace ((replace (charY, #"S", #"a")), #"E", #"z")
    in
        x >= y orelse Char.succ x = y
    end


fun findHorizontal (lst : char list list, count) =
    let fun loop (charList, count) =
            case charList of
                [] => findHorizontal (tl lst, count)
              | x::[] => findHorizontal (tl lst, count+1)
              | x::y::rest => if adjacent (x, y) andalso adjacent (y, x)
                              then (add(count, count+1);
                                    add(count+1, count);
                                    loop (y::rest, count + 1))
                              else if adjacent (x, y)
                              then (add(count, count+1);
                                    loop (y::rest, count + 1))
                              else if adjacent (y, x)
                              then (add(count+1, count);
                                    loop (y::rest, count + 1))
                              else loop (y::rest, count + 1)

    in
        case lst of
            [] => ()
          | charList::_ => loop (charList, count)
    end

fun findVertical (lst, count) =
    let
        val nCols = Array2.nCols matrix
        fun loop (lstX, lstY, count) =
            case (lstX, lstY) of
                ([], []) => findVertical (tl lst, count)
              | (x::x', y::y') => if adjacent (x, y) andalso adjacent (y, x)
                                  then (add(count, count+nCols);
                                        add(count+nCols, count);
                                        loop (x', y', count + 1))
                                  else if adjacent (x, y)
                                  then (add(count, count+nCols);
                                        loop (x', y', count + 1))
                                  else if adjacent (y, x)
                                  then (add(count+nCols, count);
                                        loop (x', y', count + 1))
                                  else loop (x', y', count + 1)
    in
        case lst of
            [] => ()
          | _::[] => ()
          | x::y::lst' => loop(x, y, count)
    end

fun readGraph (lst) =
    (findHorizontal (lst, 0);
     findVertical (lst, 0))

fun bfs (graph, start, finish) =
    let
        fun addToQueue (lst, parent, visited, queue) =
            case lst of
                [] => (queue, visited)
              | n::lst' => if List.exists (fn(x)=>x=n) visited
                           then addToQueue (lst', parent, visited, queue)
                           else (Array.update (parents, n, parent);
                                 addToQueue (lst', parent, n::visited, queue @ [n]))

        fun loop (visited, queue) =
                case queue of
                    [] => raise Match
                  | n::queue' => let val adj = Array.sub (graph, n)
                                     val (q, v) = addToQueue (adj, n, visited, queue')
                                 in
                                     if List.exists (fn(x)=>x=finish) adj
                                     then print "Found\n"
                                     else loop (v, q)
                                 end
    in
        loop([start], [start])
    end

fun countSteps (parents, start, finish, steps) =
    let val p = Array.sub (parents, finish) in
        if p = start
        then steps + 1
        else countSteps(parents, start, p, steps+1)
    end;

readGraph chars;
bfs (graph, start, finish);

val part1 = countSteps (parents, start, finish, 0)
