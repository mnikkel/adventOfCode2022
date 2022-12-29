val ins = TextIO.openIn "input.txt"
val lines = (String.tokens Char.isCntrl o TextIO.inputAll) ins
val nNodes = length lines

val flow = Array.array (nNodes + 1, 0)
val map = Array.array (676, 0)
val matrix = Array2.array (nNodes + 1, nNodes + 1, 999)

fun labelToId (label) =
    let val a::b::_ = explode label
        val x = (ord a) - 65
        val y = (x * 26) + (ord b) - 65
    in
        y
    end

fun adjacentList (list) =
    let fun loop (list, ids) =
            case list of
                [] => ids
              | label::list' => loop (list', (labelToId label)::ids)
    in
        loop (list, [])
    end

fun getRate (line) =
    let val fields = String.tokens Char.isPunct line
        val flow = Int.fromString (List.nth (fields, 1))
    in
        case flow of
            NONE => raise Match
          | SOME i => i
    end

fun buildGraph (input) =
    let
        fun fillMap (input, count) =
            case input of
                [] => ()
              | line::input' => let val fields = String.tokens Char.isSpace line
                                    val id = labelToId (List.nth (fields, 1))
                                in
                                    (Array.update (map, id, count);
                                     fillMap (input', count+1))
                                end

        fun addVertex (node, adjList) =
            case adjList of
                [] => ()
              | n::adjList' => let val node2 = Array.sub (map, n) in
                                   (Array2.update (matrix, node, node2, 1);
                                    addVertex (node, adjList'))
                               end

        fun loop (input, count) =
            case input of
                [] => ()
              | line::input' => let val fields = String.tokens Char.isSpace line
                                    val id = labelToId (List.nth (fields, 1))
                                    val adj = adjacentList (List.drop (fields, 9))
                                    val rate = getRate line;
                                in
                                    (addVertex (count, adj);
                                     Array2.update (matrix, count, count, 1);
                                     Array.update (flow, count, rate);
                                     loop (input', count+1))
                                end
    in
        (fillMap (input, 1);
        loop (input, 1))
    end

fun applyFloyd (k) =
    let
        fun floyd (i, j, weight) =
            let val throughK = (Array2.sub (matrix, i, k)) + (Array2.sub (matrix, k, j)) in
                if throughK < weight
                then throughK
                else weight
            end
    in
        if k <= nNodes
        then (Array2.modifyi Array2.RowMajor floyd {base=matrix,row=0,col=0,nrows=NONE,ncols=NONE};
              applyFloyd (k+1))
        else ()
    end
