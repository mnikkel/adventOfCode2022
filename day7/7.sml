val ins = TextIO.openIn "input.txt"
val split = String.tokens Char.isCntrl o TextIO.inputAll
val lines = split ins
val commands = map (String.tokens Char.isSpace) lines

val isDir = String.isPrefix "dir"
val dirCount = List.filter isDir
val fs = AtomMap.empty

fun cd (dir, stack) =
    case dir of
        ".." => tl stack
      | d => d::stack

fun ls (commands : string list list, stack) =
    let
        fun loop (commands, dirs, files : (int * string) list, sz : int) =
            case commands of
                [] => (dirs, files, sz)
             | x::xs => case x of
                            "dir"::x::_ => let val key = Atom.atom (x ^ (concat stack)) in
                                               loop (xs, key::dirs, files, sz) end
                          | "$"::_ => (dirs, files, sz)
                          | fileSize::fileName::_ => let val fInt = valOf (Int.fromString fileSize) in
                                                         loop (xs, dirs, (fInt, fileName)::files, fInt + sz)
                                                     end

    in
        loop (commands, [], [], 0)
    end

fun processInput (commands : string list list, fs, dirStack) =
    case commands of
        [] => fs
      | ("$"::"cd"::dir)::rest => let val d = cd (hd dir, dirStack) in
                                      processInput (rest, fs, d)
                                end
      | ("$"::"ls"::_)::rest => let
          val contents = ls (rest, dirStack)
          val key = Atom.atom (concat dirStack)
          val insert = AtomMap.insert (fs, key, contents);
      in
          processInput (rest, insert, dirStack)
      end

      | _::rest => processInput (rest, fs, dirStack)


val execute = processInput (commands, fs, [])
val dirs = AtomMap.listKeys execute

fun bad (dir, fs) =
    let
        val current = AtomMap.lookup (fs, dir)
        val (cdirs, cfiles, sized) = current
    in
        foldl (fn(d, init)=>init + bad(d, fs)) sized cdirs
    end

val under100k = List.filter (fn(d)=>(bad(d, execute)<=100000)) dirs

val totalSpace = 70000000
val usedSpace = bad (Atom.atom "/", execute)
val freeSpace = totalSpace - usedSpace
val needToFree = 30000000 - freeSpace
val bigEnough = List.filter (fn(d)=>(bad(d, execute)>=needToFree)) dirs

val part1 = foldl (fn(d, init)=>(init + (bad(d, execute)))) 0 under100k
val part2 = foldl (fn(d, min)=>Int.min (min, (bad(d, execute)))) totalSpace bigEnough
