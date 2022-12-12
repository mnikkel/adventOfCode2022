val monkeys = Array.fromList
                  [{number=0, items=[63,57], operation=(fn(x)=>x*11), test=(fn(x)=>(x mod 7) = 0), pass=(6,2)},
                   {number=1, items=[82,66,87,78,77,92,83], operation=(fn(x)=>x+1), test=(fn(x)=>(x mod 11) = 0), pass=(5,0)},
                   {number=2, items=[97, 53, 53, 85, 58, 54], operation=(fn(x)=>x*7), test=(fn(x)=>(x mod 13) = 0), pass=(4,3)},
                   {number=3, items=[50], operation=(fn(x)=>x+3), test=(fn(x)=>(x mod 3) = 0), pass=(1,7)},
                   {number=4, items=[64, 69, 52, 65, 73], operation=(fn(x)=>x+6), test=(fn(x)=>(x mod 17) = 0), pass=(3,7)},
                   {number=5, items=[57, 91, 65], operation=(fn(x)=>x+5), test=(fn(x)=>(x mod 2) = 0), pass=(0,6)},
                   {number=6, items=[67, 91, 84, 78, 60, 69, 99, 83], operation=(fn(x)=>x*x), test=(fn(x)=>(x mod 5) = 0), pass=(2,4)},
                   {number=7, items=[58, 78, 69, 65], operation=(fn(x)=>x+7), test=(fn(x)=>(x mod 19) = 0), pass=(5,1)}]

(*
val monkeys = Array.fromList
                  [{number=0, items=[79,98], operation=(fn(x)=>x*19), test=(fn(x)=>(x mod 23) = 0), pass=(2,3)},
                   {number=1, items=[54,65,75,74], operation=(fn(x)=>x+6), test=(fn(x)=>(x mod 19) = 0), pass=(2,0)},
                   {number=2, items=[79,60,97], operation=(fn(x)=>x*x), test=(fn(x)=>(x mod 13) = 0), pass=(1,3)},
                   {number=3, items=[74], operation=(fn(x)=>x+3), test=(fn(x)=>(x mod 17) = 0), pass=(0,1)}]
*)


val count = Array.array (8, 0)

fun processMonkey (monkey) =
    let val {number, items, operation, test, pass} = monkey in
        case items of
            [] => raise Match
          | n::items' => let val w = (operation n) mod 9699690
                             val from = {number=number,
                                        items=items',
                                        operation=operation,
                                        test=test,
                                        pass=pass}
                             val to = if test w
                                      then Array.sub (monkeys, #1 pass)
                                      else Array.sub (monkeys, #2 pass)
                             val c = Array.sub (count, #number from)
                         in
                             (Array.update (monkeys, number, from);
                              Array.update (monkeys, #number to,
                                            {number=(#number to),
                                             items=(#items to) @ [w],
                                             operation=(#operation to),
                                             test=(#test to),
                                             pass=(#pass to)});
                             Array.update (count, (#number from), c+1))
                         end
    end

fun processAll (m, total) =
        if m < total
        then let val i = #items (Array.sub (monkeys, m)) in
                 case i of
                     [] => processAll(m+1, total)
                   | _ => (processMonkey(Array.sub(monkeys, m));
                           processAll(m, total))
             end
        else ();


fun rounds(count) =
    if count < 10000
    then (processAll (0, 8);
         rounds(count+1))
    else ();

rounds(0);
ArrayQSort.sort Int.compare count;
val part2 = Array.sub (count, 6) * Array.sub (count, 7)
