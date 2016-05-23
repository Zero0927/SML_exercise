(*Siyuan Zhou*)

fun greetings2 x = 
	case x of
	NONE => "Hello there, you!"
	| _ => "Hello  there, " ^ valOf(x)
		 
fun length2 xs =
	case xs of
	[] => 0                         
	| _ => length2(tl xs) +1;		
	
type mytype = int
datatype bstree = Empty | Node of bstree * mytype * bstree

fun insert(bstree, n)= 
	case bstree of
		Empty => Node (Empty, n, Empty)
		| Node (left,value,right)=> if n> value then Node(left,value,insert(right,n)) else
																if n< value then Node(insert(left,n),value,right)else
																bstree
														
fun treeHeight(bstree) =
	case bstree of
		Empty => ~1
		|Node(left,value,right) => (let val val1 = treeHeight(left)
																		val val2 = treeHeight(right)
																in
																	if val1 > val2 then val1 + 1
																	else val2 +1
																end)

fun isodd(x: int)=
	x mod 2 = 1
	
fun iseven(x: int)=
	x mod 2 = 0
	
fun ispositive(x: int)=
 	x > 0

fun myfilter(f, x: int list)=
	if null x then []
	else if f(hd(x)) then hd(x)::myfilter(f,tl(x))
			else myfilter(f,tl(x))

fun add_or_multiply(choice) =
	if choice =1 then fn (v1,v2) => v1 + v2
	else fn (v1,v2) => v1 * v2