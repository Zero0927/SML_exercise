(*Siyuan Zhou*)
fun roots(a:real, b:real, c:real)=
	if b*b - 4.0*a*c< 0.0 then (0.0,0.0)
	else let
			val sq = Math.sqrt(b * b - (4.0 * a * c));
			val d = 2.0 * a;
		 in
		 	((((~(b)) - sq)/d),(((~(b))+sq)/d))
		 	end
		
fun divide(a:int list) =
	if null a then ([],[])
	else
		let 
			fun helper(b:int list) =
				if null b then []
				else if null (tl b) then hd b::[]
				else hd b :: helper(tl (tl b))
		in
			(helper(a),helper(tl a))
		end

fun reciprocal 0 = NONE
	| reciprocal n = SOME (1 div n)

fun is_older(date1: int*int*int, date2:int*int*int)=
	#1 date1 < #1 date2

fun oldest (date : (int * int * int) list) =
    if null date
        then NONE
    else
        let fun get_oldest (date : (int * int * int) list) =
                if null (tl date)
                then hd date
                else
                    let
                        val back = get_oldest (tl date)
                        val front = hd date
                    in
                        if is_older (front, back)
                        then front
                        else back
                    end
        in
            SOME (get_oldest date)
        end

    
fun oldest (date : (int * int * int) list) =
    if null date
        then NONE
    else
        let fun get_oldest (date : (int * int * int) list) =
                if null (tl date)
                then hd date
                else
                    let
                        val back = get_oldest (tl date)
                        val front = hd date
                    in
                        if is_older (front, back)
                        then front
                        else back
                    end
        in
            SOME (get_oldest date)
        end

fun isLessThan (r1: {date:string, id:int}, r2:{date:string,id:int}) = 
  #id r1 < #id r2

fun isLessThan2 ({date=a, id=b},{date=c,id=d}) = b<d

fun message3(grade: char) = 
  case grade
   of #"A" => "very good work"
    | #"B"  => "good work"
    | #"C"  => "average work"
    | #"D"  => "poor work"
    | _    => "hmmm"
    
fun message2 #"A" = "very good work"
	| message2 #"B" = "good work"
	| message2 #"C" = "average work"
	| message2 #"D" = "poor work"
	| message2   _  = "hmmm"
	

datatype card = Ace | King | Queen | Jack | Num of int

fun valueInSkat (c: card) = 
  case c of
      Ace => 11
   | King => 4
   | Queen =>3
   | Jack => 2
   | Num(x)  => case x of 10 => 10
		       | _  =>  0 

fun valueInRummy(c: card) = 
  case c of Ace => 1
	 | King => 10
	 | Queen =>10
	 | Jack => 10
	 | Num(x) => x

datatype number = I of int| R of real

fun addNumbers(num1, num2) = 
  case num1 of 
      I x => (case num2 of 
		 I y => real(x+y)
	      | R y => real(x) + y)
   | R x  => (case num2 of
		  I y => x + real(y)
	       | R y => x + y)	     
