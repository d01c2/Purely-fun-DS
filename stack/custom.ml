exception EMPTY

module type STACK = sig
  type 'a stack

  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val push : 'a * 'a stack -> 'a stack
  val top : 'a stack -> 'a (* raises EMPTY* if stack is empty *)
  val pop : 'a stack -> 'a stack (* raises EMPTY* if stack is empty *)
end

module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil
  let isEmpty s = match s with Nil -> true | _ -> false
  let push (x, s) = Cons (x, s)
  let top s = match s with Nil -> raise EMPTY | Cons (hd, _) -> hd
  let pop s = match s with Nil -> raise EMPTY | Cons (_, tl) -> tl
end
