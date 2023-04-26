(* Stack Implementation using Custom data type *)
exception EMPTY

module type STACK = sig
  type 'a stack

  val push : 'a * 'a stack -> 'a stack
  val top : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
  val isEmpty : 'a stack -> bool
  val popAll : 'a stack
end

module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let push (x, s) = Cons (x, s)
  let top s = match s with Nil -> raise EMPTY | Cons (hd, _) -> hd
  let pop s = match s with Nil -> raise EMPTY | Cons (_, tl) -> tl
  let isEmpty s = match s with Nil -> true | _ -> false
  let popAll = Nil
end
