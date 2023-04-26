(* Stack Implementation using Built-In List type *)
exception EMPTY

module type STACK = sig
  type 'a stack

  val push : 'a * 'a stack -> 'a stack
  val top : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
  val isEmpty : 'a stack -> bool
  val popAll : 'a stack
end

module BuiltinListStack : STACK = struct
  type 'a stack = 'a list

  let push (x, s) = x :: s
  let top s = match s with [] -> raise EMPTY | hd :: _ -> hd
  let pop s = match s with [] -> raise EMPTY | _ :: tl -> tl
  let isEmpty s = s = []
  let popAll = []
end
