exception EMPTY

module type STACK = sig
  type 'a stack

  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val push : 'a * 'a stack -> 'a stack
  val top : 'a stack -> 'a (* raises EMPTY* if stack is empty *)
  val pop : 'a stack -> 'a stack (* raises EMPTY* if stack is empty *)
end

module BuiltinListStack : STACK = struct
  type 'a stack = 'a list

  let empty = []
  let isEmpty s = s = []
  let push (x, s) = x :: s
  let top s = match s with [] -> raise EMPTY | hd :: _ -> hd
  let pop s = match s with [] -> raise EMPTY | _ :: tl -> tl
end
