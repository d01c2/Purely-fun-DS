exception EMPTY
exception SUBSCRIPT

module type STACK = sig
  type 'a stack

  val empty : 'a stack
  val isEmpty : 'a stack -> bool
  val cons : 'a * 'a stack -> 'a stack
  val head : 'a stack -> 'a (* raises EMPTY* if stack is empty *)
  val tail : 'a stack -> 'a stack (* raises EMPTY* if stack is empty *)
  val ( ++ ) : 'a stack -> 'a stack -> 'a stack
  val update : 'a stack * int * 'a -> 'a stack
end

module BuiltinListStack : STACK = struct
  type 'a stack = 'a list

  let empty = []
  let isEmpty s = s = []
  let cons (x, s) = x :: s
  let head s = match s with [] -> raise EMPTY | hd :: _ -> hd
  let tail s = match s with [] -> raise EMPTY | _ :: tl -> tl
  let ( ++ ) = ( @ )

  let rec update (s, i, x) =
    match s with
    | [] -> raise SUBSCRIPT
    | hd :: tl -> if i = 0 then x :: tl else hd :: update (tl, i - 1, x)
end

module CustomStack : STACK = struct
  type 'a stack = Nil | Cons of 'a * 'a stack

  let empty = Nil
  let isEmpty s = match s with Nil -> true | _ -> false
  let cons (x, s) = Cons (x, s)
  let head s = match s with Nil -> raise EMPTY | Cons (hd, _) -> hd
  let tail s = match s with Nil -> raise EMPTY | Cons (_, tl) -> tl
  let rec ( ++ ) xs ys = if isEmpty xs then ys else Cons (head xs, tail xs ++ ys)

  let rec update (s, i, x) =
    match s with
    | Nil -> raise SUBSCRIPT
    | Cons (hd, tl) ->
        if i = 0 then Cons (x, tl) else Cons (hd, update (tl, i - 1, x))
end
