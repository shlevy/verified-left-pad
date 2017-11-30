(* A sort-level list of ints *)
datasort int_list
  = int_list_nil of ()
  | int_list_cons of (int, int_list)

(* Append one sort-level list of ints to another *)
dataprop int_list_append (int_list, int_list, int_list)
  = {is : int_list} int_list_append_base (int_list_nil, is, is)
  | {hd : int} {in_tl : int_list} {is : int_list} {out_tl : int_list}
      int_list_append_recurse ( int_list_cons (hd, in_tl)
                              , is
                              , int_list_cons (hd, out_tl)
                              ) of int_list_append (in_tl, is, out_tl)

prfun int_list_append_function {is1, is2, out1, out2 : int_list}
  ( int_list_append (is1, is2, out1)
  , int_list_append (is1, is2, out2)
  ) : [out1==out2] void

prfun int_list_append_total {is1, is2 : int_list} :
  [out : int_list] int_list_append (is1, is2, out)

(* Repeat an int a given number of times into a list of ints *)
dataprop int_list_repeat (int (* element *), int (* count *), int_list)
  = {e : int} int_list_repeat_base (e, 0, int_list_nil)
  | {e : int} {n : nat} {out_tl : int_list}
      int_list_repeat_recurse (e, n + 1, int_list_cons (e, out_tl)) of
        int_list_repeat (e, n, out_tl)

prfun int_list_repeat_function
  {el, count : int} {out1, out2 : int_list}
    ( int_list_repeat (el, count, out1)
    , int_list_repeat (el, count, out2)
    ) : [out1==out2] void

prfun int_list_repeat_total
  {el, count : int} : [out : int_list] int_list_repeat (el, count, out)

(* The length of a sort-level list of ints *)
dataprop int_list_length (int_list, int)
  = int_list_length_base (int_list_nil, 0)
  | {hd : int} {tl : int_list} {out_pred : nat}
      int_list_length_recurse (int_list_cons (hd, tl), out_pred + 1)
        of int_list_length (tl, out_pred)

prfun int_list_length_function {is : int_list} {out1, out2 : int}
  ( int_list_length (is, out1)
  , int_list_length (is, out2)
  ) : [out1==out2] void

prfun int_list_length_total {is : int_list} :
  [n : nat] int_list_length (is, n)

(* A view of an array of word8s whose contents are known statically *)
dataview word8_known_array_v
  ( int_list (* the values in each cell *)
  , addr (* the address of the array *)
  ) =
    | {l : addr} word8_known_array_nil (int_list_nil, l)
    | {l : addr} {w : int8} {ws : int_list}
        word8_known_array_cons (int_list_cons (w, ws), l) of
          ( char (w) @ l
          , word8_known_array_v (ws, l + sizeof(char (w)))
          )

(* An array whose contents are known statically *)
viewtypedef word8_known_array (is : int_list, l : addr) =
   [n : nat] @{ valid_array = word8_known_array_v (is, l)
              , array_len = int_list_length (is, n)
              | arr = ptr l
              , sz  = size_t n
              }

(* Left-pad the input array with count instances of el *)
fun left_pad {input : int_list} {count : int} {el : int} {l : addr}
  (!word8_known_array (input, l), int count, int el) :
    [l' : agz] [pad : int_list] [output : int_list]
      ( int_list_repeat (el, count, pad)
      , int_list_append (pad, input, output)
      , word8_known_array_v (output, l')
      , mfree_gc_v (l')
      | ptr l'
      )

(* TODO can we parameterize over an allocation function + view? *)
(* TODO return an optional (or an either err_code?) instead of
   pretending malloc can't fail (inherited from ats stdlib)
 *)
(* TODO helper function to safely free a gc'able known_array *)
(* TODO parameterize over list view so we can support e.g. fill-pointer
   based allocation/copy optimizations
 *)
(* TODO Functions to convert to/from normal ATS arrays *)
