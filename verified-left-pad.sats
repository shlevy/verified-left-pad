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

(* TODO prove int_list_append is a total function *)

(* Repeat an int a given number of times into a list of ints *)
dataprop int_list_repeat (int (* element *), int (* count *), int_list)
  = {e : int} int_list_repeat_base (e, 0, int_list_nil)
  | {e : int} {n : nat} {out_tl : int_list}
      int_list_repeat_recurse (e, n + 1, int_list_cons (e, out_tl)) of
        int_list_repeat (e, n, out_tl)

(* TODO prove repeat is a total function *)

(* An array of word8s whose entire contents are known statically *)
dataview word8_known_array
  ( int_list (* the values in each cell *)
  , addr (* the address of the array *)
  ) =
    | {l : addr} word8_known_array_nil (int_list_nil, l)
    | {l : addr} {w : int8} {ws : int_list}
        word8_known_array_cons (int_list_cons (w, ws), l) of
          (char (w) @ l, word8_known_array (ws, l + sizeof(char (w))))

(* Left-pad the input array with count instances of el *)
fun left_pad {input : int_list} {count : int} {el : int} {l : addr}
  (!word8_known_array (input, l) | ptr l, int count, int el) :
    [l' : agz] [pad : int_list] [output : int_list]
      ( int_list_repeat (el, count, pad)
      , int_list_append (pad, input, output)
      , word8_known_array (output, l')
      , mfree_gc_v (l')
      | ptr l'
      )

(* TODO can we parameterize over an allocation function + view? *)
(* TODO return an optional (or an either err_code?) instead of
   pretending malloc can't fail (inherited from ats stdlib) *)
(* TODO helper function to safely free a gc'able known_array *)
(* TODO parameterize over list view so we can support e.g. fill-pointer
   based allocation/copy optimizations *)
