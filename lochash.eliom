{client{
open Firebug
open Printf
open Js

let to_jsstring () = Dom_html.window##location##hash

type hash_t = ((js_string t) * (js_string t)) list

let read_hash () : hash_t =
  let split_amp s : hash_t =
    let arr = s##split (Js.string "&") |> Js.str_array |> Js.to_array in
    let f s =
      let pos = s##indexOf (Js.string "=") in
      if pos<0 then (s, Js.string "")
      else (s##slice (0, pos), s##slice_end (pos+1))
    in
    Array.fold_right (fun item acc -> f item :: acc) arr []
  in
  let hash = Dom_html.window##location##hash in
  let hash_pos = hash##indexOf (Js.string "?") in
  if hash_pos < 0 then []
  else split_amp (hash##slice_end (hash_pos+1) )

let set_helper' (k,v) xs =
  let postfix = List.fold_left xs
                  ~f:(fun acc (k,v) -> acc##concat_4 (Js.string "&", k, Js.string "=", v) )
                  ~init:(Js.string "")
  in
  let hash = Dom_html.window##location##hash in
  let prefix =
    match hash##indexOf (Js.string "#") with
    | -1 -> hash
    |  n -> hash##slice (0, n)
  in
  let ans = prefix##concat_4 (k, Js.string "=", v, postfix) in
  Dom_html.window##location##hash <- prefix##concat_2 (Js.string "#", ans)

let get_value_exn (k: string) : js_string t =
  let key = Js.string k in
  ListLabels.assoc key (read_hash())

let get_value key =
  try Some(get_value_exn key)
  with Not_found -> None


   }}
