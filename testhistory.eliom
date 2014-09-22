{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module Testhistory_app =
  Eliom_registration.App (
    struct
      let application_name = "testhistory"
    end)


let main_service = Eliom_service.App.service ~path:["h"] ~get_params:Eliom_parameter.any ()

open Eliom_content.Html5.D

let cats =
  [ ("Fluffy",   "http://placekitten.com/200/210")
  ; ("Socks",    "http://placekitten.com/280/280")
  ; ("Whiskers", "http://placekitten.com/350/350")
  ; ("Bob",      "http://placekitten.com/320/270")
  ]

{client{
open Firebug
open Eliom_content.Html5

class type the_state = object (*
  method content : Js.js_string Js.t Js.prop
  method photo: Js.js_string Js.t Js.prop *)
  method name  : Js.js_string Js.t Js.prop
end

let make_state (name: string) =
  Js.Unsafe.obj [| ("name", Js.Unsafe.inject @@ Js.string name) |]

let get_state (o: Js.Unsafe.any) : the_state Js.t =
  let st : _ Js.t = Obj.magic o in
  st

}}


let content () =
  let pcontent = p ~a:[a_id "content" ] [ pcdata "Just Bob."] in
  let src : Xml.uri wrap = uri_of_string (fun () -> List.hd cats |> snd) in
  let photo = img ~a:[a_id "photo"] ~src ~alt:"" () in

  let update_content = {string -> string -> unit{ fun name url ->
    (Js.Unsafe.coerce (To_dom.of_p %pcontent))##textContent <- Js.string name;
    (To_dom.of_img %photo)##src <- Js.string url
  }}
  in

  let onload = {Dom_html.event Js.t -> unit{ fun _ev ->
   let () = match Lochash.get_value "mode" with
     | Some x when int_of_string @@ Js.to_string x < List.length %cats ->
        let q = Js.to_string x in
        let url = List.assoc q %cats in
        let new_url = Printf.sprintf "?mode=%s" q in
        %update_content q url;
        let st = make_state q in
        console##log_2 (Js.string "q = ", Js.string q);
        Dom_html.window##history##replaceState (st, Js.string q, Js.string new_url)

     | None ->
        let (name,url) = List.hd %cats in
        let new_url = Printf.sprintf "?mode=%s" name in
        %update_content name url;
        console##log_2 (Js.string "create initial state: name = ", Js.string name);
        let st = make_state name in
        console##log_2 (Js.string "state created:", st );
        console##log_3 (Js.string "current state: ", Dom_html.window##history, Dom_html.window##history##state);
        console##log (Js.string "updating state");
        Dom_html.window##history##replaceState (st, Js.string name, Js.string new_url);
        console##log_3 (Js.string "current state: ", Dom_html.window##history, Dom_html.window##history##state);
        ()

     | Some _ -> ()
   in

   Dom_html.window##onpopstate <- Dom_html.handler (fun (_ev: Dom_html.popStateEvent Js.t) ->
     let st: the_state Js.t = get_state _ev##state in
     let name = Js.to_string st##name in
     console##log_2 (Js.string "state: ", st);
     console##log_2 (Js.string "onpopstate, name = ", st##name);
     let url = List.assoc name %cats in
     %update_content name url;
     Js._true
   );
  }}
  in

  let f = fun i (name,url) ->
    let onclick = {Dom_html.mouseEvent Js.t->unit{ fun _ev ->
      %update_content %name %url;
      let new_url = Printf.sprintf "?mode=%s" %name in
      console##log_2 (Js.string "2", Js.string %name);
      let state_obj = make_state %name in
      console##log (Js.string "3");
      Js.Unsafe.global##ggg <- state_obj;
      console##log (Js.string "4");
      console##log_2 (Js.string "push state", Js.string @@ string_of_int %i);
      console##log_2 (Js.string "state object: ", state_obj);
      Dom_html.window##history##pushState (state_obj, Js.Optdef.return @@ Js.string %name, Js.string new_url);
      Dom.preventDefault _ev;
    }}
    in
    li [ Raw.a ~a:[a_onclick onclick] [pcdata name] ]
  in
  let lis = List.mapi f cats in

  body ~a:[a_class ["cf"]; a_onload onload]
       [ h1 [pcdata "kittens"]
       ; nav ~a:[]
             [ ul ~a:[a_class ["cf"]] lis
             ]
       ; pcontent
       ; photo
       ]


let () =
  Testhistory_app.register
    ~service:main_service
    (fun _ () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"testhistory"
           ~css:[["css";"testhistory.css"]]
           (content ())
        )
    )
