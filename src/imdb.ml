open! Core

let get_property node =
  let open Soup in
  match attribute "property" node with Some content -> content | None -> ""
;;

let _get_content node =
  let open Soup in
  match attribute "content" node with Some credit -> credit | None -> ""
;;

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits. *)
let get_credits contents : string list =
  let open Soup in
  let document_node = parse contents in
  let credit_list_nodes = document_node $$ "meta" |> to_list in
  List.map credit_list_nodes ~f:(fun a -> get_property a)
;;

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
