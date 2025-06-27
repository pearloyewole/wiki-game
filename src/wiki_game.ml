open! Core

(*let node_to_text node =
  let open Soup in
  texts node |> String.concat ~sep:"" |> String.strip
  ;; *)

let get_url node =
  let open Soup in
  attribute "href" node
;;

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)

let get_linked_articles contents : string list =
  let open Soup in
  let document_node = parse contents in
  let url_nodes = document_node $$ "a" |> to_list in
  let checking_no_namespace url =
    Option.is_none (Wikipedia_namespace.namespace url)
  in
  let urls : string list =
    List.filter_map url_nodes ~f:(fun a -> get_url a)
  in
  List.filter urls ~f:(fun b -> checking_no_namespace b)
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Carnivore"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Animal
    /wiki/Feliformia
    /wiki/Caniformia
    |}]
;;

let%expect_test "get_linked_articles" =
  (* This test uses existing files on the filesystem. *)
  let contents =
    File_fetcher.fetch_exn
      (Local (File_path.of_string "../resources/wiki"))
      ~resource:"Domestication_of_the_cat"
  in
  List.iter (get_linked_articles contents) ~f:print_endline;
  [%expect
    {|
    /wiki/Cat 
    |}]
;;

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

module Wiki_pair = struct
  type t =
    { parent_page : string
    ; child_page : string
    }
  [@@deriving hash, compare, sexp]
end

module G = Graph.Imperative.Graph.Concrete (String)

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)
let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let find_contents wiki_url =
    File_fetcher.fetch_exn how_to_fetch ~resource:wiki_url
  in
  let visited = String.Hash_set.create () in
  let to_visit = Queue.create () in
  Queue.enqueue to_visit origin;
  let edges = Hash_set.create (module Wiki_pair) in
  let depth = 0 in
  let rec traverse_wiki () =
    match Queue.dequeue to_visit with
    | None -> ()
    | Some current_wiki ->
      if Hash_set.mem visited current_wiki || depth > max_depth
      then traverse_wiki ()
      else (
        Hash_set.add visited current_wiki;
        let contents = find_contents current_wiki in
        let links = get_linked_articles contents in
        List.iter links ~f:(fun link ->
          Hash_set.add
            edges
            { Wiki_pair.parent_page = current_wiki; child_page = link };
          Queue.enqueue to_visit link));
      traverse_wiki ()
  in
  traverse_wiki
    ()
    dot_output_graph
    ~max_depth
    ~origin
    ~output_file
    ~how_to_fetch
    ()
;;

let dot_output_graph ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () =
  let graph = G.create () in
  let wiki_pages = edges in
  Hash_set.iter
    edges
    ~f:(fun { Wiki_pair.parent_page; Wiki_pair.child_page } ->
      G.add_edge graph parent_page child_page);
  Dot.output_graph (Out_channel.create (File_path.to_string output_file))
;;

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing articles and generate a graph visualizing links \
       to articles"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
