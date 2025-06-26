open! Core
module City = String

module Network = struct
  module Connection = struct
    module T = struct
      type t = City.t * City.t [@@deriving compare, sexp]
    end

    include Comparable.Make (T)

    (*exploring labeling *)

    let of_string s =
      let pair_strings lst = List.cartesian_product lst lst in
      (match String.split_on_chars ~on:[ ',' ] s with
       | _route_name :: cities -> pair_strings cities
       | [] -> [])
      |> List.map ~f:(fun (a, b) -> City.of_string a, City.of_string b)
    ;;

    let%expect_test "of_string" =
      (* This test uses existing files on the filesystem. *)
      let contents =
        "I-5,Seattle,Portland,Sacramento,LosAngeles,SanDiego\n\
         I-10,LosAngeles,Phoenix,Tucson,ElPaso,SanAntonio,Houston,NewOrleans,Tallahassee,Jacksonville\n\
         I-15,Sweetgrass,GreatFalls,Butte,IdahoFalls,Ogden,SaltLakeCity,Provo,LasVegas\n\
         I-20,Midland,Odessa,Shreveport,Monroe,Jackson,Birmingham,Atlanta,Augusta,Florence\n\
         I-25,Buffalo,Casper,Cheyenne,FortCollins,Denver,ColoradoSprings,Pueblo,SantaFe,Albuquerque,LasCruces\n\
         I-35,Duluth,Minneapolis,KansasCity,Wichita,OklahomaCity,FortWorth,Dallas,Austin,SanAntonio\n\
         I-40,Barstow,Needles,Kingman,Flagstaff,Albuquerque,Amarillo,OklahomaCity,LittleRock,Memphis,Nashville,Asheville,Raleigh\n\
         I-45,Dallas,Corsicana,Huntsville,Conroe,Houston,Galveston\n\
         I-55,Chicago,Springfield,St_Louis,CapeGirardeau,Memphis,Grenada,Jackson,Hammond,New_Orleans\n\
         I-64,St_Louis,Louisville,Lexington,Charleston,Richmond,Suffolk,Norfolk,Chesapeake\n\
         I-65,Gary,Chicago,Indianapolis,Louisville,Nashville,Birmingham,Mobile\n\
         I-70,GrandJunction,Denver,KansasCity,St_Louis,TerreHaute,Indianapolis,Columbus,Wheeling,Baltimore\n\
         I-75,Detroit,Toledo,Cincinnati,Lexington,Knoxville,Chattanooga,Atlanta,Macon,Gainesville,Tampa,Naples\n\
         I-80,SanFrancisco,Sacramento,Reno,SaltLakeCity,Cheyenne,Omaha,DesMoines,QuadCities,Chicago,Toledo,Cleveland\n\
         I-90,Spokane,Seattle,Billings,RapidCity,SiouxFalls,Minneapolis,Madison,Chicago,Toledo,Cleveland,Buffalo,Boston\n\
         I-95,Boston,Providence,NewYorkCity,Philadelphia,Baltimore,WashingtonDC,Richmond,Florence,Savannah,Jacksonville,Miami\n"
      in
      List.iter (of_string contents) ~f:(fun city_pair ->
        print_s [%message (city_pair : string * string)]);
      [%expect
        {|
        [("Seattle","Portland");("Sacramento","LosAngeles");"SanDiego" ]
    |}]
    ;;
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  let of_file input_file =
    In_channel.read_lines (File_path.to_string input_file)
    |> List.concat_map ~f:(fun s ->
      Connection.of_string s
      |> List.concat_map ~f:(fun (a, b) -> [ a, b; b, a ]))
    |> Connection.Set.of_list
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let network = Network.of_file input_file in
        printf !"%{sexp: Network.t}\n" network]
;;

module G = Graph.Imperative.Graph.Concrete (City)

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

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Network.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (city1, city2) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          G.add_edge graph city1 city2);
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
