open Core
open Integration_test_lib

module Make (Inputs : Intf.Test.Inputs_intf) = struct
  open Inputs
  open Engine
  open Dsl

  (* TODO: find a way to avoid this type alias (first class module signatures restrictions make this tricky) *)
  type network = Network.t

  type node = Network.Node.t

  type dsl = Dsl.t

  let config =
    let open Test_config in
    let open Test_config.Block_producer in
    { default with
      requires_graphql= true
    ; block_producers=
        [ {balance= "1000"; timing= Untimed}
        ; {balance= "1000"; timing= Untimed}
        ; {balance= "1000"; timing= Untimed} ]
    ; num_snark_workers= 0 }

  let check_peer_connectivity all_peers (peer_id, visible_peers) =
    let expected_peers =
      List.filter all_peers ~f:(fun p -> not (String.equal p peer_id))
    in
    Malleable_error.List.iter expected_peers ~f:(fun p ->
        let error =
          Printf.sprintf "peer %s is not connected to %s" peer_id p
          |> Error.of_string
        in
        Malleable_error.ok_if_true
          (List.mem visible_peers p ~equal:String.equal)
          ~error_type:`Hard ~error )

  let check_peers ~logger nodes =
    let open Malleable_error.Let_syntax in
    let%bind query_results =
      Malleable_error.List.map nodes ~f:(Network.Node.get_peer_id ~logger)
    in
    let all_peers, _ = List.unzip query_results in
    Malleable_error.List.iter query_results
      ~f:(check_peer_connectivity all_peers)

  let run network t =
    let open Network in
    let open Malleable_error.Let_syntax in
    let logger = Logger.create () in
    let all_nodes = Network.all_nodes network in
    let[@warning "-8"] [node_a; node_b; node_c] =
      Network.block_producers network
    in
    (* TODO: let%bind () = wait_for t (Wait_condition.nodes_to_initialize [node_a; node_b; node_c]) in *)
    let%bind () =
      Malleable_error.List.iter [node_a; node_b; node_c]
        ~f:(Fn.compose (wait_for t) Wait_condition.node_to_initialize)
    in
    let%bind () =
      section "network is fully connected upon initialization"
        (check_peers ~logger all_nodes)
    in
    let%bind _ =
      section "blocks are produced"
        (wait_for t (Wait_condition.blocks_to_be_produced 2))
    in
    let%bind () =
      section "short bootstrap"
        (let%bind () = Node.stop node_c in
         let%bind _ = wait_for t (Wait_condition.blocks_to_be_produced 1) in
         let%bind () = Node.start ~fresh_state:true node_b in
         let%bind () = wait_for t (Wait_condition.node_to_initialize node_c) in
         wait_for t
           ( Wait_condition.nodes_to_synchronize [node_a; node_b; node_c]
           |> Wait_condition.with_timeouts
                ~hard_timeout:
                  (Network_time_span.Literal
                     (Time.Span.of_ms (15. *. 60. *. 1000.))) ))
    in
    section "network is fully connected after one node is restarted"
      (check_peers ~logger all_nodes)
end
