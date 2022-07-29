open Mina_base
open Core_kernel

type 'a ancestry_status =
  | Processing of 'a Interruptible.t
  | Processed of 'a
  | Failed of Error.t

type 'a common_substate =
  { status : 'a ancestry_status
  ; received_via_gossip : bool
  ; children : State_hash.t list
  }

type 'v modifier =
  { modifier : 'a. 'a common_substate -> 'a common_substate * 'v }

type 'v viewer = { viewer : 'a. 'a common_substate -> 'v }

type ('a, 'b) modify_substate_t = f:'a modifier -> 'b -> ('b * 'a) option

let view ~(modify_substate : ('a, 'b) modify_substate_t) ~f =
  Fn.compose (Option.map ~f:snd)
    (modify_substate ~f:{ modifier = (fun st -> (st, f.viewer st)) })

module type State_functions = sig
  type state_t

  val modify_substate : ('a, state_t) modify_substate_t

  val header_with_hash :
    state_t -> Mina_block.Header.t State_hash.With_state_hashes.t

  val equal_state_levels : state_t -> state_t -> bool

  val logger : Logger.t
end

let collect_states (type state_t) ~state_functions ~transition_states top_state
    =
  let (module F : State_functions with type state_t = state_t) =
    state_functions
  in
  let open F in
  let is_to_be_continued =
    let f =
      { viewer =
          (fun s ->
            match (s.status, s.received_via_gossip) with
            | Failed _, _ | Processing _, false ->
                true
            (* Iteration is stopped once we encounter a Processed or a Processing with gossip received state  *)
            | _ ->
                false )
      }
    in
    Fn.compose (Option.value ~default:false) (view ~modify_substate ~f)
  in
  let rec loop state =
    let hh = header_with_hash state in
    let parent_hash =
      With_hash.data hh |> Mina_block.Header.protocol_state
      |> Mina_state.Protocol_state.previous_state_hash
    in
    Option.value_map ~default:[]
      ~f:(fun parent_state ->
        if equal_state_levels parent_state state then
          if is_to_be_continued state then state :: loop parent_state else []
        else
          (* Parent is of different state => it's of higher state => we don't need to go deeper *)
          [] )
      (State_hash.Table.find transition_states parent_hash)
  in
  loop top_state

(* Pre-condition: order of `processed` respects parent-child relationship and parent always comes first *)
let mark_processed (type state_t) ~state_functions ~transition_states processed
    =
  let (module F : State_functions with type state_t = state_t) =
    state_functions
  in
  let open F in
  let processed_set = ref (State_hash.Set.of_list processed) in
  let rec handle is_recursive_call hash =
    Option.value ~default:[]
    @@
    let open Option.Let_syntax in
    let is_in_processed_set = State_hash.Set.mem !processed_set hash in
    let%bind () =
      Option.some_if (is_in_processed_set || is_recursive_call) ()
    in
    let%bind state = State_hash.Table.find transition_states hash in
    processed_set := State_hash.Set.remove !processed_set hash ;
    let mark_processed_sm subst =
      match subst.status with
      | Failed e ->
          ( subst
          , Result.Error (sprintf "failed due to %s" (Error.to_string_mach e))
          )
      | Processing a when is_in_processed_set -> (
          match Interruptible.peek_result a with
          | None ->
              (subst, Result.Error "still processing or failed")
          | Some a_res ->
              ( { subst with status = Processed a_res }
              , Result.Ok (true, subst.children) ) )
      | Processing _ ->
          (* In resursive call, just skip this child *)
          (subst, Result.Ok (false, []))
      | Processed _ when not is_recursive_call ->
          (subst, Result.Error "already processed")
      | Processed _ ->
          (subst, Result.Ok (true, subst.children))
    in
    let%bind state', res =
      modify_substate ~f:{ modifier = mark_processed_sm } state
    in
    Option.iter ~f:(fun err -> [%log warn] "error %s" err) (Result.error res) ;
    let%bind is_processed, children = Result.ok res in
    let%bind () = Option.some_if is_processed () in
    State_hash.Table.set transition_states ~key:hash ~data:state' ;
    let%map () =
      (* Returns Some () iff the parent is in the higher state or to be promoted to the higher state *)
      if is_recursive_call then Some ()
      else
        let hh = header_with_hash state in
        let parent_hash =
          With_hash.data hh |> Mina_block.Header.protocol_state
          |> Mina_state.Protocol_state.previous_state_hash
        in
        let%bind parent_state =
          State_hash.Table.find transition_states parent_hash
        in
        Option.some_if (equal_state_levels parent_state state) ()
    in
    hash :: List.concat (List.map children ~f:(handle true))
  in
  List.concat @@ List.map processed ~f:(handle false)

(* let update_substates_on_gossip ~modify_substate ~transition_states ~state_hash
       state =
     let top_sm substate =
       if substate.received_via_gossip then (substate, [])
       else ({ substate with received_via_gossip = true }, substate.children)
     in
     let substate_modifier substate =
       if substate.received_via_gossip then (substate, [])
       else ({ substate with next_pillar = state_hash }, substate.children)
     in
     let rec update_descendants child_hash =
       Option.iter
         (State_hash.Table.find transition_states child_hash)
         ~f:
           (Fn.compose
              (Option.iter ~f:(fun (st, children) ->
                   State_hash.Table.set transition_states ~key:child_hash ~data:st ;
                   List.iter ~f:update_descendants children ) )
              (modify_substate ~f:{ substate_modifier }) )
     in
     Option.value_map ~default:state
       (modify_substate ~f:{ substate_modifier = top_sm } state)
       ~f:(fun (st', children) ->
         List.iter children ~f:update_descendants ;
         st' )

   let update_substate_on_processed ~modify_substate ~parent_state
       ~parent_state_hash state =
     let parent_sm pst =
       (pst, if pst.received_via_gossip then parent_state_hash else pst.next_pillar)
     in
     let next_pillar =
       Option.value_map ~f:snd ~default:parent_state_hash
       @@ modify_substate ~f:{ substate_modifier = parent_sm } parent_state
     in
     let substate_modifier subst = ({ subst with next_pillar }, next_pillar) in
     Option.value_map ~default:state ~f:fst
     @@ modify_substate ~f:{ substate_modifier } state *)
