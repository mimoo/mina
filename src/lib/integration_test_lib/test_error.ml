open Core_kernel

type remote_error = {node_id: string; error_message: Logger.Message.t}

(* NB: equality on internal errors ignores timestamp *)
type internal_error =
  { occurrence_time: Time.t sexp_opaque
        [@equal fun _ _ -> true] [@compare fun _ _ -> 0]
  ; error: Error.t
        [@equal
          fun a b ->
            String.equal (Error.to_string_hum a) (Error.to_string_hum b)]
        [@compare
          fun a b ->
            String.compare (Error.to_string_hum a) (Error.to_string_hum b)] }
[@@deriving eq, sexp, compare]

type t = Remote_error of remote_error | Internal_error of internal_error

let raw_internal_error error = {occurrence_time= Time.now (); error}

let internal_error error = Internal_error (raw_internal_error error)

let internal_error_from_raw error = Internal_error error

let to_string = function
  | Remote_error {node_id; error_message} ->
      Printf.sprintf "[%s] %s: %s"
        (Time.to_string error_message.timestamp)
        node_id
        (Yojson.Safe.to_string (Logger.Message.to_yojson error_message))
  | Internal_error {occurrence_time; error} ->
      Printf.sprintf "[%s] test_executive: %s"
        (Time.to_string occurrence_time)
        (Error.to_string_hum error)

let occurrence_time = function
  | Remote_error {error_message; _} ->
      error_message.timestamp
  | Internal_error {occurrence_time; _} ->
      occurrence_time

let compare_time a b = Time.compare (occurrence_time a) (occurrence_time b)

(* TODO: make polymorphic over error type *)
(* currently a flat set of contexts mapped to errors, but perhaps a tree (for nested contexts) is better *)
module Error_accumulator = struct
  type 'error t =
    { from_current_context: 'error list
    ; contextualized_errors: 'error list String.Map.t }
  [@@deriving eq, sexp_of, compare]

  let record_errors map context errors =
    String.Map.update map context ~f:(fun errors_opt ->
        errors @ Option.value errors_opt ~default:[] )

  let empty =
    {from_current_context= []; contextualized_errors= String.Map.empty}

  let error_count {from_current_context; contextualized_errors} =
    List.length from_current_context + String.Map.length contextualized_errors

  let all_errors {from_current_context; contextualized_errors} =
    from_current_context
    @ List.bind (String.Map.to_alist contextualized_errors) ~f:snd

  let contextualize context {from_current_context; contextualized_errors} =
    { empty with
      contextualized_errors=
        record_errors contextualized_errors context from_current_context }

  let singleton x = {empty with from_current_context= [x]}

  let of_context_free_list ls = {empty with from_current_context= ls}

  let of_contextualized_list context ls =
    { empty with
      contextualized_errors= record_errors String.Map.empty context ls }

  let add t error =
    {t with from_current_context= error :: t.from_current_context}

  let map {from_current_context; contextualized_errors} ~f =
    { from_current_context= List.map from_current_context ~f
    ; contextualized_errors=
        String.Map.map contextualized_errors ~f:(List.map ~f) }

  let merge (a : 'e t) (b : 'e t) : 'e t =
    let from_current_context =
      a.from_current_context @ b.from_current_context
    in
    let contextualized_errors =
      String.Map.fold a.contextualized_errors ~init:b.contextualized_errors
        ~f:(fun ~key ~data m -> record_errors m key data)
    in
    {from_current_context; contextualized_errors}

  let combine = List.fold ~init:empty ~f:merge

  let partition {from_current_context; contextualized_errors} ~f =
    let from_current_context_a, from_current_context_b =
      List.partition_tf from_current_context ~f
    in
    let contextualized_errors_a, contextualized_errors_b =
      String.Map.fold contextualized_errors
        ~init:(String.Map.empty, String.Map.empty) ~f:(fun ~key ~data (a, b) ->
          let a_data, b_data = List.partition_tf data ~f in
          (record_errors a key a_data, record_errors b key b_data) )
    in
    let a =
      { from_current_context= from_current_context_a
      ; contextualized_errors= contextualized_errors_a }
    in
    let b =
      { from_current_context= from_current_context_b
      ; contextualized_errors= contextualized_errors_b }
    in
    (a, b)
end

module Set = struct
  type nonrec t =
    {soft_errors: t Error_accumulator.t; hard_errors: t Error_accumulator.t}

  let empty =
    {soft_errors= Error_accumulator.empty; hard_errors= Error_accumulator.empty}

  let all_errors {soft_errors; hard_errors} =
    Error_accumulator.merge soft_errors hard_errors

  let soft_singleton err =
    {empty with soft_errors= Error_accumulator.singleton err}

  let hard_singleton err =
    {empty with hard_errors= Error_accumulator.singleton err}

  let of_soft_or_error = function
    | Ok () ->
        empty
    | Error err ->
        soft_singleton (internal_error err)

  let of_hard_or_error = function
    | Ok () ->
        empty
    | Error err ->
        hard_singleton (internal_error err)

  let add_soft err t =
    {t with soft_errors= Error_accumulator.add t.soft_errors err}

  let add_hard err t =
    {t with hard_errors= Error_accumulator.add t.soft_errors err}

  let merge a b =
    { soft_errors= Error_accumulator.merge a.soft_errors b.soft_errors
    ; hard_errors= Error_accumulator.merge a.hard_errors b.hard_errors }

  let combine = List.fold_left ~init:empty ~f:merge

  let partition {soft_errors; hard_errors} ~f =
    let soft_errors_a, soft_errors_b =
      Error_accumulator.partition soft_errors ~f
    in
    let hard_errors_a, hard_errors_b =
      Error_accumulator.partition hard_errors ~f
    in
    let a = {soft_errors= soft_errors_a; hard_errors= hard_errors_a} in
    let b = {soft_errors= soft_errors_b; hard_errors= hard_errors_b} in
    (a, b)
end
