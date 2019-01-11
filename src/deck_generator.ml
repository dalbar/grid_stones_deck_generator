open Belt
open Node.Fs

exception DIM of string

let generate_one_stone_permutations row = 
  let fill_copy idx = Array.copy row |. fun row -> Array.setExn row idx 1; row in  
  let rec loop perm idx =
    try
      let cur_value = Array.getExn row idx in 
      match cur_value with
      | -1 ->  List.add perm (fill_copy idx) |. loop (idx+1) 
      | _ -> loop perm  (idx + 1)
    with _ -> perm 
    in  loop (List.make 0 row) 0

let filter_dups_from_list perm = 
  let no_dup = List.make 0 (Array.make 0 0) in
  List.reduce perm no_dup (fun acc row -> if List.has acc row (fun l1 l2 -> Array.eq l1 l2 (=) ) then acc else List.add acc row)

let generate_row_permutations size max_stones =
  let empty_row = Array.make size (-1) in
  let res = List.make 1 empty_row in
  let rec loop cur_stones latest_rows res = 
    if cur_stones > max_stones then res 
    else 
      let new_rows =
       List.map latest_rows (fun row -> generate_one_stone_permutations row) |. List.flatten in 
      loop (cur_stones + 1) new_rows (List.concat res latest_rows) in
  let with_dup = loop 0 res (List.make (-1) empty_row) in
  filter_dups_from_list with_dup

let append_matrix matrix rows = 
  let res = List.make 0 (Array.make 0 0) in
  List.reduce rows res (fun acc row -> Array.concat matrix row |> List.add acc)

let shape_quad m array = 
  let len = Array.length array in
  if len mod m > 0 then raise (DIM "Dimensions do not match!") 
  else
    let res = Array.make (len/m) (Array.make 0 0) in
    let rec loop cur_idx = 
      if cur_idx > len - m then ()
      else 
        let new_row = Array.slice array ~offset:cur_idx ~len:m in
        Array.setExn res (cur_idx/m) new_row;
        loop (cur_idx + m) in
    loop 0;
    res

let has_at_least_n_stones array n = 
  Array.reduce array 0 (fun acc value -> if value = 1 then acc + 1 else acc) |. (>=) n
 
let generate_pattern ?(min_stones = 0) size max_stones = 
  let rows = generate_row_permutations size max_stones in 
  let rec loop cur_rows res = 
    if cur_rows = size then res 
    else 
      let new_res = 
        List.map res (fun matrix -> append_matrix matrix rows) |. List.flatten in
      loop (cur_rows + 1) new_res in
  loop 1 rows |. filter_dups_from_list |. List.keep (fun array -> has_at_least_n_stones array min_stones) |. List.map (fun matrix -> shape_quad size matrix)

let write_deck deck dest= 
  writeFileSync dest deck `ascii

let get_unsafe matrix x_ind y_ind = 
  Array.getExn matrix y_ind |. Array.getExn x_ind

let set_unsafe matrix x_ind y_ind value =
  Array.getExn matrix y_ind |. Array.setExn x_ind value

let rot90_square matrix = 
  let dim = Array.length matrix in
  let rotated = Array.make dim (-1) |. Array.map (Array.make dim) in 
  for i = 0 to dim - 1 do 
    for j = 0 to dim - 1 do
      get_unsafe matrix j i |> set_unsafe rotated (dim - i - 1) j
    done
  done; 
  rotated

let get_array_dim m1 = 
  let m = Array.length m1 in
  let n = Array.length (Array.getExn m1 0) in
  (n, m)

let cmp_matrix_square m1 m2 = 
  let dim = Array.length m1 in
  let rec loop idx =
    if idx = dim then 1 else 
    let row_m1 = Array.getExn m1 idx in 
    let row_m2 = Array.getExn m2 idx in 
    if Array.eq  row_m1 row_m2 (=) then
      loop (idx + 1)
    else -1 in
  loop 0
    
let eq_matrix_square m1 m2 = 
  if cmp_matrix_square m1 m2 = 1 then true else false

let is_rot_equal m1 m2 = 
  let m2_rot_90 = rot90_square m2 in
  let m2_rot_180 = rot90_square m2_rot_90 in
  let m2_rot_270 = rot90_square m2_rot_180 in 
  let rec loop matrices = 
    match matrices with 
    | [] -> false
    | y::ys -> if eq_matrix_square m1 y then true else loop ys in 
  loop [m2; m2_rot_90; m2_rot_180; m2_rot_270] 

let make_n_m_matrix n m = 
  Array.make n 0 |. Array.map (Array.make m)

let filter_rot_equal matrices = 
  let not_rot_equal = List.make 0 (make_n_m_matrix 0 0) in
  List.reduce matrices not_rot_equal (fun acc row -> if List.has acc row is_rot_equal then acc else List.add acc row)

let generate_size_3_stones_6 () = 
  let deck_string = generate_pattern ~min_stones:5 3 6 |. filter_rot_equal |. List.map (fun pattern -> Js_json.stringifyAny pattern |. Option.getExn) |> String.concat ",\n" in
  String.concat "\n" ["{ \"deck\": ["; deck_string; "]}" ] |. write_deck "deck_3_6.json"
  

let m1 () = let test_mat = make_n_m_matrix 3 3 in 
  set_unsafe test_mat 0 0 1; 
  set_unsafe test_mat 1 0 2;
  set_unsafe test_mat 2 0 3;
  set_unsafe test_mat 0 1 4;
  set_unsafe test_mat 1 1 5;
  set_unsafe test_mat 2 1 6;
  set_unsafe test_mat 0 2 7;
  set_unsafe test_mat 1 2 8;
  set_unsafe test_mat 2 2 9;
  rot90_square test_mat |. rot90_square 

let m2 () = let test_mat = Array.make 3 0 |. Array.map (Array.make 3) in 
  set_unsafe test_mat 0 0 1; 
  set_unsafe test_mat 1 0 2;
  set_unsafe test_mat 2 0 3;
  set_unsafe test_mat 0 1 4;
  set_unsafe test_mat 1 1 5;
  set_unsafe test_mat 2 1 6;
  set_unsafe test_mat 0 2 7;
  set_unsafe test_mat 1 2 8;
  set_unsafe test_mat 2 2 9;
  test_mat

let () = generate_size_3_stones_6 ()
