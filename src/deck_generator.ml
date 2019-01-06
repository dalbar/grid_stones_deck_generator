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
    if cur_rows > size then res 
    else 
      let new_res = 
        List.map res (fun matrix -> append_matrix matrix rows) |. List.flatten in
      loop (cur_rows + 1) new_res in
  loop 0 rows |. filter_dups_from_list |. List.keep (fun array -> has_at_least_n_stones array min_stones) |. List.map (fun matrix -> shape_quad size matrix)

let write_deck deck dest= 
  writeFileSync dest deck `ascii


let generate_size_3_stones_6 () = 
  let deck_string = generate_pattern ~min_stones:5 3 6 |. List.map (fun pattern -> Js_json.stringifyAny pattern |. Option.getExn) |> String.concat ",\n" in
  String.concat "\n" ["{ \"deck\": ["; deck_string; "]}" ] |. write_deck "deck_3_6.json"
let () = generate_size_3_stones_6 ()