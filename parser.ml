#use "reader.ml";;
#use "pc.ml";;
open PC;;

let spaces = star (char ' ');; 

let nt_digit = range '0' '9';;

let nt_hexDigit = disj (disj (range 'a' 'f') (range 'A' 'F')) nt_digit;;

let make_enclosed l p r =
  let enclosed = caten (caten l p) r in
  pack enclosed (fun ((l, p), r) -> p);;

let lparen =
  let lp = char '(' in
    make_enclosed spaces lp spaces;;

let rparen =
  let rp = char ')' in
    make_enclosed spaces rp spaces;;

let nt_natural =
  let digits = plus nt_digit in
    pack digits (fun (ds) -> Int(int_of_string (list_to_string ds)));;


let nt_boolean = 
  let char_to_bool ch =
    if (ch = 't') then true
    else false in 
      let _bool_t ch =
      let _hash = char '#' in 
      let  _bool = char ch in 
      let parse = caten (caten (caten _spaces hash) _bool ) _spaces in
      pack _parse (fun (((l,h),b),r) -> Bool(char_to_bool b)) in
    disj (_bool_t 'f') (_bool_t 't');;

let nt_charPrefix = 
  let _hash = char '#' in 
  let _char = char '\\' in
  let parse = caten (caten _spaces _hash) _char in
    pack _parse (fun ((l, h), c) -> (h, c));;