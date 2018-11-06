#use "reader.ml";;
#use "pc.ml";;
open PC;;

let _spaces_ = star (char ' ');; 

let nt_digit = range '0' '9';;

let nt_hexDigit = disj (disj (range 'a' 'f') (range 'A' 'F')) nt_digit;;

let _lparen_ =
  let _lp_ = char '(' in
  let _spaced_ = caten (caten _spaces_ _lp_) _spaces_ in
    pack _spaced_ (fun ((l, p), r) -> p);;

let _rparen_ =
  let _rp_ = char ')' in
  let _spaced_ = caten (caten _spaces_ _rp_) _spaces_ in
      pack _spaced_ (fun ((l, p), r) -> p);;

let nt_natural =
  let _digits_ = plus nt_digit in
    pack _digits_ (fun (ds) -> Int(int_of_string (list_to_string ds)));;


let nt_boolean = 
  let char_to_bool ch =
    if (ch = 't') then true
    else false in 
      let _bool_t ch =
      let _hash = char '#' in 
      let  _bool = char ch in 
      let _parse = caten (caten (caten _spaces_ _hash) _bool ) _spaces_ in
      pack _parse (fun (((l,h),b),r) -> Bool(char_to_bool b)) in
    disj (_bool_t 'f') (_bool_t 't');;

let nt_charPrefix = 
  let _hash = char '#' in 
  let _char = char '\\' in
    let _parse = caten (caten _spaces_ _hash) _char in 
    pack _parse (fun ((l,h),b) -> (h,b));;

