type token =
  | Tok_Int of int
  | Tok_LParen
  | Tok_RParen
  | Tok_LCurly
  | Tok_RCurly
  | Tok_Equals
  | Tok_Semi
  | Tok_Plus
  | Tok_Minus
  | Tok_Astrisk
  | Tok_FSlash
  | Tok_ID of string
  | Tok_EOF

  exception InvalidInputException of string

let string_of_token tok =
  match tok with
  | (Tok_Int i) -> "(Tok_Int:" ^ (string_of_int i) ^ ")"
  | (Tok_LParen) -> "(Tok_LParen)"
  | Tok_RParen -> "(Tok_RParen)"
  | Tok_LCurly -> "(Tok_LCurly)"
  | Tok_RCurly -> "(Tok_RCurly)"
  | Tok_Equals -> "(Tok_Equals)"
  | Tok_Semi -> "(Tok_Semi)"
  | Tok_Plus -> "(Tok_Plus)"
  | Tok_Minus -> "(Tok_Minus)"
  | Tok_Astrisk -> "(Tok_Asterisk)"
  | Tok_FSlash -> "(Tok_FSlash)"
  | (Tok_ID str) -> "(Tok_ID:" ^ str ^ ")"
  | Tok_EOF -> "(Tok_EOF)"
let print_tokens toks =
  let rec tok toks =
    match toks with
    | [] -> "[]\n"
    | hd::tl -> (string_of_token hd) ^ "::" ^ (tok tl)
  in
  print_string (tok toks)

let regex_ws_char   = Str.regexp "[ \t\n]"
let regex_id        = Str.regexp "[_a-zA-Z][_a-zA-Z0-9]*"
let regex_int_lit   = Str.regexp "[0-9]+"
let regex_l_paren   = Str.regexp "("
let regex_r_paren   = Str.regexp ")"
let regex_l_curly   = Str.regexp "{"
let regex_r_curly   = Str.regexp "}"
let regex_semi      = Str.regexp ";"
let regex_equals    = Str.regexp "="
let regex_plus      = Str.regexp "+"
let regex_minus     = Str.regexp "-"
let regex_asterisk  = Str.regexp "*"
let regex_f_slash   = Str.regexp "/"



let tokenize input =
  let rec tok str pos =
    if pos >= String.length str then [Tok_EOF]
    else if Str.string_match regex_ws_char str pos then
      (tok str (pos + 1))
    else if Str.string_match regex_id str pos then
      let t = Str.matched_string str in
      let l = String.length t in
      (Tok_ID t)::tok str (pos + l)
    else if Str.string_match regex_int_lit str pos then
      let t = Str.matched_string str in
      let l = String.length t in
      (Tok_Int (int_of_string t))::tok str (pos + l)
    else if Str.string_match regex_l_paren str pos then
      Tok_LParen::(tok str (pos + 1))
    else if Str.string_match regex_r_paren str pos then
      Tok_RParen::(tok str (pos + 1))
    else if Str.string_match regex_l_curly str pos then
      Tok_LCurly::(tok str (pos + 1))
    else if Str.string_match regex_r_curly str pos then
      Tok_RCurly::(tok str (pos + 1))
    else if Str.string_match regex_semi str pos then
      Tok_Semi::(tok str (pos + 1))
    else if Str.string_match regex_equals str pos then
      Tok_Equals::(tok str (pos + 1))
    else if Str.string_match regex_plus str pos then
      Tok_Plus::(tok str (pos + 1))
    else if Str.string_match regex_minus str pos then
      Tok_Minus::(tok str (pos + 1))
    else if Str.string_match regex_asterisk str pos then
      Tok_Astrisk::(tok str (pos + 1))
    else if Str.string_match regex_f_slash str pos then
      Tok_FSlash::(tok str (pos + 1))
    else
      raise (InvalidInputException str)
    in
  (tok input 0)

  let () = print_tokens (tokenize 
  "int main()
  ")