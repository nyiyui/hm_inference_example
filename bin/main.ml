open Js_of_ocaml
open Hm_inference_example
open Ast

let _ =
  Js.export "hm_inference_example"
    (object%js
       method eval s =
         let ast = Lexing.from_string s |> Parser.program Lexer.read in
         let result = Eval.eval ast in
         let _ = Type_check.infer ast Type_check.Env.empty |> fst in
         string_of_expr result |> Js.string

       method typeof s =
         let ast = Lexing.from_string s |> Parser.program Lexer.read in
         let inferred =
           Type_check.infer ast Type_check.Env.empty |> fst |> Type_check.lower
         in
         Type_check.string_of_typ inferred |> Js.string
    end)
