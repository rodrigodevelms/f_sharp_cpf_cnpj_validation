namespace // -> COLOQUE SEU NAMESPACE AQUI!

open System

module Main =
  let inline charToInt c = int c - int '0'

  let verifyEachNumber (numbers: int []) (number: int): Boolean =
    [| 0 .. 9 |]
    |> Array.tryFind (fun n -> numbers = Array.replicate number n)
    |> Option.isNone

  let validateDocument (document: String) =
    document
    |> Seq.filter (fun it -> Char.IsNumber it)
    |> Seq.map charToInt
    |> Seq.toArray

  let validateCpf (cpf: String): Boolean =
    let numbers = validateDocument (cpf)
    if numbers.Length <> 11 then failwith "Error, invalid CPF length"
    if not (verifyEachNumber numbers 10) then failwith "Error invalid number verification"

    let digitOne =
      let temp =
        [| 0 .. 8 |]
        |> Array.sumBy (fun it -> ((it + 1) * numbers.[it]))
        |> fun rem -> rem % 11
      if temp % 11 >= 10 then 0
      else temp % 11

    let digitTwo =
      let temp =
        [| 0 .. 8 |]
        |> Array.sumBy (fun it -> (it * numbers.[it]))
        |> fun it -> (it + (digitOne * 9))
      if temp % 11 >= 10 then 0
      else temp % 11

    if (numbers.[9].Equals(digitOne) && numbers.[10].Equals(digitTwo)) then true
    else false

  let validateCnpj (cnpj: String): Boolean =
    let mutable mutableNumbers = []
    let mutable numbers = validateDocument (cnpj)
    if numbers.Length <> 14 then failwith "Error, invalid CNPJ length"
    if not (verifyEachNumber numbers 14) then failwith "Error, invalid number verification"

    let digitOne =
      [| 5; 4; 3; 2; 9; 8; 7; 6; 5; 4; 3; 2 |]
      |> Array.mapi (fun index it -> (it * numbers.[index]))
      |> Array.sum
      |> fun it -> (it % 11) |> (-) 11

    numbers
    |> Array.toList
    |> (fun it -> mutableNumbers <- it @ [ digitOne ])

    let digitTwo =
      [| 6; 5; 4; 3; 2; 9; 8; 7; 6; 5; 4; 3; 2 |]
      |> Array.mapi (fun index it -> (it * mutableNumbers.[index]))
      |> Array.sum
      |> fun it -> (it % 11) |> (-) 11

    if mutableNumbers.[12] = digitOne && mutableNumbers.[13] = digitTwo then true
    else false
