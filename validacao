namespace // -> COLOQUE SEU NAMESPACE AQUI!

open System

module Utils =
    let apenasNumeros (documento: string) =
       documento
       |> Seq.filter (Char.IsDigit)
       |> String.Concat

    let tamanhoValido (documento: string) =
        match documento.Length with
        | 11 | 14 -> true
        | _ -> false

    let semOsDigitosFinais digitos (documento: string) =
        let documentoSemDigitos = documento.[..digitos]
        documentoSemDigitos

    let substituindo (substituto: string) ( [<ParamArray>] digitos: 'a array): Map<'a, string> =
        digitos
        |> Seq.fold (fun (newMap: Map<'a, string>) numero ->
            newMap.Add(numero, substituto)) Map.empty

    let adicionaDigito (digito: string) (documento: string) =
        String.Concat(documento, digito)

    let someOsDigitos (de: int) (ate: int) (documento: string): int =
        let multiplicador = [ for numero in de..ate do yield numero ]
        let resultado = Seq.initInfinite (fun i -> multiplicador.[i % multiplicador.Length])
        documento
        |> Seq.rev
        |> Seq.zip resultado
        |> Seq.sumBy (fun (i, c) -> i * int (Char.GetNumericValue c))

    let divisao (constante: int) (somenteDigitos: int) =
        (somenteDigitos % constante)
        |> (-) constante

    let resultado (mapa: Map<int, string>) divisao: string =
        match mapa.ContainsKey(divisao) with
        | true -> mapa.[divisao]
        | false -> divisao.ToString()

module CPF =
   let tamanhoValido (cpf: string) =
       cpf
       |> Utils.apenasNumeros
       |> Utils.tamanhoValido

   let primeiroNumero (cpf: string) =
       cpf
       |> Utils.apenasNumeros
       |> Utils.semOsDigitosFinais 8
       |> Utils.someOsDigitos 2 11
       |> Utils.divisao 11
       |> Utils.resultado (Utils.substituindo "0" [| 10; 11 |])

   let segundNumero (cpf: string) (_primeiroNumero: string) =
       cpf
       |> Utils.apenasNumeros
       |> Utils.semOsDigitosFinais 8
       |> Utils.adicionaDigito _primeiroNumero
       |> Utils.someOsDigitos 2 11
       |> Utils.divisao 11
       |> Utils.resultado (Utils.substituindo "0" [| 10; 11 |])

   let cpfValidoInvalido (_cpf: string) =
        match (tamanhoValido _cpf) with
        | false -> false
        | true -> (primeiroNumero (_cpf) + (segundNumero (_cpf) (primeiroNumero (_cpf)))) .Equals((Utils.apenasNumeros (_cpf)).[9..])


module CNPJ =
    let todo =
        "todo"
