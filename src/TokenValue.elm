module TokenValue exposing (TokenValue, add, compare, decoder, divByFloatWithWarning, divByInt, encode, fromFloatWithWarning, fromIntTokenValue, fromString, getEvmValue, getRatioWithWarning, isMaxTokenValue, isZero, mulByFloatWithWarning, mulByInt, sub, toConciseString, toFloatWithWarning, tokenValue, zero)

import BigInt exposing (BigInt)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Json.Decode
import Json.Encode
import Round


tokenDecimals : Int
tokenDecimals =
    18


type TokenValue
    = TokenValue BigInt


tokenValue : BigInt -> TokenValue
tokenValue evmValue =
    TokenValue evmValue


fromIntTokenValue : Int -> TokenValue
fromIntTokenValue val =
    BigInt.fromInt val
        |> BigInt.mul
            (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt tokenDecimals))
        |> tokenValue


fromFloatWithWarning : Float -> TokenValue
fromFloatWithWarning val =
    case userStringToEvmValue (Round.round tokenDecimals val) of
        Just bigint ->
            tokenValue bigint

        Nothing ->
            tokenValue (BigInt.fromInt 0)


fromString : String -> Maybe TokenValue
fromString s =
    Maybe.map
        TokenValue
        (userStringToEvmValue s)


zero : TokenValue
zero =
    TokenValue <| BigInt.fromInt 0


isZero : TokenValue -> Bool
isZero tv =
    getEvmValue tv == BigInt.fromInt 0


getEvmValue : TokenValue -> BigInt
getEvmValue (TokenValue tokens) =
    tokens


toFloatWithWarning : TokenValue -> Float
toFloatWithWarning tokens =
    case tokens |> toFloatString Nothing |> String.toFloat of
        Just f ->
            f

        Nothing ->
            0


toFloatString : Maybe Int -> TokenValue -> String
toFloatString maxDigitsAfterDecimal tokens =
    (case maxDigitsAfterDecimal of
        Nothing ->
            evmValueToUserFloatString (getEvmValue tokens)

        Just maxDigits ->
            evmValueToTruncatedUserFloatString maxDigits (getEvmValue tokens)
    )
        |> addZeroBeforeAnyStartingDecimal


toFixedPrecisionFloatString : Int -> TokenValue -> String
toFixedPrecisionFloatString requiredDecimals tokens =
    let
        possiblyTooShortString =
            toFloatString (Just requiredDecimals) tokens

        maybePointIndex =
            String.indexes "." possiblyTooShortString
                |> List.head
    in
    case maybePointIndex of
        Just pointIndex ->
            let
                existingDecimals =
                    String.length possiblyTooShortString - (pointIndex + 1)

                extraNeededDecimals =
                    requiredDecimals - existingDecimals
            in
            possiblyTooShortString ++ String.repeat extraNeededDecimals "0"

        Nothing ->
            possiblyTooShortString ++ "." ++ String.repeat requiredDecimals "0"


addZeroBeforeAnyStartingDecimal : String -> String
addZeroBeforeAnyStartingDecimal numStr =
    if String.startsWith "." numStr then
        "0" ++ numStr

    else
        numStr


toConciseString : TokenValue -> String
toConciseString tv =
    toFloatWithWarning tv
        |> autoFormatFloat


negate : TokenValue -> TokenValue
negate t =
    getEvmValue t
        |> BigInt.negate
        |> tokenValue


add : TokenValue -> TokenValue -> TokenValue
add t1 t2 =
    BigInt.add
        (getEvmValue t1)
        (getEvmValue t2)
        |> TokenValue


sub : TokenValue -> TokenValue -> TokenValue
sub t1 t2 =
    BigInt.sub
        (getEvmValue t1)
        (getEvmValue t2)
        |> TokenValue


mulByInt : Int -> TokenValue -> TokenValue
mulByInt i t =
    BigInt.mul
        (getEvmValue t)
        (BigInt.fromInt i)
        |> TokenValue


mulByFloatWithWarning : Float -> TokenValue -> TokenValue
mulByFloatWithWarning f t =
    toFloatWithWarning t
        * f
        |> fromFloatWithWarning


divByInt : Int -> TokenValue -> TokenValue
divByInt i t =
    BigInt.div
        (getEvmValue t)
        (BigInt.fromInt i)
        |> TokenValue


getRatioWithWarning : TokenValue -> TokenValue -> Float
getRatioWithWarning t1 t2 =
    toFloatWithWarning t1
        / toFloatWithWarning t2


divByFloatWithWarning : Float -> TokenValue -> TokenValue
divByFloatWithWarning f t =
    toFloatWithWarning t
        / f
        |> fromFloatWithWarning


compare : TokenValue -> TokenValue -> Order
compare t1 t2 =
    BigInt.compare
        (getEvmValue t1)
        (getEvmValue t2)


encode : TokenValue -> Json.Encode.Value
encode tv =
    tv
        |> getEvmValue
        |> BigInt.toString
        |> Json.Encode.string


decoder : Json.Decode.Decoder TokenValue
decoder =
    Json.Decode.map
        tokenValue
        (Json.Decode.string
            |> Json.Decode.andThen
                (\string ->
                    case BigInt.fromIntString string of
                        Just val ->
                            Json.Decode.succeed val

                        Nothing ->
                            Json.Decode.fail "Can't convert that to a BigInt"
                )
        )



-- Internal


userStringToEvmValue : String -> Maybe BigInt
userStringToEvmValue amountString =
    if amountString == "" then
        Nothing

    else
        let
            ( newString, numDigitsMoved ) =
                pullAnyFirstDecimalOffToRight amountString

            numDigitsLeftToMove =
                tokenDecimals - numDigitsMoved

            maybeBigIntAmount =
                if numDigitsLeftToMove < 0 then
                    -- indicates there is too much precision; we must cut some off the end
                    BigInt.fromIntString (String.dropRight (Basics.negate numDigitsLeftToMove) newString)

                else
                    BigInt.fromIntString newString
        in
        maybeBigIntAmount
            |> Maybe.map
                (BigInt.mul
                    (BigInt.pow (BigInt.fromInt 10) (BigInt.fromInt numDigitsLeftToMove))
                )


pullAnyFirstDecimalOffToRight : String -> ( String, Int )
pullAnyFirstDecimalOffToRight numString =
    let
        maybeDecimalPosition =
            List.head (String.indexes "." numString)
    in
    case maybeDecimalPosition of
        Nothing ->
            ( numString, 0 )

        Just decimalPos ->
            let
                numDigitsMoved =
                    (String.length numString - 1) - decimalPos

                newString =
                    String.left decimalPos numString
                        ++ String.dropLeft (decimalPos + 1) numString
            in
            ( newString, numDigitsMoved )


evmValueToTruncatedUserFloatString : Int -> BigInt -> String
evmValueToTruncatedUserFloatString maxDigitsAfterDecimal evmValue =
    let
        untruncatedString =
            evmValueToUserFloatString evmValue

        maybeDecimalPos =
            List.head (String.indexes "." untruncatedString)
    in
    case maybeDecimalPos of
        Nothing ->
            untruncatedString

        Just decimalPos ->
            if maxDigitsAfterDecimal == 0 then
                String.left decimalPos untruncatedString

            else
                String.left (decimalPos + 1 + maxDigitsAfterDecimal) untruncatedString


evmValueToUserFloatString : BigInt -> String
evmValueToUserFloatString evmValue =
    if BigInt.compare evmValue (BigInt.fromInt 0) == LT then
        "-" ++ evmValueToUserFloatString (BigInt.negate evmValue)

    else
        let
            zeroPaddedString =
                evmValue
                    |> BigInt.toString
                    |> String.padLeft tokenDecimals '0'

            withDecimalString =
                String.dropRight tokenDecimals zeroPaddedString
                    ++ "."
                    ++ String.right tokenDecimals zeroPaddedString
        in
        removeUnnecessaryZerosAndDots withDecimalString
            |> (\s ->
                    if s == "" then
                        "0"

                    else
                        s
               )


removeUnnecessaryZerosAndDots : String -> String
removeUnnecessaryZerosAndDots numString =
    if String.endsWith "." numString then
        String.slice 0 -1 numString

    else if String.endsWith "0" numString then
        removeUnnecessaryZerosAndDots (String.slice 0 -1 numString)

    else if numString == "" then
        "0"

    else
        numString


maxTokenValue : TokenValue
maxTokenValue =
    tokenValue maxUintValue


isMaxTokenValue : TokenValue -> Bool
isMaxTokenValue tv =
    compare tv maxTokenValue == EQ


toString : TokenValue -> String
toString value =
    let
        evm =
            value
                |> getEvmValue
                |> BigInt.toString

        evmLength =
            evm
                |> String.length
    in
    if evmLength <= tokenDecimals then
        evm
            |> String.padLeft tokenDecimals '0'
            |> (++) "0."

    else
        evm
            |> String.right tokenDecimals
            |> (++)
                "."
            |> (++)
                (evm
                    |> String.left
                        (evmLength - tokenDecimals)
                )


autoFormatFloat : Float -> String
autoFormatFloat f =
    let
        magnitude =
            floor <| logBase 10 f + 1

        numDecimals =
            max
                (3 - magnitude)
                0
    in
    f
        |> FormatNumber.format
            { usLocale
                | decimals = FormatNumber.Locales.Exact numDecimals
            }


maxUintValue : BigInt.BigInt
maxUintValue =
    BigInt.sub
        (BigInt.pow
            (BigInt.fromInt 2)
            (BigInt.fromInt 256)
        )
        (BigInt.fromInt 1)
