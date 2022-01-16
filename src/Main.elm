module Main exposing (Model, init, main, update, view)

import Browser
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Html exposing (Html)
import List exposing (singleton)
import List.Extra
import String exposing (join)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Msg
    = TwoND
    | PI
    | E
    | C
    | Backspace
    | Squared
    | OneOverX
    | Abs
    | Esp
    | Modulo
    | SquareRoot
    | LeftPar
    | RightPar
    | Factorial
    | Divide
    | ToTheY
    | Seven
    | Eight
    | Nine
    | Times
    | Power10
    | Four
    | Five
    | Six
    | Minus
    | Log
    | One
    | Two
    | Three
    | Plus
    | Ln
    | FlipSign
    | Zero
    | Dot
    | Equals


type Colors
    = VeryDarkGrey
    | DarkGrey
    | Grey
    | White
    | LightGrey
    | Custom Element.Color


colorPicker : Colors -> Element.Color
colorPicker color =
    case color of
        VeryDarkGrey ->
            Element.rgba255 32 32 32 1.0

        DarkGrey ->
            Element.rgba255 50 50 50 1.0

        Grey ->
            Element.rgba255 59 59 59 1.0

        LightGrey ->
            Element.rgba255 200 200 200 1.0

        White ->
            Element.rgba255 255 255 255 1.0

        Custom c ->
            c


type alias Model =
    { stack : String
    }


init : Model
init =
    { stack = ""
    }


toOperater : String -> (Float -> Float -> Float)
toOperater op =
    case op of
        "+" ->
            \x y -> x + y

        "-" ->
            \x y -> x - y

        "*" ->
            \x y -> x * y

        "/" ->
            \x y -> x / y

        _ ->
            \x y -> x



{-
   This is a realy bad way to do this. I should be using a proper parser.
   That stack should only be solved when the user presses the equals button.
   Until then, we should show an intermediate result.
-}


solve : Model -> Model
solve { stack } =
    { stack =
        case String.words stack of
            a :: op :: b :: [] ->
                String.fromFloat
                    (toOperater op
                        (Maybe.withDefault 0.0 <| String.toFloat a)
                        (Maybe.withDefault 0.0 <| String.toFloat b)
                    )

            _ ->
                stack
    }


flipsign : String -> String
flipsign stack =
    let
        frontOfStack =
            stack
                |> String.words
                |> List.Extra.init
                |> Maybe.withDefault []
    in
    case
        -- If the last entry is a number, flip the sign of the last number.
        stack
            |> String.words
            |> List.Extra.last
            |> Maybe.withDefault ""
            |> String.toFloat
    of
        Just f ->
            f
                |> (*) -1.0
                |> String.fromFloat
                |> singleton
                |> List.append frontOfStack
                |> join " "

        Nothing ->
            stack


squared : String -> String
squared stack =
    let
        frontOfStack =
            stack
                |> String.words
                |> List.Extra.init
                |> Maybe.withDefault []
    in
    case
        stack
            |> String.words
            |> List.Extra.last
            |> Maybe.withDefault ""
            |> String.toFloat
    of
        Just f ->
            f
                |> (*) f
                |> String.fromFloat
                |> singleton
                |> List.append frontOfStack
                |> join " "

        Nothing ->
            stack


log : String -> Float -> String
log stack base =
    let
        frontOfStack =
            stack
                |> String.words
                |> List.Extra.init
                |> Maybe.withDefault []
    in
    case
        stack
            |> String.words
            |> List.Extra.last
            |> Maybe.withDefault ""
            |> String.toFloat
    of
        Just f ->
            f
                |> logBase base
                |> String.fromFloat
                |> singleton
                |> List.append frontOfStack
                |> join " "

        Nothing ->
            stack


update : Msg -> Model -> Model
update msg model =
    case msg of
        Zero ->
            { model | stack = model.stack ++ "0" }

        One ->
            { model | stack = model.stack ++ "1" }

        Two ->
            { model | stack = model.stack ++ "2" }

        Three ->
            { model | stack = model.stack ++ "3" }

        Four ->
            { model | stack = model.stack ++ "4" }

        Five ->
            { model | stack = model.stack ++ "5" }

        Six ->
            { model | stack = model.stack ++ "6" }

        Seven ->
            { model | stack = model.stack ++ "7" }

        Eight ->
            { model | stack = model.stack ++ "8" }

        Nine ->
            { model | stack = model.stack ++ "9" }

        Plus ->
            { model | stack = model.stack ++ " + " }

        Minus ->
            { model | stack = model.stack ++ " - " }

        Times ->
            { model | stack = model.stack ++ " * " }

        Divide ->
            { model | stack = model.stack ++ " / " }

        Equals ->
            -- Preform the calculation
            solve model

        TwoND ->
            model

        PI ->
            model

        E ->
            model

        C ->
            { model | stack = String.join " " <| Maybe.withDefault [] <| List.Extra.init <| String.words model.stack }

        Backspace ->
            model

        Squared ->
            { model | stack = squared model.stack }

        OneOverX ->
            model

        Abs ->
            model

        Esp ->
            model

        Modulo ->
            model

        SquareRoot ->
            model

        LeftPar ->
            model

        RightPar ->
            model

        Factorial ->
            model

        ToTheY ->
            Debug.todo "branch 'ToTheY' not implemented"

        Power10 ->
            Debug.todo "branch 'Base10' not implemented"

        Log ->
            { model | stack = log model.stack 10 }

        Ln ->
            { model | stack = log model.stack e }

        FlipSign ->
            { model | stack = flipsign model.stack }

        Dot ->
            { model | stack = model.stack ++ "." }


buttons =
    [ [ ( "2nd", TwoND, DarkGrey )
      , ( "PI", PI, DarkGrey )
      , ( "e", E, DarkGrey )
      , ( "C", C, DarkGrey )
      , ( "<=", Backspace, DarkGrey )
      ]
    , [ ( "x^2", Squared, DarkGrey )
      , ( "1/x", OneOverX, DarkGrey )
      , ( "|x|", Abs, DarkGrey )
      , ( "exp", Esp, DarkGrey )
      , ( "mod", Modulo, DarkGrey )
      ]
    , [ ( "2√x", SquareRoot, DarkGrey )
      , ( "(", LeftPar, DarkGrey )
      , ( ")", RightPar, DarkGrey )
      , ( "n!", Factorial, DarkGrey )
      , ( "/", Divide, DarkGrey )
      ]
    , [ ( "x^y", ToTheY, DarkGrey )
      , ( "7", Seven, Grey )
      , ( "8", Eight, Grey )
      , ( "9", Nine, Grey )
      , ( "x", Times, DarkGrey )
      ]
    , [ ( "10^x", Power10, DarkGrey )
      , ( "4", Four, Grey )
      , ( "5", Five, Grey )
      , ( "6", Six, Grey )
      , ( "-", Minus, DarkGrey )
      ]
    , [ ( "log", Log, DarkGrey )
      , ( "1", One, Grey )
      , ( "2", Two, Grey )
      , ( "3", Three, Grey )
      , ( "+", Plus, DarkGrey )
      ]
    , [ ( "ln", Ln, DarkGrey )
      , ( "+/-", FlipSign, Grey )
      , ( "0", Zero, Grey )
      , ( ".", Dot, Grey )
      , ( "=", Equals, Custom (Element.rgba255 97 204 255 1.0) )
      ]
    ]
        |> List.map
            (\row ->
                row
                    |> List.map (\( char, botton, c ) -> viewButton ( char, botton, c ))
                    |> Element.row
                        [ Element.spacing 2, Element.width Element.fill, Element.height (Element.fillPortion 1) ]
            )
        |> Element.column
            [ Element.spacing 2, Element.width Element.fill, Element.height (Element.fillPortion 4) ]



-- subscriptions : Model -> Sub Msg
-- subscriptions _ =
--     Sub.none


viewButton : ( String, Msg, Colors ) -> Element.Element Msg
viewButton ( char, botton, color ) =
    Element.el
        [ Element.Events.onClick botton
        , Element.Border.rounded 5
        , Element.width (Element.fillPortion 1)
        , Element.height (Element.fillPortion 1)

        -- , Element.explain Debug.todo
        , Element.Border.color (Element.rgba255 255 255 255 0.05)
        , Element.Border.widthEach { bottom = 0, left = 1, right = 0, top = 1 }
        , Element.Background.color (colorPicker color)
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            , Element.Font.size 17
            , Element.Font.family [ Element.Font.typeface "Arial" ]
            , Element.Font.color (colorPicker White)
            ]
            (Element.text char)
        )


viewHistory : Model -> String
viewHistory model =
    if (Maybe.withDefault "0" <| List.Extra.last <| String.words model.stack) == "+" then
        String.trimRight model.stack

    else
        String.join " " <| Maybe.withDefault [] <| List.Extra.init <| String.words model.stack


viewValue : Model -> String
viewValue model =
    model.stack
        |> String.words
        |> List.filter
            (\f ->
                case String.toFloat f of
                    Just _ ->
                        True

                    Nothing ->
                        False
            )
        |> List.Extra.last
        |> Maybe.withDefault "0"
        |> truncateString 10



-- |> tempFixRounding
-- |> tempFixRounding
-- tempFixRounding : String -> String


truncateString : Int -> String -> String
truncateString number s =
    String.toList s
        |> List.take number
        |> String.fromList


truncateStringDecimal : String -> String
truncateStringDecimal s =
    String.split "." s
        |> List.head
        |> Maybe.withDefault "Error"


tempFixRounding s =
    if s == "0" then
        s

    else
        s
            |> String.toFloat
            |> Maybe.withDefault 0.0
            |> (*) 100.0
            |> round
            |> toFloat
            |> (*) 0.01
            |> String.fromFloat


viewDisplay : Model -> Element.Element Msg
viewDisplay model =
    Element.column [ Element.alignRight, Element.height (Element.fillPortion 1) ]
        [ Element.el
            [ Element.height (Element.fillPortion 1)
            , Element.Font.color (colorPicker LightGrey)
            , Element.alignRight
            , Element.Font.size 15
            ]
            (Element.text <| viewHistory model)
        , Element.el
            [ Element.height (Element.fillPortion 1)
            , Element.Font.color (colorPicker White)
            , Element.Font.size 45
            , Element.Font.bold
            , Element.alignRight
            ]
            (Element.text <| viewValue model)
        ]


viewDisplayPlaceholder : Model -> String -> String -> Element.Element Msg
viewDisplayPlaceholder model text num =
    Element.el
        [ Element.height (Element.fillPortion 1)
        , Element.width Element.fill
        , Element.Font.color (colorPicker White)
        ]
        (Element.text text)


debugStack : Model -> Bool -> Element.Element msg
debugStack model bool =
    if bool then
        Element.el [] <| Element.text <| "Debug Stack: " ++ model.stack

    else
        Element.none


view : Model -> Html Msg
view model =
    Element.layout [] <|
        Element.column
            [ Element.centerX
            , Element.centerY
            ]
            [ Element.column
                [ Element.Background.color <| colorPicker VeryDarkGrey
                , Element.padding 5
                , Element.width (Element.px 320)
                , Element.height (Element.px 565)
                , Element.Border.rounded 7
                ]
                [ viewDisplayPlaceholder model "Scientific" "0"
                , viewDisplay model
                , viewDisplayPlaceholder model "" "0"
                , viewDisplayPlaceholder model "" "0"
                , buttons
                ]
            , debugStack model False
            ]
