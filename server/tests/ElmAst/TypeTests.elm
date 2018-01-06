module ElmAst.TypeTests exposing (..)

import ElmAst.Type exposing (Type(..), TypeAliasDefinitionR, UnionR, parseRecord, parseTypeAlias, parseTypeConstructor, parseTypeConstructors, parseUnion)
import Expect exposing (Expectation, equalSets)
import Result.Extra exposing (isErr)
import Test exposing (..)
import Parser


suite : Test
suite =
    describe "ElmAst.Type"
        [ test "parses simple Int type" <|
            \_ ->
                "Int"
                    |> ElmAst.Type.parseTipe
                    |> Expect.equal
                        (Ok <|
                            Type "Int" []
                        )
        , test "parseRecord" <|
            \_ ->
                let
                    s =
                        "{ email : String, password : String, loading : Bool, error : Bool }"
                in
                    s
                        |> Parser.run ElmAst.Type.parseRecord
                        |> Debug.log "result"
                        |> isErr
                        |> Expect.equal False
        , test "parse type alias" <|
            \_ ->
                "type alias Id = Int"
                    |> parseTypeAlias
                    |> Expect.equal (Ok (TypeAliasDefinitionR "Id" [] (Type "Int" [])))
        , test "parse type alias with single type var" <|
            \_ ->
                "type alias Tagger msg = Int -> msg"
                    |> parseTypeAlias
                    |> Expect.equal
                        (Ok
                            (TypeAliasDefinitionR
                                "Tagger"
                                [ "msg" ]
                                (Lambda (Type "Int" []) (Var "msg"))
                            )
                        )
        , test "typeConstructor: takes no args" <|
            \_ ->
                "TypeA"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "TypeA", [] )
                        )
        , test "typeConstructor: takes one simple Type arg" <|
            \_ ->
                "TypeA Int"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "TypeA", [ Type "Int" [] ] )
                        )
        , test "typeConstructor: takes two simple Type args" <|
            \_ ->
                "MyType ArgA ArgB"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Type "ArgA" [], Type "ArgB" [] ] )
                        )
        , test "typeConstructor: takes three simple Type args" <|
            \_ ->
                "MyType ArgA ArgB ArgC"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Type "ArgA" [], Type "ArgB" [], Type "ArgC" [] ] )
                        )
        , test "typeConstructor: takes two type variables as args" <|
            \_ ->
                "MyType a b"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Var "a", Var "b" ] )
                        )
        , test "typeConstructor: takes a tuple arg" <|
            \_ ->
                "MyType (Int, String)"
                    |> parseTypeConstructor
                    |> Expect.equal
                        (Ok
                            ( "MyType", [ Tuple [ Type "Int" [], Type "String" [] ] ] )
                        )
        , test "typeConstructors: with one that doesn't take any args" <|
            \_ ->
                "MyType"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "MyType", [] ) ]
                        )
        , test "typeConstructors: with one arg, and it takes a Unit type" <|
            \_ ->
                "TypeA ()"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Tuple [] ] ) ]
                        )
        , test "typeConstructors: Two of them. Both take no args." <|
            \_ ->
                "TypeA | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  First takes a type variable as an arg." <|
            \_ ->
                "TypeA a | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Var "a" ] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Three of them.  First two take a type variable as an arg." <|
            \_ ->
                "TypeA a | TypeB b | TypeC"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Var "a" ] )
                            , ( "TypeB", [ Var "b" ] )
                            , ( "TypeC", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  First takes a simple Type as an arg." <|
            \_ ->
                "TypeA Int | TypeB"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Type "Int" [] ] )
                            , ( "TypeB", [] )
                            ]
                        )
        , test "typeConstructors: Two of them.  Both take a simple Type as an arg." <|
            \_ ->
                "TypeA Int | TypeB String"
                    |> parseTypeConstructors
                    |> Expect.equal
                        (Ok
                            [ ( "TypeA", [ Type "Int" [] ] )
                            , ( "TypeB", [ Type "String" [] ] )
                            ]
                        )
        , test "unionType: single constructor that takes one arg" <|
            \_ ->
                "type MyType = TypeA Int"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR
                                "MyType"
                                []
                                [ ( "TypeA", [ Type "Int" [] ] )
                                ]
                            )
                        )
        , test "unionType: single constructor that takes two args" <|
            \_ ->
                "type MyType = TypeA Int String"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR
                                "MyType"
                                []
                                [ ( "TypeA", [ Type "Int" [], Type "String" [] ] )
                                ]
                            )
                        )
        , test "unionType: single constructor that takes no args" <|
            \_ ->
                "type MyType = TypeA"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR "MyType"
                                []
                                [ ( "TypeA", [] )
                                ]
                            )
                        )
        , test "unionType with two constructors that take no args" <|
            \_ ->
                "type MyType = TypeA | TypeB"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR "MyType"
                                []
                                [ ( "TypeA", [] )
                                , ( "TypeB", [] )
                                ]
                            )
                        )
        , test "unionType with type variable" <|
            \_ ->
                "type MyType a = TypeA a"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR "MyType"
                                [ "a" ]
                                [ ( "TypeA", [ Var "a" ] )
                                ]
                            )
                        )
        , test "unionType with two type variables (Result error value)" <|
            \_ ->
                "type Result error value = Ok value | Err error"
                    |> parseUnion
                    |> Expect.equal
                        (Ok
                            (UnionR "Result"
                                [ "error", "value" ]
                                [ ( "Ok", [ Var "value" ] )
                                , ( "Err", [ Var "error" ] )
                                ]
                            )
                        )
        ]
