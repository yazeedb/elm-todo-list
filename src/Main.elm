module Main exposing (Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { uid : Int, todoText : String, todos : List Todo }


type alias Todo =
    { id : Int, text : String }


init : Model
init =
    { uid = 1
    , todoText = ""
    , todos = []
    }


type Msg
    = AddTodo
    | DeleteTodo Int
    | UpdateTodoText String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            if String.isEmpty model.todoText then
                model

            else
                { model
                    | uid = model.uid + 1
                    , todoText = ""
                    , todos =
                        let
                            newTodo =
                                createTodo model.uid model.todoText
                        in
                        model.todos ++ [ newTodo ]
                }

        UpdateTodoText newValue ->
            { model | todoText = newValue }

        DeleteTodo todoId ->
            let
                idDoesNotMatch =
                    \todo -> todo.id /= todoId
            in
            { model | todos = List.filter idDoesNotMatch model.todos }


createTodo : Int -> String -> Todo
createTodo id todoText =
    { id = id, text = todoText }


renderTodoItem : Todo -> Html Msg
renderTodoItem todo =
    div []
        [ li [ class "todo-item" ]
            [ text todo.text
            , button [ onClick (DeleteTodo todo.id), type_ "button" ] [ text "X" ]
            ]
        ]


view model =
    Html.form [ onSubmit AddTodo ]
        [ h1 [] [ text "todos" ]
        , input
            [ placeholder "What needs to be done?"
            , autofocus True
            , onInput UpdateTodoText
            , value model.todoText
            ]
            []
        , ul [] (List.map renderTodoItem model.todos)
        ]
