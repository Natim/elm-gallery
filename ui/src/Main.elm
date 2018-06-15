module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, program)
import Http
import Html.Attributes exposing (src, alt, style, title, class, href)
import Html.Events exposing (onClick)
import Json.Decode
    exposing
        ( Decoder
        , at
        , andThen
        , dict
        , fail
        , int
        , keyValuePairs
        , list
        , map
        , maybe
        , string
        , succeed
        )
import Json.Decode.Pipeline
    exposing
        ( decode
        , required
        , requiredAt
        , optional
        , optionalAt
        , hardcoded
        )


-- Repository URL


uri : String
uri =
    "https://api.github.com/repos/Natim/elm-gallery/git/trees/master?recursive=1"


token : String
token =
    "token <your-token-here>"


type Msg
    = UpdateInfo (Result Http.Error Info)
    | DownloadFile String (Result Http.Error ImageContent)
    | Display Gallery


type alias Image =
    { name : String
    , url : String
    }


type alias Gallery =
    { name : String
    , images : List Image
    }


type alias Model =
    { galleries : List Gallery
    , images : List ( String, ImageContent )
    , gallery : Maybe Gallery
    }


type alias File =
    { path : String
    , isTree : Bool
    , url : String
    }


type alias Info =
    { tree : List File
    }


type alias ImageContent =
    { encoding : String
    , content : String
    }


getInfo : Cmd Msg
getInfo =
    Http.send UpdateInfo <|
        Http.request
            { method = "GET"
            , headers =
                [ Http.header "Accept" "application/vnd.github.v3+json"
                , Http.header "Authorization" token
                ]
            , url = uri
            , body = Http.emptyBody
            , expect = Http.expectJson decodeInfo
            , timeout = Nothing
            , withCredentials = False
            }


decodeInfo : Decoder Info
decodeInfo =
    decode Info
        |> required "tree" (list decodeFile)


decodeFile : Decoder File
decodeFile =
    decode File
        |> required "path" string
        |> required "type" decodeIsTree
        |> required "url" string


decodeIsTree : Decoder Bool
decodeIsTree =
    string |> map typeFromString


typeFromString : String -> Bool
typeFromString typeString =
    typeString == "tree"


getImageContent : String -> Cmd Msg
getImageContent imageUrl =
    Http.send (DownloadFile imageUrl) <|
        Http.request
            { method = "GET"
            , headers =
                [ Http.header "Accept" "application/vnd.github.v3+json"
                , Http.header "Authorization" token
                ]
            , url = imageUrl
            , body = Http.emptyBody
            , expect = Http.expectJson decodeImageContent
            , timeout = Nothing
            , withCredentials = False
            }


decodeImageContent : Decoder ImageContent
decodeImageContent =
    decode ImageContent
        |> required "encoding" string
        |> required "content" string


init : ( Model, Cmd Msg )
init =
    { galleries = []
    , images = []
    , gallery = Nothing
    }
        ! [ getInfo ]


viewImage : Dict String ImageContent -> Image -> Html Msg
viewImage images image =
    let
        imageContent =
            Dict.get image.url images
    in
        case imageContent of
            Just imageContent ->
                Html.img
                    [ src <| "data:image/jpg;" ++ imageContent.encoding ++ "," ++ imageContent.content
                    , alt image.name
                    ]
                    []

            Nothing ->
                Html.text <| "Loading " ++ image.url


viewGallery : Dict String ImageContent -> Gallery -> Html Msg
viewGallery images gallery =
    Html.li [ class "col-md-3" ]
        [ Html.a [ href "#portfolio", title gallery.name, onClick <| Display gallery ]
            [ List.head gallery.images
                |> Maybe.withDefault (Image "" "")
                |> viewImage images
            ]
        ]


view : Model -> Html Msg
view model =
    Html.div [ class "gallery" ]
        [ case model.gallery of
            Nothing ->
                List.map (viewGallery <| Dict.fromList model.images) model.galleries
                    |> Html.ul []

            Just gallery ->
                List.map (\image -> Html.li [ class "col-md-3" ] [ viewImage (Dict.fromList model.images) image ]) gallery.images
                    |> Html.ul []
        ]


isImage : File -> Bool
isImage file =
    String.startsWith "images/" file.path


lastElem : List a -> Maybe a
lastElem =
    List.foldl (Just >> always) Nothing


buildImage : File -> Image
buildImage file =
    Image
        (String.split "/" file.path |> lastElem |> Maybe.withDefault "")
        file.url


buildTrees : List File -> File -> Gallery
buildTrees info file =
    List.filter (.path >> String.startsWith (file.path ++ "/")) info
        |> List.map buildImage
        |> Gallery (String.split "/" file.path |> lastElem |> Maybe.withDefault "")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInfo (Ok c) ->
            let
                trees =
                    List.filter isImage c.tree |> List.filter .isTree

                galleries =
                    List.map (buildTrees c.tree) trees

                urls =
                    List.map (.images >> List.map .url) galleries |> List.concat
            in
                ( { model | galleries = galleries }
                , List.map getImageContent urls
                    |> Cmd.batch
                )

        UpdateInfo (Err err) ->
            let
                _ =
                    Debug.log "Error" err
            in
                ( model, Cmd.none )

        DownloadFile url (Ok content) ->
            ( { model | images = model.images ++ [ ( url, content ) ] }, Cmd.none )

        DownloadFile url (Err err) ->
            let
                _ =
                    Debug.log ("DownloadFile error " ++ url) err
            in
                ( model, Cmd.none )

        Display gallery ->
            ( { model | gallery = Just gallery }, Cmd.none )


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
