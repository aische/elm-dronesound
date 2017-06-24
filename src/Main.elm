module Main exposing (..)


import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick, onCheck, onInput, defaultOptions, onWithOptions)
import Html.Attributes as A
import Json.Decode as Json
import Mouse
import Random
import String
import Svg exposing (Svg)
import Svg.Attributes as SA
import Time


import Param exposing (..)
import Sound exposing (..)


main = App.program 
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }


init : (Model, Cmd Msg)
init = 
  sendSounds (initialModel , Random.generate NewInitialSeed (Random.int 1 9999999))


subscriptions : Model -> Sub Msg
subscriptions model =  Sub.batch 
  [ case model.rand of
      True -> 
        Time.every (0.08 * Time.second) (always Rand)

      False -> 
        Sub.none

  , case model.drag of
      Nothing -> 
        Sub.none

      Just _  -> 
        Sub.batch [ Mouse.moves DragMove, Mouse.ups DragEnd ]
  ]


type alias Model = 
  { sounds : List Sound 
  , drag : Maybe Drag
  , seed : Random.Seed
  , rand : Bool
  , gain : Float
  , states : List (List Sound)
  }


initialModel : Model 
initialModel =
  { sounds = initialSounds
  , drag = Nothing
  , seed = Random.initialSeed 124232
  , rand = False
  , gain = 0.0
  , states = []
  }
  

moveSounds : Model -> List Sound
moveSounds model = 
  case model.drag of
    Just drag ->
      let 
        (dx, dy) = 
          getDragDelta drag 
      in 
      List.indexedMap
        (\i s ->
          if i==drag.index then 
            updateRangeParamPosition drag.xParam dx <|
            updateRangeParamPosition drag.yParam dy s
          else s
        )
        model.sounds
    
    Nothing ->
      model.sounds

      
setSounds : List Sound -> Cmd Msg
setSounds sounds = 
  setSoundList <| List.indexedMap (\i s -> (i, s)) sounds


type alias Drag =
  { start   : Mouse.Position
  , current : Mouse.Position
  , index   : Int
  , xParam : RangeParam Sound 
  , yParam : RangeParam Sound
  }


getDragDelta : Drag -> (Float, Float)
getDragDelta {start,current} = 
  (toFloat (current.x - start.x), toFloat (current.y - start.y))


dropLastElement : List a -> List a
dropLastElement list = 
  case list of
    []    -> []
    [x]   -> []
    x::xs -> x :: dropLastElement xs


type Msg 
  = Noop
  | SetNote Int Float
  | SetPan Int Float
  | SetAmp Int Float
  | SetCutoff Int Float
  | DragStart Int (RangeParam Sound) (RangeParam Sound) Mouse.Position 
  | DragMove Mouse.Position
  | DragEnd Mouse.Position
  | Rand
  | SetRand Bool
  | InitSounds
  | SetMasterVolume Float
  | NewInitialSeed Int
  | AddOsc
  | RemoveOsc
  | SaveState
  | SetSounds (List Sound)


sendSounds : (Model, Cmd Msg) -> (Model, Cmd Msg)
sendSounds ( model, cmd ) =
  ( model, Cmd.batch [cmd, setSounds <| moveSounds model] )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    Noop -> 
      ( model, Cmd.none )

    DragStart index xParam yParam pos -> 
      ( { model
        | drag = Just 
          { start   = pos
          , current = pos
          , index   = index 
          , xParam = xParam
          , yParam = yParam
          }
        }
      , Cmd.none
      )

    DragMove pos -> 
      sendSounds
      ( { model
        | drag = Maybe.map 
            (\d  -> {d | current = pos})
            model.drag
        } 
      , Cmd.none
      )

    DragEnd pos -> 
      case model.drag of
        Just drag ->
          ( { model
            | drag = Nothing
            , sounds = moveSounds model
            }
          , Cmd.none
          )
        Nothing ->
          ( model
          , Cmd.none
          )

    SetNote index note ->
      sendSounds
        ( { model
          | sounds = 
              List.indexedMap 
                (\i sound -> 
                  if index == i then 
                    { sound | note = note }
                  else 
                    sound
                )
                model.sounds
          }
        , Cmd.none
        )

    SetAmp index amp ->
      sendSounds
        ( { model
          | sounds = 
              List.indexedMap 
                (\i sound -> 
                  if index == i then
                    { sound | amp = amp }
                  else 
                    sound
                )
                model.sounds
          }
        , Cmd.none
        )

    SetPan index pan -> 
      sendSounds
        ( { model
          | sounds = 
              List.indexedMap 
                (\i sound -> 
                  if index == i then
                    { sound | pan = pan }
                  else 
                    sound
                )
                model.sounds
          }
        , Cmd.none
        )
        
    SetCutoff index cutoff -> 
      sendSounds
        ( { model
          | sounds = 
              List.indexedMap 
                (\i sound -> 
                  if index == i then
                    { sound | cutoff = cutoff }
                  else 
                    sound
                )
                model.sounds
          }
        , Cmd.none
        )
        
    Rand ->
      let (seed', sounds') = 
        randomizeSounds model.seed model.sounds 
      in 
        sendSounds 
          ( { model | seed = seed', sounds = sounds' }
          , Cmd.none
          )

    SetRand b ->
      ( { model | rand = b}, Cmd.none )

    InitSounds ->
      sendSounds
        ( { model | sounds = List.map (always initialSound) model.sounds }
        , Cmd.none
        )

    SetMasterVolume gain ->
      ( { model
        | gain = gain
        }
      , setMasterVolume gain
      )

    NewInitialSeed i ->
      ( { model
        | seed = Random.initialSeed i
        }
      , Cmd.none
      )

    AddOsc ->
      sendSounds
        ( { model | sounds = model.sounds ++ [initialSound]}
        , Cmd.none
        )

    RemoveOsc ->
      let len = List.length model.sounds
      in 
        if len > 0 then 
          sendSounds
            ( { model | sounds = dropLastElement model.sounds }
            , removeSound (len-1)
            )
        else
          ( model, Cmd.none )

    SaveState ->
      ( { model
        | states = model.states ++ [model.sounds]
        }
      , Cmd.none 
      )

    SetSounds sounds ->
      sendSounds
      ( { model
        | sounds = sounds
        }
      , Cmd.none 
      )


view : Model -> Html Msg
view model' =
  let 
    model = 
      { model' | sounds = moveSounds model'} 
  in
    div 
      [ A.class "container"
      ] 
      [ div
          []
          [ myLabel "volume"
          , Html.br [] []
          , Html.input 
              [ A.type' "range"
              , A.min "0.0"
              , A.max "1.0"
              , A.step "0.01"
              -- value must come after min and max, sets wrong initial value otherwise (bug?)
              , A.value <| toString <| model.gain 
              , onInput (Result.withDefault Noop << Result.map SetMasterVolume << String.toFloat)
              ]
              [
              ]
          ]
      , div 
          []
          [ button [onClick (SetRand (not model.rand))] [if model.rand then text "stop" else text "run"]
          , button [onClick InitSounds] [text "reset"]
          , button [onClick AddOsc] [text "add osc"]
          , button [onClick RemoveOsc] [text "remove osc"]
          , button [onClick SaveState] [text "save"]
          ]
      , div 
          [ A.class "slider-container"
          ] 
          ( 
            List.indexedMap (\i state -> button [onClick (SetSounds state)] [text (toString i)]) model.states
            ++
            [ Html.br [] []
            , div 
                [] 
                [ myLabel "frequency"
                , myLabel "panning"
                , myLabel "volume"
                , myLabel "cutoff"
                ] 
            ]
            ++
            List.indexedMap (\i s -> viewSound i s) model.sounds 
            ++ 
            [Html.br [] []]
          )
      , div 
          [ A.class "svg-container"
          ]
          [ viewSvg "svg1" 0
              (panParam)
              (noteParam)
              (ampParam)
              200 200 model.sounds
          , viewSvg "svg2" 120
              (ampParam)
              (invertParam noteParam)
              (panParam)
              200 200 model.sounds
          , viewSvg "svg2" 180
              (ampParam)
              (invertParam panParam)
              (noteParam)
              200 200 model.sounds
          , viewSvg "svg3" 50
              (panParam)
              (cutoffParam)
              (noteParam)
              500 300 model.sounds
          , viewSvg "svg1" 300
              (cutoffParam)
              (invertParam ampParam)
              (panParam)
              100 300 model.sounds
          , viewSvg "svg1" 240
              (noteParam)
              (invertParam ampParam)
              (panParam)
              600 100 model.sounds
          ]
      ]


viewSound : Int -> Sound -> Html Msg 
viewSound index sound =
  div
    []
    [ Html.input 
        [ A.type' "range"
        , A.min "0.0"
        , A.max "127.0"
        , A.step "0.01"
        , A.value <| toString sound.note
        , onInput (Result.withDefault Noop << Result.map (SetNote index) << String.toFloat)
        ]
        [
        ]
    , Html.input 
        [ A.type' "range"
        , A.min "-1"
        , A.max "1"
        , A.step "0.01"
        , A.value <| toString sound.pan
        , onInput (Result.withDefault Noop << Result.map (SetPan index) << String.toFloat)
        ]
        [
        ]
    , Html.input 
        [ A.type' "range"
        , A.min "0"
        , A.max "1"
        , A.step "0.01"
        , A.value <| toString sound.amp
        , onInput (Result.withDefault Noop << Result.map (SetAmp index) << String.toFloat)
        ]
        [
        ]
    , Html.input 
        [ A.type' "range"
        , A.min "1"
        , A.max "8"
        , A.step "0.01"
        , A.value <| toString sound.cutoff
        , onInput (Result.withDefault Noop << Result.map (SetCutoff index) << String.toFloat)
        ]
        [
        ]
    ]


viewSvg : String -> Int -> Param Sound -> Param Sound -> Param Sound -> Float -> Float -> List Sound -> Html Msg
viewSvg classname fillColor xParam yParam colParam width height sounds = 
  let 
    xParam' = 
      makeRangeParam xParam (0, width) 

    yParam' = 
      makeRangeParam yParam (0, height) 

    colParam' = 
      makeRangeParam colParam (0, 100) 
  in
    Svg.svg 
      [ SA.height (toString height) --"400"
      , SA.width (toString width) -- "400"
      , A.style [("border", "1px solid black")]
      , SA.class classname
      ]
      (
        List.indexedMap (\i s -> viewSoundCircle fillColor xParam' yParam' colParam' i s) sounds
      )


hsla : Int -> Float -> Float -> Float -> String
hsla h s l a = 
  "hsla(" ++ toString h ++ "," ++ toString (round s) ++ "%," ++ toString (round l) ++ "%, " ++ toString a ++ ")"


viewSoundCircle : Int -> RangeParam Sound -> RangeParam Sound -> RangeParam Sound -> Int -> Sound -> Svg Msg
viewSoundCircle fillColor xParam yParam colParam index sound = 
  let 
    percent = (getRangeParamPosition colParam sound)
  in
  Svg.g
    [ onWithOptions "mousedown" 
        {defaultOptions | stopPropagation = True, preventDefault = True } 
        (Json.map (DragStart index xParam yParam) Mouse.position)
    --, SA.fill ("hsl(0," ++ toString (getRangeParamPosition colParam sound) ++ "%,50%)")
    , SA.fill <| hsla fillColor percent (75-(percent/2)) 0.4
    ]
    [ Svg.circle
        [ SA.class "svg-circle"
        , SA.cx (toString <| getRangeParamPosition xParam sound)
        , SA.cy (toString <| getRangeParamPosition yParam sound)
        , SA.r "7"
        ]
        []
    ]


myLabel : String -> Html msg  
myLabel s =
  div 
    [ A.style [("display", "inline-block"), ("width", "96px")]
    ]
    [ text s
    ]
