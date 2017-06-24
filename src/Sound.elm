port module Sound exposing (..)


import Random


import Param exposing (..)


port setSound : (Int, Sound) -> Cmd msg
port setSoundList : List (Int, Sound) -> Cmd msg
port removeSound : Int -> Cmd msg
port setMasterVolume : Float -> Cmd msg

  
type alias Sound =
  { note : Float
  , pan : Float
  , amp : Float
  , cutoff : Float
  }


noteParam : Param Sound
noteParam = makeParam (0, 127) (.note) (\a s -> {s | note = a })


panParam : Param Sound
panParam  = makeParam (-1, 1) (.pan) (\a s -> {s | pan = a })


ampParam : Param Sound
ampParam  = makeParam (0, 1) (.amp) (\a s -> {s | amp = a })


cutoffParam : Param Sound
cutoffParam  = makeParam (1, 8) (.cutoff) (\a s -> {s | cutoff = a })


initialSound : Sound
initialSound = {note=60, pan=0.0, amp=0.5, cutoff=2.4}


initialSounds : List Sound
initialSounds = List.repeat 1 initialSound


randomizeSounds : Random.Seed -> List Sound -> (Random.Seed, List Sound)
randomizeSounds seed sounds =
  case sounds of 
    sound :: rest ->
      let (r1, seed1) = Random.step (Random.float (-0.1) 0.1) seed
          (r2, seed2) = Random.step (Random.float (-0.01) 0.01) seed1
          (r3, seed3) = Random.step (Random.float (-0.01) 0.01) seed2
          (r4, seed4) = Random.step (Random.float (-0.1) 0.1) seed3
          (seed5, rest') = randomizeSounds seed4 rest
      in 
        ( seed5
        , (noteParam.add r1 <| panParam.add r2 <| ampParam.add r3  <| cutoffParam.add r4 sound) :: rest'
        )
    _ -> (seed, [])
