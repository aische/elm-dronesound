module Param exposing (..)


type alias Spec = 
  { map : Float -> Float
  , unmap : Float -> Float -- ignores the offset (domainMin)
  }


type alias MkSpec = (Float, Float) -> Spec


makeSpec : (Float, Float) -> MkSpec
makeSpec (domainMin, domainMax) (rangeMin, rangeMax) =
  let 
    domain = 
      (domainMax - domainMin)

    range = 
      (rangeMax - rangeMin)

    div_r_d = 
      range / domain
  in 
    { map = \d -> (d - domainMin) * div_r_d + rangeMin
    , unmap = \r -> (r - rangeMin) / div_r_d
    }


-- Param (knows about the domain of the parameter)
type alias Param a =
  { get : a -> Float 
  , set : Float -> a -> a 
  , add : Float -> a -> a 
  , domain : (Float, Float)
  }


makeParam : (Float, Float) -> (a -> Float) -> (Float -> a -> a) -> Param a
makeParam domain get set =
  let 
    lowerBound = 
      min (fst domain) (snd domain)

    upperBound = 
      max (fst domain) (snd domain)

    set' = 
      \f a -> set (min upperBound (max lowerBound f)) a
  in 
    { get = get 
    , set = set'
    , add = \f a -> set' (f + get a) a
    , domain = domain
    }


invertParam : Param a -> Param a
invertParam param =
  let 
    param' = 
      { param 
      | domain = (\(x,y)->(y,x)) param.domain 
      } 
  in
    param'


-- RangeParam (param + knows about the range of the view)
type alias RangeParam a = 
  { param : Param a
  , range : (Float, Float)
  , spec : Spec
  }


makeRangeParam : Param a -> (Float, Float) -> RangeParam a
makeRangeParam param range =
  { param = param 
  , range = range
  , spec = makeSpec param.domain range 
  }

updateRangeParamPosition : RangeParam a -> Float -> a -> a
updateRangeParamPosition rangeparam value obj =
  rangeparam.param.add (rangeparam.spec.unmap value) obj


getRangeParamPosition : RangeParam a -> a -> Float
getRangeParamPosition rangeparam obj =
  rangeparam.spec.map (rangeparam.param.get obj)
