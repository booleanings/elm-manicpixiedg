module Types exposing (ApiName, Msg(..))

import Http



-- MSG


type Msg
    = NoOp
    | FetchNames (Result Http.Error ApiName)


type alias ApiName =
    { name : String }
