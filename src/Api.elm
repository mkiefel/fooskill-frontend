module Api exposing (apiUrl, Skill, Player, User, userDecoder)

import Json.Decode as Decode


type alias Skill =
    { mu : Float
    , sigma2 : Float
    }


type alias Player =
    { skill : Skill
    }


type alias User =
    { id : String
    , name : String
    , player : Player
    }


skillDecoder : Decode.Decoder Skill
skillDecoder =
    let
        toMuSigma2 pi tau =
            Skill (tau / pi) (1.0 / pi)
    in
    Decode.map2 toMuSigma2 (Decode.field "pi" Decode.float) (Decode.field "tau" Decode.float)


playerDecoder : Decode.Decoder Player
playerDecoder =
    Decode.map Player <| Decode.field "skill" skillDecoder


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map3
        User
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "player" playerDecoder)


apiUrl : String
apiUrl =
    "/api/v1.0"
