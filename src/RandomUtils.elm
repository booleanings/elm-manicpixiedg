module RandomUtils exposing (possibleEyeColors, possibleHairColors, generatedInt, generatedPair, extractColor)
import Random


possibleHairColors =
    [ "blue", "pink", "red", "green" ]


possibleEyeColors =
    [ "brown", "hazel", "green", "blue"]


extractColor pos list =
    Maybe.withDefault "black" (List.head (List.drop (pos - 1) (List.take pos list)))



-- -> "black"
-- random generated helper

-- helper function to take in a Girl and Return a ManicPixieDG.



generatedPair : Random.Generator ( Int, Int )
generatedPair =
    Random.pair (Random.int 1 4) (Random.int 1 5)


generatedInt : Random.Generator Int
generatedInt =
    Random.int 18 41
