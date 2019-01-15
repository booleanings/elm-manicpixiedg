module RandomUtils exposing (possibleHobbies, possibleEyeColors, possibleHairColors, generatedPair2, generatedPair, extractFeature)
import Random


possibleHairColors =
    [ "blue", "pink", "red", "green" ]


possibleEyeColors =
    [ "brown", "hazel", "green", "blue"]

possibleHobbies = ["3D printing","amateur radio","scrapbook","Amateur radio","Acting","Baton twirling","Board games","Book restoration","Cabaret","Calligraphy","Candle making","Computer programming","Coffee roasting","Cooking","Coloring","Cosplaying","Couponing","Creative writing"
    ,"Crocheting","Cryptography","Dance","Digital arts","Drama","Drawing","Do it yourself","Electronics","Embroidery","Fashion","Flower arranging","Foreign language learning","Gaming","tabletop games","role-playing games","Gambling","Genealogy","Glassblowing","Gunsmithing","Homebrewing","Ice skating",
    "Jewelry making","Jigsaw puzzles","Juggling","Knapping","Knitting","Kabaddi","Knife making","Lacemaking","Lapidary","Leather crafting","Lego building","Lockpicking","Machining","Macrame","Metalworking","Magic","Model building","Listening to music","Origami","Painting",
    "Playing musical instruments","Pet","Poi","Pottery","Puzzles","Quilting","Reading","Scrapbooking","Sculpting","Sewing","Singing","Sketching","Soapmaking","Sports","Stand-up comedy","Sudoku","Table tennis","Taxidermy","Video gaming","Watching movies","Web surfing","Whittling","Wood carving",
    "Woodworking","Worldbuilding","Writing","Yoga","Yo-yoing","Air sports","Archery","Astronomy","Backpacking","BASE jumping","Baseball","Basketball","Beekeeping","Bird watching","Blacksmithing","Board sports","Bodybuilding","Brazilian jiu-jitsu",
    "Community","Cycling","Dowsing","Driving","Fishing","Flag Football","Flying","Flying disc","Foraging","Gardening","Geocaching","Ghost hunting","Graffiti","Handball","Hiking","Hooping","Horseback riding","Hunting","Inline skating","Jogging","Kayaking","Kite flying",
    "Kitesurfing","LARPing","Letterboxing","Metal detecting","Motor sports","Mountain biking","Mountaineering","Mushroom hunting","Mycology","Netball","Nordic skating","Orienteering","Paintball","Parkour","Photography","Polo","Rafting","Rappelling","Rock climbing","Roller skating",
    "Rugby","Running","Sailing","Sand art","Scouting","Scuba diving","Sculling","Rowing","Shooting","Shopping","Skateboarding","Skiing","Skimboarding","Skydiving","Slacklining","Snowboarding","Stone skipping","Surfing","Swimming","Taekwondo","Tai chi","Urban exploration","Vacation","Vehicle restoration","Water sports"]


extractFeature pos list =
    Maybe.withDefault "black" (List.head (List.drop (pos - 1) (List.take pos list)))
        

-- -> "black"
-- random generated helper

-- helper function to take in a Girl and Return a ManicPixieDG.



generatedPair : Random.Generator ( Int, Int )
generatedPair =
    Random.pair (Random.int 1 4) (Random.int 1 5)

generatedPair2 : Random.Generator ( Int, Int )
generatedPair2 =
    Random.pair (Random.int 1 125) (Random.int 18 41)