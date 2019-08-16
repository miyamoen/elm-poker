module Poker exposing (Number(..), Suite(..))

import List.Extra


type Suite
    = Hart
    | Club
    | Spade
    | Diamond


suites : List Suite
suites =
    [ Hart
    , Club
    , Spade
    , Diamond
    ]


type Number
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King


numbers : List Number
numbers =
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    ]


type Card
    = Card Suite Number


type alias Deck =
    List Card


deck : Deck
deck =
    List.Extra.lift2 Card suites numbers


type Hand
    = Hand Card Card Card Card Card


type SortedHand
    = SortedHand Card Card Card Card Card


type RankedHand
    = HighCard Number Number Number Number Number
      -- 先頭がペアカード
    | OnePair Number Number Number Number
      -- 先頭２枚がペアカード
    | TwoPair Number Number Number
      -- 先頭がペアカード
    | ThreeOfAKind Number Number Number
      -- 一番後ろのカード
    | Straight Number
    | Flush Number Number Number Number Number
      -- 3枚、2枚
    | FullHouse Number Number
      -- 4枚、あまり１枚
    | FourOfAKind Number Number
      -- 一番後ろのカード
      -- ロイヤルストレートフラッシュはこれで兼用. StraightFlush Ace == ロイヤルストレートフラッシュ
    | StraightFlush Number


sortHand : Hand -> SortedHand
sortHand (Hand card1 card2 card3 card4 card5) =
    let
        sort2 c1 c2 =
            if compareCards c1 c2 == GT then
                ( c1, c2 )

            else
                ( c2, c1 )

        sort3 c1 c2 c3 =
            let
                ( c1_, c2_ ) =
                    sort2 c1 c2
            in
            if compareCards c3 c1_ == GT then
                ( c3, c1_, c2_ )

            else if compareCards c3 c2_ == GT then
                ( c1_, c3, c2_ )

            else
                ( c1_, c2_, c3 )

        sort4 c1 c2 c3 c4 =
            let
                ( c1_, c2_, c3_ ) =
                    sort3 c1 c2 c3
            in
            if compareCards c4 c1_ == GT then
                ( ( c4, c1_ ), c2_, c3_ )

            else if compareCards c4 c2_ == GT then
                ( ( c1_, c4 ), c2_, c3_ )

            else if compareCards c4 c3_ == GT then
                ( ( c1_, c2_ ), c4, c3_ )

            else
                ( ( c1_, c2_ ), c3_, c4 )

        sort5 c1 c2 c3 c4 c5 =
            let
                ( ( c1_, c2_ ), c3_, c4_ ) =
                    sort4 c1 c2 c3 c4
            in
            if compareCards c5 c1_ == GT then
                ( ( c5, c1_ ), c2_, ( c3_, c4_ ) )

            else if compareCards c5 c2_ == GT then
                ( ( c1_, c5 ), c2_, ( c3_, c4_ ) )

            else if compareCards c5 c3_ == GT then
                ( ( c1_, c2_ ), c5, ( c3_, c4_ ) )

            else if compareCards c5 c4_ == GT then
                ( ( c1_, c2_ ), c3_, ( c5, c4_ ) )

            else
                ( ( c1_, c2_ ), c3_, ( c4_, c5 ) )

        ( ( card1_, card2_ ), card3_, ( card4_, card5_ ) ) =
            sort5 card1 card2 card3 card4 card5
    in
    SortedHand card1_ card2_ card3_ card4_ card5_


compareCards : Card -> Card -> Order
compareCards (Card _ num1) (Card _ num2) =
    compareNumbers num1 num2


{-| Intにマッピングしたほうがいいだろさすがにこれ
-}
compareNumbers : Number -> Number -> Order
compareNumbers num1 num2 =
    case ( num1, num2 ) of
        ( Ace, Ace ) ->
            EQ

        ( Ace, _ ) ->
            GT

        ( King, Ace ) ->
            LT

        ( King, King ) ->
            EQ

        ( King, _ ) ->
            GT

        ( Queen, Ace ) ->
            LT

        ( Queen, King ) ->
            LT

        ( Queen, Queen ) ->
            EQ

        ( Queen, _ ) ->
            GT

        ( Jack, Ace ) ->
            LT

        ( Jack, King ) ->
            LT

        ( Jack, Queen ) ->
            LT

        ( Jack, Jack ) ->
            EQ

        ( Jack, _ ) ->
            GT

        ( Ten, Ace ) ->
            LT

        ( Ten, King ) ->
            LT

        ( Ten, Queen ) ->
            LT

        ( Ten, Jack ) ->
            LT

        ( Ten, Ten ) ->
            EQ

        ( Ten, _ ) ->
            GT

        ( Nine, Ace ) ->
            LT

        ( Nine, King ) ->
            LT

        ( Nine, Queen ) ->
            LT

        ( Nine, Jack ) ->
            LT

        ( Nine, Ten ) ->
            LT

        ( Nine, Nine ) ->
            EQ

        ( Nine, _ ) ->
            GT

        ( Eight, Ace ) ->
            LT

        ( Eight, King ) ->
            LT

        ( Eight, Queen ) ->
            LT

        ( Eight, Jack ) ->
            LT

        ( Eight, Ten ) ->
            LT

        ( Eight, Nine ) ->
            LT

        ( Eight, Eight ) ->
            EQ

        ( Eight, _ ) ->
            GT

        ( Seven, Two ) ->
            GT

        ( Seven, Three ) ->
            GT

        ( Seven, Four ) ->
            GT

        ( Seven, Five ) ->
            GT

        ( Seven, Six ) ->
            GT

        ( Seven, Seven ) ->
            EQ

        ( Seven, _ ) ->
            LT

        ( Six, Two ) ->
            GT

        ( Six, Three ) ->
            GT

        ( Six, Four ) ->
            GT

        ( Six, Five ) ->
            GT

        ( Six, Six ) ->
            EQ

        ( Six, _ ) ->
            LT

        ( Five, Two ) ->
            GT

        ( Five, Three ) ->
            GT

        ( Five, Four ) ->
            GT

        ( Five, Five ) ->
            EQ

        ( Five, _ ) ->
            LT

        ( Four, Two ) ->
            GT

        ( Four, Three ) ->
            GT

        ( Four, Four ) ->
            EQ

        ( Four, _ ) ->
            LT

        ( Three, Two ) ->
            GT

        ( Three, Three ) ->
            EQ

        ( Three, _ ) ->
            LT

        ( Two, Two ) ->
            EQ

        ( Two, _ ) ->
            LT


rank : SortedHand -> RankedHand
rank (SortedHand (Card s1 n1) (Card s2 n2) (Card s3 n3) (Card s4 n4) (Card s5 n5)) =
    case ( isFlush s1 s2 s3 s4 s5, isStrait n1 n2 n3 n4 n5 ) of
        ( True, Just number ) ->
            StraightFlush number

        ( False, Just number ) ->
            Straight number

        ( True, Nothing ) ->
            Flush n1 n2 n3 n4 n5

        ( False, Nothing ) ->
            case
                ( ( isFourOfAKind n1 n2 n3 n4 n5
                  , isFullHouse n1 n2 n3 n4 n5
                  , isThreeOfAKind n1 n2 n3 n4 n5
                  )
                , isTwoPair n1 n2 n3 n4 n5
                , isOnePair n1 n2 n3 n4 n5
                )
            of
                ( ( Just ( number1, number2 ), _, _ ), _, _ ) ->
                    FourOfAKind number1 number2

                ( ( _, Just ( number1, number2 ), _ ), _, _ ) ->
                    FullHouse number1 number2

                ( ( _, _, Just ( number1, number2, number3 ) ), _, _ ) ->
                    ThreeOfAKind number1 number2 number3

                ( ( _, _, _ ), Just ( number1, number2, number3 ), _ ) ->
                    TwoPair number1 number2 number3

                ( ( _, _, _ ), _, Just ( number1, ( number2, number3, number4 ) ) ) ->
                    OnePair number1 number2 number3 number4

                ( ( Nothing, Nothing, Nothing ), Nothing, Nothing ) ->
                    HighCard n1 n2 n3 n4 n5


{-| ストレートかどうかはみない
-}
isFlush : Suite -> Suite -> Suite -> Suite -> Suite -> Bool
isFlush suite1 suite2 suite3 suite4 suite5 =
    fiveEqual suite1 suite2 suite3 suite4 suite5


{-| フラッシュかどうかはみない
-}
isStrait : Number -> Number -> Number -> Number -> Number -> Maybe Number
isStrait number1 number2 number3 number4 number5 =
    case ( ( number1, number2, number3 ), number4, number5 ) of
        ( ( Ace, King, Queen ), Jack, Ten ) ->
            Just Ace

        ( ( King, Queen, Jack ), Ten, Nine ) ->
            Just King

        ( ( Queen, Jack, Ten ), Nine, Eight ) ->
            Just Queen

        ( ( Jack, Ten, Nine ), Eight, Seven ) ->
            Just Jack

        ( ( Ten, Nine, Eight ), Seven, Six ) ->
            Just Ten

        ( ( Nine, Eight, Seven ), Six, Five ) ->
            Just Nine

        ( ( Eight, Seven, Six ), Five, Four ) ->
            Just Eight

        ( ( Seven, Six, Five ), Four, Three ) ->
            Just Seven

        ( ( Six, Five, Four ), Three, Two ) ->
            Just Six

        ( ( Five, Four, Three ), Two, Ace ) ->
            Just Five

        _ ->
            Nothing


{-| 定義的に排他
-}
isFourOfAKind : Number -> Number -> Number -> Number -> Number -> Maybe ( Number, Number )
isFourOfAKind number1 number2 number3 number4 number5 =
    if fourEqual number1 number2 number3 number4 then
        Just ( number1, number5 )

    else if fourEqual number2 number3 number4 number5 then
        Just ( number5, number1 )

    else
        Nothing


{-| 定義的に排他
-}
isFullHouse : Number -> Number -> Number -> Number -> Number -> Maybe ( Number, Number )
isFullHouse number1 number2 number3 number4 number5 =
    if threeEqual number1 number2 number3 && number4 == number5 then
        Just ( number1, number4 )

    else if number1 == number2 && threeEqual number3 number4 number5 then
        Just ( number3, number1 )

    else
        Nothing


{-| 先にフルハウスとフォーカードをはじくこと
-}
isThreeOfAKind : Number -> Number -> Number -> Number -> Number -> Maybe ( Number, Number, Number )
isThreeOfAKind number1 number2 number3 number4 number5 =
    if threeEqual number1 number2 number3 then
        Just ( number1, number4, number5 )

    else if threeEqual number2 number3 number4 then
        Just ( number2, number1, number5 )

    else if threeEqual number3 number4 number5 then
        Just ( number3, number1, number2 )

    else
        Nothing


{-| 先にフルハウスとスリーカードとフォーカードをはじくこと
-}
isTwoPair : Number -> Number -> Number -> Number -> Number -> Maybe ( Number, Number, Number )
isTwoPair number1 number2 number3 number4 number5 =
    if number1 == number2 && number3 == number4 then
        Just ( number1, number3, number5 )

    else if number1 == number2 && number4 == number5 then
        Just ( number1, number4, number3 )

    else if number2 == number3 && number4 == number5 then
        Just ( number2, number4, number1 )

    else
        Nothing


{-| 先にフルハウスとスリーカードとフォーカードとツーペアをはじくこと
-}
isOnePair : Number -> Number -> Number -> Number -> Number -> Maybe ( Number, ( Number, Number, Number ) )
isOnePair number1 number2 number3 number4 number5 =
    if number1 == number2 then
        Just ( number1, ( number3, number4, number5 ) )

    else if number2 == number3 then
        Just ( number2, ( number1, number4, number5 ) )

    else if number3 == number4 then
        Just ( number3, ( number1, number2, number5 ) )

    else if number4 == number5 then
        Just ( number4, ( number1, number2, number3 ) )

    else
        Nothing


fiveEqual : a -> a -> a -> a -> a -> Bool
fiveEqual a1 a2 a3 a4 a5 =
    fourEqual a1 a2 a3 a4 && a1 == a5


fourEqual : a -> a -> a -> a -> Bool
fourEqual a1 a2 a3 a4 =
    threeEqual a1 a2 a3 && a1 == a4


threeEqual : a -> a -> a -> Bool
threeEqual a1 a2 a3 =
    a1 == a2 && a2 == a3


compairHands : Hand -> Hand -> Order
compairHands h1 h2 =
    case ( sortHand h1 |> rank, sortHand h2 |> rank ) of
        ( StraightFlush n1, StraightFlush n2 ) ->
            compareNumbers n1 n2

        ( StraightFlush _, _ ) ->
            GT

        ( _, StraightFlush _ ) ->
            LT

        ( FourOfAKind n11 n12, FourOfAKind n21 n22 ) ->
            compareTwoNumbers ( n11, n12 ) ( n21, n22 )

        ( FourOfAKind _ _, _ ) ->
            GT

        ( _, FourOfAKind _ _ ) ->
            LT

        ( FullHouse n11 n12, FullHouse n21 n22 ) ->
            compareTwoNumbers ( n11, n12 ) ( n21, n22 )

        ( FullHouse _ _, _ ) ->
            GT

        ( _, FullHouse _ _ ) ->
            LT

        ( Flush n11 n12 n13 n14 n15, Flush n21 n22 n23 n24 n25 ) ->
            compareFiveNumbers ( ( ( ( n11, n12 ), n13 ), n14 ), n15 ) ( ( ( ( n21, n22 ), n23 ), n24 ), n25 )

        ( Flush _ _ _ _ _, _ ) ->
            GT

        ( _, Flush _ _ _ _ _ ) ->
            LT

        ( Straight n1, Straight n2 ) ->
            compareNumbers n1 n2

        ( Straight _, _ ) ->
            GT

        ( _, Straight _ ) ->
            LT

        ( ThreeOfAKind n11 n12 n13, ThreeOfAKind n21 n22 n23 ) ->
            compareThreeNumbers ( ( n11, n12 ), n13 ) ( ( n21, n22 ), n23 )

        ( ThreeOfAKind _ _ _, _ ) ->
            GT

        ( _, ThreeOfAKind _ _ _ ) ->
            LT

        ( TwoPair n11 n12 n13, TwoPair n21 n22 n23 ) ->
            compareThreeNumbers ( ( n11, n12 ), n13 ) ( ( n21, n22 ), n23 )

        ( TwoPair _ _ _, _ ) ->
            GT

        ( _, TwoPair _ _ _ ) ->
            LT

        ( OnePair n11 n12 n13 n14, OnePair n21 n22 n23 n24 ) ->
            compareFourNumbers ( ( ( n11, n12 ), n13 ), n14 ) ( ( ( n21, n22 ), n23 ), n24 )

        ( OnePair _ _ _ _, _ ) ->
            GT

        ( _, OnePair _ _ _ _ ) ->
            LT

        ( HighCard n11 n12 n13 n14 n15, HighCard n21 n22 n23 n24 n25 ) ->
            compareFiveNumbers ( ( ( ( n11, n12 ), n13 ), n14 ), n15 ) ( ( ( ( n21, n22 ), n23 ), n24 ), n25 )


compareTwoNumbers : ( Number, Number ) -> ( Number, Number ) -> Order
compareTwoNumbers ( n11, n12 ) ( n21, n22 ) =
    case compareNumbers n11 n21 of
        EQ ->
            compareNumbers n12 n22

        ord ->
            ord


compareThreeNumbers : ( ( Number, Number ), Number ) -> ( ( Number, Number ), Number ) -> Order
compareThreeNumbers ( n11, n12 ) ( n21, n22 ) =
    case compareTwoNumbers n11 n21 of
        EQ ->
            compareNumbers n12 n22

        ord ->
            ord


compareFourNumbers :
    ( ( ( Number, Number ), Number ), Number )
    -> ( ( ( Number, Number ), Number ), Number )
    -> Order
compareFourNumbers ( n11, n12 ) ( n21, n22 ) =
    case compareThreeNumbers n11 n21 of
        EQ ->
            compareNumbers n12 n22

        ord ->
            ord


compareFiveNumbers :
    ( ( ( ( Number, Number ), Number ), Number ), Number )
    -> ( ( ( ( Number, Number ), Number ), Number ), Number )
    -> Order
compareFiveNumbers ( n11, n12 ) ( n21, n22 ) =
    case compareFourNumbers n11 n21 of
        EQ ->
            compareNumbers n12 n22

        ord ->
            ord
