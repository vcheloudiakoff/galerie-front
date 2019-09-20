module Helpers.ViewHelpers exposing (..)

-- import JsCommunication as Js
-- import List.Extra
-- import Markdown
-- import Random
-- import Remote.InputObject
-- import Remote.Mutation
-- import Remote.Object
-- import Remote.Object.Meeting
-- import Remote.Object.MeetingTemplate
-- import Remote.Object.Objective
-- import Remote.Object.Presentation
-- import Remote.Object.User
-- import Remote.Query
-- import Time exposing (Weekday(..))
-- import Time.Extra
-- import Types exposing (..)

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Events as E
import BodyBuilder.Extra as Layout
import BodyBuilder.Style as Style
import Browser
import Browser.Navigation as Nav
import Color
import Dict
import Elegant exposing (px)
import Elegant.Background as Background
import Elegant.Block as Block
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Display as Display
import Elegant.Extra
    exposing
        ( alignCenter
        , block
        , blockProperties
        , blockWithWidth
        , bold
        , border
        , box
        , cursorPointer
        , displayBlock
        , fontSize
        , grow
        , padding
        , paddingAll
        , paddingBottom
        , paddingHorizontal
        , paddingTop
        , paddingVertical
        , typoSize
        , typography
        )
import Elegant.Grid as Grid
import Elegant.Margin as Margin
import Elegant.Overflow as Overflow
import Elegant.Padding as Padding
import Elegant.Position as Position
import Elegant.Shadow as Shadow
import Elegant.Typography as Typography
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument
import Graphql.SelectionSet
import Html
import Ionicon.Android
import Ionicon.Ios
import Ionicon.Social
import Json.Decode
import Json.Encode
import Modifiers exposing (Modifier)
import Task
import Url
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query



-- import Uuid
-- VIEW HELPERS


fill : CustomGridItem msg
fill =
    fillColumn [] []


characterRound c =
    autoColumn
        [ paddingTop (px 3)
        , backgroundColor (Color.grayscale 0.4)
        , textColor Color.white
        , cornerRadius 100
        , block [ Block.alignCenter, Block.height (px 27), Block.width (px 27) ]
        , box [ typoSize 15 ]
        ]
        [ B.text c ]


mailtoLink : String -> String -> String
mailtoLink subject content =
    "mailto:?subject="
        ++ subject
        ++ "&body="
        ++ (content |> String.replace "\n" "%0D%0A")


type alias BoxContainerModifier a =
    Modifier (A.BoxContainer a)


overflowHidden : Modifiers.Modifier (A.MaybeBlockContainer a)
overflowHidden =
    block [ Block.overflowHidden ]


paddingRight : Elegant.SizeUnit -> BoxContainerModifier a
paddingRight val =
    padding [ Padding.right val ]


paddingLeft : Elegant.SizeUnit -> BoxContainerModifier a
paddingLeft val =
    padding [ Padding.left val ]


horizontalCenteredLayout :
    List
        (A.GridContainerAttributes msg
         -> A.GridContainerAttributes msg
        )
    -> List (CustomGridItem msg)
    -> NodeWithStyle msg
horizontalCenteredLayout =
    horizontalLayoutWithDetails [ centeredContent ]


verticalCenteredLayout :
    List
        (A.GridContainerAttributes msg
         -> A.GridContainerAttributes msg
        )
    -> List (CustomGridItem msg)
    -> NodeWithStyle msg
verticalCenteredLayout =
    verticalLayoutWithDetails [ centeredContent ]


centeredContent : Grid.GridContainerCoordinate -> Grid.GridContainerCoordinate
centeredContent =
    Grid.alignItems (Grid.alignWrapper Grid.center)


beautifullyStyledMainView : ( B.Node msg, List String ) -> NodeWithStyle msg
beautifullyStyledMainView a =
    horizontalLayout
        [ typography
            [ Typography.fontFamilySansSerif
            ]
        ]
        [ fillRow []
            [ ( Html.node "style"
                    []
                    [ Html.text "*{box-sizing: border-box;} .fe4pJf{border-right: #e0e0e0 1px solid} body {margin: 0px; height: 100%; display: grid} html {height: 100%; display: grid} textarea {resize : vertical} bb-grid-item {display: grid}"

                    -- , Html.text "* {outline: 1px solid red; }"
                    ]
              , []
              )
            , a
            ]
        ]


microsoftInputStyleWithGrayPlaceholder : Bool -> Modifiers.Modifier (A.BoxContainer a)
microsoftInputStyleWithGrayPlaceholder =
    microsoftInputStyle (Color.grayscale 0.3)


microsoftInputStyle : Color.Color -> Bool -> Modifiers.Modifier (A.BoxContainer a)
microsoftInputStyle placeholderColor isTextarea =
    let
        borderMethod =
            if isTextarea then
                Border.all

            else
                Border.bottom
    in
    A.style
        [ Style.box
            [ Box.borderNone
            , Box.paddingAll Constants.small
            , Box.typography [ Typography.size (px 14) ]
            , Box.textColor (Color.rgb 51 51 51)
            , Box.border
                [ borderMethod
                    [ Border.color (Color.rgb 149 149 149)
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
        , Style.box
            [ Box.outlineNone
            , Box.border
                [ borderMethod
                    [ Border.color (Color.rgb 12 97 205)
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
            |> Style.focus
        , Style.box
            [ Box.border
                [ borderMethod
                    [ Border.color Color.black
                    , Border.solid
                    , Border.thickness (px 1)
                    ]
                ]
            ]
            |> Style.hover
        , Style.box
            [ Box.textColor placeholderColor ]
            |> Style.pseudoClass ":placeholder"
        ]


blueButton =
    googleButton True (Color.rgb 59 96 221)


googleButton : Bool -> Color.Color -> String -> msg -> NodeWithStyle msg
googleButton isBlock color text msg =
    B.node
        ([ E.onClick msg
         , cursorPointer
         , A.style
            [ Style.box
                [ Box.backgroundColor color
                , Box.textColor Color.white
                , Box.cornerRadius 4
                ]
            ]
         , paddingAll
            Constants.medium
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        [ B.span
            []
            [ B.text text
            ]
        ]


transparentBorder =
    [ Border.thickness (px 1), Border.solid, Border.color (Color.rgba 0 0 0 0) ]


msButton : Bool -> Color.Color -> List (NodeWithStyle msg) -> msg -> NodeWithStyle msg
msButton isBlock color content msg =
    B.button
        ([ E.onClick msg
         , cursorPointer
         , A.style
            [ Style.box
                [ Box.backgroundColor color
                , Box.textColor Color.white
                ]
            ]
         , paddingVertical Constants.small
         , fontSize (px 14)
         , border [ Border.all transparentBorder ]
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        content


googleLink : Bool -> Color.Color -> NodeWithStyle msg -> String -> NodeWithStyle msg
googleLink isBlock color content href =
    B.a
        ([ A.href href
         , cursorPointer
         , typography [ Typography.noDecoration ]
         , block []
         , A.style
            [ Style.box
                [ Box.backgroundColor color
                , Box.textColor Color.white
                , Box.cornerRadius 4
                ]
            ]
         , paddingAll
            Constants.medium
         ]
            ++ (if isBlock then
                    [ block [ Block.alignCenter ] ]

                else
                    []
               )
        )
        [ content
        ]


cssColorString : String -> List String -> String
cssColorString kind values =
    kind ++ "(" ++ String.join ", " values ++ ")"


colorToCssRgba : Color.Color -> String
colorToCssRgba cl =
    let
        { red, green, blue, alpha } =
            Color.toRgb cl
    in
    cssColorString "rgba"
        [ String.fromInt red
        , String.fromInt green
        , String.fromInt blue
        , String.fromFloat alpha
        ]


icon iconSvg color =
    iconSvg color


{-| -}
type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


toRGBA { red, green, blue, alpha } =
    { red = (red |> toFloat) / 255
    , green = (green |> toFloat) / 255
    , blue = (blue |> toFloat) / 255
    , alpha = alpha
    }


{-| -}
ionIcon : (number -> RGBA -> Html.Html msg) -> Color.Color -> NodeWithStyle msg
ionIcon =
    ionIconWithSize 24


ionIconWithSize size fun color =
    ( fun size (Color.toRgb color |> toRGBA)
    , []
    )


leftIconWithDescription iconSvg description color =
    verticalLayoutWithDetails [ Grid.gap (px 3), Grid.alignItems (Grid.alignWrapper Grid.center) ]
        []
        [ autoRow []
            [ icon iconSvg color ]
        , autoRow
            [ typography [ Typography.color color ] ]
            description
        ]


iconWithDescription iconSvg description color =
    horizontalLayoutWithDetails [ mediumGap, Grid.alignItems (Grid.alignWrapper Grid.center) ]
        []
        [ autoRow []
            [ icon iconSvg color ]
        , fillRow
            [ typography [ Typography.color color ] ]
            description
        ]


leftIonIconWithDescription ionIconFun description color =
    verticalLayoutWithDetails [ mediumGap, Grid.alignItems (Grid.alignWrapper Grid.center) ]
        []
        [ autoRow []
            [ ionIcon ionIconFun color ]
        , fillRow
            [ typography [ Typography.color color ] ]
            description
        ]


ionIconWithDescription ionIconFun description color =
    horizontalLayoutWithDetails [ mediumGap, Grid.alignItems (Grid.alignWrapper Grid.center) ]
        []
        [ autoRow []
            [ ionIcon ionIconFun color ]
        , fillRow
            [ typography [ Typography.color color ] ]
            description
        ]


ionIconWithDescriptionAndSize ( size, myFontSize ) ionIconFun description color =
    horizontalLayoutWithDetails [ mediumGap, Grid.alignItems (Grid.alignWrapper Grid.center) ]
        [ fontSize myFontSize ]
        [ autoRow []
            [ ionIconWithSize size ionIconFun color ]
        , fillRow
            [ typography [ Typography.color color ] ]
            description
        ]


noDecoration =
    typography [ Typography.noDecoration ]



-- leftMenuItem : Url.Url -> Color.Color -> Color.Color -> String -> (Color.Color -> NodeWithStyle msg) -> CustomGridItem msg
-- leftMenuItem currentUrl colorActive colorPassive path textAndIcon =
--     leftMenuItemMulti currentUrl colorActive colorPassive path [] textAndIcon


transparent =
    Color.rgba 0 0 0 0


cornerRadius val =
    box [ Box.cornerRadius val ]


backgroundColor val =
    box [ Box.backgroundColor val ]


textColor val =
    box [ Box.textColor val ]



-- leftMenuItemMulti : Url.Url -> Color.Color -> Color.Color -> String -> List String -> (Color.Color -> NodeWithStyle msg) -> CustomGridItem msg
-- leftMenuItemMulti currentUrl colorActive colorPassive path startsWithElements textAndIcon =
--     let
--         isCurrentUrl =
--             currentUrl.path == path
--         isActive =
--             isCurrentUrl || (startsWithElements |> List.any (\e -> currentUrl.path |> String.startsWith e))
--         transparentBorder5 =
--             [ Border.thickness (px 5), Border.solid, Border.color transparent ]
--         whiteBorder5 =
--             [ Border.thickness (px 5), Border.solid, Border.color Color.white ]
--     in
--     autoRow
--         [ border
--             [ Border.right transparentBorder5
--             , Border.left
--                 (if isActive then
--                     whiteBorder5
--                  else
--                     transparentBorder5
--                 )
--             ]
--         , backgroundColor
--             (Color.rgba 255
--                 255
--                 255
--                 (if isActive then
--                     0.16
--                  else
--                     0
--                 )
--             )
--         ]
--         [ if isCurrentUrl then
--             B.span
--                 [ typography [ Typography.noDecoration, Typography.size (px 13) ]
--                 , textColor Color.white
--                 , paddingHorizontal (px 12)
--                 ]
--                 [ textAndIcon Color.white ]
--           else
--             B.a
--                 [ A.href path
--                 , typography [ Typography.noDecoration, Typography.size (px 13) ]
--                 , textColor Color.white
--                 , paddingHorizontal (px 12)
--                 ]
--                 [ textAndIcon Color.white ]
--         ]


targetBlank =
    A.target "_blank"



-- GRIDS


gridContainerProperties a =
    A.style [ Style.gridContainerProperties a ]


type alias CustomGridItem msg =
    { valType : Grid.ValType
    , attrs : List (A.GridItemAttributes msg -> A.GridItemAttributes msg)
    , content : List (NodeWithStyle msg)
    }


type alias CustomGridItemRepeatable msg =
    { repeatable : Grid.Repeatable
    , attrs : List (A.GridItemAttributes msg -> A.GridItemAttributes msg)
    , content : List (NodeWithStyle msg)
    }


horizontalLayoutWithDetails : List (Grid.GridContainerCoordinate -> Grid.GridContainerCoordinate) -> List (A.GridContainerAttributes msg -> A.GridContainerAttributes msg) -> List (CustomGridItem msg) -> NodeWithStyle msg
horizontalLayoutWithDetails gridAttrs attrs gridItems =
    B.grid
        ([ displayBlock
         , fillHeight
         , gridContainerProperties
            [ Grid.columns
                ([ Grid.template
                    (gridItems
                        |> List.map .valType
                        |> List.map Grid.simple
                    )
                 , Grid.align Grid.stretch
                 ]
                    ++ gridAttrs
                )
            ]
         ]
            ++ attrs
        )
        (gridItems |> List.map (\gridItem -> B.gridItem gridItem.attrs gridItem.content))


horizontalLayoutRepeatableWithDetails : List (Grid.GridContainerCoordinate -> Grid.GridContainerCoordinate) -> List (A.GridContainerAttributes msg -> A.GridContainerAttributes msg) -> List (CustomGridItemRepeatable msg) -> NodeWithStyle msg
horizontalLayoutRepeatableWithDetails gridAttrs attrs gridItems =
    B.grid
        ([ displayBlock
         , fillHeight
         , gridContainerProperties
            [ Grid.columns
                ([ Grid.template
                    (gridItems
                        |> List.map .repeatable
                    )
                 , Grid.align Grid.stretch
                 ]
                    ++ gridAttrs
                )
            ]
         ]
            ++ attrs
        )
        (gridItems |> List.map (\gridItem -> B.gridItem gridItem.attrs gridItem.content))


verticalLayout =
    verticalLayoutWithDetails []


verticalLayoutWithDetails : List (Grid.GridContainerCoordinate -> Grid.GridContainerCoordinate) -> List (A.GridContainerAttributes msg -> A.GridContainerAttributes msg) -> List (CustomGridItem msg) -> NodeWithStyle msg
verticalLayoutWithDetails gridAttrs attrs gridItems =
    B.grid
        ([ displayBlock
         , fillHeight
         , gridContainerProperties
            [ Grid.rows
                ([ Grid.template
                    (gridItems
                        |> List.map .valType
                        |> List.map Grid.simple
                    )
                 , Grid.alignItems Grid.spaceBetween
                 , Grid.align Grid.stretch
                 ]
                    ++ gridAttrs
                )
            ]
         ]
            ++ attrs
        )
        (gridItems |> List.map (\gridItem -> B.gridItem gridItem.attrs gridItem.content))


horizontalLayout =
    horizontalLayoutWithDetails []


stdGridItem valType attrs content =
    CustomGridItem valType attrs content


stdGridItemR valType attrs content =
    CustomGridItemRepeatable valType attrs content


column =
    stdGridItem


columnR =
    stdGridItemR


pxColumn pxSize =
    column (Grid.sizeUnitVal (px pxSize))


fillColumn =
    column (Grid.fractionOfAvailableSpace 1)


minMaxColumn val1 val2 =
    columnR (Grid.minmax val1 val2)


row =
    column


fillRow =
    fillColumn


autoRow =
    row Grid.auto


autoColumn =
    autoRow


fillHeight =
    block [ Block.height (Elegant.percent 100) ]


mediumGap =
    Grid.gap Constants.medium


largeGap =
    Grid.gap Constants.large


horizontallyCentered content =
    verticalLayoutWithDetails
        [ Grid.alignItems (Grid.alignWrapper Grid.center)
        ]
        []
        [ autoRow [] content ]


pxRow =
    pxColumn


mainBorder =
    [ Border.thickness (px 1), Border.solid, Border.color (Color.rgb 158 158 207) ]


electricGray : Color.Color
electricGray =
    Color.rgb 51 48 59


buttonColor =
    electricGray



-- pageWithLeftMenu user currentUrl content =
--     horizontalLayout []
--         [ leftMenu user currentUrl
--         , fillColumn []
--             [ horizontalLayout []
--                 content
--             ]
--         ]


borderStandard thickness color =
    [ Border.thickness (px thickness), Border.color color, Border.solid ]



-- showMeetingViewOld : Time.Posix -> Time.Zone -> Meeting -> NodeWithStyle Msg
-- showMeetingViewOld now timeZone meeting =
--     B.div
--         [ box
--             [ Box.backgroundColor (Color.rgb 158 158 207)
--             ]
--         ]
--         [ B.grid
--             [ block [ Block.minHeight (Elegant.vh 100) ]
--             , A.style
--                 [ Style.gridContainerProperties
--                     [ Grid.columns
--                         [ Grid.template
--                             [ Grid.simple (Grid.sizeUnitVal (px 310))
--                             , Grid.simple (Grid.fractionOfAvailableSpace 1)
--                             , Grid.simple (Grid.sizeUnitVal (px 310))
--                             ]
--                         , Grid.gap (px 1)
--                         , Grid.align Grid.stretch
--                         ]
--                     , Grid.rows
--                         [ Grid.template
--                             [ Grid.simple Grid.auto
--                             , Grid.simple (Grid.fractionOfAvailableSpace 1)
--                             ]
--                         , Grid.gap (px 1)
--                         , Grid.align Grid.stretch
--                         ]
--                     ]
--                 ]
--             ]
--             [ B.gridItem
--                 [ paddingAll Constants.large
--                 , box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ B.a [ A.href "/" ] [ ionIcon Ionicon.Ios.arrowThinLeft (Color.rgb 70 70 127) ]
--                 ]
--             , B.gridItem
--                 [ box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ pageTitle meeting.title ]
--             , B.gridItem
--                 [ paddingAll Constants.large
--                 , box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ Layout.customSpacer (px 3)
--                 , googleButton False buttonColor "Save" ((LoggedMsgWrapper << UpdateMeeting) meeting.id)
--                 , B.span [ paddingAll (px 3) ] []
--                 , googleButton False (Color.rgb 102 41 41) "Delete" ((LoggedMsgWrapper << DestroyMeeting) meeting.id)
--                 , B.span [ paddingAll (px 3) ] []
--                 , linkTo "Edit" ("/meetings/" ++ unwrapMeetingId meeting.id ++ "/edit")
--                 ]
--             , B.gridItem
--                 [ paddingAll Constants.large
--                 , box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ recipientsView meeting.recipients ]
--             , B.gridItem
--                 [ paddingAll Constants.large
--                 , box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ reviewMeetingViewOld now timeZone meeting ]
--             , B.gridItem
--                 [ paddingAll Constants.large
--                 , A.style
--                     []
--                 , box
--                     [ Box.backgroundColor Color.white
--                     ]
--                 ]
--                 [ googleButton True buttonColor "Save to Templates" (LoggedMsgWrapper SaveToTemplates)
--                 ]
--             ]
--         ]


linkTo : String -> String -> NodeWithStyle msg
linkTo label url =
    B.a
        [ A.href url
        , textColor <| electricGray
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]


linkToMsg : String -> msg -> NodeWithStyle msg
linkToMsg label msg =
    B.span
        [ textColor <| electricGray
        , typography [ Typography.noDecoration ]
        , E.onClick msg
        , cursorPointer
        ]
        [ B.text label ]


externalColoredLinkTo : Color.Color -> String -> String -> NodeWithStyle msg
externalColoredLinkTo color label url =
    B.a
        [ A.href url
        , textColor <| color
        , A.target "_blank"
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]


coloredLinkTo : Color.Color -> String -> String -> NodeWithStyle msg
coloredLinkTo color label url =
    B.a
        [ A.href url
        , textColor <| color
        , typography [ Typography.noDecoration ]
        ]
        [ B.text label
        ]



-- logo =
--     verticalLayout [ paddingTop Constants.medium, paddingBottom (px 96) ]
--         [ autoRow []
--             [ horizontalLayoutWithDetails [ Grid.alignItems (Grid.alignWrapper Grid.center) ]
--                 []
--                 [ autoRow []
--                     [ icon Icons.logo Color.white
--                     ]
--                 , autoRow
--                     [ fontSize (px 9), typography [ Typography.color Color.white ] ]
--                     [ B.text "Not", B.br, B.text "Only", B.br, B.text "Meetings" ]
--                 ]
--             ]
--         ]
-- logo_inverted =
--     verticalLayoutWithDetails [ Grid.alignItems (Grid.alignWrapper Grid.center) ]
--         [ paddingTop Constants.medium, paddingBottom (px 24) ]
--         [ autoRow []
--             [ horizontalLayoutWithDetails [ Grid.alignItems (Grid.alignWrapper Grid.center) ]
--                 []
--                 [ autoRow []
--                     [ icon Icons.logo (Color.rgb 1 204 206)
--                     ]
--                 , autoRow
--                     []
--                     [ B.text "Not", B.br, B.text "Only", B.br, B.text "Meetings" ]
--                 ]
--             ]
--         ]
-- leftMenu : User -> Url.Url -> CustomGridItem msg
-- leftMenu user currentUrl =
--     let
--         activeMenuItemMulti =
--             leftMenuItemMulti currentUrl Color.white (Color.grayscale 0.4)
--     in
--     pxColumn 104
--         [ backgroundColor buttonColor
--         , box
--             [ Box.textColor Color.white
--             ]
--         ]
--         [ B.div [ A.style [ Style.box [ Box.position (Position.fixed [ Position.top (px 0) ]) ], Style.blockProperties [ Block.height (Elegant.percent 100), Block.width (px 104) ] ] ]
--             [ verticalLayoutWithDetails []
--                 []
--                 [ autoRow [] [ logo ]
--                 -- , autoRow []
--                 --     [ B.a
--                 --         [ block []
--                 --         , backgroundColor Color.white
--                 --         , cornerRadius 5
--                 --         , paddingAll Constants.small
--                 --         , A.href "https://www.notonlymeetings.com/blog/how-to-create-a-new-meeting"
--                 --         , cursorPointer
--                 --         , targetBlank
--                 --         , noDecoration
--                 --         ]
--                 --         [ ionIconWithDescription Ionicon.Ios.plusEmpty [ B.text "New Meeting" ] electricGray ]
--                 --     ]
--                 , fillRow []
--                     [ verticalLayoutWithDetails []
--                         []
--                         [ activeMenuItemMulti "/" [ "/meetings" ] (leftIconWithDescription Icons.meetings [ B.text "Meetings" ])
--                         -- , activeMenuItemMulti "https://www.notonlymeetings.com/blog/how-to-activate-team-features" [ "/tasks" ] (leftIconWithDescription Icons.tasks [ B.text "Tasks" ])
--                         -- , activeMenuItemMulti "https://www.notonlymeetings.com/blog/how-to-activate-team-features" [ "/team" ] (leftIconWithDescription Icons.projects [ B.text "Projects" ])
--                         -- , activeMenuItemMulti "/templates" [ "/templates" ] (leftIconWithDescription Icons.templates [ B.text "Templates" ])
--                         -- , activeMenuItemMulti "https://www.notonlymeetings.com/blog/how-to-activate-team-features" [ "/files" ] (leftIconWithDescription Icons.files [ B.text "Files" ])
--                         , fillRow [] []
--                         -- , autoRow []
--                         --     [ B.div []
--                         --         [ ionIconWithDescriptionAndSize ( 30, px 15 ) Ionicon.Ios.bolt [ B.text "YOUR UPGRADE" ] Color.white
--                         --         , B.span [ textColor (Color.rgba 255 255 255 0.7) ] [ B.text "Your Not Only Meetings Trial will expire 14 days after your signup." ]
--                         --         , B.br
--                         --         , B.br
--                         --         , externalColoredLinkTo (Color.rgba 255 255 255 0.7) "Keep the ability to create more productive meetings for 4,50€/month" "https://payform.me/ngKV1Df"
--                         --         , B.br
--                         --         , B.br
--                         --         , B.a
--                         --             [ A.href "http://www.notonlymeetings.com/contact-us"
--                         --             , cursorPointer
--                         --             , targetBlank
--                         --             , noDecoration
--                         --             ]
--                         --             [ ionIconWithDescription Ionicon.Ios.help [ B.text "Get Help" ] (Color.rgba 255 255 255 0.7) ]
--                         --         -- , externalColoredLinkTo (Color.rgba 255 255 255 0.7) "Get Help" "http://www.notonlymeetings.com/contact-us"
--                         --         ]
--                         --     ]
--                         ]
--                     ]
--                 , autoRow
--                     [ paddingAll Constants.medium, fontSize (px 13) ]
--                     [ --  B.div [ blockProperties [ Display.textOverflowEllipsis, Block.maxWidth (px 104), Block.overflowHidden ] ]
--                       --     [ B.text "profile"
--                       --     ]
--                       -- , B.br,
--                       coloredLinkTo Color.white "Sign out" "/logout"
--                     ]
--                 ]
--             ]
--         ]
