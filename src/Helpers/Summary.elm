module Page.Meetings.Summary exposing (Model, Msg, graphqlSubscriptions, init, update, view)

import BodyBuilder as B exposing (NodeWithStyle)
import BodyBuilder.Attributes as A
import BodyBuilder.Events as E
import BodyBuilder.Extra as Layout
import BodyBuilder.Style as Style
import Browser
import Browser.Navigation as Nav
import Color
import DateFormat
import DateTime
import DateTimePicker
import Dict
import Elegant exposing (percent, px)
import Elegant.Background as Background
import Elegant.Block as Block
import Elegant.Border as Border
import Elegant.Box as Box
import Elegant.Constants as Constants
import Elegant.Corner as Corner
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
import Elegant.Padding as Padding
import Elegant.Position as Position
import Elegant.Shadow as Shadow
import Elegant.Typography as Typography
import Form
import Graphql exposing (..)
import Graphql.Document
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument
import Graphql.SelectionSet
import Html
import Icons
import Ionicon.Android
import Ionicon.Ios
import Ionicon.Social
import JsCommunication as Js
import Json.Decode
import Json.Encode
import List.Extra
import Markdown
import Maybe.Extra
import ModelsHelpers exposing (..)
import Modifiers exposing (Modifier)
import Random
import Remote.InputObject
import Remote.Mutation
import Remote.Object
import Remote.Object.Meeting
import Remote.Object.MeetingTemplate
import Remote.Object.Objective
import Remote.Object.Presentation
import Remote.Object.User
import Remote.Query
import RemoteData
import Task
import Time exposing (Weekday(..))
import Time.Extra
import TimeHelpers exposing (..)
import Types exposing (..)
import Url
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query
import Uuid
import ViewHelpers exposing (..)


type alias ServerStream a =
    { a | user : Maybe User }


type alias BrowserStream a =
    a


type alias Stream a =
    BrowserStream (ServerStream a)


type alias Model =
    { meetingId : MeetingId
    , maybeMeeting : RemoteData.WebData (Maybe MeetingForSummary)
    }


type Msg
    = GotMeeting (Maybe MeetingForSummary)


init : Maybe User -> MeetingId -> Model
init maybeUser meetingId =
    { meetingId = meetingId
    , maybeMeeting = RemoteData.Loading
    }


graphqlSubscriptions : Model -> List (SubscriptionRecords Msg)
graphqlSubscriptions model =
    [ MeetingForSummaryQuery (GraphqlQuery GotMeeting (getMeetingForSummaryReq model.meetingId))
    ]


update : Stream a -> Msg -> Model -> ( Model, Cmd Msg )
update stream msg model =
    case msg of
        GotMeeting meetingForSummary ->
            ( { model | maybeMeeting = RemoteData.Success meetingForSummary }, Cmd.none )


view : User -> BrowserStream (ServerStream a) -> Model -> NodeWithStyle Msg
view currentUser stream state =
    case state.maybeMeeting of
        RemoteData.Success Nothing ->
            B.text "Meeting not found"

        RemoteData.Success (Just meeting) ->
            showMeetingView Time.utc meeting currentUser state

        _ ->
            centered (B.span [ fontSize (px 22) ] [ B.text "Loading Summary..." ])


objectiveView : Objective -> NodeWithStyle Msg
objectiveView objective =
    horizontalLayout [ fontSize (px 13), paddingBottom Constants.small ]
        [ pxColumn 80
            [ paddingRight Constants.medium ]
            [ B.span []
                (if objective.done then
                    [ B.text "[Réalisé]"
                    ]

                 else
                    [ B.text "[En cours]" ]
                )
            ]
        , autoColumn [] [ B.span [] [ t objective.content ] ]
        , fill
        ]


objectivesView : List Objective -> NodeWithStyle Msg
objectivesView objectives =
    verticalLayout []
        [ autoRow []
            [ horizontalLayout []
                [ fillColumn [] [ panelTitle "Objectives" ]
                ]
            ]
        , autoRow [] [ B.div [] (List.map objectiveView (objectives |> List.sortBy .position)) ]
        ]


recipientsView : List User -> NodeWithStyle Msg
recipientsView users =
    B.div []
        [ panelTitle "Participants"
        , B.div [] (List.map recipientView users)
        ]


recipientView : User -> NodeWithStyle msg
recipientView user =
    horizontalCenteredLayout [ paddingBottom Constants.small ] [ avatarView user, fillColumn [ paddingLeft Constants.medium ] [ t <| userName user ] ]


panelTitle : String -> NodeWithStyle msg
panelTitle text =
    B.div
        [ fontSize (px 24)
        , paddingTop Constants.large
        , paddingBottom Constants.medium
        ]
        [ B.text text ]


feedInside : Time.Zone -> TopicWithFeed -> NodeWithStyle Msg
feedInside timeZone topicWithFeed =
    verticalLayout []
        [ autoRow [ fontSize (px 13) ]
            (topicWithFeed.feedEvents
                |> List.sortBy (.postedAt >> Time.posixToMillis)
                |> List.reverse
                |> List.map (feedEventView timeZone)
                |> List.intersperse (B.div [ paddingBottom Constants.medium ] [])
            )
        ]


centered content =
    verticalLayoutWithDetails [ centeredContent ] [] [ fillColumn [] [ horizontalCenteredLayout [] [ fillRow [] [ content ] ] ] ]


feedEventView : Time.Zone -> FeedEvent -> NodeWithStyle msg
feedEventView timeZone feedEvent =
    horizontalLayout []
        [ pxRow 70
            [ backgroundColor (Color.rgb 202 202 209)
            , textColor Color.white
            , paddingVertical Constants.medium
            , box [ Box.corner [ Corner.circular Corner.left (px 4) ] ]
            ]
            [ centered (B.text "Note") ]
        , fillRow
            [ backgroundColor (Color.rgb 248 247 252)
            , paddingVertical Constants.medium
            , box [ Box.corner [ Corner.circular Corner.right (px 4) ] ]
            , paddingHorizontal Constants.large
            ]
            [ B.text feedEvent.content ]
        , pxRow 48
            [ fontSize (px 8)
            , paddingLeft Constants.small
            , paddingTop Constants.small
            ]
            [ B.text (formatTime timeZone feedEvent.postedAt) ]
        ]


boxShadow e =
    box [ Box.boxShadow e ]


feed : Time.Zone -> TopicWithFeed -> NodeWithStyle Msg
feed timeZone topicWithFeed =
    verticalLayout []
        [ autoRow
            [ paddingLeft Constants.large, paddingVertical Constants.medium ]
            [ verticalLayout []
                [ autoRow []
                    [ horizontalLayout [ paddingTop Constants.medium, paddingBottom Constants.medium ]
                        [ fillColumn []
                            [ B.div [ fontSize (px 24) ]
                                [ B.text topicWithFeed.subject
                                ]
                            ]
                        ]
                    ]
                , fillRow [] [ t topicWithFeed.details ]
                ]
            ]
        , autoRow [ paddingAll Constants.medium ]
            [ B.div
                [ paddingLeft Constants.huge
                , paddingVertical Constants.huge
                , boxShadow [ Shadow.standard (px 61) (Color.rgba 0 0 0 0.16) ( px 0, px 3 ) ]
                , cornerRadius 8
                , backgroundColor Color.white
                ]
                [ feedInside timeZone topicWithFeed
                ]
            ]
        ]


findTopic : { a | topics : List { a | id : TopicId } } -> TopicId -> Maybe { a | id : TopicId }
findTopic meeting topicId =
    meeting.topics |> List.filter (\e -> e.id == topicId) |> List.head


t : String -> NodeWithStyle msg
t e =
    B.span [ fontSize (px 13) ] [ B.text e ]


showMeetingView : Time.Zone -> MeetingForSummary -> User -> Model -> NodeWithStyle Msg
showMeetingView timeZone meeting user state =
    verticalLayout []
        [ autoRow
            [ paddingAll Constants.large ]
            [ verticalLayout []
                [ fillRow []
                    [ verticalLayout []
                        [ autoRow
                            [ fontSize (px 30)
                            , bold
                            , paddingVertical Constants.large
                            ]
                            [ B.text meeting.title
                            ]
                        , autoRow [] [ panelTitle "Description" ]
                        , autoRow [] [ t meeting.instructions ]
                        , autoRow []
                            [ objectivesView meeting.objectives
                            ]
                        , autoRow []
                            [ recipientsView meeting.recipients
                            ]
                        , fillRow [] []
                        ]
                    ]
                ]
            ]
        , autoRow []
            [ verticalLayout []
                [ fillRow [ backgroundColor (Color.rgb 249 249 252) ]
                    (meeting.topics |> List.map (\topicWithFeed -> feed timeZone topicWithFeed))
                ]
            ]
        ]
