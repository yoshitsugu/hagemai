module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Effects exposing (Effects, Never)
import StartApp
import TransitRouter exposing (..)
import Routes exposing (..)
import ServerApi
import NewIssue
import TransitStyle
import IssueList
import IssueDetail

type alias Model = WithRoute Routes.Route                                     
  { newIssueModel : NewIssue.Model
  , issueListModel : IssueList.Model
  , issueDetailModel : IssueDetail.Model}

type Action
  = NoOp
  | NewIssueAction NewIssue.Action
  | IssueListAction IssueList.Action
  | IssueDetailAction IssueDetail.Action
  | RouterAction (TransitRouter.Action Route)


initialModel : Model
initialModel =
  { transitRouter = TransitRouter.empty Routes.EmptyRoute
  , newIssueModel = NewIssue.init
  , issueListModel = IssueList.init
  , issueDetailModel = IssueDetail.init
  }


actions : Signal Action
actions =
  Signal.map RouterAction TransitRouter.actions                         


routerConfig : TransitRouter.Config Route Action Model
routerConfig =                                                          
  { mountRoute = mountRoute
  , getDurations = \_ _ _ -> (50, 200)
  , actionWrapper = RouterAction
  , routeDecoder = Routes.decode
  }

mountRoute : Route -> Route -> Model -> (Model, Effects Action)
mountRoute prevRoute route model =                                      
  case route of
    NewIssuePage ->
      (model, Effects.none)
    IssueListPage ->
      (model, Effects.map IssueListAction (ServerApi.getIssues IssueList.HandleIssuesRetrieved))
    IssueDetailPage issueId ->
      (model, Effects.map IssueDetailAction (ServerApi.getIssueAndComments issueId IssueDetail.IssueDetailRetrieved))
    EmptyRoute ->
      (model, Effects.none)

init : String -> (Model, Effects Action)
init path =                                                             
  TransitRouter.init routerConfig path initialModel


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    NoOp ->
      (model, Effects.none)

    NewIssueAction newIssueAction ->
      let (model', effects) = NewIssue.update newIssueAction model.newIssueModel
      in ( { model | newIssueModel = model' }
         , Effects.map NewIssueAction effects )

    IssueListAction act ->                                                       
      let (model', effects) = IssueList.update act model.issueListModel
      in ( { model | issueListModel = model' }
         , Effects.map IssueListAction effects )

    IssueDetailAction act ->                                                       
      let (model', effects) = IssueDetail.update act model.issueDetailModel
      in ( { model | issueDetailModel = model' }
         , Effects.map IssueDetailAction effects )

    RouterAction routeAction ->
      TransitRouter.update routerConfig routeAction model


menu : Signal.Address Action -> Model -> Html
menu address model =                                                       
  header [class "navbar navbar-default navbar-static-top"] [
    div [class "container"] [
        div [class "navbar-header"] [
          div [ class "navbar-brand" ] [
            a (linkAttrs IssueListPage) [ text "Hagemai" ]
          ]
        ]
      , ul [class "nav navbar-nav"] [
          li [] [a ((linkAttrs NewIssuePage) ++ [class "btn btn-info navbar-btn"]) [ text "新規作成" ]]       
      ]
    ]
  ]


contentView : Signal.Address Action -> Model -> Html
contentView address model =                                                
  case (TransitRouter.getRoute model) of
    NewIssuePage ->
      NewIssue.view (Signal.forwardTo address NewIssueAction) model.newIssueModel

    IssueListPage ->                                                   
      IssueList.view (Signal.forwardTo address IssueListAction) model.issueListModel

    IssueDetailPage issueId ->                                                   
      IssueDetail.view (Signal.forwardTo address IssueDetailAction) model.issueDetailModel
      
    EmptyRoute ->
      div [style [("text-align","center"), ("padding","40px"), ("width", "100%") ]] [
        i [class "fa fa-spinner fa-pulse fa-5x fa-fw"] []
      ]

view : Signal.Address Action -> Model -> Html
view address model =
  div [class "wrapper"] [
      menu address model
    , div [ class "content"
          , style (TransitStyle.fade (getTransition model))]  
          [contentView address model]
  ]


app : StartApp.App Model
app =                                          
  StartApp.start
    { init = init initialPath
    , update = update
    , view = view
    , inputs = [ actions ]
    }



main : Signal Html
main =                                         
  app.html



port tasks : Signal (Task.Task Never ())
port tasks =                                   
  app.tasks

port initialPath : String
