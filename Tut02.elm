module Main exposing (main)

import Math.Vector3 exposing (..)
import Math.Vector4 exposing (..)
import Math.Matrix4 exposing (..)
import Math.Matrix4 as M4
import Html exposing (Html)
import Html.Attributes as Attr
import WebGL exposing (..)


type alias Model =
    { perspective : Mat4
    , triangle : Object3D Vertex
    , square : Object3D Vertex
    }


type Msg
    = NoOp


type alias Object3D a =
    { mesh : Drawable a
    , modelView : Mat4
    }


type alias Vertex =
    { position : Vec3
    , color : Vec4
    }


makeObject3D : Drawable a -> Vec3 -> Object3D a
makeObject3D mesh position =
    { mesh = mesh, modelView = makeTranslate position }


triangle : Drawable Vertex
triangle =
    Triangle
        [ ( Vertex (vec3 0.0 1.0 0.0) (vec4 1.0 0.0 0.0 1.0)
          , Vertex (vec3 -1.0 -1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
          , Vertex (vec3 1.0 -1.0 0.0) (vec4 0.0 0.0 1.0 1.0)
          )
        ]


square : Drawable Vertex
square =
    TriangleStrip
        [ Vertex (vec3 1.0 1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
        , Vertex (vec3 1.0 -1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
        , Vertex (vec3 -1.0 1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
        , Vertex (vec3 -1.0 -1.0 0.0) (vec4 0.0 1.0 0.0 1.0)
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { perspective = perspective
      , triangle = makeObject3D triangle <| vec3 -1.5 0.0 -7.0
      , square = makeObject3D square <| vec3 1.5 0.0 -7.0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    WebGL.toHtml
        [ Attr.width canvasWidth, Attr.height canvasHeight ]
        [ render vertexShader
            fragmentShader
            model.triangle.mesh
            { perspective = model.perspective
            , modelView = model.triangle.modelView
            }
        , render vertexShader
            fragmentShader
            model.square.mesh
            { perspective = model.perspective
            , modelView = model.square.modelView
            }
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


canvasHeight : Int
canvasHeight =
    400


canvasWidth : Int
canvasWidth =
    500


perspective : Mat4
perspective =
    makePerspective 45 (toFloat canvasWidth / toFloat canvasHeight) 0.1 100.0



-- Shaders


vertexShader :
    Shader
        { attr
            | position : Vec3
            , color : Vec4
        }
        { unif
            | perspective : Mat4
            , modelView : Mat4
        }
        { vcolor : Vec4 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec4 color;
uniform mat4 perspective;
uniform mat4 modelView;
varying vec4 vcolor;

void main (void) {
    gl_Position = perspective * modelView * vec4(position, 1.0);
    vcolor = color;
}

    |]


fragmentShader : Shader {} u { vcolor : Vec4 }
fragmentShader =
    [glsl|

precision mediump float;

varying vec4 vcolor;

void main (void) {
    gl_FragColor = vcolor;
}

    |]
