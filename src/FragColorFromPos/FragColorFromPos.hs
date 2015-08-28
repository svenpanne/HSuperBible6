-- FragColorFromPos, see listings 3.10 - 3.12 in the OpenGL SuperBible, 6th ed.
-- Adapted from fragcolorfrompos.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import SB6

-- TODO: Use commandline argument or key callback?
interpolateColor :: Bool
interpolateColor = True

data State = State
  { program :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Triangle" }

startup :: IO State
startup = do
  let vs_source = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0));          "
        , "                                                                           "
        , "    gl_Position = vertices[gl_VertexID];                                   "
        , "}                                                                          " ]
      fs_source = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "out vec4 color;                                                            "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    color = vec4(sin(gl_FragCoord.x * 0.25) * 0.5 + 0.5,                   "
        , "                 cos(gl_FragCoord.y * 0.25) * 0.5 + 0.5,                   "
        , "                 sin(gl_FragCoord.x * 0.15) * cos(gl_FragCoord.y * 0.1),   "
        , "                 1.0);                                                     "
        , "}                                                                          " ]
      vs_source_interpolate = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "out vec4 vs_color;                                                         "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),           "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0));          "
        , "    const vec4 colors[] = vec4[](vec4(1.0, 0.0, 0.0, 1.0),                 "
        , "                                 vec4(0.0, 1.0, 0.0, 1.0),                 "
        , "                                 vec4(0.0, 0.0, 1.0, 1.0));                "
        , "                                                                           "
        , "    gl_Position = vertices[gl_VertexID];                                   "
        , "    vs_color = colors[gl_VertexID];                                        "
        , "}                                                                          " ]
      fs_source_interpolate = unlines
        [ "#version 420 core                                                          "
        , "                                                                           "
        , "in vec4 vs_color;                                                          "
        , "out vec4 color;                                                            "
        , "                                                                           "
        , "void main(void)                                                            "
        , "{                                                                          "
        , "    color = vs_color;                                                      "
        , "}                                                                          " ]

  theProgram <- createProgram
  fs <- createShader FragmentShader
  shaderSourceBS fs $=
    packUtf8 (if interpolateColor then fs_source_interpolate else fs_source)
  compileShader fs

  vs <- createShader VertexShader
  shaderSourceBS vs $=
    packUtf8 (if interpolateColor then vs_source_interpolate else vs_source)
  compileShader vs

  mapM_ (attachShader theProgram) [ vs, fs ]
  linkProgram theProgram

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  return $ State { program = theProgram, vao = theVao }

render :: State -> Double -> IO ()
render state _currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 0 0.25 0 1)

  currentProgram $= Just (program state)
  drawArrays Triangles 0 3

shutdown :: State -> IO ()
shutdown state = do
  deleteObjectName $ vao state
  deleteObjectName $ program state

main :: IO ()
main = run $ app
  { SB6.init = Main.init
  , SB6.startup = Main.startup
  , SB6.render = Main.render
  , SB6.shutdown = Main.shutdown
  }
