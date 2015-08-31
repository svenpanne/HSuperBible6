-- FragColorFromPos, see listings 3.10 - 3.12 in the OpenGL SuperBible, 6th ed.
-- Adapted from fragcolorfrompos.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import Data.IORef ( IORef, newIORef )
import SB6

-- Note that we deviate a bit from the book: We use a key callback to toggle
-- (via the 'm' key) between the 2 shader programs instead of making this a
-- compile-time decision.
data State = State
  { interpolate :: IORef Bool
  , programPos :: Program
  , programInterpolate :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Simple Triangle" }

startup :: IO State
startup = do
  let vs_source_pos = unlines
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
      fs_source_pos = unlines
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

  theProgramInterpolate <-
    compileAndLink fs_source_interpolate vs_source_interpolate
  theProgramPos <- compileAndLink fs_source_pos vs_source_pos

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  theInterpolate <- newIORef True
  return $ State
    { interpolate = theInterpolate
    , programPos = theProgramPos
    , programInterpolate = theProgramInterpolate
    , vao = theVao
    }

compileAndLink :: String -> String -> IO Program
compileAndLink fs_source vs_source = do
  theProgram <- createProgram

  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  mapM_ (attachShader theProgram) [ vs, fs ]
  linkProgram theProgram
  return theProgram

-- TODO: Perhaps we should return the state to get rid of the IORef?
onKey :: State -> Either SpecialKey Char -> KeyState -> IO ()
onKey state (Right 'm') Down = interpolate state $~! not
onKey _ _ _ = return ()

render :: State -> Double -> IO ()
render state _currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 0 0.25 0 1)

  i <- get (interpolate state)
  currentProgram $= Just ((if i then programInterpolate else programPos) state)
  drawArrays Triangles 0 3

shutdown :: State -> IO ()
shutdown state = do
  deleteObjectName $ vao state
  deleteObjectName $ programPos state
  deleteObjectName $ programInterpolate state

main :: IO ()
main = run $ app
  { SB6.init = Main.init
  , SB6.startup = Main.startup
  , SB6.render = Main.render
  , SB6.shutdown = Main.shutdown
  , SB6.onKey = Main.onKey
  }
