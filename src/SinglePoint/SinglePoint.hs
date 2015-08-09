-- SinglePoint.hs, see listings 2.3 - 2.7 in the OpenGL SuperBible, 6th ed.
-- Adapted from singlepoint.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import SB6

data State = State
  { program :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Single Point" }

startup :: IO State
startup = do
  let vs_source = unlines
        [ "#version 420 core                             "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    gl_Position = vec4(0.0, 0.0, 0.0, 1.0);   "
        , "}                                             " ]
      fs_source = unlines
        [ "#version 420 core                             "
        , "                                              "
        , "out vec4 color;                               "
        , "                                              "
        , "void main(void)                               "
        , "{                                             "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);         "
        , "}                                             " ]

  theProgram <- createProgram
  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  mapM_ (attachShader theProgram) [ vs, fs ]

  linkProgram theProgram

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  return $ State { program = theProgram, vao = theVao }

render :: State -> Double -> IO ()
render state _currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 1 0 0 1)

  currentProgram $= Just (program state)

  pointSize $= 40

  drawArrays Points 0 1

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
