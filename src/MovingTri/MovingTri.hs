-- MovingTri.hs, see listings 3.1 - 3.2 in the OpenGL SuperBible, 6th ed.
-- Adapted from movingtri.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import SB6

data State = State
  { program :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Moving Triangle" }

startup :: IO State
startup = do
  let vs_source = unlines
        [ "#version 410 core                                                 "
        , "                                                                  "
        , "layout (location = 0) in vec4 offset;                             "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0)); "
        , "                                                                  "
        , "    // Add 'offset' to our hard-coded vertex position             "
        , "    gl_Position = vertices[gl_VertexID] + offset;                 "
        , "}                                                                 " ]
      fs_source = unlines
        [ "#version 410 core                                                 "
        , "                                                                  "
        , "out vec4 color;                                                   "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    color = vec4(0.0, 0.8, 1.0, 1.0);                             "
        , "}                                                                 " ]

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
render state currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 0 0.25 0 1)

  currentProgram $= Just (program state)

  let attrib = Vertex4 (realToFrac (sin currentTime) * 0.5)
                       (realToFrac (cos currentTime) * 0.6)
                       0
                       0 :: Vertex4 GLfloat
  vertexAttrib ToFloat (AttribLocation 0) attrib

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
