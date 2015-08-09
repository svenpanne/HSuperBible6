-- TessellatedTri.hs, see listings 3.7 - 3.8 in the OpenGL SuperBible, 6th ed.
-- Adapted from tessellatedtri.cpp which is (c) 2012-2013 Graham Sellers.

module Main ( main ) where

import SB6

data State = State
  { program :: Program
  , vao :: VertexArrayObject
  }

init :: IO AppInfo
init = return $ appInfo { title = "OpenGL SuperBible - Tessellated Triangle" }

startup :: IO State
startup = do
  let vs_source = unlines
        [ "#version 410 core                                                 "
        , "                                                                  "
        , "void main(void)                                                   "
        , "{                                                                 "
        , "    const vec4 vertices[] = vec4[](vec4( 0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4(-0.25, -0.25, 0.5, 1.0),  "
        , "                                   vec4( 0.25,  0.25, 0.5, 1.0)); "
        , "                                                                  "
        , "    gl_Position = vertices[gl_VertexID];                          "
        , "}                                                                 " ]
      tcs_source = unlines
        [ "#version 410 core                                                                 "
        , "                                                                                  "
        , "layout (vertices = 3) out;                                                        "
        , "                                                                                  "
        , "void main(void)                                                                   "
        , "{                                                                                 "
        , "    if (gl_InvocationID == 0)                                                     "
        , "    {                                                                             "
        , "        gl_TessLevelInner[0] = 5.0;                                               "
        , "        gl_TessLevelOuter[0] = 5.0;                                               "
        , "        gl_TessLevelOuter[1] = 5.0;                                               "
        , "        gl_TessLevelOuter[2] = 5.0;                                               "
        , "    }                                                                             "
        , "    gl_out[gl_InvocationID].gl_Position = gl_in[gl_InvocationID].gl_Position;     "
        , "}                                                                                 " ]
      tes_source = unlines
        [ "#version 410 core                                                                 "
        , "                                                                                  "
        , "layout (triangles, equal_spacing, cw) in;                                         "
        , "                                                                                  "
        , "void main(void)                                                                   "
        , "{                                                                                 "
        , "    gl_Position = (gl_TessCoord.x * gl_in[0].gl_Position) +                       "
        , "                  (gl_TessCoord.y * gl_in[1].gl_Position) +                       "
        , "                  (gl_TessCoord.z * gl_in[2].gl_Position);                        "
        , "}                                                                                 " ]
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
  vs <- createShader VertexShader
  shaderSourceBS vs $= packUtf8 vs_source
  compileShader vs

  tcs <- createShader TessControlShader
  shaderSourceBS tcs $= packUtf8 tcs_source
  compileShader tcs

  tes <- createShader TessEvaluationShader
  shaderSourceBS tes $= packUtf8 tes_source
  compileShader tes

  fs <- createShader FragmentShader
  shaderSourceBS fs $= packUtf8 fs_source
  compileShader fs

  mapM_ (attachShader theProgram) [ vs, tcs, tes, fs ]

  linkProgram theProgram

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  polygonMode $= (Line, Line)

  return $ State { program = theProgram, vao = theVao }

render :: State -> Double -> IO ()
render state _currentTime = do
  clearBuffer $ ClearColorBufferFloat 0 (Color4 0 0.25 0 1)

  currentProgram $= Just (program state)

  drawArrays Patches 0 3

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
