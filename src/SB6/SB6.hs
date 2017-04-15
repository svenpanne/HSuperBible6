module SB6 (
  module Graphics.Rendering.OpenGL,
  module SB6.Application
) where

import Graphics.Rendering.OpenGL
-- The hiding is needed to avoid an incorrect warning with GHC 7.0.4.
import SB6.Application hiding (extensionSupported)
