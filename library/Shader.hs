-- |
-- Combined API for creating and using shaders.
module Shader (
 Program,
 Shader (..),
 Program.create,
 Program.attribute,
 Program.shader,
 Program.uniform,
 static,
) where

import Shader.Program (Program, Shader (..))
import Shader.Program qualified as Program
import Shader.Static (static)
