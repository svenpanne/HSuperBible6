module SB6.Object (
  Object, loadObject, freeObject, renderObject, renderSubObject
) where

import Control.Applicative( (<$>) )
import Control.Monad ( forM_ )
import Data.Array ( Array, (!) )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Foreign.Ptr  ( Ptr, nullPtr, plusPtr )
import Foreign.Storable ( sizeOf )
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.Raw (
  glDrawArraysInstancedBaseInstance, glDrawElementsInstancedBaseInstance,
  gl_TRIANGLES )
import SB6.DataType
import SB6.SB6M

data Object = Object
  { vertexBuffer :: BufferObject
  , vao :: VertexArrayObject
  , drawInfo :: Either (Array Int SubObject) (BufferObject, DataType, GLsizei)
  } deriving ( Eq, Ord, Show )

loadObject :: FilePath -> IO Object
loadObject filePath = do
  sb6m <- parseSB6M <$> BS.readFile filePath

  theVertexBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just theVertexBuffer
  withRawDataAtOffset sb6m (dataOffset (vertexData sb6m)) $ \ptr ->
    bufferData ArrayBuffer $= ( dataSize (vertexData sb6m), ptr, StaticDraw )

  theVao <- genObjectName
  bindVertexArrayObject $= Just theVao

  forM_ (zip (map AttribLocation [0..])
             (vertexAttribData sb6m)) $ \(loc, vad) -> do
    vertexAttribPointer loc $= (attribIntegerHandling vad, attribDescriptor vad)
    vertexAttribArray loc $= Enabled

  theDrawInfo <- maybe
    (return . Left  . subObjectList $ sb6m)
    (\ix -> do theIndexBuffer <- genObjectName
               bindBuffer ElementArrayBuffer $= Just theIndexBuffer
               withRawDataAtOffset sb6m (indexDataOffset ix) $ \ptr -> do
                 bufferData ElementArrayBuffer $= ( size ix , ptr , StaticDraw )
               return $ Right (theIndexBuffer, indexType ix, indexCount ix))
    (indexData sb6m)

  bindVertexArrayObject $= Nothing
  bindBuffer ElementArrayBuffer $= Nothing

  return $ Object
    { vertexBuffer = theVertexBuffer
    , vao = theVao
    , drawInfo = theDrawInfo }

withRawDataAtOffset :: SB6M a -> Int -> (Ptr b -> IO c) -> IO c
withRawDataAtOffset sb6m offset f =
  BSU.unsafeUseAsCString (rawData sb6m) $ \ptr ->
    f (ptr `plusPtr` offset)

-- TODO: Turn freeObject and renderSubObject into the only fields of Object?
freeObject :: Object -> IO ()
freeObject object = do
  deleteObjectName $ vao object
  deleteObjectName $ vertexBuffer object
  either (const $ return ()) (\(b,_,_) -> deleteObjectName b) $ drawInfo object

renderObject :: Object -> IO ()
renderObject object = renderSubObject object 0 1 0

renderSubObject :: Object -> Int -> GLsizei -> GLuint -> IO ()
renderSubObject object objectIndex instanceCount baseInstance = do
  bindVertexArrayObject $= Just (vao object)
  either
    (\subObjects ->
      let subObj = subObjects ! objectIndex
      in glDrawArraysInstancedBaseInstance gl_TRIANGLES
                                           (first subObj)
                                           (count subObj)
                                           instanceCount
                                           baseInstance)
    (\(_, t, n) ->
      glDrawElementsInstancedBaseInstance gl_TRIANGLES
                                          n
                                          (marshalDataType t)
                                          nullPtr
                                          instanceCount
                                          baseInstance)
    (drawInfo object)

size :: IndexData -> GLsizeiptr
size ix =
  fromIntegral (indexCount ix) * fromIntegral (dataTypeSize (indexType ix))

dataTypeSize :: DataType -> Int
dataTypeSize x = case x of
  UnsignedByte -> sizeOf (undefined :: GLubyte)
  Byte -> sizeOf (undefined :: GLbyte)
  UnsignedShort -> sizeOf (undefined :: GLushort)
  Short -> sizeOf (undefined :: GLshort)
  UnsignedInt -> sizeOf (undefined :: GLuint)
  Int -> sizeOf (undefined :: GLint)
  HalfFloat -> sizeOf (undefined :: GLhalf)
  Float -> sizeOf (undefined :: GLfloat)
  UnsignedByte332 -> sizeOf (undefined :: GLubyte)
  UnsignedByte233Rev -> sizeOf (undefined :: GLubyte)
  UnsignedShort565 ->  sizeOf (undefined :: GLushort)
  UnsignedShort565Rev -> sizeOf (undefined :: GLushort)
  UnsignedShort4444 -> sizeOf (undefined :: GLushort)
  UnsignedShort4444Rev -> sizeOf (undefined :: GLushort)
  UnsignedShort5551 -> sizeOf (undefined :: GLushort)
  UnsignedShort1555Rev -> sizeOf (undefined :: GLushort)
  UnsignedInt8888 -> sizeOf (undefined :: GLint)
  UnsignedInt8888Rev -> sizeOf (undefined :: GLint)
  UnsignedInt1010102 -> sizeOf (undefined :: GLint)
  UnsignedInt2101010Rev -> sizeOf (undefined :: GLint)
  UnsignedInt248 -> sizeOf (undefined :: GLint)
  UnsignedInt10f11f11fRev -> sizeOf (undefined :: GLint)
  UnsignedInt5999Rev -> sizeOf (undefined :: GLint)
  Float32UnsignedInt248Rev -> 8
  Bitmap -> 0
  UnsignedShort88 -> sizeOf (undefined :: GLushort)
  UnsignedShort88Rev -> sizeOf (undefined :: GLushort)
  Double -> sizeOf (undefined :: GLdouble)
  TwoBytes -> 2
  ThreeBytes -> 3
  FourBytes -> 3
