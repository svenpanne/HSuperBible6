{-# LANGUAGE CPP #-}
-- TODO: This should somehow be exported from OpenGL.
module SB6.DataType where

import Graphics.Rendering.OpenGL
#if MIN_VERSION_OpenGLRaw(3,0,0)
import Graphics.GL
#else
import Graphics.Rendering.OpenGL.Raw
#define GL_UNSIGNED_BYTE gl_UNSIGNED_BYTE
#define GL_BYTE gl_BYTE
#define GL_UNSIGNED_SHORT gl_UNSIGNED_SHORT
#define GL_SHORT gl_SHORT
#define GL_UNSIGNED_INT gl_UNSIGNED_INT
#define GL_INT gl_INT
#define GL_HALF_FLOAT gl_HALF_FLOAT
#define GL_FLOAT gl_FLOAT
#define GL_UNSIGNED_BYTE_3_3_2 gl_UNSIGNED_BYTE_3_3_2
#define GL_UNSIGNED_BYTE_2_3_3_REV gl_UNSIGNED_BYTE_2_3_3_REV
#define GL_UNSIGNED_SHORT_5_6_5 gl_UNSIGNED_SHORT_5_6_5
#define GL_UNSIGNED_SHORT_5_6_5_REV gl_UNSIGNED_SHORT_5_6_5_REV
#define GL_UNSIGNED_SHORT_4_4_4_4 gl_UNSIGNED_SHORT_4_4_4_4
#define GL_UNSIGNED_SHORT_4_4_4_4_REV gl_UNSIGNED_SHORT_4_4_4_4_REV
#define GL_UNSIGNED_SHORT_5_5_5_1 gl_UNSIGNED_SHORT_5_5_5_1
#define GL_UNSIGNED_SHORT_1_5_5_5_REV gl_UNSIGNED_SHORT_1_5_5_5_REV
#define GL_UNSIGNED_INT_8_8_8_8 gl_UNSIGNED_INT_8_8_8_8
#define GL_UNSIGNED_INT_8_8_8_8_REV gl_UNSIGNED_INT_8_8_8_8_REV
#define GL_UNSIGNED_INT_10_10_10_2 gl_UNSIGNED_INT_10_10_10_2
#define GL_UNSIGNED_INT_2_10_10_10_REV gl_UNSIGNED_INT_2_10_10_10_REV
#define GL_UNSIGNED_INT_24_8 gl_UNSIGNED_INT_24_8
#define GL_UNSIGNED_INT_10F_11F_11F_REV gl_UNSIGNED_INT_10F_11F_11F_REV
#define GL_UNSIGNED_INT_5_9_9_9_REV gl_UNSIGNED_INT_5_9_9_9_REV
#define GL_FLOAT_32_UNSIGNED_INT_24_8_REV gl_FLOAT_32_UNSIGNED_INT_24_8_REV
#define GL_BITMAP gl_BITMAP
#define GL_UNSIGNED_SHORT_8_8_APPLE gl_UNSIGNED_SHORT_8_8_APPLE
#define GL_UNSIGNED_SHORT_8_8_REV_APPLE gl_UNSIGNED_SHORT_8_8_REV_APPLE
#define GL_DOUBLE gl_DOUBLE
#define GL_2_BYTES gl_2_BYTES
#define GL_3_BYTES gl_3_BYTES
#define GL_4_BYTES gl_4_BYTES
#endif

unmarshalDataType :: GLenum -> DataType
unmarshalDataType x
   | x == GL_UNSIGNED_BYTE = UnsignedByte
   | x == GL_BYTE = Byte
   | x == GL_UNSIGNED_SHORT = UnsignedShort
   | x == GL_SHORT = Short
   | x == GL_UNSIGNED_INT = UnsignedInt
   | x == GL_INT = Int
   | x == GL_HALF_FLOAT = HalfFloat
   | x == GL_FLOAT = Float
   | x == GL_UNSIGNED_BYTE_3_3_2 = UnsignedByte332
   | x == GL_UNSIGNED_BYTE_2_3_3_REV = UnsignedByte233Rev
   | x == GL_UNSIGNED_SHORT_5_6_5 = UnsignedShort565
   | x == GL_UNSIGNED_SHORT_5_6_5_REV = UnsignedShort565Rev
   | x == GL_UNSIGNED_SHORT_4_4_4_4 = UnsignedShort4444
   | x == GL_UNSIGNED_SHORT_4_4_4_4_REV = UnsignedShort4444Rev
   | x == GL_UNSIGNED_SHORT_5_5_5_1 = UnsignedShort5551
   | x == GL_UNSIGNED_SHORT_1_5_5_5_REV = UnsignedShort1555Rev
   | x == GL_UNSIGNED_INT_8_8_8_8 = UnsignedInt8888
   | x == GL_UNSIGNED_INT_8_8_8_8_REV = UnsignedInt8888Rev
   | x == GL_UNSIGNED_INT_10_10_10_2 = UnsignedInt1010102
   | x == GL_UNSIGNED_INT_2_10_10_10_REV = UnsignedInt2101010Rev
   | x == GL_UNSIGNED_INT_24_8 = UnsignedInt248
   | x == GL_UNSIGNED_INT_10F_11F_11F_REV = UnsignedInt10f11f11fRev
   | x == GL_UNSIGNED_INT_5_9_9_9_REV = UnsignedInt5999Rev
   | x == GL_FLOAT_32_UNSIGNED_INT_24_8_REV = Float32UnsignedInt248Rev
   | x == GL_BITMAP = Bitmap
   | x == GL_UNSIGNED_SHORT_8_8_APPLE = UnsignedShort88
   | x == GL_UNSIGNED_SHORT_8_8_REV_APPLE = UnsignedShort88Rev
   | x == GL_DOUBLE = Double
   | x == GL_2_BYTES = TwoBytes
   | x == GL_3_BYTES = ThreeBytes
   | x == GL_4_BYTES = FourBytes
   | otherwise = error ("unmarshalDataType: illegal value " ++ show x)
