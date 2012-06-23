VoxelGFX
========

High Level Voxel Graphics Library

VoxelGFX is a high level library that aims to make working with voxels as easy as it is to work with pixels using common pixel graphics libraries.  It is 
designed to rely only on OpenGL for rendering.  It however also uses fbpng/zlib to support the saving and loading of .png files containing volume 
bitmaps.  As such VoxelGFX should be highly portable to any system supporting OpenGL, and should be easy to integrate into existing OpenGL programs.

VoxelGFX is written in FreeBASIC for use with FreeBASIC (<a href="http://www.freebasic.net">www.freebasic.net</a>).  However bindings to the C 
programming language and potentially other languages are anticipated.