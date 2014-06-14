#include "SDL.h"
#include "SDL_image.h"
module Graphics.UI.SDL.Image
  ( ImageType(..)
  , version
  , initJpg
  , initPng
  , initTif
  , initWebp
  , init
  , quit
  , loadTypedRW
  , load
  , loadRW 
  , loadTexture 
  , loadTextureRW 
  , loadTextureTypedRW 
  , isICO
  , isCUR
  , isBMP
  , isGIF
  , isJPG
  , isLBM
  , isPCX
  , isPNG
  , isPNM
  , isTIF
  , isXCF
  , isXPM
  , isXV
  , isWEBP
  , loadICORW 
  , loadCURRW 
  , loadBMPRW
  , loadGIFRW
  , loadJPGRW
  , loadLBMRW
  , loadPCXRW
  , loadPNGRW
  , loadPNMRW
  , loadTGARW
  , loadTIFRW
  , loadXCFRW
  , loadXPMRW
  , loadXVRW
  , loadWEBPRW
  , savePNG
  , savePNGRW 
  ) where

import Foreign
import Foreign.C.String
import Prelude hiding (init)
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Raw (mkFinalizedSurface, mkFinalizedTexture)

version :: (Int, Int, Int)
version = ( #{const SDL_IMAGE_MAJOR_VERSION}
          , #{const SDL_IMAGE_MINOR_VERSION}
          , #{const SDL_IMAGE_PATCHLEVEL}
          )

newtype ImageFlag = ImageFlag { unwrapFlag :: #{type int} }

#{enum ImageFlag, ImageFlag
 , initJpg  = IMG_INIT_JPG
 , initPng  = IMG_INIT_PNG
 , initTif  = IMG_INIT_TIF
 , initWebp = IMG_INIT_WEBP
 }

data ImageType
   = BMP | CUR | GIF | ICO | JPG | LBM | PCX | PNG | PNM | TGA | TIF | XCF | XPM | XV
   deriving (Show, Eq)

combineImageFlag :: [ImageFlag] -> ImageFlag
combineImageFlag = ImageFlag . foldr ((.|.) . unwrapFlag) 0

foreign import ccall unsafe "IMG_Init"
  init' :: #{type int} -> IO ()

init :: [ImageFlag] -> IO ()
init flags = init' $ unwrapFlag $ combineImageFlag flags

foreign import ccall unsafe "IMG_Quit"
  quit :: IO ()

foreign import ccall unsafe "IMG_LoadTyped_RW"
  imgLoadTypedRW' :: Ptr RWopsStruct -> #{type int} -> CString -> IO (Ptr SurfaceStruct)

-- | Boolean argument will free source if true
loadTypedRW :: RWops -> Bool -> ImageType -> IO Surface
loadTypedRW rwops dofree imgtype =
  withForeignPtr rwops $ \rwops' ->
  withCString (show imgtype) $ \imgtype' ->
    imgLoadTypedRW' rwops' (fromBool dofree) imgtype' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_Load"
  imgLoad' :: CString -> IO (Ptr SurfaceStruct)

load :: String -> IO Surface
load fname =
  withCString fname $ \fname' -> imgLoad' fname' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_Load_RW"
  imgLoadRW' :: Ptr RWopsStruct -> #{type int} -> IO (Ptr SurfaceStruct)

-- | Boolean argument will free source if true
loadRW :: RWops -> Bool -> IO Surface
loadRW rwops dofree =
  withForeignPtr rwops $ \rwops' ->
    imgLoadRW' rwops' (fromBool dofree) >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadTexture"
  imgLoadTexture' :: Ptr RendererStruct -> CString -> IO (Ptr TextureStruct)

loadTexture :: Renderer -> FilePath -> IO Texture
loadTexture r t =
  withForeignPtr r $ \r' ->
  withCString t $ \t' ->
    imgLoadTexture' r' t' >>= mkFinalizedTexture

foreign import ccall unsafe "IMG_LoadTexture_RW"
  imgLoadTextureRW' :: Ptr RendererStruct -> Ptr RWopsStruct -> #{type int} -> IO (Ptr TextureStruct)

loadTextureRW :: Renderer -> RWops -> Bool -> IO Texture
loadTextureRW r rwops dofree =
  withForeignPtr r $ \r' ->
  withForeignPtr rwops $ \rwops' ->
    imgLoadTextureRW' r' rwops' (fromBool dofree) >>= mkFinalizedTexture

foreign import ccall unsafe "IMG_LoadTextureTyped_RW"
  imgLoadTextureTypedRW' :: Ptr RendererStruct -> Ptr RWopsStruct -> #{type int} -> CString -> IO (Ptr TextureStruct)

loadTextureTypedRW :: Renderer -> RWops -> Bool -> ImageType -> IO Texture
loadTextureTypedRW r rwops dofree t =
  withForeignPtr r $ \r' ->
  withForeignPtr rwops $ \rwops' ->
  withCString (show t) $ \t' ->
    imgLoadTextureTypedRW' r' rwops' (fromBool dofree) t' >>= mkFinalizedTexture

foreign import ccall unsafe "IMG_isICO"
  isICO' :: Ptr RWopsStruct -> IO #{type int}

isICO :: RWops -> IO Bool
isICO rwops =
  withForeignPtr rwops $ \rwops' -> isICO' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isCUR"
  isCUR' :: Ptr RWopsStruct -> IO #{type int}

isCUR :: RWops -> IO Bool
isCUR rwops =
  withForeignPtr rwops $ \rwops' -> isCUR' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isBMP"
  isBMP' :: Ptr RWopsStruct -> IO #{type int}

isBMP :: RWops -> IO Bool
isBMP rwops =
  withForeignPtr rwops $ \rwops' -> isBMP' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isGIF"
  isGIF' :: Ptr RWopsStruct -> IO #{type int}

isGIF :: RWops -> IO Bool
isGIF rwops =
  withForeignPtr rwops $ \rwops' -> isGIF' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isJPG"
  isJPG' :: Ptr RWopsStruct -> IO #{type int}

isJPG :: RWops -> IO Bool
isJPG rwops =
  withForeignPtr rwops $ \rwops' -> isJPG' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isLBM"
  isLBM' :: Ptr RWopsStruct -> IO #{type int}

isLBM :: RWops -> IO Bool
isLBM rwops =
  withForeignPtr rwops $ \rwops' -> isLBM' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isPCX"
  isPCX' :: Ptr RWopsStruct -> IO #{type int}

isPCX :: RWops -> IO Bool
isPCX rwops =
  withForeignPtr rwops $ \rwops' -> isPCX' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isPNG"
  isPNG' :: Ptr RWopsStruct -> IO #{type int}

isPNG :: RWops -> IO Bool
isPNG rwops =
  withForeignPtr rwops $ \rwops' -> isPNG' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isPNM"
  isPNM' :: Ptr RWopsStruct -> IO #{type int}

isPNM :: RWops -> IO Bool
isPNM rwops =
  withForeignPtr rwops $ \rwops' -> isPNM' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isTIF"
  isTIF' :: Ptr RWopsStruct -> IO #{type int}

isTIF :: RWops -> IO Bool
isTIF rwops =
  withForeignPtr rwops $ \rwops' -> isTIF' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isXCF"
  isXCF' :: Ptr RWopsStruct -> IO #{type int}

isXCF :: RWops -> IO Bool
isXCF rwops =
  withForeignPtr rwops $ \rwops' -> isXCF' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isXPM"
  isXPM' :: Ptr RWopsStruct -> IO #{type int}

isXPM :: RWops -> IO Bool
isXPM rwops =
  withForeignPtr rwops $ \rwops' -> isXPM' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isXV"
  isXV' :: Ptr RWopsStruct -> IO #{type int}

isXV :: RWops -> IO Bool
isXV rwops =
  withForeignPtr rwops $ \rwops' -> isXV' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_isWEBP"
  isWEBP' :: Ptr RWopsStruct -> IO #{type int} 

isWEBP :: RWops -> IO Bool
isWEBP rwops =
  withForeignPtr rwops $ \rwops' -> isWEBP' rwops' >>= return . toBool

foreign import ccall unsafe "IMG_LoadICO_RW"
  loadICORW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadICORW :: RWops -> IO Surface
loadICORW rwops =
  withForeignPtr rwops $ \rwops' -> loadICORW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadCUR_RW"
  loadCURRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadCURRW :: RWops -> IO Surface
loadCURRW rwops =
  withForeignPtr rwops $ \rwops' -> loadCURRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadBMP_RW"
  loadBMPRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadBMPRW :: RWops -> IO Surface
loadBMPRW rwops =
  withForeignPtr rwops $ \rwops' -> loadBMPRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadGIF_RW"
  loadGIFRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadGIFRW :: RWops -> IO Surface
loadGIFRW rwops =
  withForeignPtr rwops $ \rwops' -> loadGIFRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadJPG_RW"
  loadJPGRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadJPGRW :: RWops -> IO Surface
loadJPGRW rwops =
  withForeignPtr rwops $ \rwops' -> loadJPGRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadLBM_RW"
  loadLBMRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadLBMRW :: RWops -> IO Surface
loadLBMRW rwops =
  withForeignPtr rwops $ \rwops' -> loadLBMRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadPCX_RW"
  loadPCXRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadPCXRW :: RWops -> IO Surface
loadPCXRW rwops =
  withForeignPtr rwops $ \rwops' -> loadPCXRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadPNG_RW"
  loadPNGRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadPNGRW :: RWops -> IO Surface
loadPNGRW rwops =
  withForeignPtr rwops $ \rwops' -> loadPNGRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadPNM_RW"
  loadPNMRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadPNMRW :: RWops -> IO Surface
loadPNMRW rwops =
  withForeignPtr rwops $ \rwops' -> loadPNMRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadTGA_RW"
  loadTGARW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadTGARW :: RWops -> IO Surface
loadTGARW rwops =
  withForeignPtr rwops $ \rwops' -> loadTGARW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadTIF_RW"
  loadTIFRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadTIFRW :: RWops -> IO Surface
loadTIFRW rwops =
  withForeignPtr rwops $ \rwops' -> loadTIFRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadXCF_RW"
  loadXCFRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadXCFRW :: RWops -> IO Surface
loadXCFRW rwops =
  withForeignPtr rwops $ \rwops' -> loadXCFRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadXPM_RW"
  loadXPMRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadXPMRW :: RWops -> IO Surface
loadXPMRW rwops =
  withForeignPtr rwops $ \rwops' -> loadXPMRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadXV_RW"
  loadXVRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadXVRW :: RWops -> IO Surface
loadXVRW rwops =
  withForeignPtr rwops $ \rwops' -> loadXVRW' rwops' >>= mkFinalizedSurface

foreign import ccall unsafe "IMG_LoadWEBP_RW"
  loadWEBPRW' :: Ptr RWopsStruct -> IO (Ptr SurfaceStruct)

loadWEBPRW :: RWops -> IO Surface
loadWEBPRW rwops =
  withForeignPtr rwops $ \rwops' -> loadWEBPRW' rwops' >>= mkFinalizedSurface

-- XXX TODO
-- extern DECLSPEC SDL_Surface * SDLCALL IMG_ReadXPMFromArray(char **xpm);

foreign import ccall unsafe "IMG_SavePNG"
  imgSavePNG' :: Ptr SurfaceStruct -> CString -> IO #{type int}

savePNG :: Surface -> String -> IO Bool
savePNG surface filepath =
  withForeignPtr surface $ \surface' ->
  withCString filepath $ \filepath' ->
    imgSavePNG' surface' filepath' >>= return . toBool

foreign import ccall unsafe "IMG_SavePNG_RW"
  imgSavePNGRW' :: Ptr SurfaceStruct -> Ptr RWopsStruct -> #{type int} -> IO #{type int}

-- | Boolean frees destination on completion
savePNGRW :: Surface -> RWops -> Bool -> IO Bool
savePNGRW surface rwops dofree =
  withForeignPtr surface $ \surface' ->
  withForeignPtr rwops $ \rwops' ->
    imgSavePNGRW' surface' rwops' (fromBool dofree) >>= return . toBool

