-- | Parser for the WaveFront OBJ filetype
--
-- Inspired by bling-raytracer

module ObjParser where

import Types
import Math

import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Lazy as BS
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Data.Maybe

data ObjParserState s = OPState {
        stVertices  :: !(MV.MVector s Pt3),
        stNumVert   :: !Int,
        stNormals   :: !(MV.MVector s UVec3),
        stNumNorm   :: !Int,
        stTriangles :: [Triangle]
}

type ObjParser s a = ParsecT BS.ByteString (ObjParserState s) (ST s) a

parseObjFile :: FilePath -> IO TriangleMesh
--parseObjFile = do return $ TriangleMesh []
parseObjFile filepath = do
    input <- BS.readFile filepath

    {-
    let result = runST $ do
        state <- initialState
        runPT objFileParser state filepath input
        -}

    let result = runST $ do
            state <- initialState
            runPT objFileParser state filepath input

    case result of
        (Left error) -> fail $ show error
        (Right mesh) -> return $! mesh

test filepath = do
    input <- BS.readFile filepath

    let result = runST $ do
            state <- initialState
            runPT objFileParserTest state filepath input

    case result of
        (Left error) -> fail $ show error
        (Right mesh) -> return $! mesh
    
objFileParserTest = do
    res <- slashedInts
    eol
    return res

objFileParser :: ObjParser s TriangleMesh
objFileParser = do
    skipMany $ normal <|> texture <|> vertex <|> face <|> ignoreLine
    eof
    st <- getState
    return $ TriangleMesh $ stTriangles st

initialState :: ST s (ObjParserState s)
initialState = do
    vertices <- MV.new 100
    normals  <- MV.new 100
    return $! OPState vertices 0 normals 0 []

ignoreLine :: ObjParser s ()
ignoreLine = skipMany (noneOf "\n") >> eol

normal :: ObjParser s ()
normal = do
   _ <- try $ string "vn"
   x <- spaces1 >> float
   y <- spaces1 >> float
   z <- spaces1 >> float
   optional spaces1 >> eolf
   addNormal $ normalize $ F3 x y z

-- | Texture coordinates are skipped (for now).
texture :: ObjParser s ()
texture = do
   _ <- try $ string "vt"
   ignoreLine


vertex :: ObjParser s ()
vertex = do
   _ <- try $ char 'v'
   x <- spaces1 >> float
   y <- spaces1 >> float
   z <- spaces1 >> float
   _ <- optional $ spaces1 >> float -- ignore optional w component
   optional spaces1 >> eolf
   addVertex $ F3 x y z

-- | Parses a face. Only triangles are supported (for now).
face :: ObjParser s ()
face = do
    char 'f'
    xs <- spaces1 >> slashedInts
    ys <- spaces1 >> slashedInts
    zs <- spaces1 >> slashedInts
    -- TODO <|> fail "only triangular faces are supported"
    optional spaces1 >> eolf 

    triangle <- makeTriangle $ unzip3 [xs, ys, zs]
    addTriangle triangle

-- | Texture coordinates are ignored and per-vertex normals are 
-- mandatory for now.
makeTriangle :: ([Int], [Maybe Int], [Maybe Int]) -> ObjParser s Triangle
makeTriangle (vertexIdxs, maybeTextureIdxs, maybeNormalIdxs) = do
    st <- getState
    positions <- mapM (\i -> lift $ MV.read (stVertices st) (i-1)) vertexIdxs
    normals <- mapM (\i -> lift $ MV.read (stNormals st) (i-1)) $ catMaybes maybeNormalIdxs

    if length normals /= 3
        then fail "no (or insufficient) normals given"
        else do
            let [v1, v2, v3] = zipWith Vertex positions normals
            return $! Triangle v1 v2 v3

addTriangle :: Triangle -> ObjParser s ()
addTriangle triangle = do
    st <- getState
    let triangles = stTriangles st
    setState $ st {stTriangles = triangle : triangles}
    

-- | Parses expression of the form "int1", "int1/int2" or 
-- "int1/int2/int3", where int2 is optional in the last expression.
slashedInts :: ObjParser s (Int, Maybe Int, Maybe Int)
slashedInts = do
    int1 <- int
    (int2, int3) <- try slashedInts2 <|> try slashedInts3 <|> try slashedInts2' <|> slashedInts1
    return (int1, int2, int3)
    where
        -- TODO be smart
        slashedInts2 = do
            int3 <- string "//" >> int
            return (Nothing, Just int3)
        slashedInts3 = do
            int2 <- char '/' >> int
            int3 <- char '/' >> int
            return (Just int2, Just int3)
        slashedInts2' = do
            int2 <- char '/' >> int
            return (Just int2, Nothing)
        slashedInts1 = do
            --noneOf "/"
            return (Nothing, Nothing)

int :: (Monad m) => (ParsecT BS.ByteString u m) Int
int = fmap read $ many1 digit

addVertex :: Pt3 -> ObjParser s ()
addVertex vertex = do
    st <- getState
    newVertices <- setElement vertex (stVertices st) (stNumVert st)
    setState $ st {stVertices = newVertices, stNumVert = 1 + (stNumVert st)}

addNormal :: UVec3 -> ObjParser s ()
addNormal normal = do
    st <- getState
    newNormals <- setElement normal (stNormals st) (stNumNorm st)
    setState $ st {stNormals = newNormals, stNumNorm = 1 + (stNumNorm st)}

setElement :: a -> MV.MVector s a -> Int -> ObjParser s (MV.MVector s a)
setElement elem vector index = lift $ do
    let l = MV.length vector

    let newVec = vector
    newVec <- if l <= index
                then MV.grow vector l
                else return vector

    MV.unsafeWrite newVec index elem
    return newVec


-- | Skips at least one space character.
spaces1 :: ObjParser s ()
spaces1 = skipMany1 (char ' ') <?> "space(s)"

-- End Of Line
eol :: ObjParser s ()
eol = char '\n' >> return ()
-- End Of Line or File
eolf :: ObjParser s ()
eolf = eol <|> eof


-- | parse a floating point number
float :: (Monad m) => (ParsecT BS.ByteString u m) Flt
float = {-# SCC "float" #-} do
   sign <- option 1 $ do
      s <- oneOf "+-"
      return $! if s == '-' then (-1) else 1

   i <- many digit
   d <- option "0" (char '.' >> try (many digit))
   return $! sign * read (i ++ "." ++ d)




-- vim: expandtab smarttab sw=4 ts=4
