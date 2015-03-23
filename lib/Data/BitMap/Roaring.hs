-- |
-- Module: Data.BitMap.Roaring
-- Description: Compressed bitmap data structure with good performance.
-- Copyright: (c) Thomas Sutton 2015
-- License: BSD3
-- Maintainer: me@thomas-sutton.id.au
-- Stability: experimental
--
-- A compressed bitmaps with good space and time performance.
--
-- These modules are intended to be imported qualified, to avoid name clashes
-- with Prelude functions, e.g.
--
-- >  import Data.BitMap.Roaring (BitMap)
-- >  import qualified Data.BitMap.Roaring as Roaring
--
-- The implementation paritions values into chunks based on their high 16 bits.
-- Chunks are represented differently based on their density: low-density
-- chunks are stored as packed arrays of the low-order bits while high-density
-- chunks are stored as bit vectors.
--
--    * Samy Chambi, Daniel Lemire, Owen Kaser, Robert Godin,
--    \"/Better bitmap performance with Roaring bitmaps/\", Software: Practice
--    and Experience (to appear) <http://arxiv.org/pdf/1402.6407v4>
--
module Data.BitMap.Roaring where

import Control.Applicative hiding (empty)
import Data.Bits
import Data.Convertible
import Data.Monoid
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as VAH
import qualified Data.Vector.Unboxed as U
import Data.Word

-- | A set of bits.
data BitMap = BitMap (Vector Chunk)
  deriving (Show)

type Key = Word32

-- | A chunk representing the keys which share particular 16 high-order bits.
--
-- Chunk with low density (i.e. no more than 4096 members) are represented as a
-- sorted array of their low 16 bits. Chunks with high density (i.e. more than
-- 4096 members) are represented by a bit vector.
--
-- Both high and low density chunks include the high order bits shared by all
-- entries in the chunk, and the cardinality of the chunk.
data Chunk
    = LowDensity
        { chunkIndex       :: Word16
        , chunkCardinality :: Int
        , chunkArray       :: U.Vector Word16
        }
    | HighDensity
        { chunkIndex       :: Word16
        , chunkCardinality :: Int
        , chunkBits        :: U.Vector Word64
        }
  deriving (Eq,Show)

-- | 'Chunk's are ordered by their index.
instance Ord Chunk where
    compare c1 c2 = compare (chunkIndex c1) (chunkIndex c2)

-- * Query

-- | /O(1)./ Is the set empty?
null :: BitMap -> Bool
null (BitMap v) = V.null v

-- | Cardinality of the set.
size :: BitMap -> Int
size (BitMap cs) = V.sum $ V.map chunkCardinality cs

-- | Is the value a member of the set?
member :: Key -> BitMap -> Bool
member k (BitMap cs) =
    let (i,b) = splitWord k
    in case vLookup i cs of
        Nothing    -> False
        Just (_,c) -> chunkGet b c

-- | Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2.
isSubsetOf :: BitMap -> BitMap -> Bool
isSubsetOf _ _ = False

-- | Is this a proper subset? (i.e. a subset but not equal).
isProperSubsetOf :: BitMap -> BitMap -> Bool
isProperSubsetOf a b = a `isSubsetOf` b && not (b `isSubsetOf` a)

-- | Count the bits set in the range [0,i].
rank :: BitMap -> Int -> Int
rank (BitMap _cs) _ = 0

-- | Find the index of the ith set bit.
select :: BitMap -> Int -> Maybe Key
select (BitMap _cs) _ = Nothing

-- * Construction

-- | /O(1)./ The empty set.
empty :: BitMap
empty = BitMap mempty

-- | /O(1)./ A set of one element.
singleton :: Key -> BitMap
singleton k = insert k empty

-- | Add a value to the set.
insert :: Key -> BitMap -> BitMap
insert k (BitMap v) =
    let (i,b) = splitWord k
        f = Just . maybe (chunkNew i b) (chunkSet b)
        v' = vAlter f i v
    in BitMap v'

-- | Delete a value in the set.
--
-- Returns the original set when the value was not present.
delete :: Key -> BitMap -> BitMap
delete k (BitMap v) =
    let (i,b) = splitWord k
        v' = vAlter (f b) i v
    in BitMap v'
  where
    f _ Nothing = Nothing
    f b (Just c) =
        let c' = chunkClear b c
        in if 0 == chunkCardinality c'
           then Nothing
           else Just c'

-- * Combine

-- | The union of two sets.
union :: BitMap -> BitMap -> BitMap
union (BitMap cs) (BitMap ds) = BitMap $ mergeWith f cs ds
  where
    f :: Maybe Chunk -> Maybe Chunk -> Maybe Chunk
    f Nothing  b        = b
    f a        Nothing  = a
    f (Just a) (Just b) = Just $ mergeChunks a b

-- | The difference between two sets.
difference :: BitMap -> BitMap -> BitMap
difference _ _ = empty

-- | The intersection of two sets.
intersection :: BitMap -> BitMap -> BitMap
intersection _ _ = empty

-- * Conversion

-- ** List

elems :: BitMap -> [Key]
elems = toAscList

toList :: BitMap -> [Key]
toList = toAscList

fromList :: [Key] -> BitMap
fromList = foldl (flip insert) empty

-- ** Ordered list

-- | Produce a list of 'Key's in a 'BitMap', in descending order.
toAscList :: BitMap -> [Key]
toAscList (BitMap cs) = work cs []
  where
    work cs' l | V.null cs' = l
               | otherwise = let c    = chunkToBits $ V.head cs'
                                 cs'' = V.tail cs'
                             in work cs'' (l <> c)

-- | Produce a list of 'Key's in a 'BitMap', in descending order.
toDescList :: BitMap -> [Key]
toDescList = reverse . toAscList

-- | Build a 'BitMap' from a list of 'Key's.
--
-- Precondition: input is sorted ascending order.
--
-- TODO(thsutton) Throw error if precondition violated.
-- TODO(thsutton) Implement
fromAscList :: [Key] -> BitMap
fromAscList _ = empty

-- * Utility

-- | Split a 'Word32' into its high-order and low-order bits.
splitWord :: Word32 -> (Word16, Word16)
splitWord w =
    let h = convert $ rotate (0xffff0000 .&. w) 16
        l = convert $ 0x0000ffff .&. w
    in (h,l)

-- | Combine two 'Word16's of the high-order and low-order bits into a
-- 'Word32'.
combineWord :: Word16 -> Word16 -> Word32
combineWord h l = rotate (convert h) (-16) .|. convert l

-- | Create a new chunk.
chunkNew :: Word16 -> Word16 -> Chunk
chunkNew i v = LowDensity i 1 (U.singleton v)

-- | Set a bit in a chunk.
--
-- TODO(thsutton) Promote LowDensity chunk when it rises above threshold.
chunkSet :: Word16 -> Chunk -> Chunk
chunkSet v chunk = case chunk of
    LowDensity  i c a -> LowDensity  i c (setL v a)
    HighDensity i c a -> HighDensity i c (setH v a)
  where
    setL :: Word16 -> U.Vector Word16 -> U.Vector Word16
    setL i a = uvInsert a i
    setH :: Word16 -> U.Vector Word64 -> U.Vector Word64
    setH _ a = a -- TODO(thsutton) implement

-- | Clear a bit in a 'Chunk'.
--
-- TODO(thsutton) Demote HighDensity chunk when it falls below threshold.
chunkClear :: Word16 -> Chunk -> Chunk
chunkClear v chunk = case chunk of
    LowDensity i _ a ->
        let a' = clearL v a
            c' = U.length a'
        in LowDensity  i c' a'
    HighDensity i _ a ->
        let a' = clearH v a
            c' = U.sum $ U.map popCount a'
        in HighDensity i c' a'
  where
    clearL :: Word16 -> U.Vector Word16 -> U.Vector Word16
    clearL i a = uvDelete a i
    clearH :: Word16 -> U.Vector Word64 -> U.Vector Word64
    clearH _ a = a -- TODO(thsutton) implement

-- | Get a bit from a 'Chunk'.
chunkGet :: Word16 -> Chunk -> Bool
chunkGet v chunk = case chunk of
    LowDensity  _ _ a -> U.elem v a
    HighDensity{} -> False -- TODO(thsutton) implement

chunkToBits :: Chunk -> [Word32]
chunkToBits (LowDensity  i _ a) = combineWord i <$> U.toList a
chunkToBits (HighDensity i _ a) = U.toList . U.concatMap f $ U.indexed a
  where
    f :: (Int, Word64) -> U.Vector Word32
    f (_p,_bs) = U.map (combineWord i) U.empty

-- | Merge two 'Vector's of 'Chunk's.
--
-- Precondition: Both vectors are sorted by 'chunkIndex'.
-- Postcondition: Output vector sorted by 'chunkIndex'.
-- Postcondition: length(output) >= max(length(a),length(b))
mergeWith
    :: (Maybe Chunk -> Maybe Chunk -> Maybe Chunk)
    -- ^ Merge two chunks with the same index.
    -> Vector Chunk
    -> Vector Chunk
    -> Vector Chunk
mergeWith f v1 v2
    | V.null v1 = v2
    | V.null v2 = v1
    | otherwise =
        let a = V.head v1
            b = V.head v2
        in work a v1 b v2
  where
    -- Note: we take the head and the *entirety* of each vector; NOT the head
    -- and the tail!
    work :: Chunk -> Vector Chunk -> Chunk -> Vector Chunk -> Vector Chunk
    work a as b bs = case a `compare` b of
        LT -> a `V.cons` mergeWith f (V.tail as) bs
        EQ -> mergeChunks a b `V.cons` mergeWith f (V.tail as) (V.tail bs)
        GT -> b `V.cons` mergeWith f as (V.tail bs)

-- | Take the union of two 'Chunk's, raising an 'error' if they do not share an
-- index.
mergeChunks :: Chunk -> Chunk -> Chunk
mergeChunks c1 c2 =
    if chunkIndex c1 == chunkIndex c2
        then merge c1 c2
        else error "Attempting to merge incompatible chunks!"
  where
    aPop :: U.Vector Word64 -> Int
    aPop = U.sum . U.map popCount
    aSet :: Word16 -> U.Vector Word64 -> U.Vector Word64
    aSet _i v = v
    packA :: U.Vector Word16 -> U.Vector Word64
    packA _ = mempty
    merge (HighDensity i _ a1) (HighDensity _ _ a2) =
        let a' = U.zipWith (.|.) a1 a2 in HighDensity i (aPop a') a'
    merge (HighDensity i _ ah) (LowDensity  _ _ al) =
        let a' = U.foldr' aSet ah al in HighDensity i (aPop a') a'
    merge (LowDensity  i _ al) (HighDensity _ _ ah) =
        let a' = U.foldr' aSet ah al in HighDensity i (aPop a') a'
    merge (LowDensity  i _ a1) (LowDensity  _ _ a2) =
        let a' = vMerge a1 a2
            n' = U.length a'
        -- TODO(thsutton): Is this eager enough?
        in if n' <= 4096
            then LowDensity i n' a'
            else HighDensity i n' (packA a')

vMerge :: (U.Unbox e, Ord e) => U.Vector e -> U.Vector e -> U.Vector e
vMerge as bs
    | U.null as = bs
    | U.null bs = as
    | otherwise =
        let a = U.head as
            b = U.head bs
        in case a `compare` b of
            LT -> a `U.cons` vMerge (U.tail as) bs
            EQ -> a `U.cons` vMerge (U.tail as) (U.tail bs)
            GT -> b `U.cons` vMerge as (U.tail bs)

-- | Alter the 'Chunk' with the given index in a vector of 'Chunk's.
--
-- The function is passed 'Nothing' if the 'Chunk' is not present.
--
-- If the function returns 'Nothing' the 'Chunk', if present, is deleted;
-- otherwise it is replaced.
vAlter
    :: (Maybe Chunk -> Maybe Chunk)
    -> Word16
    -> Vector Chunk
    -> Vector Chunk
vAlter f i v = case vLookup i v of
    Nothing     -> case f Nothing of
        Nothing -> v
        Just c' -> vInsert v c' -- TODO(thsutton) Insert
    Just (p, a) -> case f (Just a) of
        Nothing -> vDelete v p
        Just c' -> V.update v (V.singleton (p, c'))

-- | Search for a 'Chunk' with a specific index.
--
-- TODO(thsutton) better search algorithm.
vLookup :: Word16 -> Vector Chunk -> Maybe (Int, Chunk)
vLookup k v = case V.findIndex p v of
    Nothing -> Nothing
    Just i  -> Just (i, v V.! i)
  where
    p :: Chunk -> Bool
    p c = k == chunkIndex c

-- | Insert a 'Chunk' into a vector, replacing the
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: a `elem` v'.
--
-- TODO(thsutton): Efficiency.
vInsert :: Ord a => Vector a -> a -> Vector a
vInsert v a =
    if a `V.elem` v
    then v
    else V.modify VAH.sort $ V.cons a v

-- | Delete the element at index.
--
-- Return the vector unchanged if the index is out of bounds.
vDelete :: Vector a -> Int -> Vector a
vDelete v p
    | p < 0 = v
    | V.length v < p = v
    | otherwise = case V.splitAt p v of
        (s, r) -> s <> V.tail r

-- | Insert a 'Chunk' into an unboxed vector.
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: a `elem` v'.
--
-- TODO(thsutton): Efficiency.
uvInsert :: (U.Unbox a, Ord a) => U.Vector a -> a -> U.Vector a
uvInsert v a =
    if a `U.elem` v
    then v
    else U.modify VAH.sort $ U.cons a v

-- | Delete an element from an unboxed vector.
--
-- Precondition: input vector is sorted.
-- Postcondition: output vector is sorted.
-- Postcondition: not $ a `elem` v'
--
-- TODO(thsutton): Efficiency.
uvDelete :: (U.Unbox a, Ord a) => U.Vector a -> a -> U.Vector a
uvDelete v a = case U.elemIndex a v of
    Nothing -> v
    Just p  -> case U.splitAt p v of
        (s, r) -> s <> U.tail r
