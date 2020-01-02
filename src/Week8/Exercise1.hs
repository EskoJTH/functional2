module Week8.Exercise1 where
import Data.Stream.Infinite (Stream (..))
import qualified Data.Tree as Rose (Forest (..), Tree (..))
import qualified Data.Tree.Binary.Inorder as Binary (Tree (..))
import Data.List.NonEmpty
----    Derivative of Maybe a.
{-

Maybe a = Just a | Nothing
(a + 1)
derivative:
1

-}
type MaybeContext = ()
data MaybeZipper a = MZ (a, ())

----    Derivative of Join (,) a.
{-

Join a = {runJoin :: p a a }
Join (,) a -> (a,a) 
-> (a×a)
-> a^2
derivative:
2a
==> (Bool, a)
-}
data TupleContext a = TC (Bool, a) 
data TupleZipper a = TZ (a,(Bool,a))

----    Derivative of [] a.
{-
[a] 
≅ μy(1 + x × y)
D >> D_x (1 + x × y)

helper definition for list:
  [a]
≅ (μy(1 + x × y))
≅ (1 + x × (μy(1 + x × y)))
≅ (1 + x × (1 + x × (μy(1 + x × y))))
≅ (1 + x × (1 + x × (1 + x × (μy(1 + x × y)))))
≅ 1 + x × (1 + x × (1 + x × ...))
≅ 1 + x + x^2 + x^3 × ...

F ≡ (1 + y × x)
  D_x (μy(1 + y × x))
≅ μz( (λy(D_x(1 + y × x))(μy.F)  +  (λy(D_y(1 + y × x))(μy.F)  × z)
≅ μz( (λy(y)(μy.F)  +  (λy(x)(μy.F)  × z)
≅ μz( (μy (1 + y × x))  +  x × z)
≅ μz( (μy (1 + y × x))  +  x × z)
≅ (μy (1 + y × x))  +  x × μz((μy (1 + y × x))  +  x × z)
≅ (μy (1 + y × x))  +  x × ((μy (1 + y × x))  +  x × μz((μy (1 + y × x))  +  x × z))
≅ [x] + (x×[x] + (x×[x] + (x×[x] + ... ))
≅ [x] + [x] × (x + x + x + ... ))
≅ [x] + [x] × x × (1 + 1 + 1 + ... ))
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × x × (1+1+1...)
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × (x+x+x...)
≅ (1 + x + x^2 + x^3 × ...) + (1 + x + x^2 × x^3 + ...) × ( x + x^2 + x^3 ...)
≅ (1 + x + x^2 × x^3 + ...) × (1 + x + x^2 + x^3 + ...)
≅ [x] × [x]

-}
-- data DSA a = DSA1 [a] | DSA2 a (DSA a)

data ListContext a = LC ([a],[a])
data ListZipper a = LZ (a, ([a],[a]))

----    Derivative of Stream a from the streams package.
{-
data Stream a = a :> Stream a
μx (x,y)    -- x×(x×(x×...))
derivative μx (x×y)
F ≡ (x×y)
≅ μz( (λy(D_x(y))(μy.F))  +  (λy(D_y(x))(μy.F)  × z))
≅ μz( (μy.(x×y))  +  x × z)
≅ (μy.(x×y))  +  x × μz( (μy.(x×y))  +  x × z)
≅ x×(x×(x×...)) + x × μz( (x×(x×(x×...))) + x × z)
≅ x×(x×(x×...)) + x × (x×(x×(x×...)) + x × μz( (x×(x×(x×...))) + x × z))
≅ x×(x×(x×...)) + x × x×(x×(x×...)) + x*2 × μz( (x×(x×(x×...))) + x × z)
≅ x×(x×(x×...)) + x × x×(x×(x×...)) + x*2 × (x×(x×(x×...))) + x^3 × μz((x×(x×(x×...))) + x × z)
≅ x×(x×(x×...)) + (1 + x + x^2 + x^3 + ...)
≅ μx (x×y) + μy(1 + y × x)
≅ Stream x + [x]

-}

data StreamContext a = SC ([a], Stream a)
data StreamZipper a = SZ (a, ([a], Stream a))

----    Derivative of Tree a from the binary-tree package.
{-

btree = 1 + x × btree^2
μy (1 + x × y^2)
derivative:
F ≡ (1 + x × y^2)
≅ μz( (λy(D_x(1 + x × y^2))(μy.F))  +  (λy(D_y(1 + x × y^2))(μy.F)  × z))
≅ μz( (λy(y^2)(μy.F))  +  (λy(2x × y)(μy.F) × z))
≅ μz( μy(1 + x × y^2)^2  +  (2x × μy(1 + x × y^2)) × z)


≅ μz( μy(1 + x × y^2)^2  +  (2x × μy(1 + x × y^2)) × z)


data BTC' a = BTCL' (Binary.Tree a) (Binary.Tree a) | BTCR' Two a (Binary.Tree a) (BTC' a)

-}

data Two = L | R

data BTC a = (Two, a, (Binary.Tree a)) :-< (BTC a) | BTCEnd (Binary.Tree a) (Binary.Tree a)
data BTZ a = BTZ (a, BTC a)

{-
data BTreeContext a = BTC [(Bool, Binary.Tree a)]
data BTreeZipper a = BTZ (a, [(Bool, Binary.Tree a)])

data BTreeContext a = BTC [(Bool, Binary.Tree a)]
data BTreeZipper a = BTZ (a, [(Bool, Binary.Tree a)])
-}





---- Derivative of Tree a from the containers package.
{-

RoseTree = (x, [RoseTree x])
RoseTree = x × [RoseTree x]
μy(x × μy'(1 + y × y'))

F ≡ (x × μy'(1 + y × y'))
F' ≡ (1 + y × y')

derivative:
-}
{-
  μz( λy(D_x(x × μy'(1 + y × y')))(μy.F)  +  λy(D_y(x × μy'(1 + y × y')))(μy.F) × z)
≅ μz( λy(μy'(1 + y × y'))(μy.F)  +  λy(D_y(μy'(x + y × y')))(μy.F) × z)
≅ μz( λy(μy'(1 + y × y'))(μy.F)  +  λy(μz' ((λy'(D_y(x + y × y')))(μy.F') + λy'(D_y'(x + y × y'))(μy.F') × z')(μy.F)) × z)
≅ μz( λy(μy'(1 + y × y'))(μy.F)  +  λy(μz'(λy'(y'))(μy.F') + λy'(y)(μy.F') × z')(μy.F) × z)
≅ μz( λy(μy'(1 + y × y'))(μy.F)  +  λy(μz'(μy'(1 + y × y') + y × z'))(μy.F) × z)
-}

type RoseContainer = Z
type RoseZipper x = (x , Z x)

data Z x = RTLZ [Rose.Tree x] | (RZ x) :* (Z x)

-- λy(μy'(1 + y × y'))(μy.F)
-- ≅ (μy'(1 + μy(x × μy'(1 + y × y')) × y'))
--type LZ x = [LZ' x]
-- μy(x × μy'(1 + y × y'))
-- data LZ' x = LZ' (x, [LZ' x])
-- type LZ' x = Rose.Tree x

-- λy(μz'(μy'(1 + y × y') + y × z'))(μy.F)
-- μz'(μy'(1 + (μy(x × μy'(1 + y × y'))) × y') + (μy(x × μy'(1 + y × y'))) × z')
data RZ x = LRZ [Rose.Tree x] | (Rose.Tree x) :-: (RZ x)

-- ([rose], [rose]) ?

-- (μy(x × μy'(1 + y × y')))
--data LRZ x = LRZ (x, [LRZ x])
-- (μy(x × μy'(1 + y × y')))
--type RRZ x = LRZ x





{-
≅ μz( λy(μy'(1 + y × y'))(μy.F)  +  μz'((1 + (x × μy'(1 + y × y')) × y') + (x × μy'(1 + y × y')) × z') × z)
≅ μz( (μy'(1 + μy(x × μy'(1 + y × y')) × y'))  +  μz'((1 + μy(x × μy'(1 + y × y')) × y') + μy(x × μy'(1 + y × y')) × z') × z)
≅ μz( (μy'(1 + μy(x × μy'(1 + y × y')) × y'))  +  μz'((1 + μy(x × μy'(1 + y × y')) × y') + μy(x × μy'(1 + y × y')) × z') × z)
-}























----  Not related derivative of a Tree a not from the containers package...
{-
RoseTree = x + [RoseTree x]
μy(x + μy'(1 + y × y'))
F ≡ (x + μy'(1 + y × y')
F' ≡ (1 + y × y')
derivative:

--  
μz( λy(D_x(x + μy'(1 + y × y')))(μy.F)  +  λy(D_y(x + μy'(1 + y × y')))(μy.F) × z)
μz( (1 + 0)    +  λy(0 + D_y μy'(1 + y × y'))(μy.F) × z)
μz( 1  +  λy(μz'( λy'(D_y(1 + y × y'))(μy'.F') + λy'(D_y'(1 + y × y'))(μy'.F') × z'))(μy.F) × z)
μz( 1  +  λy(μz'( λy'(y')(μy'.F')              + λy'(y)(μy'.F')                × z'))(μy.F) × z)
μz( 1  +  λy(μz'( λy'(y')(μy'.F') + λy'(y)(μy'.F') × z'))(μy.F) × z)
μz( 1  +  λy(μz'( μy'(1 + y × y') + y × z'))(μy.F) × z)
μz( 1  +  (μz'( μy'(1 + (μy(x + μy'(1 + y × y'))) × y') + (μy(x + μy'(1 + y × y'))) × z')) × z)
μz( 1  +  (μz'( μy'(1 + (μy(x + μy'(1 + y × y'))) × y') + (μy(x + μy'(1 + y × y'))) × z')) × z)

-}

{-
data IZ x = StopIZ | IZ (IZ' x) (IZ x) --list containing Z' things
data IZ' x = StopIZ' (IY' x) | IZ' (IY x) (IZ' x) -- list Ending in Y' and containing Y's
data IY x = StopIY x | IY (IY' x) 
data IY' x = StopIY' | IY' (IY x) (IY' x) -- list containing Y 

type IIZ x = [(IIZ' x)] --list containing IIZ' things
data IIZ' x = StopIIZ' [(IIY x)] | (IIY x) :<<<> (IIZ' x) -- list Ending in IIY' and containing IIY's
data IIY x = StopIIY x | IIY [(IIY x)] 
type IIY' x = [(IIY x)] -- list containing some sort of recursive product type

type IIIZ x = [IIIZ' x] --list containing IIIZ' things
type IIIZ' x = (IIIY' x, [IIIY x]) -- list Ending in IIIY' and containing IIIY's
data IIIY x = IIIL' x | IIIR' (IIIY' x) -- there is a cycle... ._.
type IIIY' x = [IIIY x] -- list containing some sort of recursive product type

type IIIIZ x = [([IIIIY x], [IIIIY x])]
data IIIIY x = IIIIL' x | IIIIR' [IIIIY x]

type Z x = [([Y x], [Y x])]
type Y x = (x :| [Y x])

-}

{-
type Z x = [(Z' x)] 
data Z' x = StopZ' [(Y x)] | (Y x) :<> (Z' x) -- list Ending in Y' and containing Y's
data Y x = StopY x | Y [(Y x)] 

data RoseContext x = RC (Z x)
data RoseZipperr x = RZ (x, Z x)
-}



{-
data Three = One | Two | Three
data TTreeContext a = TTC [(Three, (Rose.Tree a, Rose.Tree a))]
data TTreeZipper a =  TTZ (a, [(Three, (Rose.Tree a, Rose.Tree a))])
-}
{-

μy(x + x y)
x + x μy(x + x y)
(x + x^2 + x^3 × ...)
nonempty

(x, [x])
x × μy(1 + x y)

(1 + x + x^2 + x^3 × ...)

  -}


{-

------ Derivative of Tree a from the binary-tree package.
------ Being lost:
------ 
------ helper definition for list:
------   [a]
------ ≅ (μy(1 + x × y))
------ ≅ (1 + x × (μy(1 + x × y)))
------ ≅ (1 + x × (1 + x × (μy(1 + x × y))))
------ ≅ (1 + x × (1 + x × (1 + x × (μy(1 + x × y)))))
------ ≅ 1 + x × (1 + x × (1 + x × ...))
------ ≅ 1 + x + x^2 + x^3 × ...
------ ≅ Σ_n∈N x^n
------ 
------ 
------ helper definition for a btree:
------   btree a
------ ≅ μy (1 + x × y^2)
------ ≅ 1 + x × μy (1 + x × y^2)^2
------ ≅ 1 + x × μy (1 + x × y^2)^2
------ ≅ 1 + x × (1 + x × μy (1 + x × y^2)^2)^2
------ ≅ 1 + x × (1 + x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ 1 + x × (1 + 2x × (1 + x × μy (1 + x × y^2)^2)^2   + (x × (1 + x × μy (1 + x × y^2)^2)^2)^2)
------ ≅ 1 + x + 2x^2 × (1 + x × μy (1 + x × y^2)^2)^2      +  x(x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ 1 + x + 2x^2 (1 + x × μy (1 + x × y^2)^2)^2        +  x(x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ 1 + x + 2x^2 (1 + x × μy (1 + x × y^2)^2)^2        +  x(x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ 1 + x + 2x^2 (1 + 2x × μy (1 + x × y^2)^2 + (x × μy (1 + x × y^2)^2)^2)  +  x(x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ 1 + x + 2x^2 + 4x^3 × μy (1 + x × y^2)^2  +  2x^2 (x × μy (1 + x × y^2)^2)^2)  +  x(x × (1 + x × μy (1 + x × y^2)^2)^2)^2
------ ≅ Σ_i,j∈N ix^j
------ 
------ 
------ ≅ μz( (Σ_i,j∈N ix^j)^2  +  (2x × Σ_i,j∈N ix^j) × z)
------ summan oikeasta alkiosta löytyy sarjat joissa x:n kerroin on kahdella jaollinen ja nolla exponenttinen alkio puuttuu.
------ ≅ μz( (Σ_i,j∈N ix^j)^2  +  (Σ_i,j∈N i2x^(j+1)) × z)
------ -- koska vasen summan osa korotettuna toiseen sisälttää identiteetti alkion ja kaikki exponentista aiheutuvat arvot ovat jo loputtomaan joukkoon kuuluvaa muotoa.
------ 
------ ≅ μz( (Σ_i,j∈N ix^j)  +  2(Σ_i,j∈N ix^(j+1)) × z)
------ ≅ μz( (Σ_i,j∈N ix^0)  +  (Σ_i,j∈N ix^(j+1))  +  2(Σ_i,j∈N ix^(j+1)) × z)
------ ≅ μz( (Σ_i∈N i) + (Σ_i,j∈N ix^(j+1))  +  2(Σ_i,j∈N ix^(j+1)) × z)
------ ≅ μz( (Σ_i∈N i) + (Σ_i,j∈N ix^(j+1)) × (1 + 2 × z))
------ 
------ Olen hukassa.
------ 
------ tavoite: Σ_n∈N (2 + Σ_i,j∈N ix^j)^n
------ 
------ 
------ Onnettomia yrityksiä:
------ 
------ ≅ μz( μy(1 + x × y^2)^2)  +  (2x × (1 + x × μy(1 + x × y^2)^2)) × z)
------ ≅ μz( μy(1 + x × y^2)^2)  +  (2x + 2x^2 × μy(1 + x × y^2)^2) × z)
------ ≅ μz( μy(1 + x × y^2)^2)  +  (2xz + 2x^2z × μy(1 + x × y^2)^2 × z))
------ 
------ 
------ ≅ μz( (1 + x × μy(1 + x × y^2))^2                            +  (2xz + 2x^2z × μy(1 + x × y^2)^2 × z))
------ ≅ μz( (1 + x × μy(1 + x × y^2)) × (1 + x × μy(1 + x × y^2))  +  (2xz + 2x^2z × μy(1 + x × y^2)^2 × z))
------ ≅ μz( 1 + 2x × μy(1 + x × y^2)  +  (x × μy(1 + x × y^2))^2   +  (2xz + 2x^2z × μy(1 + x × y^2)^2 × z))
------ ≅ μz( 1 + 2x × μy(1 + x × y^2)  +  (x × μy(1 + x × y^2))^2   +  (2xz + 2x^2z × μy(1 + x × y^2)^2 × z))
------ 
------ 
------ ≅ μy(1 + x × y^2)^2  +  2x × μy(1 + x × y^2) × μz( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z)
------ ≅ (1 + x × μy(1 + x × y^2))^2                              +  2x × μy(1 + x × y^2) × μz( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z)
------ ≅ (1 + x × μy(1 + x × y^2)) × (1 + x × μy(1 + x × y^2))    +  2x × μy(1 + x × y^2) × μz( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z)
------ ≅ 1 + 4x × μy(1 + x × y^2) + x^2 + μy(1 + x × y^2)^2   +  2x × μy(1 + x × y^2) × μz( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z)
------ 
------ 
------ ≅ μy(1 + x × y^2)^2  +  2x × μy(1 + x × y^2) × ((μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × μz(( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z))
------ ≅ μy(1 + x × y^2)^2  +  2x × μy(1 + x × y^2) × ((μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × μz(( μy(1 + x × y^2)^2)  +  (2x × μy(1 + x × y^2)) × z)
------ 
------ 2 + btree
------ 
------ ≅ μy.F^2  +  2x × (μy.F^3)  +  2x × μy.F × (2x × μy.F) × μz((μy.F^2)  +  (2x × μy.F) × z))
------ ≅ μy.F^2  +  2x × (μy.F^3)  + 4x^2 × μy.F^4  +  2x × μy.F × (2x × μy.F) ×  (2x × μy.F) × μz((μy.F^2)  +  (2x × μy.F) × z)
------ 
------ tavoite:
------ 
------ ≅ (μz(1  +  2 × z  +  μy (1 + x × y^2) × z))
------ ≅ (μz(1 + (2 + μy (1 + x × y^2)) × z))
------ ≅ [2 + btree]
------ 
------ Σ_n∈N (2 + Σ_i,j∈N ix^j)^n

------ Ei minulla ole hajuakaan miten tämä pitäisi saavuttaa.
------ answer: (a, [(Bool, btree)])

-}
