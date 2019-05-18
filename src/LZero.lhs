<h1> The LZero language </h1>,

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module LZero where
> import qualified Data.Map.Strict as M
> import Control.Monad.Identity
> import Control.Monad.State
> type Variable = String
> type FunctorId = String
> type FunctorArity = Int



> (|>) :: a -> (a -> b) -> b
> (|>) = flip ($)


> newtype HeapIx = HeapIx Int deriving(Eq, Ord, Num)
> instance Show HeapIx where
>     show (HeapIx ix) = "0x" ++ show ix



> data HeapCell = HCRef HeapIx | HCStr HeapIx | HCF FunctorId FunctorArity deriving(Eq, Show)

> isHCRef :: HeapCell -> Bool
> isHCRef (HCRef _) = True
> isHCRef _ = False

> hcIx :: HeapCell -> HeapIx
> hcIx (HCRef ix) = ix
> hcIx (HCStr ix) = ix

> hcF :: HeapCell -> (FunctorId, FunctorArity)
> hcF (HCF fid far) = (fid, far)

> type Heap = M.Map HeapIx HeapCell 

> newtype Register = Register Int deriving(Eq, Ord, Show)

> data FlatFunctor = FlatFunctor FunctorId FunctorArity [Register]
> data FlatQuery = FlatQuery [(Register, Either FlatFunctor Variable)]

> newtype Stack a = Stack [a] deriving(Eq, Ord, Show)

> pushStack :: a -> Stack a -> Stack a
> pushStack a (Stack as) = Stack (a:as)

> popStack :: State (Stack a) a
> popStack = state $ \(Stack (a:as)) -> (a, Stack as)

> isStackEmpty :: Stack a -> Bool
> isStackEmpty (Stack []) = True
> isStackEmpty _ = False


> type RegisterState = (M.Map Register HeapCell)
> data Mode = MR | MW deriving(Eq, Show)
> data MachineState = 
>    MachineState { msheap :: Heap
>                 , msregs :: RegisterState
>                 , msH :: HeapIx
>                 , msMode :: Mode
>                 , msS :: HeapIx
>                 , msPDL :: Stack Addr
>                 } deriving(Eq, Show)


> setMW :: MachineState -> MachineState
> setMW s = s { msMode = MW }


> setMR :: MachineState -> MachineState
> setMR s = s { msMode = MR }

> setS :: HeapIx -> MachineState -> MachineState
> setS hix s = s { msS = hix }


mnenomic: `%` is used to denote register states.

> (!%) :: MachineState -> Register -> HeapCell
> s !% r = (msregs s) M.! r


mnenomic: `#` is used to name hex values. Heap indeces are usually written
in hex. so `#` is heap.

> (!#) :: MachineState -> HeapIx -> HeapCell
> s !# hix = (msheap s) M.! hix


mnenomic: `&` is the addressof operator. `!&` is to index with an address

> data Addr = AddrR Register | AddrH HeapIx deriving(Eq, Show)

> class Addressable a where
>    toaddr :: a -> Addr
>    (!&) :: MachineState -> a -> HeapCell


> instance Addressable Addr where
>    toaddr = id
>    s !& a = case a of
>        AddrR r -> s !% r
>        AddrH hix -> s !# hix

> instance Addressable Register where
>    toaddr = AddrR
>    (!&) = (!%)

> instance Addressable HeapIx where
>    toaddr = AddrH
>    (!&) = (!#)


> deref :: Addressable a => MachineState -> a -> Addr
> deref s a = 
>    case s !& (toaddr a) of
>        (HCRef hix) -> if toaddr hix == toaddr a 
>                       then toaddr a
>                       else deref s (toaddr hix)

Deref has a `*` since it dereferences the address / resolves it.
for follownng a chain

> (&*) :: Addressable a => MachineState -> a -> Addr
> (&*) = deref


Push a `HeapCell` into the heap of the machine state.

> machinePush :: HeapCell -> MachineState -> MachineState
> machinePush hc s = s {
>     msH = (msH s) + 1,
>     msheap = M.insert (msH s) hc (msheap s)
> }


mnenonic: `#` refers to heap indeces / heap values, << to pushing.
This is the operator version of `machinePush`.

> (<<#) :: MachineState -> HeapCell -> MachineState
> (<<#) = flip machinePush 

Set a register in the machine.

> machineSetRegister :: Register -> HeapCell -> MachineState -> MachineState
> machineSetRegister r hc s = s {  msregs = M.insert r hc (msregs s) }

> (%=) :: Register -> HeapCell -> MachineState -> MachineState
> r %= hc = machineSetRegister r hc


Push a `HeapCell` into the heap and save the heap into into the register.

> machinePushAndSave :: Register -> HeapCell  -> MachineState -> MachineState
> machinePushAndSave r hc s = machineSetRegister r hc (machinePush hc s)

mnenonic: `#` refers to heap indeces / heap values, `%` to registers, `>>` to pushing.
This is the operator version of `machinePushAndSave`.

> (<<%#) :: MachineState -> (Register, HeapCell)  ->  MachineState
> (<<%#) s (r, hc) = machinePushAndSave r hc s


> instPutStructure :: FunctorId -> FunctorArity -> Register -> MachineState -> MachineState
> instPutStructure fid far reg s =
>    s |> (<<%# (reg, HCStr (msH s + 1))) |> (<<# (HCF fid far)) 
    
> instSetVariable :: Register -> MachineState -> MachineState
> instSetVariable r s = s |> (<<%# (r, HCRef (msH s)))


> instSetValue :: Register -> MachineState -> MachineState
> instSetValue r s = s <<# (s !& r)

> type Error = String

> -- | H[addr] = HCRef hix
> bind :: Addr 
>         -> HeapIx
>         -> MachineState -> MachineState
> bind (AddrH lhs_hix) rhs_hix s = 
>   s { msheap = M.insert lhs_hix (HCRef rhs_hix) (msheap s)  }

> (&=) :: Addr -> HeapIx -> MachineState -> MachineState
> (&=) a hix ms = bind a hix ms


> -- | Destructure a heap cell as a HCF, and if not return an error
> expectHCF :: 
>    HeapCell -- ^ Heap cell to be inspected
>    -> (FunctorId -> FunctorArity -> b) -- ^ Provides the addr stored in the heap cell
>    -> Error -- ^ Error if this should fail
>    -> Either Error b
> expectHCF (HCF fid far) f _ = Right $ f fid far
> expectHCF _ _ err = Left $ err

Implementation from Figure 2.6

> instGetStructure :: FunctorId 
>    -> FunctorArity 
>    -> Register 
>    -> MachineState 
>    -> Either Error MachineState
> instGetStructure fid far r s = 
>    let addr = (s &* r)
>    in case s !& addr of
>        (HCRef _) -> 
>          let h = msH s 
>          in Right $ s |>
>                     (<<# HCStr (h + 1)) |>
>                     (<<# HCF fid far) |>
>                     (addr &= h) |>
>                      setMW 
>        (HCStr astr) -> 
>            expectHCF (s !& astr) 
>            (\_ _ -> setS (astr + 1) . setMR $ s) 
>             ("expected  functor cell at: " <> 
>              show astr <> 
>              "derived from: " <> 
>              show addr <> 
>              "derived from: " 
>              <> show r)


> incrH :: MachineState -> MachineState
> incrH s = s { msH = (msH s) + 1}

> heapAtS :: MachineState -> HeapCell
> heapAtS s = (s !# (msS s))

> instUnifyVariable :: Register -> MachineState -> MachineState
> instUnifyVariable r s = 
>    case msMode s of
>        MR -> s |> (r %= heapAtS s)
>        MW -> let h = msH s 
>              in s |> (<<%# (r, HCRef h))


> instUnifyValue :: Register -> MachineState -> MachineState
> instUnifyValue r s = 
>    case msMode s of
>        MR -> s |> unify r (msS s) 
>        MW ->  s <<# (s !% r)         


Figure 2.7, the unify operation:

> unify :: (Addressable a, Addressable b) =>
>     a -> b -> MachineState -> MachineState
> unify a a' = unify_ (toaddr a) (toaddr a')

> pushPDL :: Addressable a => a -> MachineState -> MachineState
> pushPDL a s = s { msPDL = pushStack (toaddr a) (msPDL s) }

> popPDL :: State MachineState Addr
> popPDL = error "unimplemented"

> unify_ :: Addr -> Addr -> MachineState -> MachineState
> unify_ a1 a2 s = s |> pushPDL a1 |> pushPDL a2 |> unifyRec

> isPDLEmpty :: MachineState -> Bool
> isPDLEmpty = isStackEmpty . msPDL


Note: it should be possible to adapt guard to this setting.
That is, there _is_ an Alternative instance for `MonadState s ()`??


> guardS :: Bool -> State s () -> State s ()
> guardS True s = s
> guardS False _ = return ()

> unifyRec :: MachineState -> MachineState
> unifyRec s = 
>    if isPDLEmpty s 
>    then s
>    else (flip execState) s $ do
>        d1 <- popPDL 
>        d1 <- gets (&* d1)
>        d2 <- popPDL
>        d2 <- gets (&* d2)
>        guardS (d1 /= d2) $ do
>           h1 <- gets (!& d1)
>           h2 <- gets (!& d2)
>           if isHCRef h1 && isHCRef h2
>           then let AddrH hd2 = d2 in modify (d1 &= hd2)
>           else do
>                let v1 = hcIx h1
>                let v2 = hcIx h2
>                (f1, n1) <- hcF <$> gets (!# v1)
>                (f2, n2) <- hcF <$> gets (!# v2)
>                if f1 == f2 && n1 == n2
>                then do
>                   forM_ [1..n1] $ \(i :: Int) -> do
>                      modify $ pushPDL v1 -- (v1 + i)
>                      modify $ pushPDL v2 -- (v2 + i)))
>                   modify $ unifyRec
>                else error "unable to unify" 

TODO: use view patterns!! 
unifyRec (s -> isStackEmpty) = s

