---
toptitle: Generic Coerce
keywords: haskell, Generics, coerce
---

# Generic Coerce

## Limits of `coerce`

Someone [asked a question on stack overflow](https://stackoverflow.com/questions/66011340) that got me thinking about Generics.  The question itself was about why `Coercible` is so limited.  A very simple version of the question goes like this:

> I have a data type that is isomorphic to `Bool`, like so:
> ```haskell
> data Foo = Bar | Qux
> ```
> Why is there no instance for `Coercible Bool Foo`?  How can I make one?

It's pretty obvious that these two types are isomorphic, and indeed, GHC has the same memory representation for them under the hood, but there is no way to generate a `Coercible` instance for them, which means that `coerce` is not allowed.

Of course, one could write the obvious coercion functions manually, but this is tedious and error-prone (especially with more complex types), and it may have poor performance too.  Another option is to use `unsafeCoerce`, as in:
```haskell
fooToBool :: Foo -> Bool
fooToBool = unsafeCoerce
```
This has the advantage of being high performance, but there's absolutely no type safety.  If someone goes ahead and changes `Foo` so that it's no longer isomorphic to `Bool`, this function will still type check, even if behavior becomes ... suspect.

## Generic Coerce

This is where `Generic` comes in.  Using `Generic`, we can define a type-level isomorphism between two types, up to meta data.  I'll hide the Haskell header behind this dropdown, but then consider the following definitions:

<details class="code-details">

<summary>Haskell extensions and imports</summary>

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Coerce where

import Data.Functor.Identity
import GHC.Generics
import Unsafe.Coerce (unsafeCoerce)

data Foo = Bar | Qux
  deriving (Show, Generic)
```
</details>

```haskell
class GenericCoerce a b where
  genericCoerce' :: a x -> b x

genericCoerce :: (Generic x, Generic y, GenericCoerce (Rep x) (Rep y)) => x -> y
genericCoerce = to . genericCoerce' . from
```
If we have two types that are both `Generic`, and we can coerce between their `Rep`s, then we can coerce between them.  What's left is to define the instances of `GenericCoerce`.  As we must for any `Generic` class, we need to handle the six cases: void, unit, sum, product, container, and wrapper.  These all have quite natural definitions:
```haskell
instance GenericCoerce V1 V1 where
  genericCoerce' = \case

instance GenericCoerce U1 U1 where
  genericCoerce' = id

instance (GenericCoerce f f', GenericCoerce g g') => GenericCoerce (f :+: g) (f' :+: g') where
  genericCoerce' (L1 x) = L1 (genericCoerce' x)
  genericCoerce' (R1 x) = R1 (genericCoerce' x)

instance (GenericCoerce f f', GenericCoerce g g') => GenericCoerce (f :*: g) (f' :*: g') where
  genericCoerce' (x :*: y) = genericCoerce' x :*: genericCoerce' y

instance GenericCoerce cs1 cs2 => GenericCoerce (M1 t m cs1) (M1 t m' cs2) where
  genericCoerce' (M1 x) = M1 (genericCoerce' x)

instance (Generic x, Generic y, GenericCoerce (Rep x) (Rep y)) => GenericCoerce (K1 t x) (K1 t y) where
  genericCoerce' (K1 x) = K1 (genericCoerce x)
```
For instance, unit is obviously coercible to unit.  Furthermore, if we can individually coerce the lefts and rights of two sum types, then we can coerce those sum types too.

Indeed, this seems to solve our problem and give us a whole new way to coerce between two types.  In action, we have:
```haskell
> genericCoerce @Bool @Foo True
Baz
> genericCoerce @Bool @Foo False
Qux
> genericCoerce @Foo @Bool Baz
True
```

## Non-Generic types

There are some obvious problems with `genericCoerce`.  For one, it only works on types that are instances of `Generic`, so, for example, `genericCoerce @Int @Int 3` doesn't work at all.  But in fact, because of the way we've written the `K1` (recursive) case, even something like `genericCoerce @[Int] @[Int] [3]` doesn't work.

In a perfect world, we'd be able to tell GHC to do something like "Use `coerce` if there's a `Coercible` instance, and otherwise, try `genericCoerce`", but this isn't possible.  The best I've been able to come up with is to use _incoherent instances_.  Incoherent instances are typically **considered dangerous**, and I'm not sure this is really a good enough use for them (is there one?), but let's have some fun!  Let's replace the instance we have with:
```haskell
instance {-# INCOHERENT #-} Coercible x y => GenericCoerce (K1 t x) (Rec0 y) where
  genericCoerce' (K1 x) = K1 (coerce x)

instance {-# INCOHERENT #-} (Generic x, Generic y, GenericCoerce (Rep x) (Rep y)) => GenericCoerce (Rec0 x) (K1 t y) where
  genericCoerce' (K1 x) = K1 (genericCoerce x)
```
Here we've used a stupid trick to get around the fact that these two really should be duplicate instances.  In one of them, we use `K1 t x` for the first parameter and `Rec0 y` for the second, and in the other, we flip that.  Remember that `type Rec0 = K1 R`, and `Generics` doesn't use any other type with `K1` besides `R`.  That means that these are _functionally_ the same, but since GHC doesn't know that, it gets around the duplicate instance issue.  Yay!

We still can't write `genericCoerce @Int @Int 3`, but now
```haskell
> genericCoerce @[Int] @[Int] [3]
[3]
```
works just fine, and we even get
```haskell
> genericCoerce @[Int] @[Identity Int] [3]
[Identity 3]
> genericCoerce @[Identity (Identity Int)] @[Int] [Identity (Identity 3)]
[3]
```
too!

## Higher Kinded Newtype Wrappers

That stackoverflow question I mentioned that put me onto this topic actually demanded even more.  The poster's data types were actually built using the "higher-kinded types" design principle, and they look like:
```haskell
newtype Id v a = Id a
  deriving (Generic, Show)

data Typ where
    PredT :: Typ
    ProcT :: [Typ] -> Typ
    IntT  :: Typ
    ListT :: Typ -> Typ
  deriving (Show, Generic)

data HKTyp v (f :: * -> * -> *) where
    HKPredT :: HKTyp v f
    HKProcT :: [HKTyp v f] -> HKTyp v f
    HKIntT  :: HKTyp v f
    HKListT :: f v (HKTyp v f) -> HKTyp v f
  deriving (Generic)

deriving instance Show (f v (HKTyp v f)) => Show (HKTyp v f)
```
The question is: Can we coerce between `Typ` and `HKTyp Id v`?  Alas, `genericCoerce` breaks down again.  This time, it's because of that pesky use of `f` within the definition of `HKListT`.  The types are not _generically coercible_ because the `Generic` representation of a newtype wrapped data type and the data type itself are actually different, but they're also not _data coercible_ because `Typ` and `HKTyp Id v` are not `Coercible` so obviously `Typ` and `Identity (HKType Id v)` are not `Coercible` either.  It turns out we can cheat a bit to get around this hiccup too, once again using some scary looking incoherent instances:
```haskell
instance {-# INCOHERENT #-} (Generic x, Rep x ~ D1 m x', GenericCoerce x' y) => GenericCoerce (C1 m2 (S1 m3 (Rec0 x))) y where
  genericCoerce' = genericCoerce' . unM1 . from . unK1 . unM1 . unM1

instance {-# INCOHERENT #-} (Generic y, Rep y ~ D1 m y', GenericCoerce x y') => GenericCoerce x (C1 m2 (S1 m3 (Rec0 y))) where
  genericCoerce' = M1 . M1 . K1 . to . M1 . genericCoerce'
```
These instances allow us to unwrap (or rewrap if we're going in the other direction) a newtype wrapper that has already been converted to its `Rep`.  It's a little janky, but it definitely does the trick.  For instance, we can now define:
```haskell
promoteHK :: Typ -> HKTyp v Id
promoteHK = genericCoerce

demoteHK :: HKTyp v Id -> Typ
demoteHK = genericCoerce
```
and then see them in action:
```haskell
> promoteHK PredT
HKPredT

> promoteHK (ListT PredT)
HKListT (Id HKPredT)

> promoteHK (ListT (ListT (ListT PredT)))
HKListT (Id (HKListT (Id (HKListT (Id HKPredT)))))

> demoteHK (HKProcT [HKIntT, HKPredT])
ProcT [IntT,PredT]
```

## Performance

One really nice thing about `coerce` is that, like newtype wrappers, it has no runtime cost.  On the other hand, `genericCoerce` definitely has a cost.  But why should it?  The whole idea behind `genericCoerce` is that the structure of the input type and the output type are the same, either because their _representations_ are the same (and we're assuming the `Rep` from `Generic` is a good proxy for the memory representation) or because they're equivalent up to newtype wrappers.  Therefore, if we trust that our types are doing the right thing, we can simply take a shortcut on the implementation.  Indeed, we can just write:
```haskell
genericCoerce :: (Generic x, Generic y, GenericCoerce (Rep x) (Rep y)) => x -> y
genericCoerce = unsafeCoerce
```
The result will always be the same as the old `genericCoerce`, but the performance will be much better.  Looked at another way, this is much safer than `unsafeCoerce` because it's guarded by `GenericCoerce`.

### Caveat

As far as I'm aware, there are no guarantees from GHC about how memory is utilized, which means that our use of `unsafeCoerce` is still, to some extent, unsafe.  That said, excluding the various incoherent instances we added, the original version of `genericCoerce` converts a data type with phantom type parameters into the same datatype just with different phantom parameters.  Technically, I can provide no guarantee that GHC will store multiple instances of the same runtime data the same way, but it seems to me like a pretty easy assumption to make.  Once we add in the incoherent instance shenanigans, we're standing on less solid grounds, but the fact that it all works is some consolation.

## Conclusion

Unfortunately, we were not able to build a truly more generic version of `coerce` (e.g., `genericCoerce` still can't coerce between `Int` and anything else since `Int` is not an instance of `Generic`), but `genericCoerce` does allow us a surprising amount of freedom over `Generic` types.  For instance, we get type-safe coercions between any two `Generic` data types that have the same structure, which could be useful for users of higher-kinded data types.

Another downside of our implementation is in the use of incoherent instances, which, as mentioned above are **considered harmful**.  Incoherent instances have a bad habit of seeming pretty good until you stretch them to their limits, at which point they spontaneously do something unexpected.  I'm not sure if that danger would rear its ugly hear here, but it is rather frustrating that there's no other way to write the `K1`/`Rec0` instance(s) that we were after.
