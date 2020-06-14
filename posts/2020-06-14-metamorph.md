---
toptitle: Row-Types Metamorphism
keywords: haskell, row-types, metamorph
---


# Row-Types Metamorphism

## Heterogeneous Mapping

One of the very powerful features of row-types records is the ability to map over them.  Mapping over a list or another functorial object is rather straightforward, but how does one map over a heterogeneous record?  What would the type of such a function even be?  To gain some insight into this question, we can start by considering what it might mean to map a function over a native Haskell record.  Consider a record such as the following:

```haskell
data MyRecord = MyRecord
  { a :: Int
  , b :: Bool
  , c :: Char
  }
```
And we're saying we want a function of the form `(a -> b) -> MyRecord -> MyRecord'` where `MyRecord'` is some morphed version of `MyRecord`.  But also, the function `a -> b` is specifically going to act on `Int`, `Bool`, and `Char` and turn them into whatever their new types are in `MyRecord'`.  How do we even begin?  One way to proceed is by using the idea of [Higher-Kinded Data (HKD)](https://reasonablypolymorphic.com/blog/higher-kinded-data/) and rewriting our record like so:

```haskell
data MyRecordHKD f = MyRecordHKD
  { a :: f Int
  , b :: f Bool
  , c :: f Char
  }
```
Aha!  Now, we want a function of the type `map :: (forall a. f a -> g a) -> MyRecordHKD f -> MyRecordHKD g`.  Of course, this is still a little difficult to work with — what functions can we actually provide as the first argument to this map?  I could write a simple function like
```haskell
toMaybe :: Identity a -> Maybe a
toMaybe (Identity a) = Just a
```
and this will work!  However, it doesn't let me _do_ anything with the data in `MyRecordHKD`.  For instance, I cannot choose that some values will be `Nothing` while others will be `Just`.  As another example of our current limitations, wouldn't it be great if I could `show` all the values in my record, producing a new record where every field is a `String` (i.e. `MyRecordHKD (Const String)`)?  This seems like it should be fine, but it requires that our passed in function have a constraint: it needs the type `forall a. Show a => f a -> g a`.  Of course, we could make a version of `map` like this, but it still feels lacking.  This new map function would have the type:
```haskell
map :: (forall a. Show a => f a -> g a) -> MyRecordHKD f -> MyRecordHKD g
```
From here, we can generalize no further.  It would be great to generalize `Show` to an arbitrary constraint, but how can we express that we need a constraint that applies to `Int`, `Bool`, and `Char`?  It would also be great if we could generalize to any HKD record, but how do we know that the fields of that record are all `Show`able?  What we really want is a way to generically declare in our context that all fields of our record satisfy the constraint.

## Enter row-types

With row-types, we can define a type
```haskell
type MyRow = "a" .== Int .+ "b" .== Bool .+ "c" .== Char`
type MyRowRecord = Rec MyRow
```
This behaves just like the native version of `MyRecord` (in fact, you can use `fromNative` and `toNative` to simply convert values between the two representations), but since we're using row-types, we're open to a bit more flexibility.  Specifically, we now have a mapping function at our disposal that does exactly what we want:

```haskell
map :: forall c f r. Forall r c => (forall a. c a => a -> f a)
    -> Rec r -> Rec (Map f r)
```

There are two notable differences between this `map` function and the one above:

1. We now have a requirement in the context of the form `Forall r c`.  Intuitively, this means that the constraint `c` holds on all of the types in the row `r`.

2. The output is a record over the row-type `Map f r`.  The difference between `r` and `Map f r` is exactly the difference between `MyRecord` and `MyRecordHKD` — it simply wraps the type of each value within the row with the type function `f`.

The `Forall` constraint is the key element that was missing without row-types, and with it, we can easily write a function that `show`s all the elements of `MyRowRecord`:

```haskell
showRecord :: forall r. Forall r Show => Rec r -> Rec (Map (Const String) r)
showRecord = map @Show @(Const String) @r show
```

The big question remaining is: What is this `Forall`, and how does it work?

## Forall

The `Forall` type class is one of the key elements that gives the row-types library the power it has.  It not only guarantees that each element of a row satisfies a given constraint, but it provides a function that allows one to make use of that fact.  This function works conceptually in two phases: first, it breaks apart the row into its constituent elements, and then it sews the row back up.  In this way, it acts first as a [catamorphism](https://en.wikipedia.org/wiki/Catamorphism) (a fold) and then as an [anamorphism](https://en.wikipedia.org/wiki/Anamorphism) (an unfold).  When these two morphisms happen back to back like this, they are together called a _metamorphism_.  The full definition of the `Forall` type class is:
```haskell
class Forall (r :: Row k) (c :: k -> Constraint) where
  metamorph :: forall (p :: * -> * -> *) (f :: Row k -> *) (g :: Row k -> *) (h :: k -> *) .
       Bifunctor p
    => Proxy (Proxy h, Proxy p)
    -> (f Empty -> g Empty)
    -> (forall ℓ τ ρ.
        (KnownSymbol ℓ, c τ, HasType ℓ τ ρ)
        => Label ℓ -> f ρ -> p (h τ) (f (ρ .- ℓ)))
    -> (forall ℓ τ ρ.
        (KnownSymbol ℓ, c τ, FrontExtends ℓ τ ρ, AllUniqueLabels (Extend ℓ τ ρ))
        => Label ℓ -> p (h τ) (g ρ) -> g (Extend ℓ τ ρ))
    -> f r
    -> g r
```

This is an intimidating definition, but if we take a step back and look at the shape, we can see the components.  The tail end of the function type is `f r -> g r`; this is because `metamorph` will take some value over the row `r` and return a new value over that row.  For instance, to use `metamorph` in our definition of `map`, we will take `f` as `Rec` and `g` as the composition of `Rec` and `Map f`, which we write
```haskell
newtype RMap (f :: * -> *) (ρ :: Row *) = RMap { unRMap :: Rec (Map f ρ) }
```
(It would be great if we could just write `Compose Rec (Map f)`, but `Map` is a type family and cannot be partially applied like that.  [Maybe some day](https://github.com/ghc-proposals/ghc-proposals/pull/242).)

The three prior arguments define a base case, the fold, and the unfold in that order.  This may be a bit surprising or even confusing, so let's fill in the types for `map` that we know, drop any constraints we don't need, and see what those arguments look like (we'll also fill in `(,)` for `p`, which is necessary when working with records, and `Identity` for `h`):
```haskell
  -> (Rec Empty -> RMap Empty)
  -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ, HasType ℓ τ ρ)
     => Label ℓ -> Rec ρ -> (Identity τ, Rec (ρ .- ℓ)))
  -> (forall ℓ τ ρ. (KnownSymbol ℓ, c τ)
     => Label ℓ -> (Identity τ, RMap ρ) -> RMap (Extend ℓ τ ρ))
```
If you squint, you see that the first argument is a function for how to convert an empty record from the input to the output — this is the base case.  The second argument is a function that dictates how a record gets split apart into the "next" element (the `Identity τ`) and the rest of the record — this is the guts of the catamorphism, or how to disassemble the record.  The final argument is a function for how to take a label, an element, and a partially built output record and extend the output record with that label — this is the heart of the anamorphism, or how to build up the record.

Although there are some slight subtleties, it's pretty straightforward to define these three functions for `map`.  The base case simply returns `RMap empty`, the catamorphism function returns the pair of the value at `ℓ` with the record `r .- ℓ`, and the anamorphism function is an injection of the value into the record at the given label.<sup>[1](#myfootnote1)</sup>

## Metamorphism and further

We can do much more with `metamorphism` than just a simple record `map` — the key is choosing the right types.  For instance, let's look again at the type `p`, the bifunctor that determines the structure of the output of the catamorphism and the input of the anamorphism.  For records, we chose to use `(,)`, signifying that each type in the row gives rise to a value (`h τ`) and the rest of the row.  However, if we choose `Either`, then each type in the row gives rise to _either_ a value _or_ the rest of the row.  This is _exactly_ what we need to metamorphize _variants_, and indeed, writing `map` for the `Var` type involves a call to `metamorph` with `p` as `Either`.  The two other canonical bifunctors are `Const` and `FlipConst`; `Const` can be used in cases where we simply want an arbitrary entry in the row, and `FlipConst` is useful when we don't care about the values in the row at all (`FlipConst` is used during `fromLabels`, where we generate a record/variant from a row-type that has no value yet).

Alternatively, consider if we let `f` be `RMap f` and `g` be `Compose f Rec` for some applicative functor `f`.  In this case, we'll be converting a `Rec (Map f r)` to a `f (Rec r)` — in other words, we have a `sequence` over records.

One unexpected power of `metamorph` is that it can act on _two records at once_.  At first, this may seem crazy — it only has access to one row type — but if the two records share the same row type, then there's really no problem.  For instance, we can use `metamorph` to define an equality function over records.  The input is a pair of records, so we will let `f` be `Product Rec Rec`, and the output is a Boolean, so we let `g` be `Const Bool`.  From there, the rest of the type parameters fall into place, and the argument definitions fall right out of the types:
```haskell
recordEq :: forall r. Forall r Eq => Rec r -> Rec r -> Bool
recordEq x y = getConst $ metamorph @_ @r @Eq @(,) @(Product Rec Rec) @(Const Bool) @(Const Bool)
  Proxy base cata ana (Pair x y)
  where
    base :: Product Rec Rec Empty -> Const Bool Empty
    base _ = Const True
    cata :: forall ℓ τ ρ. (KnownSymbol ℓ, Eq τ, HasType ℓ τ ρ)
         => Label ℓ -> Product Rec Rec ρ -> (Const Bool τ, Product Rec Rec (ρ .- ℓ))
    cata l (Pair r1 r2) = (Const $ a == b, Pair r1' r2')
      where
        (a, r1') = (r1 .! l, r1 .- l)
        (b, r2') = (r2 .! l, r2 .- l)
    ana :: forall ℓ τ ρ. (KnownSymbol ℓ, Eq τ)
        => Label ℓ -> (Const Bool τ, Const Bool ρ) -> Const Bool (Extend ℓ τ ρ)
    ana _ (Const b, Const b') = Const $ b && b'
```
For the first argument, `base`, we have a pair of empty records, and we need a `Bool`.  Since we're conjoining the equality of each of the elements of the record, `Const True` is an appropriate base case.
For the second argument, `cata`, we have a label and a pair of records, and we want to know if the values at this label are equal and we also want the rest of the records.  This is easily accomplished with simple row-types operators.
For the third argument, `ana`, we have a label, whether the values at this label are equal, and whether the values in the rest of the record are equal.  This is as simple as conjoining the two truth values.
Interestingly, `recordEq` is a great example of how simple `metamorph` really is at the value level; if you leave out the type, there is nothing particularly complex about the call.

### Double the Metamorphism, Double the Fun

I'll conclude with this little teaser: there's no reason we need to restrict ourselves to just one row-type.  Things get even more fun when we introduce `BiForall`, a type class over two row types that includes the `biMetamorph` function:
```haskell
class BiForall (r1 :: Row k1) (r2 :: Row k2) (c :: k1 -> k2 -> Constraint) where
  biMetamorph ::
    forall (p :: * -> * -> *) (f :: Row k1 -> Row k2 -> *) (g :: Row k1 -> Row k2 -> *)
           (h :: k1 -> k2 -> *) . Bifunctor p
    => Proxy (Proxy h, Proxy p)
    -> (f Empty Empty -> g Empty Empty)
    -> (forall ℓ τ1 τ2 ρ1 ρ2.
        (KnownSymbol ℓ, c τ1 τ2, HasType ℓ τ1 ρ1, HasType ℓ τ2 ρ2)
        => Label ℓ -> f ρ1 ρ2 -> p (h τ1 τ2) (f (ρ1 .- ℓ) (ρ2 .- ℓ)))
    -> (forall ℓ τ1 τ2 ρ1 ρ2.
        ( KnownSymbol ℓ, c τ1 τ2, FrontExtends ℓ τ1 ρ1, FrontExtends ℓ τ2 ρ2
        , AllUniqueLabels (Extend ℓ τ1 ρ1), AllUniqueLabels (Extend ℓ τ2 ρ2))
        => Label ℓ -> p (h τ1 τ2) (g ρ1 ρ2) -> g (Extend ℓ τ1 ρ1) (Extend ℓ τ2 ρ2))
    -> f r1 r2 -> g r1 r2
```

The `biMetamorph` function is scary to look at, but functionally, it follows the very same patterns as `metamorph` only with a second row.  Importantly, it allows us to add _zipping_ mechanics over row-types with the same labels _but different types_.  Without partially applied type families, `biMetamorph` becomes very tiresome to use, but it has potential, and I'm hopeful for its future.

<br>

---
#### Footnotes

<a name="myfootnote1">1</a>:
The full definition of `map` on records looks like this:
```haskell
map :: forall c f r. Forall r c => (forall a. c a => a -> f a)
    -> Rec r -> Rec (Map f r)
map f = unRMap . metamorph @_ @r @c @(,) @Rec @(RMap f) @Identity Proxy base cata ana
  where
    base _ = RMap empty
    cata l r = (Identity $ r .! l, lazyRemove l r)
    ana :: forall ℓ τ ρ. (KnownSymbol ℓ, c τ)
           => Label ℓ -> (Identity τ, RMap f ρ) -> RMap f (Extend ℓ τ ρ)
    ana l (Identity v, RMap r) = RMap (extend l (f v) r)
      \\ mapExtendSwap @ℓ @τ @ρ @f
```
It's very close to the description that I gave above, but it uses `unsafeRemove` in `cata` and this mysterious `mapExtendSwap` in `ana`.  The `unsafeRemove` function is simply a performance improvement, and in fact, replacing it with `r .- l` works perfectly well.

So what's the deal with `mapExtendSwap`?  The problem that we run into is that we need to generate a record with row-type `Map f (Extend ℓ τ ρ))`.  We have `r`, which is a record with row type `Map f ρ`, so we just `extend` that record with `f v` at label `l`, right?  Not so fast.  The type of `extend` is
```haskell
extend :: forall t l r. KnownSymbol l => Label l -> t -> Rec r -> Rec (Extend l t r)
```
where the type family `Extend` does some ordering magic to make sure that labels are always in alphabetical order.  So in order to call `extend`, we'll need to prove that `Extend ℓ (f τ) (Map f ρ) ~ Map f (Extend ℓ τ ρ)`.  This is where `mapExtendSwap` comes in.  Its type is:<sup>[2](#myfootnote2)</sup>
```haskell
mapExtendSwap :: forall ℓ τ r f. Dict(Extend ℓ (f τ) (Map f r) ~ Map f (Extend ℓ τ r))
```
In other words, it's free information at the type level that describes how `Extend` and `Map` relate and can be interchanged.  To bring it into scope, we use the function `\\` from `Data.Constraint`.


<a name="myfootnote2">2</a>:
I haven't shown the implementation of `mapExtendSwap`, and that's because there isn't one.  It's just an `unsafeCoerce` on an unconstrained `Dict`.  The result is always true, but I couldn't find a way to convince GHC of this without cheating.
