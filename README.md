# drinkery

drinkery is a yet another stream processing library themed on liquors. While it
offers a simple interface, it also tries to be as expressive as possible.

## Producers

drinkery supports three types of producers: `Barman`, `Sommelier`, and `Tap`.

`Barman r s` is a monad transformer to produce a stream of type `s`. It is good
at interactively serving values.
`topup :: s -> Barman r s m a` is the primary action.
A barman can also accept requests from the downstream using `accept`.

`Sommelier r` is a list-like backtracking monad (also known as ListT done right).
It is useful for sampling elements of containers with effects.
`taste :: Foldable f => f s -> Sommelier r m s` samples elements in any `Foldable`
container. `inquire` to interact with the downstream.

`Tap` is an endless producer. This can be connected to a 'Patron' or a 'Distiller'.

`Barman` and `Sommelier` are converted to `Tap`
by `runBarman` and `runSommelier` respectively.

## Consumer

`Patron r s` is a monad transformer which consumes `s` and may request `r`.

`MonadDrink` provides the actions of `Patron`:

* `drink :: m s` Get one element.
* `spit :: s -> m ()` Leave one element.
* `call :: r -> m ()` Send a request.

`(+&) :: (Monoid r, CloseRequest r, Monad m) => Tap m r s -> Patron r s m a -> m a`
connects a tap with a patron.

## Transducer

`Distiller p q m r s` is a stream transducer which

* Sends `p`
* Consumes `q`
* Receives `r`
* Produces `s`

It is actually a `Tap` where the underlying monad is `Patron`.

There are three composition operators:

* `$&` Distiller-patron
* `$$$` Distiler-distiller
* `++$` Tap-distiller

`+`, `&`, and `$` means a tap, a patron, and a distiller respectively. The middle
characters of these operators signify the resulting structures.

## Why drinkery?

drinkery is designed to be fully featured and complements other libraries' missing
functionalities.

### pipes

`pipes` is quite similar in that both `Proxy` and `Distiller` are bidirectional.
Still there are some differences:

* `Distiller` does not terminate.
* Unlike pipes' `>->`, `$$$` propagates inner requests:
    * `($$$) :: Monoid r => Distiller p q m r s -> Distiller r s m t u -> Distiller p q m t u`
    * `(>->) :: Proxy a' a () b m r	-> Proxy () b c' c m r -> Proxy a' a c' c m r`
* `Patron`, the consumer monad, may leave unconsumed inputs.
* `drinkery` has much fewer operators.

### conduit

Both `drinkery` and `conduit` support leftovers, closing operation, and end of stream.
The main difference is interactivity.

### machines

`machines` has multi-channel consumers but `drinkery` doesn't.
`machines` does not support leftovers, nor interactive producers.

### iteratee

`iteratee` has an ability to handle requests but those are untyped (`SomeException`).
`drinkery` provides a more robust interface for handling requests.
Two monadic producers - `Barman` and `Sommelier` - are easier to use.
