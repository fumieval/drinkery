# drinkery

drinkery is a yet another stream processing library themed on liquors. While it
offers a simple interface, it also tries to be as expressive as possible.

## Producers

drinkery supports three types of producers: `Barman`, `Sommelier`, and `Tap`.

`Barman r s` is a monad transformer to produce a stream of type `s`.
`topup :: s -> Barman r s m a` is the primary action.
A barman can also accept requests from the downstream using `accept`.

`Sommelier r` is a list-like backtracking monad.
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

* `$&&` Distiller-patron, creates a patron
* `$$$` Distiler-distiller
* `++$` Tap-distiller
