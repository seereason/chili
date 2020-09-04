In Dominator/Chili we have two types for each event. One is an
algebraic data type which represents the event type (aka, event
name). The other type is a wrapper around the `JSVal` which actually
contains the event.

So, for mouse events we have:

```
data MouseEvent
  = Click
  | ContextMenu
  | DblClick
  | MouseDown
  | MouseEnter
  | MouseLeave
  | MouseMove
  | MouseOver
  | MouseOut
  | MouseUp
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
```

```
newtype MouseEventObject = MouseEventObject { unMouseEventObject :: JSVal }
```

We, of course, want to link these to things together. If we are
listening for a `Click` event then our handler is going to receive a
`MouseEventObject`.

There is a type family which associates events with their event objects.

```
type family EventObjectOf event :: *
type instance EventObjectOf MouseEvent     = MouseEventObject
```

When we call `addEventListener` we see that the type ensures the handler matches the event,

```
addEventListener :: (MonadIO m, IsEventTarget self, IsEvent event, FromJSVal (EventObjectOf event)) =>
                  self
               -> event -- ^ event to listen for
               -> (EventObjectOf event -> IO ()) -- ^ event handler
               -> Bool
               -> m ()
addEventListener self event callback useCapture = ...
```

The DOM provides a `CustomEvent` object which allows use to create our own events. Unlike `MouseEvent`, `KeyboardEvent`, etc, we do not know what event types the user might want to use. So we need to get a bit more abstract.

Our `CustomEvent` type now looks like this,

```
data CustomEvent (e :: k) = CustomEvent
```

to create user supplied types we have two mechanisms. We can refer adhoc `CustomEvents` using Symbols.

    CustomEvent ("myevent" :: Symbol)

Alteratively, if we use `DataKinds` we can use promoted types:

    data MyEvents = MyEvent1 | MyEvent2

    myEvent1 :: CustomEvent 'MyEvent1
    myEvent1 = CustomEvent

The `CustomEventObject` looks like:

```
newtype CustomEventObject (e :: k) a = CustomEventObject { unCustomEventObject :: JSVal }
```

The type variable `a` is the type of the value that may be stored in the `detail` of a `CustomEvent`.

to tie the `CustomEvent` and `CustomEventObject` together we create a type family instances,

```
type instance EventObjectOf (CustomEvent 'MyEvent1)   = CustomEventObject 'MyEvent1 Text
type instance EventObjectOf (CustomEvent 'MyEvent2)   = CustomEventObject 'MyEvent2 Int
```

This tells the compiler that for `'MyEvent1` it is going to need a
handler for the type `CustomEventObject 'MyEvent1 Text`. The `Text`
will be the type of the `detail`.

With the above instances, you could not have a single handler that works for any `MyEvent` because each constructor maps to a unique `CustomEventObject` type. It would be possible to have them all resolve to the same type:

```
type instance EventObjectOf (CustomEvent 'MyEvent1)   = CustomEventObject 'MyEvent Text
type instance EventObjectOf (CustomEvent 'MyEvent2)   = CustomEventObject 'MyEvent Text
```

To add an event listener we just create handler and use `addEventListener`

```
myEvent1Handler :: CustomEventObject 'EditorAppendEvent Text -> IO ()
myEvent1Handler ev = ...

main :: IO ()
main =
 do  ...
     addEventListener rootNode (CustomEvent :: CustomEvent 'MyEvent1) myEvent1Handler False
```

To create a new CustomEvent we use the `newCustomEvent` function

```
newCustomEvent :: (IsEvent (CustomEvent ev), FromJSVal a, ToJSVal a) => CustomEvent ev -> Maybe a -> Bool -> Bool -> IO (CustomEventObject ev a)
newCustomEvent ev detail bubbles cancelable = ...
```

Once we have our CustomEvent we can use the standard `dispatchEvent` function:

```
     myEvent1 <- newCustomEvent (CustomEvent :: CustomEvent 'MyEvent1) (Just ("my event 1 detail" :: Text)) True True
     dispatchEvent eventTarget myEvent1
```

That is the same `dispatchEvent` function which is used for built-in
events like `MouseEvent`, `KeyboardEvent`, etc. The only difference is
that we use `newCustomeEvent` instead of `newEvent` to create the
object.

If we do not want to create a custom data type we can just use symbols,

```
foobarHandler :: CustomEventObject "foobar" Text -> IO ()
foobarHandler e =
  do do md <- detail e
        putStrLn $ "foobarHandler -> " ++ show md

type instance EventObjectOf (CustomEvent "foobar")   = CustomEventObject "foobar" Text

main :: IO ()
main =
  do ...
     addEventListener rootNode (CustomEvent :: CustomEvent "foobar") foobarHandler False

     foobarEvent <- newCustomEvent (CustomEvent :: CustomEvent "foobar") (Just ("symbol event detail" :: Text)) True True
     dispatchEvent rootNode foobarEvent
```

Note that we still use `EventObjectOf` to ensure that type for
`detail` is consistent between `newCustomEvent`, `addEventListener`,
and `foobarHandler` for the custom `"foobar"` event.

