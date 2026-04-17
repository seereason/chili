# GHC 9 JS Backend Support

This document describes the work done on the `rawjs` branch to make chili compile
with the GHC native JavaScript backend (GHC 9.x, `arch(javascript) && !impl(ghcjs)`).

## Background

Chili was originally written for GHCJS, which ships its own companion library
`ghcjs-base`. That library provides:

- `GHCJS.Types` — `JSVal`, `JSString`, `IsJSVal`, `nullRef`, `isNull`, `isUndefined`
- `GHCJS.Marshal` / `GHCJS.Marshal.Pure` — `ToJSVal`, `FromJSVal`, `PToJSVal`, `PFromJSVal`
- `GHCJS.Nullable` — `Nullable`, `nullableToMaybe`, `maybeToNullable`
- `GHCJS.Buffer` — `ByteString` ↔ `ArrayBuffer` conversion
- `GHCJS.Foreign` — `jsNull` and related primitives
- `JavaScript.TypedArray` / `JavaScript.TypedArray.ArrayBuffer` — typed array types
- `JavaScript.Web.MessageEvent` — WebSocket message events
- `JavaScript.Web.WebSocket` — WebSocket client

GHC's built-in JS backend does **not** include `ghcjs-base`. It provides a much
smaller surface in `GHC.JS.Prim` (`JSVal`, `toJSString`, `fromJSString`, `toJSInt`,
`fromJSInt`, `jsNull`, `isNull`, `isUndefined`) and `GHC.JS.Foreign.Callback`
(`Callback`, `asyncCallback1`, `syncCallback1`).

### What about ghcjs-dom?

`ghcjs-dom` (the DOM binding library) has two backends:

- `ghcjs-dom-javascript` — uses raw GHCJS FFI, GHCJS only
- `jsaddle-dom` — uses the jsaddle abstraction layer, works with both GHCJS and
  the GHC JS backend

Chili already uses `jsaddle` and `jsaddle-dom`. The jsaddle ecosystem packages
(`jsaddle`, `jsaddle-dom`, `jsaddle-warp`, etc.) are the correct path forward;
`ghcjs-dom-javascript` is not needed.

The `ghcjs-base` library itself is not available for the GHC JS backend. There
is ongoing community discussion about a compatibility layer but no official package
exists as of 2025.

## Approach: `src-js-compat` shim directory

Rather than CPP-guarding every GHCJS import site in the chili source, the chosen
approach is to provide a drop-in compatibility shim under `src-js-compat/`. Under
the GHC JS backend build condition, this directory is added to the source path
alongside `.`, so the shim modules shadow `ghcjs-base`'s modules by name.

In `chili.cabal`:

```cabal
if impl(ghcjs)
  build-depends: ghcjs-base

if arch(javascript) && !impl(ghcjs)
  hs-source-dirs: . src-js-compat
  other-modules:
    Data.JSString
    Data.JSString.Text
    GHCJS.Buffer
    GHCJS.Foreign
    GHCJS.Marshal
    GHCJS.Marshal.Pure
    GHCJS.Nullable
    GHCJS.Types
    JavaScript.TypedArray
    JavaScript.TypedArray.ArrayBuffer
    JavaScript.Web.MessageEvent
    JavaScript.Web.WebSocket
```

Note that `hs-source-dirs: . src-js-compat` must include `.` explicitly — a
conditional `hs-source-dirs` stanza replaces (not augments) the default.

## Key implementation details

### JSString must be a newtype over JSVal

In `ghcjs-base`, `JSString` is a JS-native string type backed by a JS value —
it is **not** `[Char]`. The GHC JS backend panics (`jsResultWrapper [Char]`) if
`[Char]` appears as the return type of a `foreign import javascript` declaration.

The shim therefore defines:

```haskell
newtype JSString = JSString { unJSString :: JSVal }
```

This makes `JSString` FFI-compatible (it is coercible to/from `JSVal`) and
avoids the panic. Conversion to/from Haskell strings goes through
`GHC.JS.Prim.toJSString` / `fromJSString`.

### GHC.JS.Prim has no numeric FFI helpers beyond Int

`GHC.JS.Prim` provides `toJSInt`/`fromJSInt` but no `toJSDouble`/`fromJSDouble`.
The `GHCJS.Marshal` shim uses direct `foreign import javascript` for `Double`
conversion:

```haskell
foreign import javascript unsafe "(($1) => { return $1; })" js_toDouble   :: Double -> JSVal
foreign import javascript unsafe "(($1) => { return $1; })" js_fromDouble :: JSVal -> Double
```

### Callback is opaque

`GHC.JS.Foreign.Callback.Callback` does not export its constructor in the GHC
JS backend. It cannot be `coerce`d to `JSVal`. Use `Callback (JSVal -> IO ())`
directly in `foreign import javascript` signatures and pass `Callback` values
directly — do not wrap or unwrap them.

### Instances needed on JSString

Because `JSString` is an opaque newtype (not `[Char]`), standard instances are
not derived automatically. The shim provides:

- `IsString` — so string literals produce `JSString`
- `Eq`, `Ord` — via round-trip through `unpack`
- `Show`, `Read` — via `String`
- `PToJSVal`, `PFromJSVal` — via the underlying `JSVal`

### PToJSVal instances needed beyond JSString

The jsaddle-dom `setProperty` function is polymorphic in `PToJSVal`. The shim
provides instances for:

- `JSVal` — identity
- `JSString` — unwrap newtype
- `Data.Text.Text` — convert via `textToJSString`
- `Bool` — map to JS int (1/0) via `toJSInt`
- `Maybe a` — `Nothing` → `jsNull`, `Just` → delegate

## Files in src-js-compat

| File | Replaces (from ghcjs-base) | Notes |
|------|---------------------------|-------|
| `Data/JSString.hs` | `Data.JSString` | JSString as newtype over JSVal |
| `Data/JSString/Text.hs` | `Data.JSString.Text` | re-exports from Data.JSString |
| `GHCJS/Types.hs` | `GHCJS.Types` | IsJSVal, nullRef, re-exports JSVal/JSString |
| `GHCJS/Marshal.hs` | `GHCJS.Marshal` | ToJSVal/FromJSVal for JSVal, JSString, String, Double |
| `GHCJS/Marshal/Pure.hs` | `GHCJS.Marshal.Pure` | PToJSVal/PFromJSVal; coerce default |
| `GHCJS/Nullable.hs` | `GHCJS.Nullable` | Nullable newtype, nullableToMaybe, maybeToNullable |
| `GHCJS/Buffer.hs` | `GHCJS.Buffer` | stubbed; ByteString↔ArrayBuffer not implemented |
| `GHCJS/Foreign.hs` | `GHCJS.Foreign` | re-exports jsNull |
| `JavaScript/TypedArray.hs` | `JavaScript.TypedArray` | empty; imported for instances only |
| `JavaScript/TypedArray/ArrayBuffer.hs` | `JavaScript.TypedArray.ArrayBuffer` | ArrayBuffer/MutableArrayBuffer newtypes |
| `JavaScript/Web/MessageEvent.hs` | `JavaScript.Web.MessageEvent` | MessageEvent newtype; getData via FFI |
| `JavaScript/Web/WebSocket.hs` | `JavaScript.Web.WebSocket` | WebSocket connect/send via FFI; uses Callback directly |

## What is not replaced

- `ghcjs-dom` / `ghcjs-dom-javascript` — chili uses `jsaddle-dom` instead, which
  works on both GHCJS and the GHC JS backend
- `ghcjs-base` numeric/array utilities beyond what chili uses — the shim only
  covers modules actually imported by chili source files
- `GHCJS.Buffer` ByteString conversion — stubbed with `error`; not exercised by
  the current WebSocket code path
