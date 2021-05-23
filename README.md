Packages:
- `vk-api` - working with the API inside plain `IO` monad (can be easily tested inside GHCi) (see
   `examples/`);
- `vk-api-mtl` - same as `vk-api` but all the code is lifted inside monad transformers and type
   classes ([MTL](https://hackage.haskell.org/package/mtl) style) (see `examples/`).

Currently implemented:
- Calling API methods (`callMethod` and `callMethod_` functions);
- Long Poll API support (`awaitEvents` function);
- Events:
  - `message_new`.
