## Functionality

Packages:
- `vk-api` - working with the API inside plain `IO` monad (can be easily tested inside GHCi) (see
   `examples/` and `vk-api/README`);
- `vk-api-mtl` - same as `vk-api` but all the code is lifted inside monad transformers and type
   classes ([MTL](https://hackage.haskell.org/package/mtl) style) (see `examples/` and
   `vk-api-mtl/README`).

Currently implemented:
- Calling [API methods](https://vk.com/dev/methods) (`callMethod` and `callMethod_` functions);
- [Long Poll API](https://vk.com/dev/bots_longpoll) support (`awaitEvents` function);
- Events:
  - `message_new` with incomplete `message` object.

Target API version: `5.126`.
