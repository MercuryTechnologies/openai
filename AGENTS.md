# AGENTS.md

## Project Overview

Haskell library providing type-safe bindings to the OpenAI API using Servant. Published on Hackage as `openai`. Current version tracked in `openai.cabal`.

## Build & Development

```bash
# Environment setup (Nix + direnv)
direnv allow              # or: nix develop

# Build
cabal build

# Run tests (requires OPENAI_KEY env var for live API calls)
cabal test

# Format code
stylish-haskell -i <file.hs>

# Run an example
cabal run openai-example
cabal run responses-stream-example
```

GHC version: 9.6.6. All language extensions are set globally in `openai.cabal` (not per-file pragmas).

## Architecture

### Entry Point: `src/OpenAI/V1.hs`

- Defines the Servant `API` type combining all endpoints with `:<|>`
- Exports `Methods` record containing all API operations as functions
- `makeMethods` creates the record from a `ClientEnv`, API key, optional org, and optional project
- `getClientEnv` builds the HTTP client environment from a base URL

### Module Layout: `src/OpenAI/V1/`

Each OpenAI API endpoint has a corresponding module defining request/response types:
- `Chat/Completions.hs` + `Chat/Completions/Stream.hs` — chat types, streaming event types split out
- `Responses.hs` — Responses API (large module with tool definitions, structured outputs, streaming events)
- `Audio/`, `Images/`, `Threads/`, `VectorStores/` — nested by API path
- Shared types: `ListOf.hs`, `Message.hs`, `Order.hs`, `DeletionStatus.hs`, `AutoOr.hs`

### Key Patterns

**Prelude** (`src/OpenAI/Prelude.hs`): Re-exports common types (Text, Vector, Map, Generic, Servant combinators) and provides JSON helpers (`aesonOptions` with `omitNothingFields`, `labelModifier` for lowercase field names, trailing underscore stripping for reserved words).

**Request type convention**: Types prefixed with `Create*` (e.g., `CreateChatCompletion`). Use `_CreateChatCompletion` as the default value and override fields with record syntax, since `-Wno-missing-fields` is enabled.

**Streaming**: Custom SSE parser in `V1.hs` using `http-client` directly (not Servant client). Two variants: raw `Value` stream and typed deserialization. Streaming types live in dedicated `Stream` submodules.

**Methods record**: Internal `_`-prefixed methods handle raw Servant types (e.g., `ListOf`). Public methods wrap these to return `Vector` etc. Multipart file uploads use `servant-multipart`.

### Adding a New Endpoint

1. Create module in `src/OpenAI/V1/` with request/response types
2. Add endpoint to Servant `API` type in `src/OpenAI/V1.hs`
3. Add method to `Methods` record and wire it in `makeMethods`
4. Add tests in `tasty/Main.hs`
5. Expose module in `openai.cabal`

## Code Conventions

- **Imports**: Unqualified first (sorted), then qualified (sorted). Qualifiers must be full words (e.g., `import qualified Data.Vector as Vector`, not `as V`).
- **JSON deriving**: Use `genericParseJSON`/`genericToJSON` with `aesonOptions` from `OpenAI.Prelude`. Sum types use `constructorTagModifier = labelModifier`.
- **Formatting**: Use `stylish-haskell` (config in `.stylish-haskell.yaml`).

## Test Suite

Tests are in `tasty/Main.hs` using Tasty + HUnit. They make live API calls — `OPENAI_KEY` must be set. Test data files live in `tasty/data/` (audio, images, JSONL for uploads/fine-tuning/batches).

## Publishing

Version bump in `openai.cabal`, update `CHANGELOG.md`, then `cabal sdist && cabal upload --publish`.
