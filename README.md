# `openai`

This provides a binding to OpenAI's API using `servant`

## Example usage

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}

module Main where

import Data.Foldable (traverse_)
import OpenAI.V1
import OpenAI.V1.Chat.Completions

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Environment as Environment

main :: IO ()
main = do
    key <- Environment.getEnv "OPENAI_KEY"

    clientEnv <- getClientEnv "https://api.openai.com"

    let Methods{ createChatCompletion } = makeMethods clientEnv (Text.pack key)

    text <- Text.IO.getLine

    ChatCompletionObject{ choices } <- createChatCompletion _CreateChatCompletion
        { messages = [ User{ content = [ Text{ text } ], name = Nothing } ]
        , model = "gpt-4o-mini"
        }

    let display Choice{ message } = Text.IO.putStrLn (messageToContent message)

    traverse_ display choices
```

## Setup

### Using Nix with Flakes (Recommended)

This project uses Nix with flakes for development environment setup.

1. Ensure you have Nix with flakes enabled
2. Copy the sample environment file and configure your OpenAI API key:

```bash
# Copy the sample environment file
cp .envrc.sample .envrc
```

3. Edit the `.envrc` file and replace the placeholder API key with your actual key

4. Use direnv to automatically load the development environment:

```bash
# Install direnv if you haven't already
# macOS: brew install direnv
# Linux: your-package-manager install direnv

# Enable direnv hook in your shell
eval "$(direnv hook bash)" # or zsh, fish, etc.

# Clone the repository and enter the directory
git clone https://github.com/MercuryTechnologies/openai.git
cd openai

# Allow direnv (this will automatically load the environment)
direnv allow
```

### Manual Setup

Without Nix:

```bash
# Clone the repository
git clone https://github.com/MercuryTechnologies/openai.git
cd openai

# Build with cabal
cabal build
```

## Environment Variables

Set your OpenAI API key as an environment variable:

```bash
# Option 1: Set directly in your shell
export OPENAI_KEY="your-openai-api-key"

# Option 2: Using .envrc with direnv (recommended)
(umask 077; cp .envrc.sample .envrc)
# Edit .envrc to add your API key
direnv allow
```

The API key is needed for running the test suite and example program.

## Testing

Run the test suite:

```bash
cabal test
```

The test suite is in the `tasty/` directory with test data located in `tasty/data/`.

## Running the Example

```bash
# Make sure your API key is set (either via .envrc or export)
# If using direnv with proper .envrc setup, this happens automatically

# Build and run the example
cabal run openai-example
```
