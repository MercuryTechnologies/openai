# OpenAI Haskell Examples

This directory contains examples demonstrating various features of the OpenAI Haskell library.

## Examples

### [`openai-example`](./openai-example/)

A simple example demonstrating basic chat completion with the OpenAI API.

**Features:**

- Basic chat completion
- Simple text input/output
- Minimal setup

**Usage:**

```bash
cabal run openai-example
```

### [`weather-chatbot-example`](./weather-chatbot-example/)

A chatbot example demonstrating tool calling and turn-based conversation flow.

**Features:**

- OpenAI Function Calling (Tools)
- Interactive chatbot with conversation history
- Mock weather tool implementation
- Turn-based conversation flow
- Multiple tool calls support

**Usage:**

```bash
cabal run weather-chatbot-example
```

### [`responses-example`](./responses-example/)

Minimal example using the Responses API to generate text output.

**Features:**

- Calls `/v1/responses`
- Prints aggregated output text from response items

**Usage:**

```bash
cabal run responses-example
```

### [`responses-stream-example`](./responses-stream-example/)

Streams events from the Responses API and prints text deltas in real time.

**Features:**

- Calls `/v1/responses` with `stream = true`
- Handles `response.output_text.delta` and prints deltas as they arrive

**Usage:**

```bash
cabal run responses-stream-example
```

## Setup

All examples require an OpenAI API key set as an environment variable:

```bash
export OPENAI_KEY="your-openai-api-key-here"
```

## Building

Build all examples:

```bash
cabal build
```

Build specific example:

```bash
cabal build openai-example
cabal build weather-chatbot-example
cabal build responses-example
cabal build responses-stream-example
```
