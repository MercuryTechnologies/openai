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
```
