# Weather Chatbot Example

This example demonstrates how to create a chatbot using the OpenAI Haskell library with tool calling functionality. The chatbot can respond to weather queries by calling a mock weather tool.

## Features

- **Tool Calling**: Demonstrates how to define and use tools with the OpenAI API
- **Turn-based Chat**: Interactive chatbot that maintains conversation context
- **Multiple Tool Calls**: The LLM can make multiple tool calls before responding to the user
- **Mock Data**: Uses stub weather data for demonstration purposes

## Setup

1. Make sure you have an OpenAI API key set as an environment variable:

   ```bash
   export OPENAI_KEY="your-openai-api-key-here"
   ```

2. Build the example:

   ```bash
   cabal build weather-chatbot-example
   ```

3. Run the example:
   ```bash
   cabal run weather-chatbot-example
   ```

## Usage

Once running, you can interact with the weather chatbot:

```
🌤️  Weather Chatbot with Tool Calling
======================================
Ask me about the weather in different cities!
Try: 'What's the weather like in London?'
Available cities: London, Paris, Tokyo, New York, San Francisco
Type 'quit' or 'exit' to end the conversation.

You: What's the weather like in London?
🤖 Processing 1 tool call(s)...
🔧 Processing tool call: get_weather
   Arguments: {"city":"London"}
   Result: London: 18°C, partly cloudy, 60% humidity, light rain expected
```
