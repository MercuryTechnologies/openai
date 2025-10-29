2.2.0:

- Add structured reasoning support to `OpenAI.V1.Responses`, including the `Reasoning`, `ReasoningEffort`, and `ReasoningSummary` types, plus the `ServiceTier` alias.
- Extend `CreateResponse` and `ResponseObject` with `reasoning` and `service_tier` fields to round-trip reasoning configuration.

2.1.0:

- Add `Item_Input_Reasoning` constructor and JSON round-trip test so reasoning traces from tool calls can be echoed back via the Responses API.

2.0.0:

- **BREAKING CHANGE**: Renamed `Item_InputMessage` to `Item_Input_Message` in `OpenAI.V1.Responses` for consistency with new constructors

  The `InputItem` data type in the Responses API has been updated to use a consistent naming scheme. If you are using the Responses API, you will need to update your code:

  ```haskell
  -- Before (v1.2.0):
  Item_InputMessage { role = ..., content = ..., status = ... }

  -- After (v2.0.0):
  Item_Input_Message { role = ..., content = ..., status = ... }
  ```

- Add support for function tool calls in Responses API
  - New `InputItem` constructors: `Item_Input_Function_Call`, `Item_Input_Function_Call_Output`, `Item_Input_Item_Reference`
  - Add flattened tool JSON representation for Responses API compatibility
  - Export status constants: `statusIncomplete`, `statusCompleted`
  - Export tool choice constants: `toolChoiceNoneText`, `toolChoiceAutoText`, `toolChoiceRequiredText`

- Code quality improvements:
  - Optimize `isFunctionField` using `HashSet` for O(1) lookups
  - Simplify `unflattenChoice` with guards
  - Extract magic strings as named constants

1.2.0:

- [`/v1/responses`: Add support for Responses API](https://platform.openai.com/docs/api-reference/responses)
- Add `Tool_Web_Search`

1.1.1:

- [Remove timeout on default `ClientEnv`](https://github.com/MercuryTechnologies/openai/pull/55)
- [`/v1/chat/completions`: Add support for Search](https://github.com/MercuryTechnologies/openai/pull/57)
- [Fix CreateSpeech JSON instances, add new voices and optional instructions field](https://github.com/MercuryTechnologies/openai/pull/58)
- [Correct `ToJSON` of `FileSearchResources`](https://github.com/MercuryTechnologies/openai/pull/49)
- [New example app for tool-calling and chat-loop](https://github.com/MercuryTechnologies/openai/pull/60)

1.1.0:

- BREAKING CHANGE: Fix details representations for various types [[#44](https://github.com/MercuryTechnologies/openai/pull/44)] [[#45](https://github.com/MercuryTechnologies/openai/pull/45)] [[#50](https://github.com/MercuryTechnologies/openai/pull/50)] [[#51](https://github.com/MercuryTechnologies/openai/pull/51)]

  A few details-related fields were fixed to match the behavior of the OpenAI
  API.

- Add `FromJSON`/`ToJSON` instances for all types [[#42](https://github.com/MercuryTechnologies/openai/pull/42)] [[#47](https://github.com/MercuryTechnologies/openai/pull/47)]

- [Add support for `reasoning_effort` parameter in chat completions](https://github.com/MercuryTechnologies/openai/pull/48)

1.0.1:

- Include `README`
- Include usage example

1.0.0:

- Initial release
