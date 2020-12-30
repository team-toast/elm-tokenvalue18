# Elm TokenValue(18)

The TokenValue type is meant to hold exact EVM values, and helps to translate between user input (i.e. "14.2") and EVM values (i.e. 14200000000000000000) assuming 18 decimal precision in the underlying EVM value.

It uses BigInt under the hood, and provides functionality for
* basic arithmetic
* formatting to the user
* interpreting from the user
* translating to/from Float or Int
