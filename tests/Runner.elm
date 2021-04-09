module Runner exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import TokenValue


suite : Test
suite =
    describe "describe"
        [ test "test"
            (\_ ->
                TokenValue.isZero TokenValue.zero
                    |> Expect.true "Bad zero"
            )
        ]
