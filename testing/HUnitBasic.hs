import Char
import Test.HUnit

test1 = TestCase $ assertEqual "test upCase" "FOO" (map toUpper "foo")