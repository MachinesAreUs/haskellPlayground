import Test.Hspec
import Test.HUnit
import Control.Exception
import StringCalculator

mySpecs = describe "StringCalculator" [
	it "adds 3 comma separated integers" 
		( add5 "1,2,3" == Right 6 ),
	it "adds an arbitrary number of comma separated integers"
		( add5 "10,9,8,7,6,5,4,3,2,1" == Right 55 ),
	it "adds comma or newline separated integers"
		( add5 "1,2\n3" == Right 6 ),
	it "accepts a delimiter as prefix, specified by '//[delimiter]\\n'"
		( add5 "//_\n1_2_3" == Right 6 ),
	it "accepts a delimiter but newlines should still be working as delimiters"
		( add5 "//_\n1_2\n3" == Right 6 ),
--	it "should throw an error when given a negative number"
--		( try ( add5 "1,-2,3" ) == Left (Exception "One negative!") ),
	it "should return the list of negative numbers when given more that one"
		( add5 "1,-2,-3" == Left [-2,-3] ),
	it "should ignore numbers > 1000"
		( add5 "1,2,3,1001" == Right 6 )
	]
	
main = hspec mySpecs