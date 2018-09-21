module Test.Main where

import Data.List
import Prelude

import Data.AddressBook as AddressBook
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

address :: AddressBook.Address
address = { street: "123 Fake Str", city: "FakeTown", state: "CA" }

entry :: AddressBook.Entry
entry = ({ firstName: "John", lastName: "Smith", address: address })

main :: Effect Unit
main = run [consoleReporter] do
  describe "Address book" do
    describe "showAddress" do
      it "returns a formated address" do
        let expectedString =  "123 Fake Str, FakeTown, CA"
        (AddressBook.showAddress address) `shouldEqual` expectedString

    describe "showEntry" do
      it "returns entry string" do
        let expectedString = "Smith, John: 123 Fake Str, FakeTown, CA"

        (AddressBook.showEntry entry) `shouldEqual` expectedString

    describe "insertEntry" do
      it "adds and entry to the list" do
        let book = AddressBook.emptyBook

        (AddressBook.insertEntry entry book) `shouldEqual` (fromFoldable [entry])

    describe "findEntry" do
      it "returns a matching entry" do
        let book = fromFoldable [entry]

        (AddressBook.findEntry "John" "Smith" book) `shouldEqual` Just entry


      it "return Nothing when it doesn't find an entry" do
        let book = AddressBook.emptyBook
        (AddressBook.findEntry "Foo" "Bar" book) `shouldEqual` Nothing


