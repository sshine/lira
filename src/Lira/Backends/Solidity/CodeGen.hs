--- MIT License
--
-- Copyright (c) 2020 eToroX Labs
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Lira.Backends.Solidity.CodeGen where

import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           NeatInterpolation (trimming)
import           System.FilePath.Posix ((</>))

import           Lira.Contract
import           Lira.Contract.Intermediate

translate :: IntermediateContract -> Text
translate IntermediateContract{..} =
  contractTemplate activateCode executeCode
  where
    executeCode :: Text
    executeCode = undefined

    activateCode :: Text
    activateCode = undefined

    transferFrom :: ((Address, PartyIndex), Integer) -> Text
    transferFrom ((address, partyIndex), amount) =

      "    " <> asset <> ".transferFrom("
        <> party <> ", address(this), " <> tshow amount <> ");\n"
      where
        party = tshow partyIndex -- FIXME
        asset = Text.pack address -- FIXME

tshow :: Show a => a -> Text
tshow = Text.pack . show

contractTemplate :: Text -> Text -> Text
contractTemplate activateCode executeCode = [trimming|
pragma solidity ^0.5.0;

import "@openzeppelin/contracts/token/ERC20/IERC20.sol";
// import "@openzeppelin/contracts/math/SafeMath.sol";

contract LiraContract {
    // using SafeMath for uint256;

    uint256 creationTimestamp;
    bool private activated = false;
    mapping(bytes32 => Trade) trades;
    uint256 memoryExpressions;
    event Activated();

    function activate() public {
        $activateCode

        creationTimestamp = now;
        activated = true;
        emit Activated();
    }

    function execute() public {
        $executeCode
    }
}
|]
