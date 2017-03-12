module IntermediateCompiler where

import IntermediateBahrLanguageDefinition
import BahrLanguageDefinition

scale :: Integer -> TransferCall -> TransferCall
scale factor transferCall = transferCall { _amount = _amount transferCall * factor }

translate :: Integer -> TransferCall -> TransferCall
translate seconds transferCall = transferCall { _delay = _delay transferCall + seconds }

intermediateCompile :: Contract -> IntermediateContract
intermediateCompile = IntermediateContract . getTransferCalls

getTransferCalls :: Contract -> [TransferCall]
getTransferCalls (Transfer sym to from) = [TransferCall 1 0 sym to from]
getTransferCalls (Scale factor contract ) = map (scale factor) (getTransferCalls contract)
getTransferCalls (Both contractA contractB) = getTransferCalls contractA ++ getTransferCalls contractB
getTransferCalls (Translate time contract ) = map (translate time) (getTransferCalls contract)