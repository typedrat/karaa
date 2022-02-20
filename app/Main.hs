module Main where

import Control.Lens

import Karaa.CPU.Registers

main :: IO ()
main = do
    let regs = makeRegisterFile 0 0 0 0 0 0

    print regs

    let regs' = regs & flag Zero .~ True

    print regs'
