-- SDS is a simple DLX simulator
-- Copyright (C) 2007 Christian Mueller (cm@cs.uni-sb.de)
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

module Main where

import DLX
import Parser
import IO
import Data.List
import Array


main    =   do
                putStrLn "Welcome to Simple DLX Simulator v0.01!\n"
                sdsShell zeros32 initGPR initM ""

sdsShell pc gpr m filep
            =   do
                putStr "sds>"
                hFlush stdout
                ui <- getLine
                if filter (\x -> not (elem x [' ','\t'])) ui /= [] then 
                    if elem ui ["help","usage"] then do putStrLn "You can use the following commands:\n\nabout\t\tto see the information about the SDS\nexec\t\tto reload and execute the last loaded file step by step\nexit\t\tto exit the simulator\ngpr\t\tto see the content of gpr\nhelp\t\tto see this help message\ninstr\t\tto see the list of all available instructions\nload <path>\tto load your assembler file\nmem <addr>\tto see the content of memory at the given address\npc\t\tto show the current pc\nreset\t\tto reset the pc, GPR and memory\nrun\t\tto execute the program in one run (divergency!)\n"
                                                        sdsShell  pc gpr m filep
                                    else 
                    if ui == "about" then do putStrLn "SDS (Simple DLX Simulator) v0.01 is a simple implementation of the DLX\nspecification as given in [1]. It is only academically interesting and can be\nused to test assembler programs.\n\nAuthor: Christian Mueller\neMail: cm@cs.uni-sb.de\n\n[1] \"Computer Architecture: Complexity and Correctnes\" by Wolfgang J. Paul,\n    Silvia Mueller."
                                             sdsShell zeros32 initGPR initM filep
                                     else 
                    if ui == "instr" then do putStrLn "Syntax for all instructions in an assembler source file:\n\naddr : instr [rs1] [rs2] [rd] [imm/sa]\n\nAvailable instructions:\n\nlb, lbu, lh, lhu, lw, sb, sh, sw, addi, subi, andi, ori, xori, lhgi, clri, sgri,\nseqi, sgei, slsi, snei, slei, seti, beqz, bnez, jr, jalr, slli, srli, srai, sll,\nsrl, sra, add, sub, and, or, xor, lhg, clr, set, sgr, seq, sge, sls, sne, sle,\nj, jal\n\nFor instruction set semantics and further information see \"Computer\nArchitecture: Complexity and Correctnes\" by Wolfgang J. Paul, Silvia Mueller."
                                             sdsShell zeros32 initGPR initM filep
                                     else 
                    if ui == "exit"  then do return ()
                                     else 
                    if ui == "reset"  then do   putStrLn "Reset...done!"
                                                sdsShell zeros32 initGPR initM filep
                                     else 
                    if ui == "gpr"  then do showGPR gpr
                                            sdsShell pc gpr m filep
                                     else 
                    if ui == "pc"   then do putStrLn $ "pc: " ++ show (bv2int pc)
                                            sdsShell pc gpr m filep
                                     else 
                    if isPrefixOf "mem" ui then do  let l = words ui
                                                    putStrLn $ "M(" ++ (l!!1) ++ ") = " ++ show (bv2int (memory_read_word m (int2bv (read (l!!1) ::Int))))
                                                    sdsShell pc gpr m filep
                                     else 
                    if elem ui ["run","exec"] 
                                     then do if null filep then do putStrLn  "Load file first!"
                                                                   sdsShell pc gpr m filep
                                                        else do
                                             filec <- readFile filep
                                             putStr $ "Loading " ++ filep ++ " into the DLX memory..."
                                             let initM' = fillM (input2program filec) initM
                                             putStrLn "done!"
                                             putStrLn  "Execute..."
                                             (npc,nGPR,nM) <- execShell zeros32 initGPR initM' (if ui == "run" then -1 else 0)
                                             putStrLn "done!"
                                             sdsShell npc nGPR nM filep
                                     else 
                    if isPrefixOf "load" ui then do  let l = words ui
                                                     putStrLn $ "Loading " ++ (l!!1) ++ "...\n"
                                                     filec <- readFile (l!!1)
                                                     putStrLn filec
                                                     sdsShell  pc gpr m (l!!1)
                                     else 
                                     do putStrLn "Wrong command!"
                                        sdsShell pc gpr m filep
                            else do sdsShell pc gpr m filep

nat2gpr_addr_helper i = drop 27 $ nat2bv i

execShell pc gpr m p = do 
                            if all (==f) (memory_read_word m pc) then do return (pc,gpr,m)
                                else do
                                if p == 0 then do
                                putStr $ "sds executing [" ++ show (bv2nat pc) ++ ": " ++ show (bv2instr (memory_read_word m pc)) ++ "] >"
                                hFlush stdout
                                ui <- getLine
                                if isPrefixOf "skip" ui then do  let l = words ui
                                                                 execShell pc gpr m (read (l!!1) :: Int)
                                                else do
                                if ui == "gpr"
                                     then do showGPR gpr
                                             execShell pc gpr m p
                                     else do
                                if ui == "pc"   then do putStrLn $ "pc: " ++ show (bv2int pc)
                                                        execShell pc gpr m p
                                                else do
                                if ui == "stop"   then do putStrLn "Exit the execution mode..."
                                                          return (pc,gpr,m)
                                                  else do
                                if isPrefixOf "mem" ui then do  let l = words ui
                                                                putStrLn $ "M(" ++ (l!!1) ++ ") = " ++ show (bv2int (memory_read_word m (int2bv (read (l!!1) ::Int))))
                                                                execShell pc gpr m p
                                                       else do
                                if ui /= ""     then do putStrLn "You are in execution mode! Press ENTER or use the following commands: pc, gpr,\nmem <addr>, skip <n>, stop.\n"
                                                        execShell pc gpr m p
                                                else do
                                putStrLn "-> ok"
                                if not (all (==f) (memory_read_word m pc))
                                    then do  let (DLX npc nGPR nM) = delta (DLX pc gpr m)
                                             execShell npc nGPR nM p
                                    else do return (pc,gpr,m)
                                 else do
                                        let (DLX npc nGPR nM) = delta (DLX pc gpr m)
                                        execShell npc nGPR nM (p - if p<0 then 0 else 1)


showGPR gpr  =   do
                    putStrLn $ "r0:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 0)) ++ 
                                    "\tr1:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 1)) ++ 
                                    "\tr2:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 2)) ++ 
                                    "\tr3:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 3))
                    putStrLn $ "r4:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 4)) ++ 
                                    "\tr5:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 5)) ++ 
                                    "\tr6:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 6)) ++ 
                                    "\tr7:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 7))
                    putStrLn $ "r8:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 8)) ++ 
                                    "\tr9:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 9)) ++ 
                                    "\tr10:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 10)) ++ 
                                    "\tr11:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 11))
                    putStrLn $ "r12:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 12)) ++ 
                                    "\tr13:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 13)) ++ 
                                    "\tr14:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 14)) ++ 
                                    "\tr15:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 15))
                    putStrLn $ "r16:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 16)) ++ 
                                    "\tr17:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 17)) ++ 
                                    "\tr18:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 18)) ++ 
                                    "\tr19:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 19))
                    putStrLn $ "r20:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 20)) ++ 
                                    "\tr21:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 21)) ++ 
                                    "\tr22:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 22)) ++ 
                                    "\tr23:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 23))
                    putStrLn $ "r24:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 24)) ++ 
                                    "\tr25:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 25)) ++ 
                                    "\tr26:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 26)) ++ 
                                    "\tr27:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 27))
                    putStrLn $ "r28:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 28)) ++ 
                                    "\tr29:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 29)) ++ 
                                    "\tr30:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 30)) ++ 
                                    "\tr31:\t" ++ (show . bv2int) (gpr (nat2gpr_addr_helper 31))
