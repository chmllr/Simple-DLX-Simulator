
module Parser where

import DLX

clear_code s = filter (not . null) $ map (\x -> takeWhile (/=';') x) $ lines s

extract_addr s = int2bv (read (filter (/=' ') (takeWhile (/=':') s)) :: Int)

extract_instr' s = words $ drop 1 $ dropWhile (/=' ') $ drop 1 $ dropWhile (/=':') s

extract_instr s = let l = extract_instr' s in
                    case head l of
                        "lb"    ->  I "lb" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "lh"    ->  I "lh" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "lw"    ->  I "lw" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "lbu"   ->  I "lbu" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "lhu"   ->  I "lhu" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "sb"    ->  I "sb" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "sh"    ->  I "sh" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "sw"    ->  I "sw" (l!!1) (l!!2) (read (l!!3) :: Int)

                        "addi"  ->  I "addi" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "subi"  ->  I "subi" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "andi"  ->  I "andi" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "ori"   ->  I "ori" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "xori"  ->  I "xori" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "lhgi"  ->  I "lhgi" "r0" (l!!1) (read (l!!2) :: Int)

                        "clri"  ->  I "clri" "r0" (l!!1) 0
                        "seti"  ->  I "seti" "r0" (l!!1) 0
                        "sgri"  ->  I "sgri" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "seqi"  ->  I "seqi" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "sgei"  ->  I "sgei" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "slsi"  ->  I "slsi" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "snei"  ->  I "snei" (l!!1) (l!!2) (read (l!!3) :: Int)
                        "slei"  ->  I "slei" (l!!1) (l!!2) (read (l!!3) :: Int)
                        
                        "beqz"  ->  I "beqz" (l!!1) "r0" (read (l!!2) :: Int)
                        "bnez"  ->  I "bnez" (l!!1) "r0" (read (l!!2) :: Int)
                        "jr"    ->  I "jr" (l!!1) "r0" 0
                        "jalr"  ->  I "jalr" (l!!1) "r0" 0

                        "nop"   ->  I "addi" "r0" "r0" 0

                        "slli"  ->  R (l!!1) "r0" (l!!2) (read (l!!3) :: Int) "slli" 
                        "srli"  ->  R (l!!1) "r0" (l!!2) (read (l!!3) :: Int) "srli" 
                        "srai"  ->  R (l!!1) "r0" (l!!2) (read (l!!3) :: Int) "srai" 

                        "sll"   ->  R (l!!1) (l!!2) (l!!3) 0 "sll"
                        "srl"   ->  R (l!!1) (l!!2) (l!!3) 0 "srl"
                        "sra"   ->  R (l!!1) (l!!2) (l!!3) 0 "sra"

                        "add"   ->  R (l!!1) (l!!2) (l!!3) 0 "add"
                        "sub"   ->  R (l!!1) (l!!2) (l!!3) 0 "sub"
                        "and"   ->  R (l!!1) (l!!2) (l!!3) 0 "and"
                        "or"    ->  R (l!!1) (l!!2) (l!!3) 0 "or"
                        "xor"   ->  R (l!!1) (l!!2) (l!!3) 0 "xor"
                        "lhg"   ->  R "r0" (l!!1) (l!!2) 0 "lhg"

                        "clr"   ->  R "r0" "r0" (l!!1) 0 "clr"
                        "set"   ->  R "r0" "r0" (l!!1) 0 "set"
                        "sgr"   ->  R (l!!1) (l!!2) (l!!3) 0 "sgr"
                        "seq"   ->  R (l!!1) (l!!2) (l!!3) 0 "seq"
                        "sge"   ->  R (l!!1) (l!!2) (l!!3) 0 "sge"
                        "sls"   ->  R (l!!1) (l!!2) (l!!3) 0 "sls"
                        "sne"   ->  R (l!!1) (l!!2) (l!!3) 0 "sne"
                        "sle"   ->  R (l!!1) (l!!2) (l!!3) 0 "sle"

                        "j"     ->  J "j" (read (l!!1) :: Int)
                        "jal"   ->  J "jal" (read (l!!1) :: Int)
                        _       ->  Illegal
                    

input2program i = map (\l -> (extract_addr l, extract_instr l)) $ clear_code i
input2program' i = map (\l -> (extract_addr l, unwords (extract_instr' l))) $ clear_code i
