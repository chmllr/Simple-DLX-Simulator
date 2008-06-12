
module Parser where

import DLX

clear_code s = filter (not . null) $ map (\x -> takeWhile (/=';') x) $ lines s

extract_addr s = int2bv (read (filter (/=' ') (takeWhile (/=':') s)) :: Int)

extract_instr True s 
		= let l = words $ drop 1 $ dropWhile (/=' ') $ drop 1 $ dropWhile (/=':') s
                  in
                    case head l of
                        "lb"    ->  I "lb" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "lh"    ->  I "lh" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "lw"    ->  I "lw" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "lbu"   ->  I "lbu" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "lhu"   ->  I "lhu" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "sb"    ->  I "sb" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "sh"    ->  I "sh" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "sw"    ->  I "sw" (l!!2) (l!!1) (read (l!!3) :: Int)

                        "addi"  ->  I "addi" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "subi"  ->  I "subi" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "andi"  ->  I "andi" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "ori"   ->  I "ori" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "xori"  ->  I "xori" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "lhgi"  ->  I "lhgi" "r0" (l!!1) (read (l!!2) :: Int)

                        "clri"  ->  I "clri" "r0" (l!!1) 0
                        "seti"  ->  I "seti" "r0" (l!!1) 0
                        "sgri"  ->  I "sgri" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "seqi"  ->  I "seqi" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "sgei"  ->  I "sgei" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "slsi"  ->  I "slsi" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "snei"  ->  I "snei" (l!!2) (l!!1) (read (l!!3) :: Int)
                        "slei"  ->  I "slei" (l!!2) (l!!1) (read (l!!3) :: Int)
                        
                        "beqz"  ->  I "beqz" (l!!1) "r0" (read (l!!2) :: Int)
                        "bnez"  ->  I "bnez" (l!!1) "r0" (read (l!!2) :: Int)
                        "jr"    ->  I "jr" (l!!1) "r0" 0
                        "jalr"  ->  I "jalr" (l!!1) "r0" 0

                        "nop"   ->  I "addi" "r0" "r0" 0

                        "slli"  ->  R (l!!2) "r0" (l!!1) (read (l!!3) :: Int) "slli" 
                        "srli"  ->  R (l!!2) "r0" (l!!1) (read (l!!3) :: Int) "srli" 
                        "srai"  ->  R (l!!2) "r0" (l!!1) (read (l!!3) :: Int) "srai" 

                        "sll"   ->  R (l!!2) (l!!3) (l!!1) 0 "sll"
                        "srl"   ->  R (l!!2) (l!!3) (l!!1) 0 "srl"
                        "sra"   ->  R (l!!2) (l!!3) (l!!1) 0 "sra"

                        "add"   ->  R (l!!2) (l!!3) (l!!1) 0 "add"
                        "sub"   ->  R (l!!2) (l!!3) (l!!1) 0 "sub"
                        "and"   ->  R (l!!2) (l!!3) (l!!1) 0 "and"
                        "or"    ->  R (l!!2) (l!!3) (l!!1) 0 "or"
                        "xor"   ->  R (l!!2) (l!!3) (l!!1) 0 "xor"
                        "lhg"   ->  R "r0" (l!!2) (l!!1) 0 "lhg"

                        "clr"   ->  R "r0" "r0" (l!!1) 0 "clr"
                        "set"   ->  R "r0" "r0" (l!!1) 0 "set"
                        "sgr"   ->  R (l!!2) (l!!3) (l!!1) 0 "sgr"
                        "seq"   ->  R (l!!2) (l!!3) (l!!1) 0 "seq"
                        "sge"   ->  R (l!!2) (l!!3) (l!!1) 0 "sge"
                        "sls"   ->  R (l!!2) (l!!3) (l!!1) 0 "sls"
                        "sne"   ->  R (l!!2) (l!!3) (l!!1) 0 "sne"
                        "sle"   ->  R (l!!2) (l!!3) (l!!1) 0 "sle"

                        "j"     ->  J "j" (read (l!!1) :: Int)
                        "jal"   ->  J "jal" (read (l!!1) :: Int)
                        _       ->  Illegal

extract_instr False s 
		= let l = words $ drop 1 $ dropWhile (/=' ') $ drop 1 $ dropWhile (/=':') s
                  in
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
                    

opc2id False	[False,False,False,False,True,False] = "j"
opc2id False	[False,False,False,False,True,True] = "jal"
opc2id False	[False,False,False,True,False,False] = "beqz"
opc2id False	[False,False,False,True,False,True] = "bnez"
opc2id False	[False,False,True,False,False,True] = "addi"
opc2id False	[False,False,True,False,True,True] = "subi"
opc2id False	[False,False,True,True,False,False] = "andi"
opc2id False	[False,False,True,True,False,True] = "ori"
opc2id False	[False,False,True,True,True,False] = "xori"
opc2id False	[False,False,True,True,True,True] = "lhgi"
opc2id False	[False,True,False,True,True,False] = "jr"
opc2id False	[False,True,False,True,True,True] = "jalr"
opc2id False	[False,True,True,False,False,False] = "clri"
opc2id False	[False,True,True,False,False,True] = "sgri"
opc2id False	[False,True,True,False,True,False] = "seqi"
opc2id False	[False,True,True,False,True,True] = "sgei"
opc2id False	[False,True,True,True,False,False] = "slsi"
opc2id False	[False,True,True,True,False,True] = "snei"
opc2id False	[False,True,True,True,True,False] = "slei"
opc2id False	[False,True,True,True,True,True] = "seti"
opc2id False	[True,False,False,False,False,False] = "lb"
opc2id False	[True,False,False,False,False,True] = "lh"
opc2id False	[True,False,False,False,True,True] = "lw"
opc2id False	[True,False,False,True,False,False] = "lbu"
opc2id False	[True,False,False,True,False,True] = "lhu"
opc2id False	[True,False,True,False,False,False] = "sb"
opc2id False	[True,False,True,False,False,True] = "sh"
opc2id False	[True,False,True,False,True,True] = "sw"
opc2id True	[False,False,False,False,False,False] = "slli"
opc2id True	[False,False,False,False,True,False] = "srli"
opc2id True	[False,False,False,False,True,True] = "srai"
opc2id True	[False,False,False,True,False,False] = "sll"
opc2id True	[False,False,False,True,True,False] = "srl"
opc2id True	[False,False,False,True,True,True] = "sra"
opc2id True	[True,False,False,False,False,True] = "add"
opc2id True	[True,False,False,False,True,True] = "sub"
opc2id True	[True,False,False,True,False,False] = "and"
opc2id True	[True,False,False,True,False,True] = "or"
opc2id True	[True,False,False,True,True,False] = "xor"
opc2id True	[True,False,False,True,True,True] = "lhg"
opc2id True	[True,False,True,False,False,False] = "clr"
opc2id True	[True,False,True,False,False,True] = "sgr"
opc2id True	[True,False,True,False,True,False] = "seq"
opc2id True	[True,False,True,False,True,True] = "sge"
opc2id True	[True,False,True,True,False,False] = "sls"
opc2id True	[True,False,True,True,False,True] = "sne"
opc2id True	[True,False,True,True,True,False] = "sle"
opc2id True	[True,False,True,True,True,True] = "set"

bv2instr i = let    rtype = all (==f) (take 6 i)
                    opc = opc2id rtype  (if rtype then drop 26 i else take 6 i)
                    rs1 = ("r" ++) $ show $ bv2nat $ slice (25,21) i
                    rs2 = ("r" ++) $ show $ bv2nat $  slice (20,16) i
                    rdI  = ("r" ++) $ show $ bv2nat $ slice (20,16) i
                    rdR = ("r" ++) $ show $ bv2nat $ slice (15,11) i
                    immI = bv2int $ slice (15,0) i
                    immJ = bv2int $ slice (25,0) i
                    sa  = bv2int $ slice (10,6) i
             in
                    case opc of
                        "lb"    ->  I "lb" rs1 rdI immI
                        "lh"    ->  I "lh" rs1 rdI immI
                        "lw"    ->  I "lw" rs1 rdI immI
                        "lbu"   ->  I "lbu" rs1 rdI immI
                        "lhu"   ->  I "lhu" rs1 rdI immI
                        "sb"    ->  I "sb"  rs1 rdI immI
                        "sh"    ->  I "sh"  rs1 rdI immI
                        "sw"    ->  I "sw"  rs1 rdI immI

                        "addi"  ->  I "addi" rs1 rdI immI
                        "subi"  ->  I "subi" rs1 rdI immI
                        "andi"  ->  I "andi" rs1 rdI immI
                        "ori"   ->  I "ori"  rs1 rdI immI
                        "xori"  ->  I "xori" rs1 rdI immI
                        "lhgi"  ->  I "lhgi" "r0" rdI immI

                        "clri"  ->  I "clri" "r0" rdI 0
                        "seti"  ->  I "seti" "r0" rdI 0
                        "sgri"  ->  I "sgri" rs1 rdI immI
                        "seqi"  ->  I "seqi" rs1 rdI immI
                        "sgei"  ->  I "sgei" rs1 rdI immI
                        "slsi"  ->  I "slsi" rs1 rdI immI
                        "snei"  ->  I "snei" rs1 rdI immI
                        "slei"  ->  I "slei" rs1 rdI immI
                        
                        "beqz"  ->  I "beqz" rs1 "r0" immI
                        "bnez"  ->  I "bnez" rs1 "r0"  immI
                        "jr"    ->  I "jr" rs1 "r0" 0
                        "jalr"  ->  I "jalr" rs1 "r0" 0

                        "nop"   ->  I "addi" "r0" "r0" 0

                        "slli"  ->  R rs1 "r0" rdR sa "slli" 
                        "srli"  ->  R rs1 "r0" rdR sa "srli" 
                        "srai"  ->  R rs1 "r0" rdR sa "srai" 

                        "sll"   ->  R rs1 rs2 rdR 0 "sll"
                        "srl"   ->  R rs1 rs2 rdR 0 "srl"
                        "sra"   ->  R rs1 rs2 rdR 0 "sra"

                        "add"   ->  R rs1 rs2 rdR 0 "add"
                        "sub"   ->  R rs1 rs2 rdR 0 "sub"
                        "and"   ->  R rs1 rs2 rdR 0 "and"
                        "or"    ->  R rs1 rs2 rdR 0 "or"
                        "xor"   ->  R rs1 rs2 rdR 0 "xor"
                        "lhg"   ->  R "r0" rs2 rdR 0 "lhg"

                        "clr"   ->  R "r0" "r0" rdR 0 "clr"
                        "set"   ->  R "r0" "r0" rdR 0 "set"
                        "sgr"   ->  R rs1 rs2 rdR 0 "sgr"
                        "seq"   ->  R rs1 rs2 rdR 0 "seq"
                        "sge"   ->  R rs1 rs2 rdR 0 "sge"
                        "sls"   ->  R rs1 rs2 rdR 0 "sls"
                        "sne"   ->  R rs1 rs2 rdR 0 "sne"
                        "sle"   ->  R rs1 rs2 rdR 0 "sle"

                        "j"     ->  J "j" immJ
                        "jal"   ->  J "jal" immJ
                        _       ->  Illegal

input2program ns i = map (\l -> (extract_addr l, extract_instr ns l)) $ clear_code i
