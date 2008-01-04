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

module DLX where

type Bit = Bool
type BitVec = [Bit]

data DLX_Conf =  DLX BitVec BitVec (BitVec -> BitVec) (BitVec -> BitVec) Bool

-- abbreviations for convinience
t	= True
f	= False
zeros8	= replicate 8 f
zeros32	= replicate 32 f
ones32	= replicate 32 t

-- bit vector to natural numbers convertion and vice versa
bv2nat []     = 0
bv2nat (a:as) = 2^(length as) * (if a then 1 else 0) + bv2nat as

nat2bv i = nat2bv' i 31
	where
		nat2bv' 0 i = replicate (i + 1) f
		nat2bv' n i = let z = n - 2^i in let x = z>=0 in x : nat2bv' (if x then z else n ) (i - 1)

-- convertion between two's complement numbers
bv2int []     = 0
bv2int (a:as) = bv2nat as - 2^(length as) * (if a then 1 else 0)

int2bv i = if i > 0 then nat2bv i else bvneg $ nat2bv $ 0 - (i + 1)

-- bitvector operations
-- take elements from a to b
slice (a,b) v = let l = length v - 1 in foldr (\ x y -> v !! x : y) [] [(l - a)..(l - b)]
-- take ith element
ith i v = v !! (length v - i - 1)

-- hardware:
-- logical gates
bxor a b = a /= b
band a b = a && b
bor a b = a || b

-- full adder
fa a b c = (bor (band a b) (band c (bxor a b)),bxor a (bxor b c))

-- carry chain adder
add [a] [b] c_in    = let (c_out,s) = fa a b c_in in [s]
add a b c_in        = let (c_out,s) = fa (last a) (last b) c_in in
                           add (take (length a - 1) a) (take (length b - 1) b) c_out ++ [s]

-- incrementer
inc v = add zeros32 v t

-- bit vector negation
bvneg	[]	=	[]
bvneg	(x:xs)	=	not x : bvneg xs

-- bitwise and
bvand	[]	_	=	[]
bvand	(x:xs) (y:ys)	=	band x y : bvand xs ys

-- bitwise or
bvor	[]	_	=	[]
bvor	(x:xs) (y:ys)	=	bor x y : bvor xs ys

-- bitwise xor
bvxor	[]	_	=	[]
bvxor	(x:xs) (y:ys)	=	bxor x y : bvxor xs ys

-- greater-than circuit
bvgt []	    _			= f
bvgt (a:as) (b:bs)
	|	a==b		=	bvgt as bs
	|	otherwise	=	a>b

-- arithmetical logical unit implementation
alu_unit a b [f_3,f_2,f_1,f_0]
	=	if f_3 == f then
			case [f_2,f_1,f_0] of
				[False,False,False]	->	add a b f
				[False,False,True]	->	add a b f
				[False,True,False]	->	add a (bvneg b) t
				[False,True,True]	->	add a (bvneg b) t
				[True,False,False] 	->	bvand a b
				[True,False,True] 	->	bvor a b
				[True,True,False] 	->	bvxor a b
				_       			->	slice (15,0) b ++ replicate 16 f
		else	if f_2 && f_1 && f_0 then replicate 32 t else
			replicate 31 f ++
			[case [f_2,f_1,f_0] of
                                [False,False,False] 	-> 	f
				[False,False,True] 	-> 	bvgt a b
				[False,True,False]	->	not (bvgt a b) && not (bvgt b a)
				[False,True,True]	->	bvgt a b || (not (bvgt a b) && not (bvgt b a))
				[True,False,False]	->	bvgt b a
				[True,False,True]	->	bvgt a b || bvgt b a
				_                 	->	bvgt b a || (not (bvgt a b) && not (bvgt b a))]

-- shifter unit implementation
sh_unit	a b [False,False]	=	let n = bv2int b in drop n a ++ replicate n f
sh_unit	a b [False,True]	=	zeros32
sh_unit	a b [True,False]	=	let n = bv2int b in replicate n f ++ take (length a - n) a
sh_unit	a b [True,True]		=	let n = bv2int b in replicate n (a!!0) ++ take (length a - n) a

-- initial GPR and memory
initGPR	= (\x -> zeros32)
initM	= (\x -> zeros8)

-- update functions for GPR and memory
updateGPR gpr [False,False,False,False,False]   _   =  gpr
updateGPR gpr addr          din = \x -> if x==addr then din else gpr x
updateM   m   addr          din = \x -> if x==addr then din else m x

-- memory read abbreviations for a byte, half word or word
memory_read_word m pc	= m (inc (inc (inc pc))) ++ m (inc (inc pc)) ++ m (inc pc) ++ m pc
memory_read_hword m pc  = m (inc pc) ++ m pc
memory_read_byte m pc   = m pc

-- DLX delta function
delta (DLX pc dpc gpr m dpc_enabled) =
	let
        i	    =	memory_read_word m (if dpc_enabled then dpc else pc)
        opc	    =	slice (31,26) i
        rtype  	=	all (==f) opc
        jtype	=	slice (5,1) opc == [f,f,f,f,t]
        itype	=	not (jtype || rtype)
        
        rs1	    =	slice (25,21) i
        rs2	    =	slice (20,16) i
        rd	    =	slice (if itype then (20,16) else (15,11)) i
        
        sa	    =	slice (10,6) i
        fu	    =	slice (5,0) i
        imm	    =	slice (if itype then (15,0) else (25,0)) i
        sxtimm	=	(if itype then replicate 16 (ith 15 i) else replicate 6 (ith 25 i)) ++ imm
        co	    =	if rtype then replicate 27 f ++ sa else sxtimm
        
        shifti	=	rtype && (all (==f) fu || slice (5,1) fu == [f,f,f,f,t])
        alui	=	slice (5,3) opc == [f,t,t] || slice (5,2) opc == [f,f,t,t] || slice (5,2) opc == [f,f,t,f] && ith 0 opc 
        alu     =  	rtype && (slice (5,3) fu == [t,f,t] || slice (5,2) fu == [t,f,f,t] || slice (5,2) fu == [t,f,f,f] && ith 0 fu)
        shift   =  	rtype && (fu == [f,f,f,t,f,f] || slice (5,1) fu == [f,f,f,t,t])
        load    =  	slice (5,1) opc == [t,f,f,f,f] || opc == [t,f,f,f,t,t] || slice (5,1) opc == [t,f,f,t,f]
        store   =  	slice (5,1) opc == [t,f,t,f,f] || opc == [t,f,t,f,t,t]
        jump    =  	slice (5,1) opc == [f,f,f,f,t] || slice (5,1) opc == [f,t,f,t,t]
        branch  =  	slice (5,1) opc == [f,f,f,t,f]
        btaken  =  	bxor (ith 0 opc) $ all (==f) (gpr rs1)
        bjtaken =  	jump || branch && btaken
        jumpr   =  	ith 30 i
    	link	=	elem opc [[f,f,f,f,t,t],[f,t,f,t,t,t]]

        btarget =	if jumpr then gpr rs1 else add pc sxtimm f
        d       =  	add (f : slice (27,26) i) [f,f,f] t
        u       =  	ith 28 i
        aluf    =  	if itype then ith 4 opc : slice (2,0) opc else slice (3,0) fu
        shf     =  	slice (1,0) fu

        lop     =	gpr rs1
        rop     =	if rtype && not shifti then gpr rs2 else co

        ea      =  	add (gpr rs1) sxtimm f
        loadres'=   (if elem opc [[t,f,f,f,f,f],[t,f,f,t,f,f]] then memory_read_byte else 
                     if elem opc [[t,f,f,f,f,t],[t,f,f,t,f,t]] then memory_read_hword else memory_read_word) m ea
    	fill	=	not u && head loadres'
    	loadres	=	replicate (32-8*(bv2int d)) fill ++ loadres'
        

        alures  =   	alu_unit lop rop aluf
        shres   =   	sh_unit lop rop shf

        nextPC  =	if bjtaken then btarget else add pc (inc (inc (inc (inc zeros32)))) f
        nextDPC =   pc
        nextGPR = 	if link then updateGPR gpr [t,t,t,t,t] (inc (inc (inc (inc pc)))) else
		        	if alu || alui then updateGPR gpr rd alures else
        			if shift || shifti then updateGPR gpr rd shres else 
        			if load then updateGPR gpr rd loadres else gpr
        nextM   = 	if not store then m else
		        	if slice (2,0) opc == [f,f,f] then updateM m ea (slice (7,0) (gpr rd)) else
        			if slice (2,0) opc == [f,f,t] then updateM (updateM m ea (slice (7,0) (gpr rd))) (inc ea) (slice (15,8) (gpr rd)) else
        			updateM (updateM (updateM (updateM m ea (slice (7,0) (gpr rd))) (inc ea) (slice (15,8) (gpr rd))) 
		    		(inc (inc ea)) (slice (23,16) (gpr rd)))
			    	(inc (inc (inc ea))) (slice (31,24) (gpr rd))
	in
	(DLX nextPC nextDPC nextGPR nextM dpc_enabled)

-- Instruction Parsing

-- Instruction data type
--data Instruction =	I String GPReg GPReg Int | R GPReg GPReg GPReg Int String | J String Int
data Instruction =	I String String String Int | R String String String Int String | J String Int | Illegal

instance Show Instruction where
    show (I "lhgi" s2 s3 i)   =  "lhgi " ++ s3 ++ " " ++ show i
    show (I "jr" s2 s3 i)   =  "jr " ++ s2
    show (I "jalr" s2 s3 i)   =  "jalr " ++ s2
    show (I "clri" s2 s3 i)   =  "clri " ++ s3
    show (I "seti" s2 s3 i)   =  "seti " ++ s3
    show (I "beqz" s2 s3 i)   =  "beqz " ++ s2 ++ " " ++ show i 
    show (I "bnez" s2 s3 i)   =  "bnez " ++ s2 ++ " " ++ show i 
    show (I "addi" "r0" "r0" 0) = "nop"
    show (I s1 s2 s3 i)   =  s1 ++ " " ++ s2 ++ " " ++ s3 ++ " " ++ show i
    show (R s1 s2 s3 i "slli")   =   "slli " ++ s1 ++ " " ++ s3 ++ " "++ show i
    show (R s1 s2 s3 i "srli")   =   "srli " ++ s1 ++ " " ++ s3 ++ " "++ show i
    show (R s1 s2 s3 i "srai")   =   "srai " ++ s1 ++ " " ++ s3 ++ " "++ show i
    show (R s1 s2 s3 i "lhg")   =   "lhg " ++ s2 ++ " " ++ s3
    show (R s1 s2 s3 i "clr")   =   "clr " ++ s3
    show (R s1 s2 s3 i "set")   =   "set " ++ s3
    show (R s1 s2 s3 i s4)   =   s4 ++ " " ++ s1 ++ " " ++ s2 ++ " " ++ s3
    show (J s i)   =   s ++ " " ++ show i
    show Illegal = "Illegal"

-- instruction name to opcode converter
id2opc 1	"j"	    =	[f,f,f,f,t,f]
id2opc 1	"jal"	=	[f,f,f,f,t,t]
id2opc 1	"beqz"	=	[f,f,f,t,f,f]
id2opc 1	"bnez"	=	[f,f,f,t,f,t]
id2opc 1	"addi"	=	[f,f,t,f,f,t]
id2opc 1	"subi"	=	[f,f,t,f,t,t]
id2opc 1	"andi"	=	[f,f,t,t,f,f]
id2opc 1	"ori"	=	[f,f,t,t,f,t]
id2opc 1	"xori"	=	[f,f,t,t,t,f]
id2opc 1	"lhgi"	=	[f,f,t,t,t,t]
id2opc 1	"jr"	=	[f,t,f,t,t,f]
id2opc 1	"jalr"	=	[f,t,f,t,t,t]
id2opc 1	"clri"	=	[f,t,t,f,f,f]
id2opc 1	"sgri"	=	[f,t,t,f,f,t]
id2opc 1	"seqi"	=	[f,t,t,f,t,f]
id2opc 1	"sgei"	=	[f,t,t,f,t,t]
id2opc 1	"slsi"	=	[f,t,t,t,f,f]
id2opc 1	"snei"	=	[f,t,t,t,f,t]
id2opc 1	"slei"	=	[f,t,t,t,t,f]
id2opc 1	"seti"	=	[f,t,t,t,t,t]
id2opc 1	"lb"	=	[t,f,f,f,f,f]
id2opc 1	"lh"	=	[t,f,f,f,f,t]
id2opc 1	"lw"	=	[t,f,f,f,t,t]
id2opc 1	"lbu"	=	[t,f,f,t,f,f]
id2opc 1	"lhu"	=	[t,f,f,t,f,t]
id2opc 1	"sb"	=	[t,f,t,f,f,f]
id2opc 1	"sh"	=	[t,f,t,f,f,t]
id2opc 1	"sw"	=	[t,f,t,f,t,t]
id2opc 0 	"slli"	=	[f,f,f,f,f,f]
id2opc 0 	"srli"	=	[f,f,f,f,t,f]
id2opc 0 	"srai"	=	[f,f,f,f,t,t]
id2opc 0 	"sll"	=	[f,f,f,t,f,f]
id2opc 0 	"srl"	=	[f,f,f,t,t,f]
id2opc 0 	"sra"	=	[f,f,f,t,t,t]
id2opc 0 	"add"	=	[t,f,f,f,f,t]
id2opc 0 	"sub"	=	[t,f,f,f,t,t]
id2opc 0 	"and"	=	[t,f,f,t,f,f]
id2opc 0 	"or"	=	[t,f,f,t,f,t]
id2opc 0 	"xor"	=	[t,f,f,t,t,f]
id2opc 0 	"lhg"	=	[t,f,f,t,t,t]
id2opc 0 	"clr"	=	[t,f,t,f,f,f]
id2opc 0 	"sgr"	=	[t,f,t,f,f,t]
id2opc 0 	"seq"	=	[t,f,t,f,t,f]
id2opc 0 	"sge"	=	[t,f,t,f,t,t]
id2opc 0 	"sls"	=	[t,f,t,t,f,f]
id2opc 0 	"sne"	=	[t,f,t,t,f,t]
id2opc 0 	"sle"	=	[t,f,t,t,t,f]
id2opc 0 	"set"	=	[t,f,t,t,t,t]

-- register address to bitvector converter
reg2bv (r:rs)	=	drop 27 (int2bv (read rs :: Int))

-- whole instruction to bitvector converter
instr2bv	(I id rs1 rd imm)	=	id2opc 1 id ++ reg2bv rs1 ++ reg2bv rd ++ drop 16 (int2bv imm)
instr2bv	(R rs1 rs2 rd sa fu)	=	[f,f,f,f,f,f] ++ reg2bv rs1 ++ reg2bv rs2 ++ reg2bv rd ++ drop 27 (int2bv sa) ++ id2opc 0 fu
instr2bv	(J id imm)		=	id2opc 1 id ++ drop 6 (int2bv imm)

-- filling memory procedure (with a given assembler program)
fillM [] 	m = m
fillM (x:xs)	m = let (addr,instr_raw) = x
			instr 		 = instr2bv instr_raw
			n'	     	 = updateM (
					    updateM (
					     updateM (updateM m addr (slice (7,0) instr))
						     (inc addr) (slice (15,8) instr))
					     (inc (inc addr)) (slice (23,16) instr))
                                            (inc (inc (inc addr))) (slice (31,24) instr)
		     in fillM xs n'
