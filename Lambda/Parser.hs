{-# OPTIONS_GHC -w #-}
module Lambda.Parser
    ( parse
    ) where

import Control.Lens
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Data.Text.Lazy (Text)
import Data.Scientific (Scientific)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Lambda.Name
import Lambda.Type
import Lambda.Lexer
import Lambda.Syntax
import Lambda.SourceLoc

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (Located Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Located (Decl LExpr)])
	| HappyAbsSyn6 (Located (Decl LExpr))
	| HappyAbsSyn7 ([Located ConsDef])
	| HappyAbsSyn8 (Located ConsDef)
	| HappyAbsSyn9 ([Type])
	| HappyAbsSyn10 (Type)
	| HappyAbsSyn11 (LExpr)
	| HappyAbsSyn13 ([LExpr])
	| HappyAbsSyn14 ([(Located Pattern, LExpr)])
	| HappyAbsSyn15 ((Located Pattern, LExpr))
	| HappyAbsSyn16 (Located Pattern)
	| HappyAbsSyn17 ([Located (Maybe Name)])
	| HappyAbsSyn18 (Located (Maybe Name))
	| HappyAbsSyn19 (Located Name)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Located Token)
	-> HappyState (Located Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Located Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90 :: () => Int -> ({-HappyReduction (Parser) = -}
	   Int 
	-> (Located Token)
	-> HappyState (Located Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)
	-> [HappyState (Located Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Parser) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46 :: () => ({-HappyReduction (Parser) = -}
	   Int 
	-> (Located Token)
	-> HappyState (Located Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)
	-> [HappyState (Located Token) (HappyStk HappyAbsSyn -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (Parser) HappyAbsSyn)

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (24) = happyShift action_6
action_2 (31) = happyShift action_7
action_2 (42) = happyShift action_8
action_2 (6) = happyGoto action_4
action_2 (19) = happyGoto action_5
action_2 _ = happyReduce_1

action_3 (45) = happyAccept
action_3 _ = happyFail

action_4 (29) = happyShift action_13
action_4 _ = happyFail

action_5 (44) = happyShift action_12
action_5 (17) = happyGoto action_11
action_5 _ = happyReduce_41

action_6 (43) = happyShift action_10
action_6 _ = happyFail

action_7 (44) = happyShift action_9
action_7 _ = happyFail

action_8 _ = happyReduce_45

action_9 (32) = happyShift action_20
action_9 _ = happyFail

action_10 (26) = happyShift action_19
action_10 _ = happyFail

action_11 (26) = happyShift action_17
action_11 (31) = happyShift action_7
action_11 (37) = happyShift action_18
action_11 (42) = happyShift action_8
action_11 (18) = happyGoto action_15
action_11 (19) = happyGoto action_16
action_11 _ = happyFail

action_12 (31) = happyShift action_7
action_12 (42) = happyShift action_8
action_12 (19) = happyGoto action_14
action_12 _ = happyFail

action_13 _ = happyReduce_3

action_14 (26) = happyShift action_37
action_14 _ = happyFail

action_15 _ = happyReduce_42

action_16 _ = happyReduce_43

action_17 (20) = happyShift action_26
action_17 (22) = happyShift action_27
action_17 (25) = happyShift action_28
action_17 (31) = happyShift action_29
action_17 (33) = happyShift action_30
action_17 (38) = happyShift action_31
action_17 (39) = happyShift action_32
action_17 (40) = happyShift action_33
action_17 (41) = happyShift action_34
action_17 (42) = happyShift action_35
action_17 (43) = happyShift action_36
action_17 (11) = happyGoto action_24
action_17 (12) = happyGoto action_25
action_17 _ = happyFail

action_18 _ = happyReduce_44

action_19 (43) = happyShift action_23
action_19 (7) = happyGoto action_21
action_19 (8) = happyGoto action_22
action_19 _ = happyFail

action_20 _ = happyReduce_46

action_21 (30) = happyShift action_50
action_21 _ = happyReduce_6

action_22 _ = happyReduce_7

action_23 (9) = happyGoto action_49
action_23 _ = happyReduce_10

action_24 (31) = happyShift action_29
action_24 (33) = happyShift action_30
action_24 (38) = happyShift action_31
action_24 (39) = happyShift action_32
action_24 (40) = happyShift action_33
action_24 (41) = happyShift action_34
action_24 (42) = happyShift action_35
action_24 (43) = happyShift action_36
action_24 (44) = happyShift action_48
action_24 (12) = happyGoto action_47
action_24 _ = happyReduce_4

action_25 _ = happyReduce_21

action_26 (31) = happyShift action_7
action_26 (42) = happyShift action_8
action_26 (19) = happyGoto action_46
action_26 _ = happyFail

action_27 (20) = happyShift action_26
action_27 (22) = happyShift action_27
action_27 (25) = happyShift action_28
action_27 (31) = happyShift action_29
action_27 (33) = happyShift action_30
action_27 (38) = happyShift action_31
action_27 (39) = happyShift action_32
action_27 (40) = happyShift action_33
action_27 (41) = happyShift action_34
action_27 (42) = happyShift action_35
action_27 (43) = happyShift action_36
action_27 (11) = happyGoto action_45
action_27 (12) = happyGoto action_25
action_27 _ = happyFail

action_28 (31) = happyShift action_7
action_28 (37) = happyShift action_18
action_28 (42) = happyShift action_8
action_28 (18) = happyGoto action_44
action_28 (19) = happyGoto action_16
action_28 _ = happyFail

action_29 (20) = happyShift action_26
action_29 (22) = happyShift action_27
action_29 (25) = happyShift action_28
action_29 (31) = happyShift action_29
action_29 (32) = happyShift action_42
action_29 (33) = happyShift action_30
action_29 (38) = happyShift action_31
action_29 (39) = happyShift action_32
action_29 (40) = happyShift action_33
action_29 (41) = happyShift action_34
action_29 (42) = happyShift action_35
action_29 (43) = happyShift action_36
action_29 (44) = happyShift action_43
action_29 (11) = happyGoto action_41
action_29 (12) = happyGoto action_25
action_29 _ = happyFail

action_30 (20) = happyShift action_26
action_30 (22) = happyShift action_27
action_30 (25) = happyShift action_28
action_30 (31) = happyShift action_29
action_30 (33) = happyShift action_30
action_30 (38) = happyShift action_31
action_30 (39) = happyShift action_32
action_30 (40) = happyShift action_33
action_30 (41) = happyShift action_34
action_30 (42) = happyShift action_35
action_30 (43) = happyShift action_36
action_30 (11) = happyGoto action_39
action_30 (12) = happyGoto action_25
action_30 (13) = happyGoto action_40
action_30 _ = happyReduce_32

action_31 _ = happyReduce_26

action_32 _ = happyReduce_27

action_33 _ = happyReduce_28

action_34 _ = happyReduce_29

action_35 _ = happyReduce_30

action_36 _ = happyReduce_31

action_37 (20) = happyShift action_26
action_37 (22) = happyShift action_27
action_37 (25) = happyShift action_28
action_37 (31) = happyShift action_29
action_37 (33) = happyShift action_30
action_37 (38) = happyShift action_31
action_37 (39) = happyShift action_32
action_37 (40) = happyShift action_33
action_37 (41) = happyShift action_34
action_37 (42) = happyShift action_35
action_37 (43) = happyShift action_36
action_37 (11) = happyGoto action_38
action_37 (12) = happyGoto action_25
action_37 _ = happyFail

action_38 (31) = happyShift action_29
action_38 (33) = happyShift action_30
action_38 (38) = happyShift action_31
action_38 (39) = happyShift action_32
action_38 (40) = happyShift action_33
action_38 (41) = happyShift action_34
action_38 (42) = happyShift action_35
action_38 (43) = happyShift action_36
action_38 (44) = happyShift action_48
action_38 (12) = happyGoto action_47
action_38 _ = happyReduce_5

action_39 (31) = happyShift action_29
action_39 (33) = happyShift action_30
action_39 (38) = happyShift action_31
action_39 (39) = happyShift action_32
action_39 (40) = happyShift action_33
action_39 (41) = happyShift action_34
action_39 (42) = happyShift action_35
action_39 (43) = happyShift action_36
action_39 (44) = happyShift action_48
action_39 (12) = happyGoto action_47
action_39 _ = happyReduce_33

action_40 (28) = happyShift action_62
action_40 (34) = happyShift action_63
action_40 _ = happyFail

action_41 (31) = happyShift action_29
action_41 (32) = happyShift action_61
action_41 (33) = happyShift action_30
action_41 (38) = happyShift action_31
action_41 (39) = happyShift action_32
action_41 (40) = happyShift action_33
action_41 (41) = happyShift action_34
action_41 (42) = happyShift action_35
action_41 (43) = happyShift action_36
action_41 (44) = happyShift action_48
action_41 (12) = happyGoto action_47
action_41 _ = happyFail

action_42 _ = happyReduce_25

action_43 (32) = happyShift action_60
action_43 _ = happyFail

action_44 (17) = happyGoto action_59
action_44 _ = happyReduce_41

action_45 (23) = happyShift action_58
action_45 (31) = happyShift action_29
action_45 (33) = happyShift action_30
action_45 (38) = happyShift action_31
action_45 (39) = happyShift action_32
action_45 (40) = happyShift action_33
action_45 (41) = happyShift action_34
action_45 (42) = happyShift action_35
action_45 (43) = happyShift action_36
action_45 (44) = happyShift action_48
action_45 (12) = happyGoto action_47
action_45 _ = happyFail

action_46 (17) = happyGoto action_57
action_46 _ = happyReduce_41

action_47 _ = happyReduce_19

action_48 (20) = happyShift action_26
action_48 (22) = happyShift action_27
action_48 (25) = happyShift action_28
action_48 (31) = happyShift action_29
action_48 (33) = happyShift action_30
action_48 (38) = happyShift action_31
action_48 (39) = happyShift action_32
action_48 (40) = happyShift action_33
action_48 (41) = happyShift action_34
action_48 (42) = happyShift action_35
action_48 (43) = happyShift action_36
action_48 (11) = happyGoto action_56
action_48 (12) = happyGoto action_25
action_48 _ = happyFail

action_49 (31) = happyShift action_53
action_49 (33) = happyShift action_54
action_49 (43) = happyShift action_55
action_49 (10) = happyGoto action_52
action_49 _ = happyReduce_9

action_50 (43) = happyShift action_23
action_50 (8) = happyGoto action_51
action_50 _ = happyFail

action_51 _ = happyReduce_8

action_52 (27) = happyShift action_70
action_52 _ = happyReduce_11

action_53 (31) = happyShift action_53
action_53 (33) = happyShift action_54
action_53 (43) = happyShift action_55
action_53 (10) = happyGoto action_69
action_53 _ = happyFail

action_54 (31) = happyShift action_53
action_54 (33) = happyShift action_54
action_54 (43) = happyShift action_55
action_54 (10) = happyGoto action_68
action_54 _ = happyFail

action_55 _ = happyReduce_12

action_56 (31) = happyShift action_29
action_56 (33) = happyShift action_30
action_56 (38) = happyShift action_31
action_56 (39) = happyShift action_32
action_56 (40) = happyShift action_33
action_56 (41) = happyShift action_34
action_56 (42) = happyShift action_35
action_56 (43) = happyShift action_36
action_56 (12) = happyGoto action_47
action_56 _ = happyReduce_20

action_57 (26) = happyShift action_67
action_57 (31) = happyShift action_7
action_57 (37) = happyShift action_18
action_57 (42) = happyShift action_8
action_57 (18) = happyGoto action_15
action_57 (19) = happyGoto action_16
action_57 _ = happyFail

action_58 (35) = happyShift action_66
action_58 _ = happyFail

action_59 (27) = happyShift action_65
action_59 (31) = happyShift action_7
action_59 (37) = happyShift action_18
action_59 (42) = happyShift action_8
action_59 (18) = happyGoto action_15
action_59 (19) = happyGoto action_16
action_59 _ = happyFail

action_60 _ = happyReduce_22

action_61 _ = happyReduce_23

action_62 (20) = happyShift action_26
action_62 (22) = happyShift action_27
action_62 (25) = happyShift action_28
action_62 (31) = happyShift action_29
action_62 (33) = happyShift action_30
action_62 (38) = happyShift action_31
action_62 (39) = happyShift action_32
action_62 (40) = happyShift action_33
action_62 (41) = happyShift action_34
action_62 (42) = happyShift action_35
action_62 (43) = happyShift action_36
action_62 (11) = happyGoto action_64
action_62 (12) = happyGoto action_25
action_62 _ = happyFail

action_63 _ = happyReduce_24

action_64 (31) = happyShift action_29
action_64 (33) = happyShift action_30
action_64 (38) = happyShift action_31
action_64 (39) = happyShift action_32
action_64 (40) = happyShift action_33
action_64 (41) = happyShift action_34
action_64 (42) = happyShift action_35
action_64 (43) = happyShift action_36
action_64 (44) = happyShift action_48
action_64 (12) = happyGoto action_47
action_64 _ = happyReduce_34

action_65 (20) = happyShift action_26
action_65 (22) = happyShift action_27
action_65 (25) = happyShift action_28
action_65 (31) = happyShift action_29
action_65 (33) = happyShift action_30
action_65 (38) = happyShift action_31
action_65 (39) = happyShift action_32
action_65 (40) = happyShift action_33
action_65 (41) = happyShift action_34
action_65 (42) = happyShift action_35
action_65 (43) = happyShift action_36
action_65 (11) = happyGoto action_81
action_65 (12) = happyGoto action_25
action_65 _ = happyFail

action_66 (31) = happyShift action_7
action_66 (37) = happyShift action_18
action_66 (42) = happyShift action_8
action_66 (43) = happyShift action_79
action_66 (44) = happyShift action_80
action_66 (14) = happyGoto action_75
action_66 (15) = happyGoto action_76
action_66 (16) = happyGoto action_77
action_66 (18) = happyGoto action_78
action_66 (19) = happyGoto action_16
action_66 _ = happyFail

action_67 (20) = happyShift action_26
action_67 (22) = happyShift action_27
action_67 (25) = happyShift action_28
action_67 (31) = happyShift action_29
action_67 (33) = happyShift action_30
action_67 (38) = happyShift action_31
action_67 (39) = happyShift action_32
action_67 (40) = happyShift action_33
action_67 (41) = happyShift action_34
action_67 (42) = happyShift action_35
action_67 (43) = happyShift action_36
action_67 (11) = happyGoto action_74
action_67 (12) = happyGoto action_25
action_67 _ = happyFail

action_68 (27) = happyShift action_70
action_68 (34) = happyShift action_73
action_68 _ = happyFail

action_69 (27) = happyShift action_70
action_69 (32) = happyShift action_72
action_69 _ = happyFail

action_70 (31) = happyShift action_53
action_70 (33) = happyShift action_54
action_70 (43) = happyShift action_55
action_70 (10) = happyGoto action_71
action_70 _ = happyFail

action_71 (27) = happyShift action_70
action_71 _ = happyReduce_13

action_72 _ = happyReduce_15

action_73 _ = happyReduce_14

action_74 (21) = happyShift action_87
action_74 (31) = happyShift action_29
action_74 (33) = happyShift action_30
action_74 (38) = happyShift action_31
action_74 (39) = happyShift action_32
action_74 (40) = happyShift action_33
action_74 (41) = happyShift action_34
action_74 (42) = happyShift action_35
action_74 (43) = happyShift action_36
action_74 (44) = happyShift action_48
action_74 (12) = happyGoto action_47
action_74 _ = happyFail

action_75 (29) = happyShift action_85
action_75 (36) = happyShift action_86
action_75 _ = happyFail

action_76 _ = happyReduce_35

action_77 (27) = happyShift action_84
action_77 _ = happyFail

action_78 _ = happyReduce_38

action_79 (17) = happyGoto action_83
action_79 _ = happyReduce_41

action_80 (17) = happyGoto action_82
action_80 _ = happyReduce_41

action_81 (31) = happyShift action_29
action_81 (33) = happyShift action_30
action_81 (38) = happyShift action_31
action_81 (39) = happyShift action_32
action_81 (40) = happyShift action_33
action_81 (41) = happyShift action_34
action_81 (42) = happyShift action_35
action_81 (43) = happyShift action_36
action_81 (44) = happyShift action_48
action_81 (12) = happyGoto action_47
action_81 _ = happyReduce_16

action_82 (31) = happyShift action_7
action_82 (37) = happyShift action_18
action_82 (42) = happyShift action_8
action_82 (18) = happyGoto action_15
action_82 (19) = happyGoto action_16
action_82 _ = happyReduce_40

action_83 (31) = happyShift action_7
action_83 (37) = happyShift action_18
action_83 (42) = happyShift action_8
action_83 (18) = happyGoto action_15
action_83 (19) = happyGoto action_16
action_83 _ = happyReduce_39

action_84 (20) = happyShift action_26
action_84 (22) = happyShift action_27
action_84 (25) = happyShift action_28
action_84 (31) = happyShift action_29
action_84 (33) = happyShift action_30
action_84 (38) = happyShift action_31
action_84 (39) = happyShift action_32
action_84 (40) = happyShift action_33
action_84 (41) = happyShift action_34
action_84 (42) = happyShift action_35
action_84 (43) = happyShift action_36
action_84 (11) = happyGoto action_90
action_84 (12) = happyGoto action_25
action_84 _ = happyFail

action_85 (31) = happyShift action_7
action_85 (37) = happyShift action_18
action_85 (42) = happyShift action_8
action_85 (43) = happyShift action_79
action_85 (44) = happyShift action_80
action_85 (15) = happyGoto action_89
action_85 (16) = happyGoto action_77
action_85 (18) = happyGoto action_78
action_85 (19) = happyGoto action_16
action_85 _ = happyFail

action_86 _ = happyReduce_18

action_87 (20) = happyShift action_26
action_87 (22) = happyShift action_27
action_87 (25) = happyShift action_28
action_87 (31) = happyShift action_29
action_87 (33) = happyShift action_30
action_87 (38) = happyShift action_31
action_87 (39) = happyShift action_32
action_87 (40) = happyShift action_33
action_87 (41) = happyShift action_34
action_87 (42) = happyShift action_35
action_87 (43) = happyShift action_36
action_87 (11) = happyGoto action_88
action_87 (12) = happyGoto action_25
action_87 _ = happyFail

action_88 (31) = happyShift action_29
action_88 (33) = happyShift action_30
action_88 (38) = happyShift action_31
action_88 (39) = happyShift action_32
action_88 (40) = happyShift action_33
action_88 (41) = happyShift action_34
action_88 (42) = happyShift action_35
action_88 (43) = happyShift action_36
action_88 (44) = happyShift action_48
action_88 (12) = happyGoto action_47
action_88 _ = happyReduce_17

action_89 _ = happyReduce_36

action_90 (31) = happyShift action_29
action_90 (33) = happyShift action_30
action_90 (38) = happyShift action_31
action_90 (39) = happyShift action_32
action_90 (40) = happyShift action_33
action_90 (41) = happyShift action_34
action_90 (42) = happyShift action_35
action_90 (43) = happyShift action_36
action_90 (44) = happyShift action_48
action_90 (12) = happyGoto action_47
action_90 _ = happyReduce_37

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (reverse happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_2) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAssign <$> happy_var_1 <*> gatherLoc (genAbs (reverse happy_var_2) happy_var_4)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn19  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DAssign <$> fmap getName happy_var_2 <*> gatherLoc (genAbs (map (fmap Just) [happy_var_1, happy_var_3]) happy_var_5)
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (DData <$ happy_var_1 <*> fmap getName happy_var_2 <* happy_var_3 <*> gatherLoc (reverse happy_var_4)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (ConsDef <$> fmap getName happy_var_1 <*> pure (reverse happy_var_2)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 ([]
	)

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (TLit . getName . unLoc $ happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (TList happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 11 happyReduction_16
happyReduction_16 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (genAbs (happy_var_2:(reverse happy_var_3)) happy_var_5
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 11 happyReduction_17
happyReduction_17 ((HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (fELet (spanLoc happy_var_1 happy_var_7) (unLoc happy_var_2) (genAbs (reverse happy_var_3) happy_var_5) happy_var_7
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 11 happyReduction_18
happyReduction_18 ((HappyTerminal happy_var_6) `HappyStk`
	(HappyAbsSyn14  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (fECase (spanLoc happy_var_1 happy_var_6) happy_var_2 (reverse happy_var_5)
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  11 happyReduction_19
happyReduction_19 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (fEApp (spanLoc happy_var_1 happy_var_2) happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (operApp happy_var_1 (fmap getName happy_var_2) happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  12 happyReduction_22
happyReduction_22 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn11
		 (fEVar (getLoc happy_var_2) (getName $ unLoc happy_var_2)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  12 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  12 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_3)
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (genList happy_var_1 (reverse happy_var_2) happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  12 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fELit (spanLoc happy_var_1 happy_var_2) LitUnit
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fELit (getLoc happy_var_1) (LitInteger . getInteger . unLoc $ happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  12 happyReduction_27
happyReduction_27 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fELit (getLoc happy_var_1) (LitReal . getReal . unLoc $ happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fELit (getLoc happy_var_1) (LitChar . getCh . unLoc $ happy_var_1)
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  12 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fELit (getLoc happy_var_1) (LitString . getText . unLoc $ happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fEVar (getLoc happy_var_1) (getName $ unLoc happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  12 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn11
		 (fEVar (getLoc happy_var_1) (getName $ unLoc happy_var_1)
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  13 happyReduction_32
happyReduction_32  =  HappyAbsSyn13
		 ([]
	)

happyReduce_33 = happySpecReduce_1  13 happyReduction_33
happyReduction_33 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  14 happyReduction_35
happyReduction_35 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  14 happyReduction_36
happyReduction_36 (HappyAbsSyn15  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  15 happyReduction_37
happyReduction_37 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn15
		 ((happy_var_1, happy_var_3)
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (Wildcard <$> happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  16 happyReduction_39
happyReduction_39 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (Decons <$> fmap getName happy_var_1 <*> gatherLoc (reverse happy_var_2)
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  16 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn16
		 (Decons <$> fmap getName happy_var_1 <*> gatherLoc (reverse happy_var_2)
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  17 happyReduction_41
happyReduction_41  =  HappyAbsSyn17
		 ([]
	)

happyReduce_42 = happySpecReduce_2  17 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_2 : happy_var_1
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  18 happyReduction_43
happyReduction_43 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (fmap Just happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  18 happyReduction_44
happyReduction_44 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (fmap (const Nothing) happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  19 happyReduction_45
happyReduction_45 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn19
		 (fmap getName happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  19 happyReduction_46
happyReduction_46 _
	(HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn19
		 (fmap getName happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyNewToken action sts stk
	= lexerWrapped(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	L _ TEOF -> action 45 45 tk (HappyState action) sts stk;
	L _ TLet -> cont 20;
	L _ TIn -> cont 21;
	L _ TCase -> cont 22;
	L _ TOf -> cont 23;
	L _ TData -> cont 24;
	L _ TLambda -> cont 25;
	L _ TAssign -> cont 26;
	L _ TArrow -> cont 27;
	L _ TComma -> cont 28;
	L _ TSemi -> cont 29;
	L _ TPipe -> cont 30;
	L _ TOParen -> cont 31;
	L _ TCParen -> cont 32;
	L _ TOBracket -> cont 33;
	L _ TCBracket -> cont 34;
	L _ TOBrace -> cont 35;
	L _ TCBrace -> cont 36;
	L _ TUnderscore -> cont 37;
	L _ (TInteger _) -> cont 38;
	L _ (TReal _) -> cont 39;
	L _ (TChar _) -> cont 40;
	L _ (TString _) -> cont 41;
	L _ (TLowerId _) -> cont 42;
	L _ (TUpperId _) -> cont 43;
	L _ (TSymbol _) -> cont 44;
	_ -> happyError' tk
	})

happyError_ 45 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (>>=)
happyReturn :: () => a -> Parser a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => (Located Token) -> Parser a
happyError' tk = parseError tk

parser = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- | Extracts a name from a token.
getName :: Token -> Name
getName (TLowerId n) = n
getName (TUpperId n) = n
getName (TSymbol n) = n
getName t = error $ "called getName on " ++ show t

-- | Extracts an integer from a token.
getInteger :: Token -> Integer
getInteger (TInteger i) = i
getInteger t = error $ "called getInteger on " ++ show t

-- | Extracts an real from a token.
getReal :: Token -> Scientific
getReal (TReal r) = r
getReal t = error $ "called getReal on " ++ show t

-- | Extracts a character from a token.
getCh :: Token -> Char
getCh (TChar c) = c
getCh t = error $ "called getCh on " ++ show t

-- | Extracts text from a token.
getText :: Token -> Text
getText (TString t) = t
getText t = error $ "called getText on " ++ show t

-- Applies an operator.
operApp :: LExpr -> Located Name -> LExpr -> LExpr
operApp e1 n e2 = app2
    where
        opVar = fEVar (getLoc n) (unLoc n)
        app1 = fEApp (spanLoc e1 n) opVar e1
        app2 = fEApp (spanLoc e1 e2) app1 e2

-- Generates a list by applying operator ':'.
genList :: Located Token -> [LExpr] -> Located Token -> LExpr
genList pre list post = foldr listCons emptyList list
    where
        listCons :: LExpr -> LExpr -> LExpr
        listCons e l = operApp e (L NoRange ":") l
        emptyList :: LExpr
        emptyList = fELit (getLoc post) LitEmptyList

-- Generates abstraction from a list of names.
genAbs :: [Located (Maybe Name)] -> LExpr -> LExpr
genAbs l e = foldr addAbs e l
    where 
        addAbs name expr = fEAbs (spanLoc name expr) (nameFromMb $ unLoc name) expr

-- Returns a reserved name if the maybe is nothing.
nameFromMb :: Maybe Name -> Name
nameFromMb (Just n) = n
nameFromMb Nothing = reservedName

-- | Parser state, contains lexer state.
data ParserState = ParserState
    { _lexerState :: LexerState
    }

-- | Happy does not work with template haskell..
lexerState :: Lens' ParserState LexerState
lexerState f (ParserState lexSt) = ParserState <$> f lexSt

-- | The Parser monad.
type Parser = StateT ParserState (Except String)

-- | Runs the given on a byte string.
runParser :: Parser a -> ByteString -> Either String a
runParser m bs = runExcept (evalStateT m st)
    where st = ParserState (initLexerState bs)

-- | Parses a lambda program.
parse :: ByteString -> Either String [Located (Decl LExpr)]
parse = runParser parser

-- | Calls the lexer.
lexerWrapped :: (Located Token -> Parser a) -> Parser a
lexerWrapped cont = zoom lexerState lexer >>= cont

-- | Reports a parser error.
parseError :: Located Token -> Parser a
parseError (L _ t) = do
    lexSt <- use lexerState
    throwError $ "Parse error before token " ++ show t 
        ++ " on line " ++ (show . line . location $ lexSt)
        ++ ", column " ++ (show . column . location $ lexSt)
        ++ "."

main :: IO ()
main = ByteString.getContents >>= print . parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
