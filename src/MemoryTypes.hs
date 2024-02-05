module MemoryTypes where
    import Control.Monad.State
    import Control.Monad.Reader
    import Control.Monad.Except

    import Data.Map as Map

    import Grammar.Abs

    -- & Mapping variable names to location
    type VarName = String
    type Loc = Integer
    type Env = Map.Map VarName Loc

    -- & Value returned by peroforming statement
    type ReturnVal = Maybe MemVal

    -- & Mapping locations to memory values
    type LocMap = Map.Map Loc MemVal

    -- & Memory
    -- ? Function definition
    type FunDef = ([MemVal] -> InterpreterMonad MemVal)

    -- ? Possible values
    data MemVal = IntVal Integer
                | BoolVal Bool
                | StringVal String
                | FunVal FunDef
                | IntRead Integer
                | BoolRead Bool
                | StringRead String

    instance Show MemVal where
        show (IntVal _)     = "Int"
        show (BoolVal _)    = "Bool"
        show (StringVal _)  = "String"
        show (FunVal _)     = "Fun"
        show (IntRead _)    = "read-only Int"
        show (BoolRead _)   = "read-only Bool"
        show (StringRead _) = "read-only String"

    type_to_string :: Type -> String
    type_to_string (Int _)      = "Int"
    type_to_string (Bool _)     = "Bool"
    type_to_string (Str _)   = "String"
    type_to_string (FunT _)     = "Fun"

    -- & Store
    type Store = (LocMap, Loc)

    -- & Exceptions
    type Line = Int
    type Column = Int

    position_to_lncol :: BNFC'Position -> (Line, Column)
    position_to_lncol (Just (ln, col)) = (ln, col)

    type ExpectedType = String
    type ActualType = String

    data Exception =  InvalidType (ExpectedType, ActualType) (Line, Column)
                    | InvalidPrintType (Line, Column)
                    | DivisionByZero (Line, Column)
                    | VariableNotDeclared VarName (Line, Column)
                    | VariableAlreadyDeclared VarName (Line, Column)
                    | FunctionNotDeclared VarName (Line, Column)
                    | WrongTypeReturn VarName (ExpectedType, ActualType) (Line, Column)
                    | InvalidNumberOfArguments (Line, Column)
                    | FunctionDidNotReturn VarName (Line, Column)
                    | NoDefaultForFunctionType VarName (Line, Column)
    
    instance Show Exception where
        show (InvalidType (exp_t, act_t) (ln, col))             = "InvalidType" ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col) ++ ", Expected type: " ++ exp_t ++ ", Actual type: " ++ act_t
        show (InvalidPrintType (ln, col))                       = "InvalidPrintType" ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (DivisionByZero (ln, col))                         = "DivisionByZero" ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (VariableNotDeclared name (ln, col))               = "VariableNotDeclared, variable name: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (VariableAlreadyDeclared name (ln, col))           = "VariableAlreadyDeclared, variable name: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (FunctionNotDeclared name (ln, col))               = "FunctionNotDeclared, function name: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (WrongTypeReturn name (exp_t, act_t) (ln, col))    = "WrongTypeReturn, at function: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col) ++ ", Expected type: " ++ exp_t ++ ", Actual type: " ++ act_t
        show (InvalidNumberOfArguments (ln, col))               = "InvalidNumberOfArguments" ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (FunctionDidNotReturn name (ln, col))              = "FunctionDidNotReturn, at function: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)
        show (NoDefaultForFunctionType name (ln, col))          = "NoDefaultForFunctionType, variable name: " ++ name ++ ", at Line: " ++ (show ln) ++ ", Column: " ++ (show col)

    type InterpreterMonad = ReaderT Env (StateT Store (ExceptT Exception IO))

    -- & Check if two MemVals are the same type
    check_type :: MemVal -> MemVal -> Bool
    check_type (IntVal _)       (IntVal _)      = True
    check_type (BoolVal _)      (BoolVal _)     = True
    check_type (StringVal _)    (StringVal _)   = True
    check_type (FunVal _)       (FunVal _)      = True
    check_type (IntRead _)      (IntRead _)     = True
    check_type (BoolRead _)     (BoolRead _)    = True
    check_type (StringRead _)   (StringRead _)  = True
    check_type _ _ = False

    -- & Check if MemVal is of a given type
    is_of_type :: Type -> MemVal -> Bool
    is_of_type (Int _)      (IntVal _)      = True
    is_of_type (Bool _)     (BoolVal _)     = True
    is_of_type (Str _)      (StringVal _)   = True
    is_of_type (FunT _)     (FunVal _)      = True
    is_of_type _ _ = False

    -- & Casting to readonly
    to_readonly :: MemVal -> MemVal
    to_readonly (IntVal x) = IntRead x
    to_readonly (BoolVal x) = BoolRead x
    to_readonly (StringVal x) = StringRead x
    to_readonly x = x  -- ? For the rest it remains the same
