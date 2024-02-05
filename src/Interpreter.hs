module Interpreter where
    import qualified Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import Grammar.Abs

    import MemoryTypes
    import Variables

    -- & Returns only environment, but skips the return value
    return_env = do
        env <- ask
        return (env, Nothing)

    -- & Evaluating an expression
    evaluate_expression :: Expr -> InterpreterMonad MemVal
    
    -- ~ Variable value
    evaluate_expression (EVar location ident) =
        get_variable location ident

    -- ~ Literal int
    evaluate_expression (ELitInt location integer) = return $ IntVal integer

    -- ~ Literal boolean
    evaluate_expression (ELitTrue true) = return $ BoolVal True
    evaluate_expression (ELitFalse false) = return $ BoolVal False

    -- ~ Function application
    evaluate_expression (EApp location (Ident ident) expressions) = do
        f <- get_variable location (Ident ident)
        case f of
            FunVal function -> do
                args <- mapM evaluate_expression expressions
                return_val <- function args
                return return_val
            _ -> throwError $ FunctionNotDeclared ident $ position_to_lncol location

    -- ~ String expression
    evaluate_expression (EString location string) = return $ StringVal string

    -- ~ Negation
    evaluate_expression (Neg location expression) = do
        value <- evaluate_expression expression
        case value of
            (IntVal integer) -> return $ IntVal $ -integer
            other -> throwError $ InvalidType (show (IntVal 0), show other) $ position_to_lncol location

    -- ~ Not
    evaluate_expression (Not location expression) = do
        value <- evaluate_expression expression
        case value of
            (BoolVal boolean) -> return $ BoolVal $ not boolean
            other -> throwError $ InvalidType (show (BoolVal True), show other) $ position_to_lncol location
    
    -- ~ Multiplication operators
    evaluate_expression (EMul location expression1 operator expression2) = do
        value1 <- evaluate_expression expression1
        value2 <- evaluate_expression expression2
        case (operator, value1, value2) of
            -- ? '*' operator
            (Times location, IntVal integer1, IntVal integer2) ->
                return $ IntVal $ integer1 * integer2
            -- ? '/' operator
            (Div location, IntVal _, IntVal 0) -> throwError $ DivisionByZero $ position_to_lncol location
            (Div location, IntVal integer1, IntVal integer2) ->
                return $ IntVal $ div integer1 integer2
            -- ? '%'
            (Mod location, IntVal _, IntVal 0) -> throwError $ DivisionByZero $ position_to_lncol location
            (Mod location, IntVal integer1, IntVal integer2) ->
                return $ IntVal $ mod integer1 integer2
            -- ? invalid type
            (_, other1, other2) -> throwError $ InvalidType ("Int and Int", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location

    -- ~ Addition operators
    evaluate_expression (EAdd location expression1 operator expression2) = do
        value1 <- evaluate_expression expression1
        value2 <- evaluate_expression expression2
        case (operator, value1, value2) of
            -- ? '+' operator
            (Plus location, IntVal integer1, IntVal integer2) ->
                return $ IntVal $ integer1 + integer2
            (Plus location, StringVal string1, StringVal string2) ->
                return $ StringVal $ string1 ++ string2
            (Plus location, StringVal string1, BoolVal bool2) ->
                return $ StringVal $ string1 ++ (show bool2)
            (Plus location, StringVal string1, IntVal integer2) ->
                return $ StringVal $ string1 ++ (show integer2)
            (Plus location, BoolVal bool1, StringVal string2) ->
                return $ StringVal $ (show bool1) ++ string2
            (Plus location, IntVal integer1, StringVal string2) ->
                return $ StringVal $ (show integer1) ++ string2
            -- ? '-' operator
            (Minus location, IntVal integer1, IntVal integer2) ->
                return $ IntVal $ integer1 - integer2
            -- ? invalid type
            (Plus location, other1, other2) -> throwError $ InvalidType ("Valid types for + operation", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location
            (Minus location, other1, other2) -> throwError $ InvalidType ("Int and Int", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location

    -- ~ Relation operators
    evaluate_expression (ERel location expression1 operator expression2) = do
        value1 <- evaluate_expression expression1
        value2 <- evaluate_expression expression2
        case (operator, value1, value2) of
            -- ! for Int type
            -- ? '<' operator
            (LTH location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 < integer2
            -- ? '<=' operator
            (LE location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 <= integer2
            -- ? '>' operator
            (GTH location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 > integer2
            -- ? '>=' operator
            (GE location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 >= integer2
            -- ? '==' operator
            (EQU location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 == integer2
            -- ? '!=' operator
            (NE location, IntVal integer1, IntVal integer2) ->
                return $ BoolVal $ integer1 /= integer2
            
            -- ! for read-only Int type
            -- ? '<' operator
            (LTH location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 < integer2
            -- ? '<=' operator
            (LE location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 <= integer2
            -- ? '>' operator
            (GTH location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 > integer2
            -- ? '>=' operator
            (GE location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 >= integer2
            -- ? '==' operator
            (EQU location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 == integer2
            -- ? '!=' operator
            (NE location, IntRead integer1, IntRead integer2) ->
                return $ BoolVal $ integer1 /= integer2

            -- ! for String type
            -- ? '==' operator
            (EQU location, StringVal string1, StringVal string2) ->
                return $ BoolVal $ string1 == string2
            -- ? '!=' operator
            (NE location, StringVal string1, StringVal string2) ->
                return $ BoolVal $ string1 /= string2

            -- ! for String read-only type
            -- ? '==' operator
            (EQU location, StringRead string1, StringRead string2) ->
                return $ BoolVal $ string1 == string2
            -- ? '!=' operator
            (NE location, StringRead string1, StringRead string2) ->
                return $ BoolVal $ string1 /= string2

            -- ! for Bool type
            -- ? '==' operator
            (EQU location, BoolVal bool1, BoolVal bool2) ->
                return $ BoolVal $ bool1 == bool2
            -- ? '!=' operator
            (NE location, BoolVal bool1, BoolVal bool2) ->
                return $ BoolVal $ bool1 /= bool2

            -- ! for Bool read-only type
            -- ? '==' operator
            (EQU location, BoolRead bool1, BoolRead bool2) ->
                return $ BoolVal $ bool1 == bool2
            -- ? '!=' operator
            (NE location, BoolRead bool1, BoolRead bool2) ->
                return $ BoolVal $ bool1 /= bool2
            
            -- ? invalid type
            (_, other1, other2) -> throwError $ InvalidType ("Comparable types", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location

    -- ~ Logical expression
    -- ? And
    evaluate_expression (EAnd location expression1 expression2) = do
        value1 <- evaluate_expression expression1
        value2 <- evaluate_expression expression2
        case (value1, value2) of
            (BoolVal boolean1, BoolVal boolean2) ->
                return $ BoolVal $ boolean1 && boolean2
            (other1, other2) -> throwError $ InvalidType ("Bool and Bool", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location
    -- ? Or
    evaluate_expression (EOr location expression1 expression2) = do
        value1 <- evaluate_expression expression1
        value2 <- evaluate_expression expression2
        case (value1, value2) of
            (BoolVal boolean1, BoolVal boolean2) ->
                return $ BoolVal $ boolean1 || boolean2
            (other1, other2) -> throwError $ InvalidType ("Bool and Bool", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location

    -- ~ Read only assignment
    -- ! User cannot access this functionality (for loop purpose)
    evaluate_expression (EToRead location expression) = do
        value <- evaluate_expression expression
        return $ to_readonly $ value

    -- & Utility for storing a function
    declare_arguments :: BNFC'Position -> Env -> [Argument] -> [MemVal] -> InterpreterMonad Env
    declare_arguments _ env [] [] = return env
    declare_arguments location env ((Arg arg_location ident):t_arguments) (mem_val:t_argument_inits) = do
        updated_env <- local (const env) $ store_initialized location ident mem_val
        final_env <- declare_arguments location updated_env t_arguments t_argument_inits
        return final_env
    declare_arguments location _ _ _ = throwError $ InvalidNumberOfArguments $ position_to_lncol location

    create_function :: BNFC'Position -> Env -> Ident -> Type -> [Argument] -> [Stmt] -> InterpreterMonad ([MemVal] -> InterpreterMonad (MemVal))
    create_function location fun_env (Ident ident) fun_type arguments statements = do
        let f argument_inits = do
            updated_env <- declare_arguments location fun_env arguments argument_inits
            (_, return_val) <- local (const updated_env) $ interpret_statements statements
            case return_val of
                Nothing -> throwError $ FunctionDidNotReturn ident $ position_to_lncol location
                (Just value) -> do
                    if is_of_type fun_type value then
                        return value
                    else
                        throwError $ WrongTypeReturn ident (type_to_string fun_type, show value) $ position_to_lncol location
        return f

    -- & Executing a statement
    execute_statement :: Stmt -> InterpreterMonad (Env, ReturnVal)
    
    -- ~ Uninitialized variable declaration
    execute_statement (Decl decl_location var_type (NoInit ident_location ident)) = do
        updated_env <- store_uninitialized decl_location ident var_type
        return (updated_env, Nothing)

    -- ~ Initialized variable declaration
    execute_statement (Decl decl_location var_type (Init ident_location ident expression)) = do
        value <- evaluate_expression expression
        if is_of_type var_type value then do
            updated_env <- store_initialized decl_location ident value
            return (updated_env, Nothing)
        else
            throwError $ InvalidType (type_to_string var_type, show value) $ position_to_lncol ident_location

    -- ~ Read only variable declaration
    execute_statement (ReadDecl location var_type ident expression) = do
        value <- evaluate_expression expression
        if is_of_type var_type value then do
            updated_env <- store_initialized location ident (to_readonly value)
            return (updated_env, Nothing)
        else
            throwError $ InvalidType (type_to_string var_type, show value) $ position_to_lncol location

    -- ~ Variable assignemnt
    execute_statement (Ass location ident expression) = do
        value <- evaluate_expression expression
        update_variable location ident value
        return_env

    -- ~ Return expression
    execute_statement (Ret location expression) = do
        value <- evaluate_expression expression
        env <- ask
        return (env, Just value)

    -- ~ If statement
    execute_statement (Cond location expression (Blk blk_location statements)) = do
        value <- evaluate_expression expression
        case value of
            (BoolVal True) -> do
                (_, return_val) <- interpret_statements statements
                env <- ask
                return (env, return_val)
            (BoolVal False) -> return_env
            other -> throwError $ InvalidType (show (BoolVal True), show other) $ position_to_lncol location
    
    -- ~ Eif statement
    execute_statement (CondElse location expression (Blk true_location true_statements) (Blk false_location false_statements)) = do
        value <- evaluate_expression expression
        env <- ask
        case value of
            (BoolVal True) -> do
                (_, return_val) <- interpret_statements true_statements
                return (env, return_val)
            (BoolVal False) -> do
                (_, return_val) <- interpret_statements false_statements
                return (env, return_val)
            other -> throwError $ InvalidType (show (BoolVal True), show other) $ position_to_lncol location

    -- ~ While loop
    execute_statement (While location expression (Blk blk_location statements)) = do
        value <- evaluate_expression expression
        env <- ask
        case value of
            (BoolVal True) -> do
                (_, return_val) <- interpret_statements statements
                case return_val of
                    Nothing -> do
                        (_, new_return_val) <- execute_statement (While location expression (Blk blk_location statements))
                        return (env, new_return_val)
                    _ -> return (env, return_val)
                        
            (BoolVal False) -> return_env
            other -> throwError $ InvalidType (show (BoolVal True), show other) $ position_to_lncol location

    -- ~ For loop
    execute_statement (For location ident beg_expression end_expression (Blk blk_location statements)) = do
        beg_value <- evaluate_expression beg_expression
        end_value <- evaluate_expression end_expression
        case (beg_value, end_value) of
            (IntVal beg, IntVal end) -> do
                let declare_ident = ReadDecl location (Int location) ident (ELitInt location beg)
                let while_terminate_expression = (ERel location (EVar location ident) (LTH location) (ELitInt location end))
                let while_increment = [Ass location ident (EToRead location (EAdd location (EVar location ident) (Plus location) (ELitInt location 1)))]
                let while_block = (Blk blk_location (statements ++ while_increment))
                let while_statement = While location while_terminate_expression while_block
                (_, return_val) <- interpret_statements [declare_ident, while_statement]
                env <- ask
                return (env, return_val)
            (other1, other2) -> throwError $ InvalidType ("Int and Int", (show other1) ++ " and " ++ (show other2)) $ position_to_lncol location

    -- ~ Function definition
    execute_statement (FnDef location fun_type ident arguments (Blk blk_location statements)) = do
        (fn_loc, fn_env) <- define_function location ident
        fn <- create_function location fn_env ident fun_type arguments statements
        -- ? Storing the function after creating the declaration
        (store, loc) <- get
        put (Map.insert fn_loc (FunVal fn) store, loc)
        return (fn_env, Nothing)

    -- ~ Print
    execute_statement (Print location []) = do
        liftIO $ putStrLn ""
        return_env
    execute_statement (Print location (h_expression:t)) = do
        value <- evaluate_expression h_expression
        case value of
            (IntVal integer) -> liftIO $ putStr $ (show integer) ++ " "
            (BoolVal boolean) -> liftIO $ putStr $ (show boolean) ++ " "
            (StringVal string) -> liftIO $ putStr $ (string) ++ " "
            _ -> throwError $ InvalidPrintType $ position_to_lncol location
        (env, return_val) <- execute_statement (Print location t)
        return (env, return_val)

    -- & Executing the statements one after another
    interpret_statements :: [Stmt] -> InterpreterMonad (Env, ReturnVal)
    interpret_statements [] = return_env
    interpret_statements (head:tail) = do
        (env, return_val) <- execute_statement head
        case return_val of
            Nothing -> local (const env) $ interpret_statements tail
            _       -> return (env, return_val)

    -- & Executing the program
    interpret :: [Stmt] -> IO(Either Exception ((Env, ReturnVal), Store))
    interpret statements = runExceptT $ runStateT (runReaderT (interpret_statements statements) Map.empty) (Map.empty, 0)