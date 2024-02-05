module Variables where
    import Grammar.Abs

    import Data.Map as Map

    import Control.Monad.Reader
    import Control.Monad.State
    import Control.Monad.Except

    import MemoryTypes

    -- & Default value for each of the types
    default_type_value :: Type -> MemVal
    default_type_value (Int _) = IntVal 0
    default_type_value (Bool _) = BoolVal False
    default_type_value (Str _) = StringVal ""

    -- & Casting readonly
    cast_readonly :: MemVal -> MemVal
    cast_readonly (IntRead x) = IntVal x
    cast_readonly (BoolRead x) = BoolVal x
    cast_readonly (StringRead x) = StringVal x
    cast_readonly x = x  -- ? For the rest it remains the same

    -- & Storing variables and functions
    store_initialized :: BNFC'Position -> Ident -> MemVal -> InterpreterMonad Env
    store_initialized location (Ident identifier) memory_value = do
        env <- ask
        (store, loc) <- get
        -- ? Check if the variable is already stored
        let maybe_identifier_loc = Map.lookup identifier env
        case maybe_identifier_loc of
            Nothing -> do
                -- ? Storing the variable
                let updated_env = Map.insert identifier loc env
                put (Map.insert loc memory_value store, loc + 1)
                return updated_env
            _ -> throwError $ VariableAlreadyDeclared identifier $ position_to_lncol location

    -- ? If the variable is uninitialized then we store default value
    store_uninitialized :: BNFC'Position -> Ident -> Type -> InterpreterMonad Env
    store_uninitialized location (Ident identifier) var_type =
        case var_type of
            (FunT _) -> throwError $ NoDefaultForFunctionType identifier $ position_to_lncol location
            _ -> store_initialized location (Ident identifier) (default_type_value var_type)

    define_function :: BNFC'Position -> Ident -> InterpreterMonad (Loc, Env)
    define_function location (Ident identifier) = do
        env <- ask
        -- ? Check if the identifier is unused
        let maybe_identifier_loc = Map.lookup identifier env
        case maybe_identifier_loc of
            Nothing -> do
                (_, loc) <- get
                modify (\(s, l) -> (s, l + 1))
                let updated_env = Map.insert identifier loc env
                return (loc, updated_env)
            _ -> throwError $ VariableAlreadyDeclared identifier $ position_to_lncol location

    -- & Updating variables
    update_variable :: BNFC'Position -> Ident -> MemVal -> InterpreterMonad ()
    update_variable location (Ident identifier) memory_value = do
        env <- ask
        (store, loc) <- get
        let maybe_identifier_loc = Map.lookup identifier env
        case maybe_identifier_loc of
            Nothing -> throwError $ VariableNotDeclared identifier $ position_to_lncol location
            Just identifier_loc -> do
                let maybe_identifier_value = Map.lookup identifier_loc store
                case maybe_identifier_value of
                    Nothing -> throwError $ VariableNotDeclared identifier $ position_to_lncol location
                    Just identifier_value ->
                        if check_type identifier_value memory_value then do
                            put (Map.insert identifier_loc memory_value store, loc)
                            return ()
                        else
                            throwError $ InvalidType (show identifier_value, show memory_value) $ position_to_lncol location

    -- & Get variable value
    get_variable :: BNFC'Position -> Ident -> InterpreterMonad MemVal
    get_variable location (Ident identifier) = do
        env <- ask
        (store, loc) <- get
        let maybe_identifier_loc = Map.lookup identifier env
        case maybe_identifier_loc of
            Nothing -> throwError $ VariableNotDeclared identifier $ position_to_lncol location
            Just identifier_loc -> do
                let maybe_identifier_value = Map.lookup identifier_loc store
                case maybe_identifier_value of
                    Nothing -> throwError $ VariableNotDeclared identifier $ position_to_lncol location
                    Just identifier_value -> return $ cast_readonly identifier_value