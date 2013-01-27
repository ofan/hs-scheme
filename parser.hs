{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric
import Data.IORef
import Data.Maybe
import System.IO
import System.Console.Readline

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Fraction Rational
             | Character Char
             | String String
             | Bool Bool
             | Space
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                      body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showVal :: LispVal -> String
showVal (String contents) = show contents
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ show t ++ ")"
showVal (Float contents) = show contents
showVal (Character contents) = show contents
showVal (Fraction contents) = show contents
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal Space = ""

instance Show LispVal where show = showVal

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ show func
  show (NumArgs expected found) = "Expected " ++ show expected
                                ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                ++ ", found " ++ show found
  show (Parser parserErr) = "Parse error at " ++ show parserErr
  show (Default msg) = msg

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

literalCharMap :: [(String, Char)]
literalCharMap = [("backspace", '\b')
                , ("newline"  ,  '\n')
                , ("nul"      ,  '\0')
                , ("page"     , '\x0c')
                , ("return"   , '\x0d')
                , ("rubout"   , '\x7f')
                , ("space"    , '\x20')
                , ("tab"      , '\x09')
                , ("NUL"      , '\NUL')
                , ("SOH"      , '\SOH')
                , ("STX"      , '\STX')
                , ("ETX"      , '\ETX')
                , ("EOT"      , '\EOT')
                , ("ENQ"      , '\ENQ')
                , ("ACK"      , '\ACK')
                , ("BEL"      , '\BEL')
                , ("BS"       ,  '\BS')
                , ("HT"       ,  '\HT')
                , ("LF"       ,  '\LF')
                , ("VT"       ,  '\VT')
                , ("FF"       ,  '\FF')
                , ("CR"       ,  '\CR')
                , ("SO"       ,  '\SO')
                , ("SI"       ,  '\SI')
                , ("DLE"      , '\DLE')
                , ("DC1"      , '\DC1')
                , ("DC2"      , '\DC2')
                , ("DC3"      , '\DC3')
                , ("DC4"      , '\DC4')
                , ("NAK"      , '\NAK')
                , ("SYN"      , '\SYN')
                , ("ETB"      , '\ETB')
                , ("CAN"      , '\CAN')
                , ("EM"       ,  '\EM')
                , ("SUB"      , '\SUB')
                , ("ESC"      , '\ESC')
                , ("FS"       ,  '\FS')
                , ("GS"       ,  '\GS')
                , ("RS"       ,  '\RS')
                , ("US"       ,  '\US')
                , ("DEL"      , '\DEL')
                ]

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+))
            , ("-", numericBinop (-))
            , ("*", numericBinop (*))
            , ("/", numericBinop div)
            , ("mod", numericBinop mod)
            , ("<", numBoolBinop (<))
            , (">", numBoolBinop (>))
            , ("<=", numBoolBinop (<=))
            , (">=", numBoolBinop (>=))
            , ("=", numBoolBinop (==))
            , ("car", car)
            , ("cdr", cdr)
            , ("cons", cons)
            , ("eqv?", eqv)
            , ("eq?", eqv)
            , ("equal?", equal)
            , ("quotient", numericBinop quot)
            , ("remainder", numericBinop rem)
            , ("string=?", strBoolBinop (==))
            , ("string<?", strBoolBinop (<))
            , ("string>?", strBoolBinop (<=))
            , ("string>=?", strBoolBinop (>=))
            ]

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc)
              , ("open-input-file", makePort ReadMode)
              , ("open-output-file", makePort WriteMode)
              , ("close-input-port", closePort)
              , ("close-output-port", closePort)
              , ("read", readProc)
              , ("write", writeProc)
              , ("read-contents", readContents)
              , ("read-all", readAll)]

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool)
          -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ head args
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool)
             -> [LispVal]
             -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool)
             -> [LispVal]
             -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool)
              -> [LispVal]
              -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = liftM (Number . foldl1 op) $ mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                         then throwError $ TypeMismatch "number" $ String n
                         else return $ (fst . head) parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList [_] x] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ Bool $ (length arg1 == length arg2) &&
                                  (all eqvPair $ zip arg1 arg2)
                          where eqvPair (x1, x2) = case eqv [x1, x2] of
                                                    Left _ -> False
                                                    Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- Weak-typed equal
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
            do unpacked1 <- unpacker arg1
               unpacked2 <- unpacker arg2
               return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList ::  String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

spaces :: Parser LispVal
spaces = skipMany1 space >> return Space

escapeChar :: Parser Char
escapeChar = oneOf "\"abnrt"

quotedString :: Parser String
quotedString = many $ (char '\\' >> escapeChar) <|> noneOf "\""

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- quotedString
                 _ <- char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

literalToChar :: Parser LispVal
literalToChar = do
  xs <- many1 letter
  if length xs == 1
  then return $ (Character . head) xs
  else case lookup xs literalCharMap of
         Nothing -> unexpected $ "Invalid literal character '" ++ xs ++ "'"
         Just s -> return $ Character s

parseCharacter :: Parser LispVal
parseCharacter = try (string "#\\") >> literalToChar

readBin :: String -> Parser LispVal
readBin = return . Number . foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) (many1 digit)
          <|> (try (string "#d") >>
            liftM (Number . fst . head . readDec) (many1 digit))
          <|> (try (string "#x") >>
            liftM (Number . fst . head . readHex) (many1 hexDigit))
          <|> (try (string "#o") >>
            liftM (Number . fst . head . readOct) (many1 octDigit))
          <|> (try (string "#b") >> many1 (oneOf "01") >>= readBin)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseBool :: Parser LispVal
parseBool = do _ <- char '#'
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> try parseNumber
          <|> try parseCharacter
          <|> try parseBool
          <|> parseQuoted
          <|> do _ <- char '('
                 x <- try parseList <|> parseDottedList
                 _ <- char ')'
                 return x

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

elemVal :: LispVal -> LispVal -> Bool
elemVal e (List ls) = any (\x -> case equal [e, x] of
                                   Right (Bool r) -> r
                                   _ -> False) ls
elemVal e ls = case equal [e, ls] of
                 Right (Bool x) -> x

eval:: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom var) = getVar env var
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", p, conseq, alt]) =
  do result <- eval env p
     case result of
       Bool False -> eval env alt
       Bool _ -> eval env conseq
       _ -> throwError $ TypeMismatch "boolean" result
eval env (List (Atom "cond":clauses)) =
  evalClauses clauses
  where evalClauses (t:ts) = case t of
          (List (List test@(Atom _:_):exprs)) ->
            do res <- eval env $ List test
               case res of
                 Bool True -> eval env $ List exprs
                 Bool _ -> evalClauses ts
                 _ -> throwError $ TypeMismatch "boolean" res
          (List (Atom "else":expr)) ->
            eval env $ List expr
          _ -> throwError $ BadSpecialForm "Unrecongnized cond clause" t
        evalClauses _ = throwError $ Default "undefined"
eval env (List (Atom "case":key:clauses)) = do
  ekey <- eval env key
  matchCase clauses ekey
  where matchCase (c:cs) k = case c of
          (List (Atom "else":expr)) -> eval env $ List expr
          (List (datum:exprs)) -> do
            dat <- eval env datum
            if elemVal k dat
            then eval env $ List exprs
            else matchCase cs k
          _ -> throwError $ BadSpecialForm "Unrecongnized case clause" c
        matchCase [] _ = throwError $ Default "undefined"
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : fparams) : fbody)) =
  makeNormalFunc env fparams fbody >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) =
  makeVarargs varargs env fparams fbody >>= defineVar env var
eval env (List (Atom "lambda" : List fparams : fbody)) =
  makeNormalFunc env fparams fbody
eval env (List (Atom "lambda" : DottedList fparams varargs : fbody)) =
  makeVarargs varargs env fparams fbody
eval env (List (Atom "lambda" : varargs@(Atom _) : fbody)) =
  makeVarargs varargs env [] fbody
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function: args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env (List l) = mapErrorT (\x -> do
                      res <- x
                      case res of
                          Right w -> return $ Right $ List w
                          Left y -> return $ Left y)
                      (mapM (eval env) l)
eval _ badForm = throwError $ BadSpecialForm "Unrecongnized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $
              map (makeFunc IOFunc) ioPrimitives
            ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

makeFunc :: Monad m =>Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal-> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarargs = makeFunc . Just . showVal

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll ::  [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = liftM extractValue $ runErrorT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = liftM (isJust . lookup var) $ readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError$ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . flip writeIORef value)
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
  then setVar envRef var value >> return value
  else liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = readline prompt >>= \x -> case x of
                      Nothing -> putStrLn "\nBye." >> return "quit"
                      Just x -> return x

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $
  (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ p prompt action = do
  result <- prompt
  unless (p result) $ action result >> until_ p prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)]))
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") .  evalAndPrint

main :: IO ()
main = do args <- getArgs
          if null args then runRepl else runOne args
