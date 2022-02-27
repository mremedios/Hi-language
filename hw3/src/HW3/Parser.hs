module HW3.Parser where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word
import HW3.Base
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between skipSpace eof pFullExpr) ""

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

pChar :: Char -> Parser Char
pChar c = lexeme $ char c

pFunName :: HiFun -> Parser HiFun
pFunName f = f <$ string (functionToString f)

pHiFun :: Parser HiValue
pHiFun = do
  fun <- choice (map (\x -> pFunName x) functions)
  pure $ HiValueFunction fun

pNumeric :: Parser HiValue
pNumeric = lexeme $ do
  val <- L.signed skipSpace L.scientific
  pure $ HiValueNumber $ toRational val

pBoolean :: Parser HiValue
pBoolean = lexeme $ do
  bool <-
    choice
      [ True <$ string "true",
        False <$ string "false"
      ]
  pure $ HiValueBool bool

pNull :: Parser HiValue
pNull = lexeme $ HiValueNull <$ string "null"

pText :: Parser HiValue
pText = lexeme $ do
  text <- char '"' *> manyTill L.charLiteral (char '"')
  pure $ HiValueString $ T.pack text

pList :: Parser HiExpr
pList = lexeme $ do
  array <- between (pChar '[') (pChar ']') pArguments
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) array

-- | Parse 2-digit hexadecimal number
pHex :: Parser Word8
pHex = do
  arr <- count 2 hexDigitChar
  let val = read $ "0x" <> arr
  return $ toEnum val

pByteString :: Parser HiValue
pByteString = lexeme $ do
  array <- between (pChar '[' *> pChar '#') (pChar '#' <* pChar ']') (sepEndBy pHex space1)
  pure $ HiValueBytes $ B.pack array

pEntry :: Parser (HiExpr, HiExpr)
pEntry = lexeme $ do
  key <- pFullExpr <* pChar ':'
  value <- pFullExpr
  pure $ (key, value)

pDict :: Parser HiExpr
pDict = lexeme $ do
  array <- between (pChar '{') (pChar '}') (sepEndBy pEntry (char ',' <* space))
  pure $ HiExprDict array

-- | Parse all HiValues
pHiValue :: Parser HiValue
pHiValue = pNumeric <|> pHiFun <|> pBoolean <|> pNull  <|> pHiAction
  <|> try pByteString <|> pText

-- | Parse HiValue and wrap into HiExpr
pHiExprValue :: Parser HiExpr
pHiExprValue = lexeme $ HiExprValue <$> pHiValue

-- | Parse arguments in parenthesis and wrap into HiExprApply
pHiExprApply
  :: HiExpr         -- ^ function expression
  -> Parser HiExpr
pHiExprApply op = lexeme $ do
  args <- pChar '(' *> pArguments <* pChar ')'
  let applied = HiExprApply op args
  pHiExprApply applied <|> pure applied

-- | Parse maybe nested '!' and wrap given expression into HiExprRun
pE :: HiExpr -> Parser HiExpr
pE expr = lexeme $ do
  _ <- (pChar '!')
  let run = HiExprRun expr
  pE run <|> pure run

-- | Parse maybe nested '.' and wrap given expression into function application
pDot :: HiExpr -> Parser HiExpr
pDot op = lexeme $ do
  _ <- pChar '.'
  arg <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` (char '-')
  let argT = map T.pack arg
      str = T.intercalate (T.singleton '-') argT
      expr = HiExprApply op [HiExprValue $ HiValueString str]
  pDot expr <|> pure expr

-- | Parse expression containing only HiValues or expression enclosed in any brackets
pSimpleExpr :: Parser HiExpr
pSimpleExpr = lexeme $ pParen <|> pDict <|> pHiExprValue <|> pList

-- | Take expression and try to parse '.', '!'  or function application
pExtendedExpr :: HiExpr -> Parser HiExpr
pExtendedExpr expr = lexeme $ do
  next <- pDot expr <|> pE expr <|> pHiExprApply expr
  pExtendedExpr next <|> pure next

-- | Parse the whole expression
pExpr :: Parser HiExpr
pExpr = lexeme $ do
  value <- pSimpleExpr
  pExtendedExpr value <|> pure value

pParen :: Parser HiExpr
pParen = between (pChar '(') (pChar ')') pFullExpr

-- | Parse a list of arguments separated by comma
pArguments :: Parser [HiExpr]
pArguments = sepEndBy pFullExpr (char ',' <* space)

pHiAction :: Parser HiValue
pHiAction =
  HiValueAction
    <$> choice
      [ HiActionCwd <$ string "cwd",
        HiActionNow <$ string "now"
      ]

--- Operators

wrap :: HiFun -> HiExpr -> HiExpr -> HiExpr
wrap fun a b = HiExprApply (HiExprValue $ HiValueFunction fun) [a, b]

pOperator :: [Char] -> HiFun -> Parser (HiExpr -> HiExpr -> HiExpr)
pOperator name f = lexeme $ (wrap f) <$ (string name)

infixLFun, infixNFun, infixRFun :: String -> HiFun -> Operator Parser HiExpr
infixLFun name f = InfixL $ pOperator name f
infixNFun name f = InfixN $ pOperator name f
infixRFun name f = InfixR $ pOperator name f

-- | Parse the whole expression with operators
pFullExpr :: Parser HiExpr
pFullExpr = makeExprParser pExpr operatorTable

divOp :: Operator Parser HiExpr
divOp = InfixL $ lexeme $ (wrap HiFunDiv) <$ (try $ string "/" <* notFollowedBy (string "="))

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ infixLFun "*" HiFunMul,
      divOp
    ],
    [ infixLFun "+" HiFunAdd,
      infixLFun "-" HiFunSub
    ],
    [ infixNFun ">=" HiFunNotLessThan,
      infixNFun "<=" HiFunNotGreaterThan,
      infixNFun "<" HiFunLessThan,
      infixNFun ">" HiFunGreaterThan,
      infixNFun "==" HiFunEquals,
      infixNFun "/=" HiFunNotEquals
    ],
    [ infixRFun "&&" HiFunAnd
    ],
    [ infixRFun "||" HiFunOr
    ]
  ]
