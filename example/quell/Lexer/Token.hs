module Lexer.Token (
  T,
  Token (..),
) where

import qualified GHC.Show                   as GHC
import qualified Lexer.TextId as TextId
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Word (Word8)


type T = Token

data Token
  = KwAlias
  | KwAs
  | KwCase
  | KwData
  | KwDefault
  | KwDerive
  | KwDo
  | KwExport
  | KwFamily
  | KwForeign
  | KwImpl
  | KwIn
  | KwInfix
  | KwLet
  | KwLetrec
  | KwModule
  | KwNewtype
  | KwNone
  | KwOf
  | KwPattern
  | KwRec
  | KwRecord
  | KwRole
  | KwSelf
  | KwSignature
  | KwStatic
  | KwTrait
  | KwType
  | KwUnderscore
  | KwUse
  | KwWhen
  | KwWhere

  | LKwDefault
  | LKwSelf

  | SymArrow
  | SymAt
  | SymBang
  | SymColon
  | SymDArrow
  | SymDColon
  | SymDLeftArrow
  | SymDot
  | SymDots
  | SymEqual
  | SymForall
  | SymLambda
  | SymLeftArrow
  | SymOr
  | SymTilde
  | SymUnknown

  | SpBackquote
  | SpBrackOpen
  | SpBrackClose
  | SpComma
  | SpBraceOpen
  | SpBraceClose
  | SpDBraceOpen
  | SpDBraceClose
  | SpParenOpen
  | SpParenClose
  | SpSemi
  | SpVBraceOpen
  | SpVBraceClose
  | SpVDBraceOpen
  | SpVDBraceClose
  | SpVSemi

  | IdConId TextId.T
  | IdConOp TextId.T
  | IdVarId TextId.T
  | IdVarOp TextId.T

  | LitByteChar Word8
  | LitByteString ByteString
  | LitInteger Integer
  | LitRational Rational
  | LitChar Char
  | LitString Text
  | LitInterpStringWithoutInterp Text
  | LitInterpStringStart Text
  | LitInterpStringContinue Text
  | LitInterpStringEnd Text

  | CommentLine Text
  | CommentMultiline Text
  | CommentPragma Text
  | CommentDoc Text
  deriving (Eq, Show)
